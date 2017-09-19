########## Sberbank Competition ##########

####Loading libraries####
library(dplyr) #Data manipulation 
library(glmnet) # Lasso Regression 
library(ggplot2) # Data Visualization 
library("purrr")
library(randomForest)

####Loading Dataset#####
train = read.csv("train.csv", header = TRUE)
test = read.csv("test.csv", header = TRUE)

# I. Exploratory Data Analysis 

# I/ a. Identifying variable categories 
split(names(train),sapply(train, function(x) paste(class(x), collapse=" ")))

### Breaking down subareas by frequency of apperance in dataset, 
### with real estate mean and standard deviation
subArea = train %>% 
  group_by(sub_area) %>%
  summarise(Freq = n(), "Mean" = mean(price_doc), "SD" = sd(price_doc), 
            "SD" = (SD/Mean)*100) %>% 
  arrange(SD) # arrange by increasing SDT

subArea %>% 
  filter(Freq < 10) 
subArea %>% 
  filter(Freq > 100) 

#Comments: some sub-areas appear in this dataset way more often than others. e.g. 1611 for 
# "Nekrasovka" and only 2 for "Poselenie Shhapovskoe". 

#I. b. Identifying and Visualizing missing data  

predictors = read.csv("key_features.csv") #pre-selected key features 

colN <- match(predictors$Predictors, names(train))

train_set = train %>% 
  select(colN , price_doc) #trimming down training dataset with only key features 

miss_pct <- map_dbl(train_set, function(x) { round((sum(is.na(x)) / length(x)) * 100, 1) })
#calculating % of missing data by feature 

miss_pct <- miss_pct[miss_pct > 0] #removing non-missing/full features 

#visualizing missing data 
data.frame(miss=miss_pct, var=names(miss_pct), row.names=NULL) %>%
  ggplot(aes(x=reorder(var, -miss), y=miss)) + 
  geom_bar(stat='identity', fill='blue') +
  labs(x='', y='% missing', title='Percent missing data by feature') +
  theme(axis.text.x=element_text(angle=90, hjust=1))

#Comments: build_year, state have the most missing data (~45% are missing), followed by
#kitch_sq, material, max_floor, num_room (> 30%), followed by life_sq (~20%) and build_count_monolith (>15%)
#These features are important predictors for pricing. With this much missingness, some of these 
#features cannot be used for prediction. 

# II. Missing data imputation 
#imputing missing values for feature "material" using random forest 

internal_chars <- c('full_sq', 'life_sq', 'floor', 'build_year', 'num_room', 
                    'kitch_sq', 'state', "material")
colN <- match(internal_chars, names(train))
material_train = train %>% 
  select(colN)
material_test = test  %>% 
  select(colN)
dim(material_test)

#Imputing Missing Values for Material 
set.seed(0)
rf.material = randomForest(material ~ ., data = material_train, ntree = 300, 
                           importance = TRUE, na.action = na.omit)
rf.material

pred = predict(rf.material, newdata = material_test, 
               type="response")
pred 
#Comments: too many missing values in the surrounding feature for this imputation to work ==> Fail to use.


# III. Machine Learning : Feature Selection 
# a. Lasso Regression 
train_trial = train_set
train_trial[is.na(train_trial)] <- 0 #replacing NA with 0
sum(is.na(train_trial)) #no more NAs 

grid = 10^seq(5, -2, length = 100) #setting the lambda values grid 

x = model.matrix(price_doc ~ ., train_trial)[, -1]
y = log(train_trial$price_doc)

dim(x)
length(y)

set.seed(0)
cv.lasso.out = cv.glmnet(x, y,
                         lambda = grid, alpha = 1, nfolds = 10,
                         standardize = TRUE)
plot(cv.lasso.out, main = "Lasso Regression\n")
bestlambda.lasso = cv.lasso.out$lambda.min
bestlambda.lasso
log(bestlambda.lasso)

lasso.models = glmnet(x, y, alpha = 1, lambda = grid)
plot(lasso.models, xvar = "lambda", label = TRUE, main = "Lasso Regression")

# b. Random Forest 
#see Sberbank Part II: RandomForest_Subgroups.R 



