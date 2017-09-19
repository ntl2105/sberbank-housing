############################
##### Winning Features######
############################
############################
####industrial_places#######
# incineration_km
# oil_chemistry_km
# water_treatment_km
# ts_km
############################
######distance_to_road######
# ID_big_road1
# mkad_km
# big_road2_km
# ID_big_road2
############################
###School Office and Health ###########
# public_healthcare_km
# workplaces_km
# preschool_km
# additional_education_km
# university_km
# kindergarten_km
############################
####water_and_park##########
# water_km
# green_zone_km
# green_part_500 
#############################
######public transportation##
# railroad_station_walk_km
# railroad_station_avto_min
# railroad_km
# zd_vokzaly_avto_km
#############################
########Sports###############
# sport_count_5000
#############################
########Churches#############
# church_synagogue_km
# big_church_km
# mosque_km 
# big_church_count_5000
#############################
########Cafe Resto###########
# cafe_count_5000_price_high
# cafe_avg_price_2000
# cafe_sum_500_max_price_avg
# cafe_count_500    
#############################
########Building Type########
# build_count_monolith
# build_count_panel
# build_count_1971.1995 
# build_count_after_1995
#############################
########Market########
# market_count_2000
# big_market_km
# market_count_1000
#############################
#######shopping center#######
# trc_sqm_5000
# trc_sqm_3000
# trc_count_3000
#############################
##### Neighborhood ##########
# indust_part
# preschool_quota 
# green_zone_part 
#############################
########demographics########
# young_male
# female_f
# male_f 
# full_all
###########################
###demographics age########
# X0_17_male
# X16_29_female
# X7_14_all
# X7_14_male
###########################
#####life style center#####
# fitness_km
# basketball_km
# ice_rink_km
###########################
####Lifestyle Facilities###
# office_raion
# sport_objects_raion
###########################

library(dplyr)
setwd("~/NYCDSA/Homework/Sberbank Competition")

sch = read.csv("school_office_and_health.csv", header = TRUE)
industrial_fac = read.csv("industrial_facilities.csv", header = TRUE) #y/n variables 
industrial_places = read.csv("industrial_places.csv", header = TRUE) 

cafe_resto = read.csv("cafe_restaurants.csv", header = TRUE)
market = read.csv("market.csv",header = TRUE)
building_type = read.csv("building_type.csv", header = TRUE)
churches = read.csv("churches.csv", header = TRUE)
lifestyle_fac = read.csv("lifestyle_facilities.csv", header = TRUE)
lifestyle_centers= read.csv("life_style_centers.csv", header = TRUE)
park_and_water = read.csv("park_and_water.csv", header = TRUE)
public_transp = read.csv("public_trasportation.csv", header = TRUE)
sports = read.csv("sports.csv", header = TRUE)
shopping_center = read.csv("shopping_center.csv",header = TRUE)

demographics = read.csv("demographics.csv", header = TRUE)
demographics_age = read.csv("demographics_age.csv", header = TRUE)

neighborhood = read.csv("neighborhood char.csv", header = TRUE)
distance_to_road = read.csv("distance_to_road.csv", sep = "\t", row.names = NULL)
#colnames(distance_to_road) = c("Predictor", "Description")
####################################################################  
train = read.csv("train.csv")
###################################################################
colNums <- match(sch$Predictor, names(train))
sch_train = train %>% 
  select(colNums, price_doc) 

colNums1 <- match(industrial_fac$Predictor, names(train))

ind_fac_train = train %>% 
                select(colNums1, price_doc) 

colNums2 = match(industrial_places$Predictor, names(train))
ind_fac_places_train = train %>% 
                select(colNums2, price_doc) 

colNums3 =  match(distance_to_road$Predictor, names(train))
distance_to_road_train = train %>% 
                        select(colNums3, price_doc) 
colNums4 = match(park_and_water$Predictor, names(train))
park_and_water_train = train %>% 
                          select(colNums4, price_doc) 
  
colNums5 = match(public_transp$Predictor, names(train))
public_transp_train = train %>% 
                      select(colNums5, price_doc) 

colNums6 = match(sports$Predictor, names(train))
sports_train = train %>% 
  select(colNums6, price_doc)

colNums7 = match(churches$Predictor, names(train))
churches_train = train %>% 
  select(colNums7, price_doc)


colNums8 = match(cafe_resto$Predictor, names(train)) #doesn't work 
cafe_resto_train = train %>% 
  select(colNums8, price_doc)

colNums9 = match(building_type$Predictor, names(train)) #doesn't work 
building_type_train = train %>% 
                      select(colNums9, price_doc)

colNums10 = match(market$Predictor, names(train))
market_train = train %>% 
              dplyr::select(colNums10, price_doc)

colNums11 = match(shopping_center$Predictor, names(train))
shopping_center_train = train %>% 
                select(colNums11, price_doc)

colNums12 = match(neighborhood$Predictor, names(train))
neighborhood_train = train %>% 
                      select(colNums12, price_doc)

colNums13 = match(demographics$Predictor, names(train))
demographics_train = train %>% 
              select(colNums13, price_doc)

colNums14 = match(demographics_age$Predictor, names(train)) #doesn't work 
demographics_age_train = train %>% 
                      select(colNums14, price_doc)

colNums15 = match(demographics_age$Predictor, names(train)) #doesn't work 
demographics_age_train = train %>% 
                select(colNums15, price_doc)

colNums16 = match(lifestyle_centers$Predictor, names(train)) #doesn't work 
lifestyle_centers_train = train %>% 
                      select(colNums16, price_doc)

colNums17 = match(lifestyle_fac$Predictor, names(train)) #doesn't work 
lifestyle_fac_train = train %>% 
  select(colNums17, price_doc)


# library(plyr)
#industrial_places$Predictor = revalue(industrial_places$Predictor, c("industrial_zone_km"="industrial_km"))
###################################################################

#Creating a training set on 70% of the data.
#set.seed(0)
#train_70 = sample(1:nrow(sch_train), 7*nrow(sch_train)/10)

##########################
#####Regression Trees#####
##########################
#Inspecting the housing values in the suburbs of Boston.
#Creating a training set on 70% of the data.
# set.seed(0)
#train = sample(1:nrow(Boston), 7*nrow(Boston)/10)


################## Random Forest ###################
##################################
#####Bagging & Random Forests#####
##################################
library(randomForest)

#Fitting an initial random forest to the training subset.
set.seed(0)
rf.boston = randomForest(price_doc ~ ., data = sch_train, 
                         importance = TRUE, na.action=na.omit)
rf.boston

#################################################
set.seed(0)
rf.ind_fac = randomForest(price_doc ~ ., data = ind_fac_train, mtry = 8, 
                          importance = TRUE, na.action=na.omit)
rf.ind_fac
#################################################
set.seed(0)
rf.ind_fac_places_train= randomForest(price_doc ~ ., data = ind_fac_places_train, 
                          importance = TRUE, na.action=na.omit)
rf.ind_fac_places_train
#################################################
set.seed(0)
rf.distance_to_road_train= randomForest(price_doc ~ ., data = distance_to_road_train, 
                                      importance = TRUE, na.action=na.omit)
rf.distance_to_road_train
#################################################
set.seed(0)
rf.park_and_water_train= randomForest(price_doc ~ ., data = park_and_water_train, 
                                        importance = TRUE, na.action=na.omit)
rf.park_and_water_train
#################################################
set.seed(0)
rf.public_transp_train= randomForest(price_doc ~ ., data = public_transp_train, 
                                      importance = TRUE, na.action=na.omit)
rf.public_transp_train
#################################################
set.seed(0)
rf.sports_train= randomForest(price_doc ~ ., data = sports_train, 
                                     importance = TRUE, na.action=na.omit)
rf.sports_train
#################################################
set.seed(0)
rf.churches_train= randomForest(price_doc ~ ., data = churches_train, 
                              importance = TRUE, na.action=na.omit)
rf.churches_train 
#################################################
set.seed(0)
rf.cafe_resto_train= randomForest(price_doc ~ ., data = cafe_resto_train, 
                                importance = TRUE, na.action=na.omit)
rf.cafe_resto_train
#################################################
set.seed(0)
rf.markettrain= randomForest(price_doc ~ ., data = market_train, 
                                  importance = TRUE, na.action=na.omit)
rf.markettrain

#################################################
set.seed(0)
rf.shopping_center_train = randomForest(price_doc ~ ., data = shopping_center_train, 
                             importance = TRUE, na.action=na.omit)
rf.shopping_center_train
#################################################
set.seed(0)
rf.neighborhood_train = randomForest(price_doc ~ ., data = neighborhood_train, 
                                        importance = TRUE, na.action=na.omit)
rf.neighborhood_train
#################################################
set.seed(0)
rf.demographics_train = randomForest(price_doc ~ ., data = demographics_train, 
                                     importance = TRUE, na.action=na.omit)
rf.demographics_train
#################################################
set.seed(0)
rf.demographics_age_train = randomForest(price_doc ~ ., data = demographics_age_train, 
                                     importance = TRUE, na.action=na.omit)
rf.demographics_age_train
#################################################
set.seed(0)
rf.building_type_train = randomForest(price_doc ~ ., data = building_type_train, 
                                         importance = TRUE, na.action=na.omit)
rf.building_type_train
#################################################
set.seed(0)
rf.lifestyle_centers_train = randomForest(price_doc ~ ., data = lifestyle_centers_train, 
                                      importance = TRUE, na.action=na.omit)
rf.lifestyle_centers_train 
#################################################
set.seed(0)
rf.lifestyle_fac_train = randomForest(price_doc ~ ., data = lifestyle_fac_train, 
                                          importance = TRUE, na.action=na.omit)
rf.lifestyle_fac_train 
#The MSE and percent variance explained are based on out-of-bag estimates,
#yielding unbiased error estimates. The model reports that mtry = 4, which is
#the number of variables randomly chosen at each split. Since we have 13 overall
#variables, we could try all 13 possible values of mtry. We will do so, record
#the results, and make a plot.

#Varying the number of variables used at each step of the random forest procedure.
# set.seed(0)
# oob.err = numeric(20)
# for (mtry in 1:20) {
#   fit = randomForest(price_doc ~ ., data = sch_train[train, ], mtry = mtry)
#   oob.err[mtry] = fit$mse[500]
#   cat("We're performing iteration", mtry, "\n")
# }
# 
# #Visualizing the OOB error.
# plot(1:13, oob.err, pch = 16, type = "b",
#      xlab = "Variables Considered at Each Split",
#      ylab = "OOB Mean Squared Error",
#      main = "Random Forest OOB Error Rates\nby # of Variables")

#Can visualize a variable importance plot.
importance(rf.boston)
varImpPlot(rf.boston)

importance(rf.ind_fac)
varImpPlot(rf.ind_fac)

importance(rf.ind_fac_places_train)
varImpPlot(rf.ind_fac_places_train)

importance(rf.distance_to_road_train)
varImpPlot(rf.distance_to_road_train)

importance(rf.park_and_water_train)
varImpPlot(rf.park_and_water_train)

importance(rf.public_transp_train)
varImpPlot(rf.public_transp_train)

importance(rf.sports_train)
varImpPlot(rf.sports_train)

importance(rf.churches_train)
varImpPlot(rf.churches_train)

importance(rf.markettrain)
varImpPlot(rf.markettrain)

importance(rf.cafe_resto_train)
varImpPlot(rf.cafe_resto_train)

importance(rf.shopping_center_train)
varImpPlot(rf.shopping_center_train)

importance(rf.neighborhood_train)
varImpPlot(rf.neighborhood_train)

importance(rf.demographics_train)
varImpPlot(rf.demographics_train)

importance(rf.demographics_age_train)
varImpPlot(rf.demographics_age_train)

importance(rf.building_type_train)
varImpPlot(rf.building_type_train)

importance(rf.lifestyle_centers_train )
varImpPlot(rf.lifestyle_centers_train )

importance(rf.lifestyle_fac_train)
varImpPlot(rf.lifestyle_fac_train)


