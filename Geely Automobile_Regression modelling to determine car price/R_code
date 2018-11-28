###################################################################################################
#################################### Geely Automobile Assignment #################################
###################################################################################################

###Problem statement
#1. To understand the variables significant in predicting the price of a car
#2. How well those varilables describe the price of the car

###Methodology
#Using CRISM-DM framework
#Following UpGrads teachings in regression modelling

##Loading libraries
library(tidyverse)
library(stringr)
install.packages("MASS")
install.packages("car")
library("car")
library(MASS)

###Data preparation
Cars_data <- read.csv("CarPrice_Assignment.csv", stringsAsFactors = T)
str(Cars_data)
View(Cars_data)

#Checking for NA values in the data set - 0 values
sum(is.na(Cars_data))

#Checking for duplicate values - 0 values
sum(duplicated(Cars_data))

#Seperating the CarName variable to two new columns
Cars_data <- separate(Cars_data, CarName, c("Car_Company", "Car_Model"), sep = " ")
##Variables to be removed - Car_Model and serial number
Cars_data <- Cars_data[ , -4]
Cars_data <- Cars_data[ ,-1]

#Checking for spelling and casing errors in the coulmns
table(Cars_data$Car_Company)
#Identifying spelling and casing errors in: alfa-romero, maxda, Nissan, porcshce, 
#toyouta, vokswagen and vw
#Changing the above errors accordingly
Cars_data$Car_Company[which(Cars_data$Car_Company == "maxda")] <- "mazda"
Cars_data$Car_Company[which(Cars_data$Car_Company == "alfa-romero")] <- "alfa romeo"
Cars_data$Car_Company[which(Cars_data$Car_Company == "porcshce")] <- "porsche"
Cars_data$Car_Company[which(Cars_data$Car_Company == "toyouta")] <- "toyota"
Cars_data$Car_Company[which(Cars_data$Car_Company == "vokswagen")] <- "volkswagen"
Cars_data$Car_Company[which(Cars_data$Car_Company == "vw")] <- "volkswagen"
Cars_data$Car_Company <- str_to_lower(Cars_data$Car_Company)

Cars_data$Car_Company <- as.factor(Cars_data$Car_Company)

##Treating factors at 2 levels
levels(Cars_data$fueltype) <- c(0,1)
Cars_data$fueltype <- as.numeric(levels(Cars_data$fueltype))[Cars_data$fueltype]

levels(Cars_data$aspiration) <- c(0,1)
Cars_data$aspiration <- as.numeric(levels(Cars_data$aspiration))[Cars_data$aspiration]

levels(Cars_data$doornumber) <- c(0,1)
Cars_data$doornumber <- as.numeric(levels(Cars_data$doornumber))[Cars_data$doornumber]

levels(Cars_data$enginelocation) <- c(0,1)
Cars_data$enginelocation <- as.numeric(levels(Cars_data$enginelocation))[Cars_data$enginelocation]

##Treating factors with more than 2 levels

##Changing symboling to a categorical format
##(-3 to -1) <- Very Safe, (0 to 1) <- Safe, (1, 3) <- Risky
Cars_data2$symboling2 <- cut(Cars_data2$symboling, breaks = c(-3,0,2,4), labels = c("Very Safe", "Safe", "Risky"), right = FALSE)

dummy_drivewheel <- data.frame(model.matrix( ~drivewheel, data = Cars_data))
View(dummy_drivewheel)
dummy_drivewheel <- dummy_drivewheel[,-1]

dummy_CC <- data.frame(model.matrix( ~Car_Company, data = Cars_data))
dummy_CC <- dummy_CC[,-1]

dummy_carbody <- data.frame(model.matrix(~carbody, data = Cars_data))
dummy_carbody <- dummy_carbody[,-1]

dummy_enginetype <- data.frame(model.matrix(~enginetype, data = Cars_data))
dummy_enginetype <- dummy_enginetype[,-1]

dummy_cylindernumber <- data.frame(model.matrix(~cylindernumber, data = Cars_data))
dummy_cylindernumber <- dummy_cylindernumber[,-1]

dummy_fuelsystem <- data.frame(model.matrix(~fuelsystem, data = Cars_data))
dummy_fuelsystem <- dummy_fuelsystem[,-1]

dummy_symboling <- data.frame(model.matrix(~symboling2, data = Cars_data2))
dummy_symboling <- dummy_symboling[,-1]

Cars_data2 <- cbind(Cars_data[, c(-1, -2, -6, -7, -14, -15, -17, -66)], c(dummy_drivewheel, dummy_CC, dummy_carbody, dummy_cylindernumber, dummy_enginetype, dummy_fuelsystem, dummy_symboling))

####Performing EDA to get a sense of the data

ggplot(Cars_data) + geom_point(mapping = aes(x = carbody, y = price, col = Car_Company))
ggplot(Cars_data) + geom_point(mapping = aes(x = citympg, y = price))
ggplot(Cars_data) + geom_point(mapping = aes(x = highwaympg, y = price))
ggplot(Cars_data) + geom_point(mapping = aes(x = enginetype, y = price))
ggplot(Cars_data) + geom_point(mapping = aes(x = enginesize, y = price))

##Training and testing data - Building the model for regression analysis 

set.seed(100)
training_cars_1 <- sample(1:nrow(Cars_data2), 0.7*nrow(Cars_data2))
training_data_1 <- Cars_data2[training_cars_1,]
testing_data_1 <- Cars_data2[-training_cars_1,]
model_cars_1 <- lm(price~., data = training_data_1)
summary(model_cars_1)

##Using StepAIC method on the model_cars_1
step_aic <- stepAIC(model_cars_1, direction = "both")
step_aic


##MODEL 2 - Removing the variables based on stepAIC function
model_cars_2 <- lm(formula = price ~ Car_Companyhonda + Car_Companynissan 
                   + Car_Companymazda + carbodyhardtop + fuelsystemspdi +
                     symboling2Safe + Car_Companyvolkswagen + carbodywagon +
                     carheight + carbodysedan + Car_Companypeugeot + 
                     fuelsystem2bbl + Car_Companyplymouth + Car_Companymitsubishi + 
                     Car_Companyporsche + symboling2Risky + carbodyhatchback
                   + Car_Companydodge + carlength + Car_Companybuick + Car_Companyaudi
                   + enginelocation + Car_Companysaab + peakrpm + curbweight
                   + aspiration + carwidth + enginetypeohcv + stroke + cylindernumbertwelve
                   + cylindernumberfour + cylindernumberthree + boreratio + cylindernumbertwo
                   + Car_Companybmw + enginesize, data = training_data_1)

summary(model_cars_2)
vif(model_cars_2)

##MODEL 3 - Remobing variable Symboling2Safe

model_cars_3 <- lm(formula = price ~ Car_Companyhonda + Car_Companynissan 
                   + Car_Companymazda + carbodyhardtop + fuelsystemspdi +
                    Car_Companyvolkswagen + carbodywagon +
                     carheight + carbodysedan + Car_Companypeugeot + 
                     fuelsystem2bbl + Car_Companyplymouth + Car_Companymitsubishi + 
                     Car_Companyporsche + symboling2Risky + carbodyhatchback
                   + Car_Companydodge + carlength + Car_Companybuick + Car_Companyaudi
                   + enginelocation + Car_Companysaab + peakrpm + curbweight
                   + aspiration + carwidth + enginetypeohcv + stroke + cylindernumbertwelve
                   + cylindernumberfour + cylindernumberthree + boreratio + cylindernumbertwo
                   + Car_Companybmw + enginesize, data = training_data_1)

summary(model_cars_3)

##MODEL 4 - Remobing variable Carbodysedan

model_cars_4 <- lm(formula = price ~ Car_Companyhonda + Car_Companynissan 
                   + Car_Companymazda + carbodyhardtop + fuelsystemspdi +
                     Car_Companyvolkswagen + carbodywagon +
                     carheight + Car_Companypeugeot + 
                     fuelsystem2bbl + Car_Companyplymouth + Car_Companymitsubishi + 
                     Car_Companyporsche + symboling2Risky + carbodyhatchback
                   + Car_Companydodge + carlength + Car_Companybuick + Car_Companyaudi
                   + enginelocation + Car_Companysaab + peakrpm + curbweight
                   + aspiration + carwidth + enginetypeohcv + stroke + cylindernumbertwelve
                   + cylindernumberfour + cylindernumberthree + boreratio + cylindernumbertwo
                   + Car_Companybmw + enginesize, data = training_data_1)

summary(model_cars_4)

##MODEL 5 - Remobing variable Carbodywagon

model_cars_5 <- lm(formula = price ~ Car_Companyhonda + Car_Companynissan 
                   + Car_Companymazda + carbodyhardtop + fuelsystemspdi +
                     Car_Companyvolkswagen +
                     carheight + Car_Companypeugeot + 
                     fuelsystem2bbl + Car_Companyplymouth + Car_Companymitsubishi + 
                     Car_Companyporsche + symboling2Risky + carbodyhatchback
                   + Car_Companydodge + carlength + Car_Companybuick + Car_Companyaudi
                   + enginelocation + Car_Companysaab + peakrpm + curbweight
                   + aspiration + carwidth + enginetypeohcv + stroke + cylindernumbertwelve
                   + cylindernumberfour + cylindernumberthree + boreratio + cylindernumbertwo
                   + Car_Companybmw + enginesize, data = training_data_1)

summary(model_cars_5)

##MODEL 6 - Remobing variable Carbodyhardtop

model_cars_6 <- lm(formula = price ~ Car_Companyhonda + Car_Companynissan 
                   + Car_Companymazda + fuelsystemspdi +
                     Car_Companyvolkswagen +
                     carheight + Car_Companypeugeot + 
                     fuelsystem2bbl + Car_Companyplymouth + Car_Companymitsubishi + 
                     Car_Companyporsche + symboling2Risky + carbodyhatchback
                   + Car_Companydodge + carlength + Car_Companybuick + Car_Companyaudi
                   + enginelocation + Car_Companysaab + peakrpm + curbweight
                   + aspiration + carwidth + enginetypeohcv + stroke + cylindernumbertwelve
                   + cylindernumberfour + cylindernumberthree + boreratio + cylindernumbertwo
                   + Car_Companybmw + enginesize, data = training_data_1)

summary(model_cars_6)

##MODEL 7 - Remobing variable Car_Companyvolkswagen

model_cars_7 <- lm(formula = price ~ Car_Companyhonda + Car_Companynissan 
                   + Car_Companymazda + fuelsystemspdi +
                     carheight + Car_Companypeugeot + 
                     fuelsystem2bbl + Car_Companyplymouth + Car_Companymitsubishi + 
                     Car_Companyporsche + symboling2Risky + carbodyhatchback
                   + Car_Companydodge + carlength + Car_Companybuick + Car_Companyaudi
                   + enginelocation + Car_Companysaab + peakrpm + curbweight
                   + aspiration + carwidth + enginetypeohcv + stroke + cylindernumbertwelve
                   + cylindernumberfour + cylindernumberthree + boreratio + cylindernumbertwo
                   + Car_Companybmw + enginesize, data = training_data_1)

summary(model_cars_7)

##MODEL 8 - Remobing variable Car_Companynissan

model_cars_8 <- lm(formula = price ~ Car_Companyhonda 
                   + Car_Companymazda + fuelsystemspdi +
                     carheight + Car_Companypeugeot + 
                     fuelsystem2bbl + Car_Companyplymouth + Car_Companymitsubishi + 
                     Car_Companyporsche + symboling2Risky + carbodyhatchback
                   + Car_Companydodge + carlength + Car_Companybuick + Car_Companyaudi
                   + enginelocation + Car_Companysaab + peakrpm + curbweight
                   + aspiration + carwidth + enginetypeohcv + stroke + cylindernumbertwelve
                   + cylindernumberfour + cylindernumberthree + boreratio + cylindernumbertwo
                   + Car_Companybmw + enginesize, data = training_data_1)

summary(model_cars_8)

##MODEL 9 - Remobing variable Car_Companymazda

model_cars_9 <- lm(formula = price ~ Car_Companyhonda 
                   + fuelsystemspdi +
                     carheight + Car_Companypeugeot + 
                     fuelsystem2bbl + Car_Companyplymouth + Car_Companymitsubishi + 
                     Car_Companyporsche + symboling2Risky + carbodyhatchback
                   + Car_Companydodge + carlength + Car_Companybuick + Car_Companyaudi
                   + enginelocation + Car_Companysaab + peakrpm + curbweight
                   + aspiration + carwidth + enginetypeohcv + stroke + cylindernumbertwelve
                   + cylindernumberfour + cylindernumberthree + boreratio + cylindernumbertwo
                   + Car_Companybmw + enginesize, data = training_data_1)

summary(model_cars_9)

vif(model_cars_9)

##MODEL 10 - Removing variable fuelsystemspdi

model_cars_10 <- lm(formula = price ~ Car_Companyhonda 
                   + carheight + Car_Companypeugeot + 
                     fuelsystem2bbl + Car_Companyplymouth + Car_Companymitsubishi + 
                     Car_Companyporsche + symboling2Risky + carbodyhatchback
                   + Car_Companydodge + carlength + Car_Companybuick + Car_Companyaudi
                   + enginelocation + Car_Companysaab + peakrpm + curbweight
                   + aspiration + carwidth + enginetypeohcv + stroke + cylindernumbertwelve
                   + cylindernumberfour + cylindernumberthree + boreratio + cylindernumbertwo
                   + Car_Companybmw + enginesize, data = training_data_1)

summary(model_cars_10)

##MODEL 11 - Removing variable symboling2Risky

model_cars_11 <- lm(formula = price ~ Car_Companyhonda 
                    + carheight + Car_Companypeugeot + 
                      fuelsystem2bbl + Car_Companyplymouth + Car_Companymitsubishi + 
                      Car_Companyporsche + carbodyhatchback
                    + Car_Companydodge + carlength + Car_Companybuick + Car_Companyaudi
                    + enginelocation + Car_Companysaab + peakrpm + curbweight
                    + aspiration + carwidth + enginetypeohcv + stroke + cylindernumbertwelve
                    + cylindernumberfour + cylindernumberthree + boreratio + cylindernumbertwo
                    + Car_Companybmw + enginesize, data = training_data_1)

summary(model_cars_11)

##Removing variable Carheight

model_cars_12 <- lm(formula = price ~ Car_Companyhonda 
                     + Car_Companypeugeot + 
                    fuelsystem2bbl + Car_Companyplymouth + Car_Companymitsubishi + 
                      Car_Companyporsche + carbodyhatchback
                    + Car_Companydodge + carlength + Car_Companybuick + Car_Companyaudi
                    + enginelocation + Car_Companysaab + peakrpm + curbweight
                    + aspiration + carwidth + enginetypeohcv + stroke + cylindernumbertwelve
                    + cylindernumberfour + cylindernumberthree + boreratio + cylindernumbertwo
                    + Car_Companybmw + enginesize, data = training_data_1)

summary(model_cars_12)

##Removing variable Car_Companyhonda

model_cars_13 <- lm(formula = price ~  
                    Car_Companypeugeot + 
                      fuelsystem2bbl + Car_Companyplymouth + Car_Companymitsubishi + 
                      Car_Companyporsche + carbodyhatchback
                    + Car_Companydodge + carlength + Car_Companybuick + Car_Companyaudi
                    + enginelocation + Car_Companysaab + peakrpm + curbweight
                    + aspiration + carwidth + enginetypeohcv + stroke + cylindernumbertwelve
                    + cylindernumberfour + cylindernumberthree + boreratio + cylindernumbertwo
                    + Car_Companybmw + enginesize, data = training_data_1)

summary(model_cars_13)

##Removing variable Car_Companyporsche

model_cars_14 <- lm(formula = price ~  
                      Car_Companypeugeot + 
                      fuelsystem2bbl + Car_Companyplymouth + Car_Companymitsubishi 
                       + carbodyhatchback
                    + Car_Companydodge + carlength + Car_Companybuick + Car_Companyaudi
                    + enginelocation + Car_Companysaab + peakrpm + curbweight
                    + aspiration + carwidth + enginetypeohcv + stroke + cylindernumbertwelve
                    + cylindernumberfour + cylindernumberthree + boreratio + cylindernumbertwo
                    + Car_Companybmw + enginesize, data = training_data_1)
summary(model_cars_14)

##Removing variable Car_Companydodge

model_cars_14 <- lm(formula = price ~  
                      Car_Companypeugeot + 
                      fuelsystem2bbl + Car_Companyplymouth + Car_Companymitsubishi 
                    + carbodyhatchback
                     + carlength + Car_Companybuick + Car_Companyaudi
                    + enginelocation + Car_Companysaab + peakrpm + curbweight
                    + aspiration + carwidth + enginetypeohcv + stroke + cylindernumbertwelve
                    + cylindernumberfour + cylindernumberthree + boreratio + cylindernumbertwo
                    + Car_Companybmw + enginesize, data = training_data_1)

summary(model_cars_14)

##Removing variable Car_Companyplymouth

model_cars_15 <- lm(formula = price ~  
                      Car_Companypeugeot + 
                      fuelsystem2bbl + Car_Companymitsubishi 
                    + carbodyhatchback
                    + carlength + Car_Companybuick + Car_Companyaudi
                    + enginelocation + Car_Companysaab + peakrpm + curbweight
                    + aspiration + carwidth + enginetypeohcv + stroke + cylindernumbertwelve
                    + cylindernumberfour + cylindernumberthree + boreratio + cylindernumbertwo
                    + Car_Companybmw + enginesize, data = training_data_1)

summary(model_cars_15)

##Removing variable fuelsystem2bbl

model_cars_16 <- lm(formula = price ~  
                      Car_Companypeugeot + 
                       Car_Companymitsubishi 
                    + carbodyhatchback
                    + carlength + Car_Companybuick + Car_Companyaudi
                    + enginelocation + Car_Companysaab + peakrpm + curbweight
                    + aspiration + carwidth + enginetypeohcv + stroke + cylindernumbertwelve
                    + cylindernumberfour + cylindernumberthree + boreratio + cylindernumbertwo
                    + Car_Companybmw + enginesize, data = training_data_1)

summary(model_cars_16)

##Removing variable Car_Companymitsubishi

model_cars_17 <- lm(formula = price ~  
                      Car_Companypeugeot  
                    + carbodyhatchback
                    + carlength + Car_Companybuick + Car_Companyaudi
                    + enginelocation + Car_Companysaab + peakrpm + curbweight
                    + aspiration + carwidth + enginetypeohcv + stroke + cylindernumbertwelve
                    + cylindernumberfour + cylindernumberthree + boreratio + cylindernumbertwo
                    + Car_Companybmw + enginesize, data = training_data_1)

summary(model_cars_17)


##Removing variable aspiration

model_cars_17 <- lm(formula = price ~  
                      Car_Companypeugeot  
                    + carbodyhatchback
                    + carlength + Car_Companybuick + Car_Companyaudi
                    + enginelocation + Car_Companysaab + peakrpm + curbweight
                    + carwidth + enginetypeohcv + stroke + cylindernumbertwelve
                    + cylindernumberfour + cylindernumberthree + boreratio + cylindernumbertwo
                    + Car_Companybmw + enginesize, data = training_data_1)

summary(model_cars_17)

##Removing variable Car_Companyaudi

model_cars_18 <- lm(formula = price ~  
                      Car_Companypeugeot  
                    + carbodyhatchback
                    + carlength + Car_Companybuick
                    + enginelocation + Car_Companysaab + peakrpm + curbweight
                    + carwidth + enginetypeohcv + stroke + cylindernumbertwelve
                    + cylindernumberfour + cylindernumberthree + boreratio + cylindernumbertwo
                    + Car_Companybmw + enginesize, data = training_data_1)

summary(model_cars_18)

##Removing variable Car_Companysaab

model_cars_19 <- lm(formula = price ~  
                      Car_Companypeugeot  
                    + carbodyhatchback
                    + carlength + Car_Companybuick
                    + enginelocation + peakrpm + curbweight
                    + carwidth + enginetypeohcv + stroke + cylindernumbertwelve
                    + cylindernumberfour + cylindernumberthree + boreratio + cylindernumbertwo
                    + Car_Companybmw + enginesize, data = training_data_1)

summary(model_cars_19)

##Removing variable Car_Companybuick

model_cars_20 <- lm(formula = price ~  
                      Car_Companypeugeot  
                    + carbodyhatchback
                    + carlength
                    + enginelocation + peakrpm + curbweight
                    + carwidth + enginetypeohcv + stroke + cylindernumbertwelve
                    + cylindernumberfour + cylindernumberthree + boreratio + cylindernumbertwo
                    + Car_Companybmw + enginesize, data = training_data_1)

summary(model_cars_20)

##Removing varible peakrpm

model_cars_21 <- lm(formula = price ~  
                      Car_Companypeugeot  
                    + carbodyhatchback
                    + carlength
                    + enginelocation + curbweight
                    + carwidth + enginetypeohcv + stroke + cylindernumbertwelve
                    + cylindernumberfour + cylindernumberthree + boreratio + cylindernumbertwo
                    + Car_Companybmw + enginesize, data = training_data_1)

summary(model_cars_21)

##Remove variable carbodyhatchback

model_cars_22 <- lm(formula = price ~  
                      Car_Companypeugeot  
                    + carlength
                    + enginelocation + curbweight
                    + carwidth + enginetypeohcv + stroke + cylindernumbertwelve
                    + cylindernumberfour + cylindernumberthree + boreratio + cylindernumbertwo
                    + Car_Companybmw + enginesize, data = training_data_1)

summary(model_cars_22)

####**Remove variable carlength

model_cars_23 <- lm(formula = price ~  
                      Car_Companypeugeot  
                    + enginelocation + curbweight
                    + carwidth + enginetypeohcv + stroke + cylindernumbertwelve
                    + cylindernumberfour + cylindernumberthree + boreratio + cylindernumbertwo
                    + Car_Companybmw + enginesize, data = training_data_1)

summary(model_cars_23)


##Remove variable curbweight

model_cars_24 <- lm(formula = price ~  
                      Car_Companypeugeot  
                    + enginelocation
                    + carwidth + enginetypeohcv + stroke + cylindernumbertwelve
                    + cylindernumberfour + cylindernumberthree + boreratio + cylindernumbertwo
                    + Car_Companybmw + enginesize, data = training_data_1)

summary(model_cars_24)

##Remove variable Car_companypeugeot

model_cars_25 <- lm(formula = price ~  
                    aspiration  
                    + enginelocation
                    + carwidth + enginetypeohcv + stroke + cylindernumbertwelve
                    + cylindernumberfour + cylindernumberthree + boreratio + cylindernumbertwo
                    + Car_Companybmw + enginesize, data = training_data_1)

summary(model_cars_25)

##Remove variable cylindernumberfour

model_cars_26 <- lm(formula = price ~  
                    aspiration  
                    + enginelocation
                    + carwidth + enginetypeohcv + stroke + cylindernumbertwelve
                    + cylindernumberthree + boreratio + cylindernumbertwo
                    + Car_Companybmw + enginesize, data = training_data_1)

summary(model_cars_26)

##Remove variable cylindernumberthree

model_cars_27 <- lm(formula = price ~  
                      aspiration  
                    + enginelocation
                    + carwidth + enginetypeohcv + stroke + cylindernumbertwelve
                    + boreratio + cylindernumbertwo
                    + Car_Companybmw + enginesize, data = training_data_1)
summary(model_cars_27)


###Working on the prediction model
Predict_Cars <- predict(model_cars_27, testing_data_1[, -1])
testing_data_1$test_price <- Predict_Cars
r <- cor(testing_data_1$price, testing_data_1$test_price)
rsquared <- cor(testing_data_1$price, testing_data_1$test_price)^2
rsquared


