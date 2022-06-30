# Importing the dataset
df = read.csv('housing_clean_2.csv')

#PLOTS FOR ROOM 
boxplot(df$price,
        main = "Price",
        xlab = "Price",
        col = "blue",
        border = "black",
        horizontal = TRUE,
        notch = TRUE
)

hist(df$price, 
     main="Price", 
     xlab="Price", 
     border="black", 
     col="blue",
     xlim=c(3300,329510000),
     las=1, 
     breaks=100)


#find Q1, Q3, and interquartile range for values in column A
Q1 <- quantile(df$price, .25)
Q3 <- quantile(df$price, .75)
IQR <- IQR(df$price)

#only keep rows in dataframe that have values within 1.5*IQR of Q1 and Q3
no_outliers <- subset(df, df$price> (Q1 - 1.5*IQR) & df$price< (Q3 + 1.5*IQR))

#view row and column count of new data frame
dim(no_outliers) 


#PLOTS FOR Price 
boxplot(no_outliers$price,
        main = "Price",
        xlab = "Price",
        col = "blue",
        border = "black",
        horizontal = TRUE,
        notch = TRUE
)

hist(no_outliers$price, 
     main="Price", 
     xlab="Price", 
     border="black", 
     col="blue",
     las=1, 
     breaks=100)

price_density <- density(no_outliers$price)
plot(price_density)
summary(no_outliers$price)
###PRICE IS POSITIVELY SKEWED
#TRANSFORMATION FOR PRICE

#SQRT TRANSFORMATION
norm_price1 <- sqrt(no_outliers$price)
price_density1 <- density(norm_price1)
plot(price_density1)
summary(norm_price1)

library(nortest)
ad.test(norm_price)

#LOG10 TRANSFORMATION
norm_price2 <- log10(no_outliers$price)
price_density2 <- density(norm_price2)
plot(price_density2)
summary(norm_price2)

ad.test(norm_price)
skewness(norm_price)


#no_outliers$price <- sqrt(no_outliers$price)

#REMOVING OUTLIERS AGAIN
#find Q1, Q3, and interquartile range for values in column A
Q1 <- quantile(no_outliers$price, .25)
Q3 <- quantile(no_outliers$price, .75)
IQR <- IQR(no_outliers$price)

#only keep rows in dataframe that have values within 1.5*IQR of Q1 and Q3
no_outliers2 <- subset(no_outliers, no_outliers$price> (Q1 - 1.5*IQR) & no_outliers$price< (Q3 + 1.5*IQR))

#view row and column count of new data frame
dim(no_outliers2) 

#SQRT TRANSFORMATION TO REMOVED OUTLIERS TWICE
norm_price3 <- sqrt(no_outliers2$price)
price_density3 <- density(norm_price3)
plot(price_density3)
summary(norm_price3)

ad.test(norm_price)
skewness(norm_price)


#QQ PLOT
par(mfrow=c(1,1))
qqnorm(norm_price2, main='Price Q-Q')
qqline(norm_price2)
ggqqplot(norm_price2)

boxplot(norm_price)

library(moments)
skewness(no_outliers$price, na.rm = TRUE)
ks.test(norm_price, 'pnorm') 



#VISUALIZATION FOR SPACE
boxplot(no_outliers$space,
        main = "Space",
        xlab = "Space",
        col = "blue",
        border = "black",
        horizontal = TRUE,
        notch = TRUE
)

hist(no_outliers$space, 
     main="Space", 
     xlab="Space", 
     border="black", 
     col="blue",
     las=1, 
     breaks=100)

space_density <- density(no_outliers$space)
plot(space_density)

#OUTLIER REMOVAL FOR SPACE
#find Q1, Q3, and interquartile range for values in column A
Q1 <- quantile(no_outliers$space, .25)
Q3 <- quantile(no_outliers$space, .75)
IQR <- IQR(no_outliers$space)

#only keep rows in dataframe that have values within 1.5*IQR of Q1 and Q3
no_outliers <- subset(no_outliers, no_outliers$space> (Q1 - 1.5*IQR) & no_outliers$space< (Q3 + 1.5*IQR))

boxplot(no_outliers$space,
        main = "Space",
        xlab = "Space",
        col = "blue",
        border = "black",
        horizontal = TRUE,
        notch = TRUE
)

hist(no_outliers$space, 
     main="Space", 
     xlab="Space", 
     border="black", 
     col="blue",
     las=1, 
     breaks=100)

space_density <- density(no_outliers$space)
plot(space_density)

space_norm <- sqrt(no_outliers$space)
space_density <- density(space_norm)
plot(space_density)
summary(space_norm)


no_outliers$space <- sqrt(no_outliers$space)

#VISUALIZATION FOR FLOOR
boxplot(no_outliers$floor,
        main = "Floor",
        xlab = "Flor",
        col = "blue",
        border = "black",
        horizontal = TRUE,
        notch = TRUE
)

hist(no_outliers$floor, 
     main="Floor", 
     xlab="Floor", 
     border="black", 
     col="blue",
     las=1, 
     breaks=100)

floor_density <- density(no_outliers$floor)
plot(floor_density)
summary(no_outliers$floor)


#find Q1, Q3, and interquartile range for values in column A
Q1 <- quantile(no_outliers$floor, .25)
Q3 <- quantile(no_outliers$floor, .75)
IQR <- IQR(no_outliers$floor)

#only keep rows in dataframe that have values within 1.5*IQR of Q1 and Q3
no_outliers <- subset(no_outliers, no_outliers$floor> (Q1 - 1.5*IQR) & no_outliers$floor< (Q3 + 1.5*IQR))

#view row and column count of new data frame
dim(no_outliers) 

#VISUALIZATION FOR FLOOR
boxplot(no_outliers$floor,
        main = "Floor",
        xlab = "Flor",
        col = "blue",
        border = "black",
        horizontal = TRUE,
        notch = TRUE
)

hist(no_outliers$floor, 
     main="Floor", 
     xlab="Floor", 
     border="black", 
     col="blue",
     las=1, 
     breaks=100)

floor_density <- density(no_outliers$floor)
plot(floor_density)
summary(no_outliers$floor)

#VISUALIZATION FOR MAX_FLOOR
boxplot(no_outliers$max_floor,
        main = "Max Floor",
        xlab = "Max Floor",
        col = "blue",
        border = "black",
        horizontal = TRUE,
        notch = TRUE
)

hist(no_outliers$max_floor, 
     main="Max Floor", 
     xlab="Max Floor", 
     border="black", 
     col="blue",
     las=1, 
     breaks=100)

max_floor_density <- density(no_outliers$max_floor)
plot(max_floor_density)
summary(no_outliers$max_floor)

#find Q1, Q3, and interquartile range for values in column A
Q1 <- quantile(no_outliers$max_floor, .25)
Q3 <- quantile(no_outliers$max_floor, .75)
IQR <- IQR(no_outliers$floor)

#only keep rows in dataframe that have values within 1.5*IQR of Q1 and Q3
no_outliers <- subset(no_outliers, no_outliers$max_floor> (Q1 - 1.5*IQR) & no_outliers$max_floor< (Q3 + 1.5*IQR))

#view row and column count of new data frame
dim(no_outliers)


#VISUALIZATION FOR MAX_FLOOR
boxplot(no_outliers$max_floor,
        main = "Max Floor",
        xlab = "Max Floor",
        col = "blue",
        border = "black",
        horizontal = TRUE,
        notch = TRUE
)

hist(no_outliers$max_floor, 
     main="Max Floor", 
     xlab="Max Floor", 
     border="black", 
     col="blue",
     las=1, 
     breaks=100)

max_floor_density <- density(no_outliers$max_floor)
plot(max_floor_density)
summary(no_outliers$max_floor)

#VISUALIZATION FOR ROOM
boxplot(no_outliers$room,
        main = "Room",
        xlab = "Room",
        col = "blue",
        border = "black",
        horizontal = TRUE,
        notch = TRUE
)

hist(no_outliers$room, 
     main="Room", 
     xlab="Room", 
     border="black", 
     col="blue",
     las=1, 
     breaks=100)

room_density <- density(no_outliers$room)
plot(room_density)
summary(no_outliers$room)

#VISUALIZATION FOR BEDROOM
boxplot(no_outliers$bedroom,
        main = "Bedroom",
        xlab = "Bedroom",
        col = "blue",
        border = "black",
        horizontal = TRUE,
        notch = TRUE
)

hist(no_outliers$bedroom, 
     main="Bedroom", 
     xlab="Bedroom", 
     border="black", 
     col="blue",
     las=1, 
     breaks=100)

bedroom_density <- density(no_outliers$bedroom)
plot(bedroom_density)
summary(no_outliers$bedroom)



















library(ggplot2)
library(dplyr)
ggplot(data = no_outliers, aes(x = sqrt(price))) +
        geom_density(fill = 'cyan')

no_outliers$price <- sqrt(no_outliers$price)

#HISTOGRAMS
hist (no_outliers$price, breaks = 50)
hist (no_outliers$room, breaks = 50)
hist (no_outliers$bedroom, breaks = 50)
hist (no_outliers$floor, breaks = 50)
hist (no_outliers$max_floor, breaks = 50)
hist (log10(no_outliers$space), breaks = 50)
hist ((no_outliers$balcony), breaks = 5)

#SCATTER PLOTS
plot(x = no_outliers$room,y = no_outliers$price,
     xlab = "Room",
     ylab = "Price",
)

plot(x = no_outliers$bedroom,y = no_outliers$price,
     xlab = "Bedroom",
     ylab = "Price",
)

plot(x = no_outliers$floor,y = no_outliers$price,
     xlab = "Floor",
     ylab = "Price",
)

plot(x = no_outliers$max_floor,y = no_outliers$price,
     xlab = "Max Floor",
     ylab = "Price",
)

plot(x = no_outliers$balcony,y = no_outliers$price,
     xlab = "Balcony",
     ylab = "Price",
)

plot(x = no_outliers$space,y = no_outliers$price,
     xlab = "Space",
     ylab = "Price",
)

plot(x = no_outliers$longitude,y = no_outliers$price,
     xlab = "Longitude",
     ylab = "Price",
)

plot(x = no_outliers$latitude,y = no_outliers$price,
     xlab = "Latitude",
     ylab = "Price",
)

ggplot(no_outliers, aes(x = city_area)) +
        geom_density(aes(color = city_area))

ggplot(no_outliers, aes(x = apartment_type)) +
        geom_density(aes(color = apartment_type))

ggplot(no_outliers, aes(x = renovation_type)) +
        geom_density(aes(color = renovation_type))

ggboxplot(no_outliers, x = "room", y = "price", 
           palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          ylab = "Price", xlab = "Room")

ggboxplot(no_outliers, x = "bedroom", y = "price", 
          palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          ylab = "Price", xlab = "Bedroom")

ggboxplot(no_outliers, x = "city_area", y = "price", 
          palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          ylab = "Price", xlab = "City Area")


ggboxplot(no_outliers, x = "apartment_type", y = "price", 
          palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          ylab = "Price", xlab = "Apartment Type")


ggboxplot(no_outliers, x = "renovation_type", y = "price", 
          palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          ylab = "Price", xlab = "Renovation Type")

ggboxplot(no_outliers, x = "balcony", y = "price", 
          palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          ylab = "Price", xlab = "Balcony")



#T-TEST FOR BALCONY
#t.test(no_outliers$price, no_outliers$balcony)
kruskal.test(price ~ balcony, data = no_outliers)
kruskal.test(price ~ apartment_type, data = no_outliers)
kruskal.test(price ~ city_area, data = no_outliers)
kruskal.test(price ~ floor, data = no_outliers)
kruskal.test(price ~ max_floor, data = no_outliers)
kruskal.test(price ~ bedroom, data = no_outliers)
kruskal.test(price ~ renovation_type, data = no_outliers)
#p-value < 2.2e-16 THE RESULT IS SIGNIFFICANT

#CORRELATION
cor(no_outliers$price, no_outliers$space)
cor(no_outliers$price, no_outliers$longitude)
cor(no_outliers$price, no_outliers$latitude)
#STRONG


library(corrplot)
corrplot(cor(dataf[,c(1:7,9:10,13:31)]), method="color")

corrplot(cor(no_outliers[,c(2:8,10:11,14)]), method = "number")

library("Hmisc")

corrplot(dataf[,c(1:7,9:10,13:31)])
cor(dataf[,c(1:7,9:10,13:31)])


#####MODEL BUILDING
##MULTIPLE REGRESSION
#install.packages(c("fastDummies", "recipes"))

library(caTools)
library(car)
library(quantmod)
library(MASS)
library(corrplot)
library(fastDummies)  
dataf <- dummy_cols(no_outliers, select_columns = c('renovation_type', 'city_area', 'renovation_type'))

lmFit <- lm(price ~ room+bedroom+floor+max_floor+space+city_area+renovation_type+balcony ,no_outliers)

summary(lmFit)
par(mfrow=c(2,2))
plot(lmFit)
vif(lmFit)

lmFit2 <- lm(price ~ bedroom+floor+max_floor+space+city_area+renovation_type+
                     balcony+longitude+latitude, data = no_outliers )


summary(lmFit2)
summary(lmFit2)
par(mfrow=c(2,2))
plot(lmFit2)
vif(lmFit2)

colnames(dataf)

dataf <- subset(dataf , select = -X)



# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(no_outliers$price, SplitRatio = 0.75)
training_set = subset(no_outliers, split == TRUE)
test_set = subset(no_outliers, split == FALSE)

# data <- na.omit(dataf) 
dataf = dataf[complete.cases(dataf), ]   

# Fitting Multiple Linear Regression to the Training set
regressor = lm(formula = price ~ ., data = no_outliers)
summary(regressor)

summary(regressor)
par(mfrow=c(2,2))
plot(regressor)
vif(regressor)

# Predicting the Test set results
y_pred = predict(regressor, newdata = test_set)


mean((test_set[,2] - y_pred)^2) #mse
caret::MAE(test_set[,2], y_pred) #mae
caret::RMSE(test_set[,2], y_pred) #rmse
caret::R2(test_set[,2], y_pred)



#DECISION TREE REGRESSION
library(rpart)
dt <- rpart(formula = price ~ .,
            data = no_outliers)
# Plotting the tree
plot(dt)
text(dt)
print(dt)

p <- predict(dt, training_set)
print(cor(training_set$price, p)^2)

printcp(dt)
print(cor(training_set$price, p)^2)
# taking the object from the question:
actual <- no_outliers$price
predicted <- unname(predict(dt, no_outliers))

R2 <- 1 - (sum((actual-predicted)^2)/sum((actual-mean(actual))^2))
R2
caret::RMSE(predicted,actual)
caret::MAE(predicted,actual)
caret::R2(predicted,actual)

####RANDOM FOREST

# Install the required package for function
#install.packages("randomForest")
# Load the library
library(randomForest)

# Create random forest for regression
no_outliers = no_outliers[complete.cases(no_outliers), ]   

rf <- randomForest(price ~ floor+max_floor+space+city_area+renovation_type+
                           balcony+longitude+latitude, data = no_outliers, importance = TRUE)

print(rf)
round(importance(rf), 2)
p <- predict(rf, training_set)
print(cor(training_set$price, p)^2)
summary(rf)
# taking the object from the question:
actual <- no_outliers$price
predicted <- unname(predict(rf, no_outliers))

R2 <- 1 - (sum((actual-predicted)^2)/sum((actual-mean(actual))^2))
R2
caret::MAE(predicted,actual)
caret::RMSE(predicted,actual)
caret::R2(predicted,actual)

# package to perform data manipulation
# and visualization
#install.packages("tidyverse")
library(tidyverse)

# package to compute
# cross - validation methods
library(ggplot2)
library(lattice)
library(caret)
# R program to implement
# Leave one out cross validation

# defining training control
# as Leave One Out Cross Validation
# train_control <- trainControl(method = "LOOCV")
# 
# # training the model by assigning sales column
# # as target variable and rest other column
# # as independent variable
# model <- train(price ~., data = dataf,
#                method = "lm",
#                trControl = train_control)
# 
# # printing model performance metrics
# # along with other details
# print(model)


# R program to implement
# K-fold cross-validation

# setting seed to generate a
# reproducible random sampling
set.seed(125)

# defining training control
# as cross-validation and
# value of K equal to 10
train_control <- trainControl(method = "cv",
                              number = 10)

# training the model by assigning sales column
# as target variable and rest other column
# as independent variable
model <- train(price ~., data = no_outliers,
               method = "lm",
               trControl = train_control)

# printing model performance metrics
# along with other details
print(model)

set.seed(125)

# defining training control
# as cross-validation and
# value of K equal to 10
train_control <- trainControl(method = "cv",
                              number = 10)

# training the model by assigning sales column
# as target variable and rest other column
# as independent variable
model <- train(price ~., data = no_outliers,
               method = "ranger",
               trControl = train_control)

# printing model performance metrics
# along with other details
print(model)


####LASSO

#define response variable
y <- dataf$price

#define matrix of predictor variables
x <- (subset(dataf , select = -price))
x <- (subset(x , select = -apartment_type))
x <- (subset(x , select = -renovation_type))
x <- (subset(x , select = -city_area))

x <- as.matrix(x)
#install.packages("glmnet")
library(glmnet)

#perform k-fold cross-validation to find optimal lambda value
cv_model <- cv.glmnet(x, y, alpha = 1)

#find optimal lambda value that minimizes test MSE
best_lambda <- cv_model$lambda.min
best_lambda

#produce plot of test MSE by lambda value
plot(cv_model) 

#find coefficients of best model
best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
coef(best_model)

#use fitted best model to make predictions
y_predicted <- predict(best_model, s = best_lambda, newx = x)

#find SST and SSE
sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)

#find R-Squared
rsq <- 1 - sse/sst
rsq
caret::RMSE(y_predicted,y)
caret::R2(y, y_predicted)


####XGBoost
library(xgboost) #for fitting the xgboost model
library(caret)   #for general data preparation and model fitting

split = sample.split(no_outliers$price, SplitRatio = 0.75)
training_set = subset(no_outliers, split == TRUE)
test_set = subset(no_outliers, split == FALSE)


#make this example reproducible
set.seed(0)


#define predictor and response variables in training set
train_x = data.matrix(training_set[, -2])
train_y = training_set[,2]

#define predictor and response variables in testing set
test_x = data.matrix(test_set[, -2])
test_y = test_set[, 2]

#define final training and testing sets
xgb_train = xgb.DMatrix(data = train_x, label = train_y)
xgb_test = xgb.DMatrix(data = test_x, label = test_y)

#define watchlist
watchlist = list(train=xgb_train, test=xgb_test)

#fit XGBoost model and display training and testing data at each round
model = xgb.train(data = xgb_train, max.depth = 3, watchlist=watchlist, nrounds = 70)

#define final model
final = xgboost(data = xgb_train, max.depth = 3, nrounds = 56, verbose = 0)

pred_y = predict(final, xgb_test)

mean((test_y - pred_y)^2) #mse
caret::MAE(test_y, pred_y) #mae
caret::RMSE(test_y, pred_y) #rmse
caret::R2(test_y, pred_y)
