# Question 1

# Reading the Datasets

house_train <- read.csv("~/Desktop/Data Science 808W/Datasets/house_train.csv")
View(house_train)
house_test <- read.csv("~/Desktop/Data Science 808W/Datasets/house_test.csv")
View(house_test)

# Question 2

# Checking the missing values

sum(is.na(house_test))
sum(is.na(house_train))

# Checking the levels

levels(house_train$state)

# Making linear models

fit.lm1 <- lm(formula = price2013 ~ state, data = house_train)
View(fit.lm1$coefficients)

# Summarizing and finding max and min values in the model

max(fit.lm1$coefficients)
min(fit.lm1$coefficients)
fit.lm_sm1 <- summary(fit.lm1)
(fit.lm_sm1)

# Sorting the model to find the max and min values

fit.lm1_decre1 <- sort(fit.lm1$coefficients, decreasing = T)
fit.lm1_decre1
fit.lm1_incre1 <- sort(fit.lm1$coefficients, decreasing = F)
fit.lm1_incre1

# Using the formula y = mx + c to predict the average price of housing

DC_avg <- fit.lm1$coefficients[1] + fit.lm1_decre1[1]
DC_avg
WV_avg <- fit.lm1$coefficients[1] + fit.lm1_incre1[1]
WV_avg

# Finding the mean values of housing in Dc and WV state from training dataset

DC <- (house_train$price2007[house_train$state == "DC"])
View(DC)
mean(DC)
mean(house_train$price2007[house_train$state == "DC"])
WV <- (house_train$price2007[house_train$state == "WV"])
View(WV)
mean(WV)
mean(house_train$price2007[house_train$state == "WV"])

# Question 3

# Making linear model with state and county information

fit.lm2 <- lm(formula = price2013 ~ state + county, data = house_train)

# Exploring the model

fit.lm2$coefficients
attributes(fit.lm2)
max(fit.lm2$coefficients, na.rm = T)
min(fit.lm2$coefficients, na.rm = T)
fit.lm_sm2 <- summary(fit.lm2)
fit.lm_sm2

# Sorting the model

fit.lm1_decre2 <- sort(fit.lm2$coefficients, decreasing = T)
View(fit.lm1_decre2)
fit.lm1_incre2 <- sort(fit.lm2$coefficients, decreasing = F)
View(fit.lm1_incre2)

# Question 4

# Making different models

fit.lm3 <- lm(formula = price2013 ~ price2007 + state + county + poverty, data = house_train)
fit.lm_sm3 <- summary(fit.lm3)

fit.lm4 <- lm(formula = price2013 ~ county + poverty, data = house_train)
fit.lm_sm4 <- summary(fit.lm4)

fit.lm5 <- lm(formula = price2013 ~ price2007 + poverty, data = house_train)
fit.lm_sm5 <- summary(fit.lm5)

fit.lm6 <- lm(formula = price2013 ~ price2007 + poverty + county, data = house_train)
fit.lm_sm6 <- summary(fit.lm6)

fit.lm7 <- lm(formula = price2013 ~ price2007 + county, data = house_train)
fit.lm_sm7 <- summary(fit.lm7)

fit.lm8 <- lm(formula = price2013 ~ zip + price2007 + state + county + poverty, data = house_train)
fit.lm_sm8 <- summary(fit.lm8)


# Making the levels of county same in trained Linear Regression model and test data

fit.lm3$xlevels[["county"]] <- union(fit.lm3$xlevels[["county"]], levels(house_test$county))
fit.lm4$xlevels[["county"]] <- union(fit.lm4$xlevels[["county"]], levels(house_test$county))
fit.lm6$xlevels[["county"]] <- union(fit.lm6$xlevels[["county"]], levels(house_test$county))
fit.lm7$xlevels[["county"]] <- union(fit.lm7$xlevels[["county"]], levels(house_test$county))
fit.lm8$xlevels[["county"]] <- union(fit.lm8$xlevels[["county"]], levels(house_test$county))

# Predicting the housing price for 2007 using linear models

p1 <- predict(fit.lm1, house_test)
p2 <- predict(fit.lm2, house_test)
p3 <- predict(fit.lm3, house_test)
p4 <- predict(fit.lm4, house_test)
p5 <- predict(fit.lm5, house_test)
p6 <- predict(fit.lm6, house_test)
p7 <- predict(fit.lm7, house_test)
p8 <- predict(fit.lm8, house_test)

# Writing predictions into a csv file

pr1 <- cbind(house_test$id, p1)
colnames(pr1) <- c("id", "prediction")
View(pr1)
write.csv(pr1, "Prediction Model 1.csv")
pr2 <- cbind(house_test$id, p2)
colnames(pr2) <- c("id", "prediction")
View(pr2)
write.csv(pr2, "Prediction Model 2.csv")
pr3 <- cbind(house_test$id, p3)
colnames(pr3) <- c("id", "prediction")
View(pr3)
write.csv(pr3, "Prediction Model 3.csv")
pr4 <- cbind(house_test$id, p4)
colnames(pr4) <- c("id", "prediction")
View(pr4)
write.csv(pr4, "Prediction Model 4.csv")
pr5 <- cbind(house_test$id, p5)
colnames(pr5) <- c("id", "prediction")
View(pr5)
write.csv(pr5, "Prediction Model 5.csv")
pr6 <- cbind(house_test$id, p6)
colnames(pr6) <- c("id", "prediction")
View(pr6)
write.csv(pr6, "Prediction Model 6.csv")
pr7 <- cbind(house_test$id, p7)
colnames(pr7) <- c("id", "prediction")
View(pr7)
write.csv(pr7, "Prediction Model 7.csv")

pr8 <- cbind(house_test$id, p8)
colnames(pr8) <- c("id", "prediction")
View(pr8)
write.csv(pr8, "Prediction Model 8.csv")

