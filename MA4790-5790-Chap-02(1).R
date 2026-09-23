# R Program for MA4790/5790 Predictve Modeling
# Chapter 2 - A Short Tour of the Predictive Modeling Process

# set working folder/direcory
setwd("G:/My Drive/Zkui/Teaching/MA5790/data")

# load packages
library(AppliedPredictiveModeling)
library(caret)
library(earth)

#-------------------------------------------------------------------------
# Fuel Economy Data
#-------------------------------------------------------------------------
# load fuel economy data
data(FuelEconomy)
# step 1: look at data first
dim(cars2010) # cars in 2010
dim(cars2011) # cars in 2011 not in 2010
dim(cars2012) # cars in 2012 not in 2010 & 2011
names(cars2010)
head(cars2010)
summary(cars2010)
# look at some predictors
table(cars2010$NumCyl)
table(cars2010$Transmission)
table(cars2010$TransLockup)
table(cars2010$VarValveLift)
table(cars2010$IntakeValvePerCyl)

# combine 2010 and 2011 data for illustration
cars2010 <- cars2010[order(cars2010$EngDispl),]
cars2011 <- cars2011[order(cars2011$EngDispl),]
cars2010a <- cars2010
cars2010a$Year <- "2010 Model Year"
cars2011a <- cars2011
cars2011a$Year <- "2011 Model Year"
plotData <- rbind(cars2010a, cars2011a)
head(plotData)

#-------------------------------------------------------------------------
# Step 1: understand your data 
#-------------------------------------------------------------------------
# dimension of data
dim(cars2010)
dim(cars2011)
#-------------------------------------------------------------------------
# outcome
res <- data.frame(year = c(2010, 2011),
	mean = c(mean(cars2010$FE), mean(cars2011$FE)),
	sd = c(sd(cars2010$FE), sd(cars2011$FE)) )
res
# histgram
# Figure 2.S.1
par(mfrow = c(2, 2))
hist(cars2010$FE, main = "2010", freq = FALSE,
	xlab = "Fuel Efficiency (MPG)", 
	ylab = ("Density") )
hist(cars2011$FE, main = "2011", freq = FALSE,
	xlab = "Fuel Efficiency (MPG)", 
	ylab = ("Density") )
#-------------------------------------------------------------------------	
# predictor
res <- data.frame(year = c(2010, 2011),
	mean = c(mean(cars2010$EngDispl), mean(cars2011$EngDispl)),
	sd = c(sd(cars2010$EngDispl), sd(cars2011$EngDispl)) )
res
# Figure 2.S.2
par(mfrow = c(2, 2))
hist(cars2010$EngDispl, main = "2010", freq = FALSE,
	xlab = "Engine Displacement", 
	ylab = ("Density") )
hist(cars2011$EngDispl, main = "2011", freq = FALSE,
	xlab = "Engine Displacement", 
	ylab = ("Density") )
#-------------------------------------------------------------------------	
# outcome vs predictor
# Figure 2.1
xyplot(FE ~ EngDispl|Year, plotData,
	xlab = "Engine Displacement",
	ylab = "Fuel Efficiency (MPG)",
	between = list(x = 1.2))
# another way for this plot
# Figure 2.S.3
par(mfrow = c(2, 2))
plot(FE ~ EngDispl, data = cars2010,
	xlab = "Engine Displacement",
	ylab = "Fuel Efficiency (MPG)",
	main = "2010",
	xlim = c(0, 8), ylim = c(15, 70) )
plot(FE ~ EngDispl, data = cars2011,
	xlab = "Engine Displacement",
	ylab = "Fuel Efficiency (MPG)",
	main = "2011",
	xlim = c(0, 8), ylim = c(15, 70) )
	
#-------------------------------------------------------------------------
# Step 5: Model Building and Evaluation – Find a Model
#-------------------------------------------------------------------------
# linear model
m1 <- lm(FE ~ EngDispl, data = cars2010)
summary(m1)
m1.rmse <- sqrt(sum(m1$res^2) / length(m1$res))
m1.rmse
# Figure 2.S.4/2.2
par(mfrow = c(2, 2))
plot(FE ~ EngDispl, data = cars2010,
	xlab = "Engine Displacement",
	ylab = "Fuel Economy", ylim = c(15, 70), 
	main = "" ) 
abline(m1, col = "RED", lwd = 2)
plot(FE ~ predict(m1), data = cars2010,
	xlab = "Predicted", 	
	ylab = "Observed", ylim = c(10, 70),
	main = "")
abline(a = 0, b = 1, col = "RED", lwd = 2)
# fit linear model and conduct 10-fold CV
set.seed(1)
m1.fit <- train(FE ~ EngDispl,
	data = cars2010,
    method = "lm", 
	trControl = trainControl(method = "cv"))
m1.fit
# understand S3 object
class(m1)
class(m1.fit)
is.list(m1)
is.list(m1.fit)
names(m1)
names(m1.fit)
m1.fit$pred # which null
cbind(predict(m1), m1$fitted.values, predict(m1.fit) )
#-------------------------------------------------------------------------
# linear model with a quadratic term 
# add quadratic term to data
cars2010$ED2 <- cars2010$EngDispl^2
cars2011$ED2 <- cars2011$EngDispl^2
# model fit
m2 <- lm(FE ~ EngDispl + ED2, data = cars2010)
summary(m2)
m2.rmse <- sqrt(sum(m2$res^2) / length(m2$res))
m2.rmse
# Figure 2.S.5/2.3
par(mfrow = c(2, 2))
plot(FE ~ EngDispl, data = cars2010,
	xlab = "Engine Displacement",
	ylab = "Fuel Economy", ylim = c(15, 70), 
	main = "" ) 
m2.x <- cars2010$EngDispl # note that it is already ordered from smallest to largest
m2.y <- predict(m2)
lines(m2.y ~ m2.x, type = "l", col = "RED", lwd = 2)
plot(FE ~ predict(m2), data = cars2010,
	xlab = "Predicted",
	ylab = "Observed", ylim = c(15, 70),
	main = "")
abline(a = 0, b = 1, col = "RED", lwd = 2)
# fit linear model and conduct 10-fold CV
set.seed(1)
m2.fit <- train(FE ~ EngDispl + ED2,
	data = cars2010,
    method = "lm", 
	trControl = trainControl(method = "cv"))
m2.fit
#-------------------------------------------------------------------------
# a MARS model (via the earth package)
set.seed(1)
mars.fit <- train(FE ~ EngDispl, 
	data = cars2010,
	method = "earth",
	tuneLength = 15,
	trControl = trainControl(method= "cv"))
mars.fit
# Figure 2.S.6/2.5
par(mfrow = c(2, 2))
plot(FE ~ EngDispl, data = cars2010,
	xlab = "Engine Displacement",
	ylab = "Fuel Economy", ylim = c(10, 72), 
	main = "" ) 
m2.x <- cars2010$EngDispl # note that it is already ordered from smallest to largest
m2.y <- predict(mars.fit)
lines(m2.y ~ m2.x, type = "l", col = "RED", lwd = 2)
plot(FE ~ predict(mars.fit), data = cars2010,
	xlab = "Predicted",	
	ylab = "Observed", ylim = c(15, 70),
	main = "")
abline(a = 0, b = 1, col = "RED", lwd = 2)

#-------------------------------------------------------------------------
# Step 6: Model Building and Evaluation – Final Model
#-------------------------------------------------------------------------
# compare quadratic and MARS model with testing data
# Figure 2.S.7/2.6 
par(mfrow = c(2, 2))
plot(FE ~ EngDispl, data = cars2011,
	xlab = "Engine Displacement",
	ylab = "Fuel Economy", ylim = c(10, 72), 
	main = "MARS" ) 
m2.x <- cars2011$EngDispl # note that it is already ordered from smallest to largest
m2.y <- predict(mars.fit, newdata = cars2011)
test.rmse <- sqrt(sum((m2.y - cars2011$FE)^2) / length(m2.y))
test.rmse
lines(m2.y ~ m2.x, type = "l", col = "RED", lwd = 2)
plot(FE ~ EngDispl, data = cars2011,
	xlab = "Engine Displacement",
	ylab = "Fuel Economy", ylim = c(10, 72), 
	main = "Quadratic" ) 
m2.x <- cars2011$EngDispl # note that it is already ordered from smallest to largest
m2.y <- predict(m2, newdata = cars2011)
lines(m2.y ~ m2.x, type = "l", col = "RED", lwd = 2)
test.rmse <- sqrt(sum((m2.y - cars2011$FE)^2) / length(m2.y))
test.rmse

