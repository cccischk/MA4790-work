# R Program for MA4790/5790 Predictve Modeling
# Chapter 1 - Introduction

# set working folder/direcory

setwd("C:/School/Fall 2026/MA4790/Grant Applications")

# load packages
library("AppliedPredictiveModeling")

# Grant Application Data
# This data is available from csv file
gapp_exp <- read.csv("unimelb_example.csv")
gapp_train <- read.csv("unimelb_training.csv")
gapp_test <- read.csv("unimelb_test.csv")
# check if they are data frame
is.data.frame(gapp_train)
is.data.frame(gapp_test)
# check dimension
dim(gapp_train)
dim(gapp_test)
# first and last few samples
head(gapp_train)
tail(gapp_train)
# first few lines and column
gapp_train[1:10, 1:6]
# summary of Data
summary(gapp_train)
summary(gapp_train[ , 1:10])
# response
table(gapp_train$Grant.Status)
is.factor(gapp_train$Grant.Status)
is.numeric(gapp_train$Grant.Status)
# some predictors
gapp_train$Country.of.Birth.1
names(gapp_train)
gapp_train$Country.of.Birth.1[1:20]
gapp_train[1:20, 30]

# Chemical Manufacturing
# This data is available in AppliedPredictiveModeling package
data(ChemicalManufacturingProcess)
# check if it is data frame
is.data.frame(ChemicalManufacturingProcess)
# check dimension
dim(ChemicalManufacturingProcess)
# first and last few samples
head(ChemicalManufacturingProcess)
tail(ChemicalManufacturingProcess)
# first few lines and column
ChemicalManufacturingProcess[1:10, 1:10]
# summary of Data
summary(ChemicalManufacturingProcess)
summary(ChemicalManufacturingProcess[ , 1:10])
# response
hist(ChemicalManufacturingProcess$Yield,
	main = "", xlab = "Yield")
# some predictors
names(ChemicalManufacturingProcess)
ChemicalManufacturingProcess$BiologicalMaterial02
hist(ChemicalManufacturingProcess$BiologicalMaterial02,
	main = "", xlab = "Biological Material")
ChemicalManufacturingProcess$ManufacturingProcess38
table(ChemicalManufacturingProcess$ManufacturingProcess38)
is.factor(ChemicalManufacturingProcess$ManufacturingProcess38)
is.numeric(ChemicalManufacturingProcess$ManufacturingProcess38)
