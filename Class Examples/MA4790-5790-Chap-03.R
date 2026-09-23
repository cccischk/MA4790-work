# R Program for MA4790/5790 Predictve Modeling
# Chapter 3 - Data Pre-processing

# set working folder/direcory
setwd("G:/My Drive/Zkui/Teaching/MA5790/data")

# load packages
library(AppliedPredictiveModeling)
library(caret)
library(earth)
library(e1071)

#-------------------------------------------------------------------------
# Cell Segmentation Data
#-------------------------------------------------------------------------
# load cell segmentation data
data(segmentationOriginal)
# look at data first
dim(segmentationOriginal)
names(segmentationOriginal)
# outcome for traning and test
table(segmentationOriginal$Case)
table(segmentationOriginal$Class)
table(segmentationOriginal[, 2:3])
#-------------------------------------------------------------------------
# retain the original training set
segTrain <- subset(segmentationOriginal, Case == "Train")
# remove the first three columns (identifier columns)
segTrainX <- segTrain[, -(1:3)]
segTrainClass <- segTrain$Class

#-------------------------------------------------------------------------
# Skewness
#-------------------------------------------------------------------------
# Figure 3.2/3.S.1
par(mfrow = c(2, 2))
hist(segTrainX$VarIntenCh3, xlab = "Natural Units",
	ylab = "Count", main = "")
hist(log(segTrainX$VarIntenCh3), xlab = "Log Units",
	ylab = "Count", main = "")
#-------------------------------------------------------------------------
# skewness
max(segTrainX$VarIntenCh3)/min(segTrainX$VarIntenCh3)	
skewness(segTrainX$VarIntenCh3)	
skewness(log(segTrainX$VarIntenCh3))	
#-------------------------------------------------------------------------
# Box-Cox transformation
# use caret's preProcess function to transform for skewness
segPP <- preProcess(segTrainX, method = "BoxCox")
# understand output
is.list(segPP)
summary(segPP)
summary(segPP$bc)
segPP$bc[[1]]
class(segPP$bc[[1]])
names(segPP$bc[[1]])
## apply the transformations
segTrainTrans <- predict(segPP, segTrainX)
#-------------------------------------------------------------------------
# Figure 3.3/3.S.2
par(mfrow = c(2, 2))
hist(segTrainX$PerimCh1, xlab = "Natural Units",
	ylab = "Count", main = "")
hist(segTrainTrans$PerimCh1, xlab = "Transformed Units",
	ylab = "Count", main = "")

#-------------------------------------------------------------------------
# Section 3.3 - PCA
#-------------------------------------------------------------------------
# R's prcomp is used to conduct PCA
pr <- prcomp(~ AvgIntenCh1 + EntropyIntenCh1,
	data = segTrainTrans, 
	scale. = TRUE)
# correlation of coefficient
cor(segTrainTrans$AvgIntenCh1, segTrainTrans$EntropyIntenCh1)
# Figure 3.5
par(mfrow = c(2, 2))
transparentTheme(pchSize = .7, trans = .3)
xyplot(AvgIntenCh1 ~ EntropyIntenCh1,
	data = segTrainTrans,
	groups = segTrain$Class,
	xlab = "Channel 1 Fiber Width",
	ylab = "Intensity Entropy Channel 1",
	auto.key = list(columns = 2),
	type = c("p", "g"),
	main = "Original Data",
	aspect = 1)
xyplot(PC2 ~ PC1,
	data = as.data.frame(pr$x),
	groups = segTrain$Class,
	xlab = "Principal Component #1",
	ylab = "Principal Component #2",
	main = "Transformed",
	xlim = extendrange(pr$x),
	ylim = extendrange(pr$x),
	type = c("p", "g"),
	aspect = 1)
#-------------------------------------------------------------------------	
# apply PCA to the entire set of predictors.
# remove predictors with only a single value
isZV <- apply(segTrainX, 2, function(x) length(unique(x)) == 1)
segTrainX <- segTrainX[, !isZV]
segPP <- preProcess(segTrainX, c("BoxCox", "center", "scale"))
segTrainTrans <- predict(segPP, segTrainX)
# PCA for all predictors
segPCA <- prcomp(segTrainTrans, center = TRUE, scale. = TRUE)
#-------------------------------------------------------------------------	
# choose number of PCs
res <- summary(segPCA)$importance
res
numPC <- 1 : ncol(res)
# Figure 3.6
plot(res[2, ] ~ numPC, main = "", type = "l",
	xlab = "Component",
	ylab = "Propotion of Total Variance")
# Figure 3.S.3
plot(res[3, ] ~ numPC, main = "", type = "l",
	xlab = "Component",
	ylab = "Accumulated Propotion of Total Variance")
# compare with average Variance
which(res[1, ]^2 >= (sum(res[1, ]^2) / ncol(res)))
which(res[1, ]^2 >= 1)
#-------------------------------------------------------------------------	
# plot a scatterplot matrix of the first three components
# Figure 3.7
transparentTheme(pchSize = .8, trans = .3)
panelRange <- extendrange(segPCA$x[, 1:3])
splom(as.data.frame(segPCA$x[, 1:3]),
      groups = segTrainClass,
      type = c("p", "g"),
      as.table = TRUE,
      auto.key = list(columns = 2),
      prepanel.limits = function(x) panelRange)

#-------------------------------------------------------------------------
# Section 3.4 - Dealing with Missing Data 
#-------------------------------------------------------------------------
#-------------------------------------------------------------------------
# data from chemical manufacturing process
data(ChemicalManufacturingProcess)
cmp <- ChemicalManufacturingProcess
# find portation of missing data
missSam <- apply(is.na(cmp), FUN = mean, MARGIN = 1)
missPre <- apply(is.na(cmp), FUN = mean, MARGIN = 2)
sort(missSam)
sort(missPre)
# test if the missing is related to the outcome
summary(lm(Yield ~ is.na(ManufacturingProcess10), data = cmp))
summary(lm(Yield ~ is.na(ManufacturingProcess11), data = cmp))
summary(lm(Yield ~ is.na(ManufacturingProcess14), data = cmp))
# impute data with 4NN
preCmp <- preProcess(cmp, method = c("knnImpute"), k = 4)
newCmp <- predict(preCmp, cmp)
#-------------------------------------------------------------------------
# soybean data
library(mlbench)
data(Soybean)
# find portation of missing data
missSam <- apply(is.na(Soybean), FUN = mean, MARGIN = 1)
missPre <- apply(is.na(Soybean), FUN = mean, MARGIN = 2)
sort(missSam)
sort(missPre)
# test if the missing is related to the outcome
t1 <- table(Soybean$Class, is.na(Soybean$lodging))
t2 <- table(Soybean$Class, is.na(Soybean$seed.tmt))
chisq.test(t1)
fisher.test(t1, simulate.p.value = TRUE)
fisher.test(t2, simulate.p.value = TRUE)
# impute data with 5NN 
# not work for categorical data
preSoy <- preProcess(Soybean, method = c("knnImpute"))
newSoy <- predict(preSoy, Soybean)

#-------------------------------------------------------------------------
# Section 3.5 - Removing Predictors
#-------------------------------------------------------------------------
segTrain <- subset(segmentationOriginal, Case == "Train")
# remove the first three columns (identifier columns)
segTrainX <- segTrain[, -(1:3)]
# find predictors with only a single value
isZV <- apply(segTrainX, 2, function(x) length(unique(x)) == 1)
which(isZV)
# look at number of unique values for predictors
numZV <- apply(segTrainX, 2, function(x) length(unique(x)))
sort(numZV)
table(numZV)
table(segTrainX$MemberAvgAvgIntenStatusCh2) 
table(segTrainX$IntenCoocMaxStatusCh3)
table(segTrainX$SpotFiberCountCh3)
table(segTrainX$SpotFiberCountCh4)
# remove predictors with only a single value
isZV <- apply(segTrainX, 2, function(x) length(unique(x)) == 1)
segTrainX <- segTrainX[, !isZV]
#-------------------------------------------------------------------------
# remove predictors based on their correlation
segCorr <- cor(segTrainTrans)
segCorr <- cor(segTrainTrans)
# Figure 3.10 
library(corrplot)
corrplot(segCorr, order = "hclust", tl.cex = .35)
# use findCorrelation function in caret to identify columns to remove
highCorr <- findCorrelation(segCorr, 0.75)
length(highCorr)
sort(highCorr)
finalTrainX <- segTrainX[, -highCorr]
