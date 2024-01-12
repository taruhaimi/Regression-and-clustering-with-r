################################################################################
# Free Analytics Environment R
# 0565878
# 3.11.2022
################################################################################

rm(list=ls())
library(dplyr)
library(corrplot)
library(ggplot2)
library(lmtest)
library(car)
library(tsoutliers)
library(scales)
library(purrr)
library(NbClust)
################################################################################
# Part 1: Regression analysis for predicting murder arrests.

# 1) Loading data and removing observations containing missing value(s):
mydata = read.csv('dataArrests.csv', header = TRUE, sep=";")
data <-  data[complete.cases(data),]

# 2) Exploratory data analysis:
subset1 = select(data, Murder, Assault, UrbanPop, Traffic, CarAccidents)
variables = c("Murder", "Assault", "UrbanPop", "Traffic", "CarAccidents")
mins = c(min(data$Murder), min(data$Assault), min(data$UrbanPop), min(data$Traffic), min(data$CarAccidents))
maxs = c(max(data$Murder), max(data$Assault), max(data$UrbanPop), max(data$Traffic), max(data$CarAccidents))
means = c(mean(data$Murder), mean(data$Assault), mean(data$UrbanPop), mean(data$Traffic), mean(data$CarAccidents))
stds = c(sd(data$Murder), sd(data$Assault), sd(data$UrbanPop), sd(data$Traffic), sd(data$CarAccidents))
expldf = data.frame("Variable" = variables,"MIN" = mins, "MAX" = maxs, "MEAN" = means, "STD" = stds)

par(mfrow = c(2,2))
plot(subset1$Assault, subset1$Murder, xlab="Assault", ylab="Murder")
plot(subset1$UrbanPop, subset1$Murder, xlab="UrbanPop", ylab="Murder")
plot(subset1$Traffic, subset1$Murder, xlab="Traffic", ylab="Murder")
plot(subset1$CarAccidents, subset1$Murder, xlab="CarAccidents", ylab="Murder")
par(mfrow = c(2,1))
boxplot(subset1$Murder, subset1$UrbanPop, subset1$Assault, names = c("Murder","UrbanPop", "Assault"), horizontal=TRUE, las=2)
boxplot(subset1$Traffic, subset1$CarAccidents, names = c("Traffic","CarAccidents"), horizontal=TRUE, las=2)
par(mfrow = c(1,1))

# 3-4)
# Correlation matrix for all variables and visualizing it:
cormatrix = cor(data)
corrplot(cormatrix,"ellipse")
corrplot(cormatrix,"number")

# 5)
# Since there're two variables that have high absolute correlation (>0.8),
# we choose to remove the one with higher average correlation to other variables.
caracccor = mean(cormatrix[2:10,10]) # CarAccidents
trafficcor = mean(cormatrix[2:10,5]) # Traffic
newdata = select(data, !CarAccidents)

cormatrix2 = cor(newdata)
corrplot(cormatrix2,"number")

# 6)
# Linear regression and its results:
linRegModel = lm(Murder~Assault+UrbanPop+Drug+Traffic+Cyber+Kidnapping+Domestic+Alcohol, data=newdata)
summary(linRegModel) 

# 7-8)
# From the linear model summary it can be seen that the model can be improved. 
# Improvements are made by deleting one-by-one non-significant variables, starting 
# from the most non-significant variable and stopping when all the coefficients
# are significant.Thus, new model is calculated after every variable removal.

adjustedData = select(newdata, !Kidnapping)
linRegModel = lm(Murder~Assault+UrbanPop+Drug+Traffic+Cyber+Domestic+Alcohol, data=adjustedData)
summary(linRegModel) 

adjustedData = select(adjustedData, !Alcohol)
linRegModel = lm(Murder~Assault+UrbanPop+Drug+Traffic+Cyber+Domestic, data=adjustedData)
summary(linRegModel) 

adjustedData = select(adjustedData, !Domestic)
linRegModel = lm(Murder~Assault+UrbanPop+Drug+Traffic+Cyber, data=adjustedData)
summary(linRegModel)

adjustedData = select(adjustedData, !Drug)
linRegModel = lm(Murder~Assault+UrbanPop+Traffic+Cyber, data=adjustedData)
summary(linRegModel)

adjustedData = select(adjustedData, !Traffic)
linRegModel = lm(Murder~Assault+UrbanPop+Cyber, data=adjustedData)
summary(linRegModel)

adjustedData = select(adjustedData, !Cyber)
linRegModel = lm(Murder~Assault+UrbanPop, data=adjustedData)
summary(linRegModel)

adjustedData = select(adjustedData, !UrbanPop)
linRegModel = lm(Murder~Assault, data=adjustedData)
summary(linRegModel)

# Result model plotting:
plot(data$Assault, data$Murder, xlab = "Assault", ylab = "Murder", main="Linear regression for murder arrests (per 100000)")
fittedVals = fitted.values(linRegModel)
lines(data$Assault, fittedVals)

# 9) 
# Testing 5 properties of ols related to residuals and plotting residuals:
res = residuals(linRegModel)
plot(fittedVals,res, main="Residuals",  xlab = "Fitted Values", ylab = "Residuals")

# 1. E(e) = 0
mean(res)
# 2. var(e) = sigma^2 < inf: Breusch-Pagan test
bptest(linRegModel)
# 3. cov(e_i, e_j) = 0: Durbin-Watson test
durbinWatsonTest(linRegModel)
# 4. cov(e, x_i) = 0: 
cor(res, data$Assault)
# 5. e~N(0,sigma^2): Jarque-Bera test
JarqueBera.test(res)


################################################################################
# Part 2: Clustering client types
rm(list=ls())

# 1) 
# Loading data and removing possible missing value(s)
data = read.csv('wholesale.csv', header = TRUE, sep=",")
data <-  data[complete.cases(data),]

# 2)
# Exploratory data analysis
variables = c("Channel", "Region", "Fresh", "Milk", "Grocery", "Frozen","Detergents_Paper", "Delicassen")
mins = c(min(data$Channel),min(data$Region),min(data$Fresh),min(data$Milk),min(data$Grocery),min(data$Frozen),min(data$Detergents_Paper),min(data$Delicassen))
maxs = c(max(data$Channel),max(data$Region),max(data$Fresh),max(data$Milk),max(data$Grocery),max(data$Frozen),max(data$Detergents_Paper),max(data$Delicassen))
means = c(mean(data$Channel),mean(data$Region),mean(data$Fresh),mean(data$Milk),mean(data$Grocery),mean(data$Frozen),mean(data$Detergents_Paper),mean(data$Delicassen))
stds = c(sd(data$Channel),sd(data$Region),sd(data$Fresh),sd(data$Milk),sd(data$Grocery),sd(data$Frozen),sd(data$Detergents_Paper),sd(data$Delicassen))
expldf = data.frame("Variable" = variables,"MIN" = mins, "MAX" = maxs, "MEAN" = means, "STD" = stds)

par(mfrow = c(1,2))
barplot(table(data$Channel), main="Channel", ylab="Frequency")
barplot(table(data$Region), main="Region", ylab="Frequency")
par(mfrow = c(1,1))
variablenames = c("Fresh", "Milk", "Grocery", "Frozen","Detergents_Paper", "Delicassen")
boxplot(data$Fresh, data$Milk, data$Grocery, data$Frozen, data$Detergents_Paper, data$Delicassen, horizontal=TRUE, names=variablenames, las=2)
plot(data)

# 3)
# Correlation analysis
corM = cor(data)
corrplot(corM, "number")
corrplot(corM, "ellipse")

# 4)
# Normalizing data with min-max method
normData = apply(data, 2, rescale, to=c(0,1))

# 5)
# Defining optimal cluster number
# Elbow method:
tot_within_ss = map_dbl(1:10, function(k){
  model=kmeans(normData,centers=k, nstart=25)
  model$tot.withinss
})

par(mfrow = c(1,4))
plot(1:10, tot_within_ss, type="o", xlab="Number of clusters", ylab="Total WSS", main="Elbow method", panel.first =  grid())

# Silhouette method
SilClust = NbClust(normData, distance = "euclidean", min.nc = 2, max.nc=10, method = "kmeans", index = "silhouette")
plot(2:10, SilClust$All.index, type="o", xlab="Number of clusters", ylab="Silhouette Value", main="Silhouette", panel.first =  grid())

# Calinski-Harabasz Index
CalHarClust = NbClust(normData, distance = "euclidean", min.nc = 2, max.nc=10, method = "kmeans", index = "ch")
plot(2:10, CalHarClust$All.index, type="o", xlab="Number of clusters", ylab="CH Value", main="Calinski-Harabasz Index", panel.first =  grid())

# Gap statistic method
GapClust = NbClust(normData, distance = "euclidean", min.nc = 2, max.nc=10, method = "kmeans", index = "gap")
plot(2:10, GapClust$All.index, type="o", xlab="Number of clusters", ylab="GAP Value", main="GAP Statistic", panel.first =  grid())
par(mrow = c(1,1))

# -> Optimal k = 3

# 6)
# Calculating k-means and visualizing found clusters:
kmeansmdl = kmeans(data, centers=3, nstart=50)
plot(data, col=kmeansmdl$cluster, pch=16)
par(mfrow = c(1,3))
plot(data$Fresh, data$Milk, col=kmeansmdl$cluster, pch=16, xlab="Fresh", ylab="Milk")
plot(data$Fresh, data$Grocery, col=kmeansmdl$cluster, pch=16, xlab="Fresh", ylab="Grocery")
plot(data$Fresh, data$Detergents_Paper, col=kmeansmdl$cluster, pch=16, xlab="Fresh", ylab="Detergents_Paper")
par(mfrow = c(1,1))
plot(data$Channel, data$Region, col=kmeansmdl$cluster, pch=16, xlab="Channel", ylab="Region")

################################################################################
# EOF