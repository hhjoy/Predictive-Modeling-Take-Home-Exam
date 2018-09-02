#Hope Knopf, MSBA Summer 2018
#R Script for Intro to Predictive Modeling Take Home Exam 


#Chapter 2 #10#
#10a
rm(list=ls())
library(MASS)
data=Boston
attach(data)
dim(data)

#10b
pairs(data)
#crim variable is significantly correlated with age, dis, rad, tax, medv and ptratio

#10c
par(mfrow=c(1, 5))
plot(age, crim)
##Older homes associated with higher crime rate
plot(dis, crim)
##Lower distance (ie closer) to employment centres associated with higher crime rate
plot(rad, crim)
##Higher accessibility to radial highways associated with higher crime rate
plot(tax, crim)
##Higher property tax rate associated with higher crime rate 
plot(ptratio, crim)
##Higher pupil-teacher ratio associated with higher crime rate
plot(medv, crim)
##Lower median value of homes associated with higher crime rate

#10d
summary(crim)
summary(tax)
summary(ptratio)

par(mfrow=c(1,3))
boxplot(crim, main='Crime Rate')
boxplot(tax, main='Tax Rate')
boxplot(ptratio, main='Pupil-teacher ratio')

##Very large range of crime rates 
##Large range of tax rates
##Smaller range of pupil-teacher ratios


#10e
summary(chas)
margin.table(table(crim,chas),2)

##chas=1 means the suburb bounds the river 
##35 suburbs in the datasetbound the river 

#10f
median(ptratio)
##Median pupil-teacher ratio is 19.05

#10g
summary(medv)
##Lowest median value of homes in a Boston suburb is $5,000

values = data[order(medv),]
x=sapply(data, summary)
y=values[c(1,2),]
z=as.data.frame(rbind(y,x))

z1 = z[, 1:7] 
z2 = z[, 8:14] 
z1
z2

##Suburb # 399 and 406 have the lowest median values of owner-occupied homes
##They have well above the mean crime rate at 38.35%and 67.92% 
##Both have 100% of homes built prior to 1940 as compared to an average of 68.57% home built before 1940 across the dataset
##Closer to employment centers at 1.49 and 1.43 than the average of3.80
##Higher accessibility to radial highways than average
##Higher pupil-teacher ratio than average
##Higher full-value property tax rate than average

#10h
length(rm[rm > 7]) 
##64 suburbs average more than 7 rooms per dwelling
length(rm[rm > 8])
##13 suburbs average more than 8 rooms per dwelling

summary(data[which(rm>8),])
##Suburbs that average more than 8 rooms per dwelling have a lower crime rate than the overall average,higher median value than overall average, lower status of population than the overall average


#Chapter 3 #15#
#15a
rm(list=ls())

library(MASS)
data=Boston
attach(data)

par(mfrow=c(1,4))
chas = factor(chas, labels=c('N','Y'))

lm.zn=lm(crim~zn)
summary(lm.zn)
plot(lm.zn)

lm.indus = lm(crim~indus)
summary(lm.indus) 
plot(lm.indus)

lm.chas = lm(crim~chas) 
summary(lm.chas) 
plot(lm.chas)

lm.nox = lm(crim~nox) 
summary(lm.nox) 
plot(lm.nox)

lm.rm = lm(crim~rm) 
summary(lm.rm) 
plot(lm.rm)

lm.age = lm(crim~age) 
summary(lm.age) 
plot(lm.age)

lm.dis = lm(crim~dis) 
summary(lm.dis)
plot(lm.dis)

lm.rad = lm(crim~rad) 
summary(lm.rad) 
plot(lm.rad)

lm.tax = lm(crim~tax)
summary(lm.tax) 
plot(lm.tax)

lm.ptratio = lm(crim~ptratio) 
summary(lm.ptratio) 
plot(lm.ptratio)

lm.black = lm(crim~black) 
summary(lm.black) 
plot(lm.black)

lm.lstat = lm(crim~lstat) 
summary(lm.lstat) 
plot(lm.lstat)

lm.medv = lm(crim~medv)
summary(lm.medv) 
plot(lm.medv)

#All variables stastically significant except for chas 

#15b

lm.fit = lm(crim~., data = Boston)
summary(lm.fit)

#Can reject the null hypothesis for zn, dis, rad, medv and black because they have significant coefficients ie beta is not equal to 0

#15c

x = c(coefficients(lm.zn)[2], coefficients(lm.indus)[2], coefficients(lm.chas)[2], coefficients(lm.nox)[2], coefficients(lm.rm)[2], coefficients(lm.age)[2], coefficients(lm.dis)[2], coefficients(lm.rad)[2], coefficients(lm.tax)[2], coefficients(lm.ptratio)[2], coefficients(lm.black)[2], coefficients(lm.lstat)[2], coefficients(lm.medv)[2])
y = coefficients(lm.fit)[-1]
plot(y~x)

#The results are fairly comparable, nox is the only predictor not in the cluster on the top left corner of the plot, thus nox is the only outlier

#15d

lm.zn = lm(crim~poly(zn,3)) 
summary(lm.zn)
#shows support for x^2

lm.indus = lm(crim~poly(indus,3)) 
summary(lm.indus)
#shows support for x^2 and x^3

lm.nox = lm(crim~poly(nox,3)) 
summary(lm.nox)
#shows support for x^2 and x^3

lm.rm = lm(crim~poly(rm,3))
summary(lm.rm)
#shows support for x^2

lm.age = lm(crim~poly(age,3))
summary(lm.age)
#shows support for x^2 and x^3

lm.dis = lm(crim~poly(dis,3))
summary(lm.dis)
#shows support for x^2 and x^3

lm.rad = lm(crim~poly(rad,3)) 
summary(lm.rad)
#shows support for x^2

lm.tax = lm(crim~poly(tax,3))
summary(lm.tax)
#shows support for x^2

lm.ptratio = lm(crim~poly(ptratio,3)) 
summary(lm.ptratio)
#shows support for x^2 and x^3

lm.black = lm(crim~poly(black,3)) 
summary(lm.black)
#shows support for x only

lm.lstat = lm(crim~poly(lstat,3)) 
summary(lm.lstat)
#shows support for x^2

lm.medv = lm(crim~poly(medv,3)) 
summary(lm.medv)
#shows support for x^2 and x^3

#chas does not have non-linear association with response variable crim

#Chapter 6 #9#
#9a
rm(list = ls())
library(ISLR) 
data = College 
attach(data)
set.seed(1)

n=dim(data)
ind = sample(1:n, size=0.2*nrow(data))
train = data[-ind, ]
test = data[ind, ]

#9b
set.seed(1)
lm.fit = lm(Apps~., data = train)
lm.pred = predict(lm.fit, test)
error = mean((test[, "Apps"] - lm.pred)^2) 
error
#Observed test error is 2046102

#9c
library(glmnet)

set.seed(1)
train.mat = model.matrix(Apps~., data=train)
test.mat = model.matrix(Apps~., data=test)
grid = 10^seq(4,-2, length=100)

mod.ridge = cv.glmnet(train.mat, train[, "Apps"], alpha=0, lambda=grid, thresh=1e-12)
lambda.best = mod.ridge$lambda.min

ridge.pred = predict(mod.ridge, newx=test.mat, s=lambda.best)
mean((test[, "Apps"] - ridge.pred)^2)
#Observed test error is 2169911

#9d
set.seed(1)
mod.lasso = cv.glmnet(train.mat, train[, "Apps"], alpha=1, lambda=grid, thresh=1e-12)
lambda.best = mod.lasso$lambda.min

lasso.pred = predict(mod.lasso, newx=test.mat, s=lambda.best)
mean((test[, "Apps"] - lasso.pred)^2)
#Observed test error is 2085586

mod.lasso = glmnet(model.matrix(Apps~., data=College), College[, "Apps"], alpha=1)
#Number of non-zero coefficient estimates shown in output above
predict(mod.lasso, s=lambda.best, type="coefficients")
#Number of non-zero coefficient estimates shown in output above

#9e
library(pls)

set.seed(1)
pcr.fit = pcr(Apps~., data=train, scale = TRUE, validation = "CV") 
validationplot(pcr.fit, val.type="MSEP")
summary(pcr.fit)

pcr.pred = predict(pcr.fit, test, ncomp = 15) 
pcr.error = mean((test[, 'Apps'] - data.frame(pcr.pred))^2)
pcr.error
#Obtained test error of 5239167

#9f
pls.fit = plsr(Apps~., data = train, scale = TRUE, validation = "CV") validationplot(pls.fit, val.type="MSEP")
summary(pls.fit)

set.seed(1)
pls.pred = predict(pls.fit, test, ncomp = 10)
pls.error = mean((test[, "Apps"] - data.frame(pls.pred))^2) 
pls.error
#Obtained test error of 2030057

#9g
test.avg = mean(test[, "Apps"])

lm.test.r2 = 1 - mean((test[, "Apps"] - lm.pred)^2) /mean((test[, "Apps"] - test.avg)^2)

ridge.test.r2 = 1 - mean((test[, "Apps"] - ridge.pred)^2) /mean((test[, "Apps"] - test.avg)^2)

lasso.test.r2 = 1 - mean((test[, "Apps"] - lasso.pred)^2) /mean((test[, "Apps"] - test.avg)^2)

pcr.test.r2 = 1 - mean((test[, "Apps"] - data.frame(pcr.pred))^2) /mean((test[, "Apps"] - test.avg)^2)

pls.test.r2 = 1 - mean((test[, "Apps"] - data.frame(pls.pred))^2) /mean((test[, "Apps"] - test.avg)^2)

barplot(c(lm.test.r2, ridge.test.r2, lasso.test.r2, pcr.test.r2, pls.test.r2), col="red", names.arg=c("OLS", "Ridge", "Lasso", "PCR", "PLS"), main="Test R-squared")

#From the bar plot output above, the test R-sqaured for all models is around 0.9, with the exception of PCR, which had an R-sqaured around 0.8.  So the models predict college application with fairly high accuracy, except for PCR.

#Chapter 6 #11#
#11a
rm(list = ls())
set.seed(100)
library(MASS)
library(leaps)
library(glmnet)
library(pls)

data=Boston
attach(data)

#best subset selection
predict.regsubsets = function(object, newdata, id, ...) {
  form = as.formula(object$call[[2]])
  mat = model.matrix(form, newdata)
  coefi = coef(object, id = id)
  mat[, names(coefi)] %*% coefi
}

k = 10
p = ncol(data) - 1
folds = sample(rep(1:k, length = nrow(data)))
cv.errors = matrix(NA, k, p)
for (i in 1:k) {
  best.fit = regsubsets(crim~., data = Boston[folds != i, ], nvmax = p)
  for (j in 1:p) {
    pred = predict(best.fit, Boston[folds == i, ], id = j)
    cv.errors[i, j] = mean((Boston$crim[folds == i] - pred)^2)
  }
}
rmse.cv = sqrt(apply(cv.errors, 2, mean))
plot(rmse.cv, pch = 19, type = "b")

which.min(rmse.cv)
rmse.cv[which.min(rmse.cv)]

#lasso 
x = model.matrix(crim~. -1, data = Boston)
y = data$crim
cv.lasso = cv.glmnet(x, y, type.measure = "mse")
plot(cv.lasso)
coef(cv.lasso)
sqrt(cv.lasso$cvm[cv.lasso$lambda == cv.lasso$lambda.1se])


#ridge regression
x = model.matrix(crim~. -1, data = Boston)
y = data$crim
cv.ridge = cv.glmnet(x, y, type.measure = "mse", alpha = 0)
plot(cv.ridge)

coef(cv.ridge)
sqrt(cv.ridge$cvm[cv.ridge$lambda == cv.ridge$lambda.1se])

#PCR
library(pls)
pcr.fit = pcr(crim~., data = Boston, scale = TRUE, validation = "CV")
validationplot(pcr.fit, val.type="MSEP")
summary(pcr.fit)

#The best subset model and PCR model have comparable RMSE
#lasso and ridge have higher RMSE

#11b

#I would propose the 9 component best subset model for this dataset, it's cross-validated RMSE was best and it is simpler than the PCR, which had a similar RMSE but 13 components

#11c
#Model has 9 components, which is simpler, as stated in part (b)

#Chapter 4 #10(not e & f)#
#10a
rm(list = ls())
library(ISLR) 
data=Weekly 
attach(Weekly)

summary(data)
cor(data[,-9])
pairs(data)

#year and volume related 

#10b

glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Weekly, family=binomial)

summary(glm.fit)
#Lag 2 variable is statistically significant 

#10c
glm.probs=predict(glm.fit, type='response')

glm.pred=rep('Down', length(glm.probs))
glm.pred[glm.probs>.5] = 'Up'
table(glm.pred, data$Direction)

#56.1% overall correct predictions
#92.1% Market Up correct predictions
#11.2% Market Down correct predictions
#So logistic regression is right most of the time when the market is up, and wrong most of the time when the market is down


#10d
train=(Year<2009)
test=data[!train, ]

glm.fit = glm(Direction ~ Lag2, data=Weekly, family = binomial, subset=train)
glm.probs = predict(glm.fit, test, type = "response")

glm.pred = rep("Down", length(glm.probs))
glm.pred[glm.probs > 0.5] = "Up"

Dir=Direction[!train]
table(glm.pred, Dir)

mean(glm.pred == Dir)

#Skip 10e
#Skip 10f

#10g
library(class)
train.X = as.matrix(Lag2[train])
test.X = as.matrix(Lag2[!train])
train.Direction = Direction[train]

set.seed(1)
knn.pred = knn(train.X, test.X, train.Direction, k = 1)

table(knn.pred, Dir)

mean(knn.pred == Dir)


#10h
#logistic regression provides best results

#10i
# Logistic regression with Lag2:Lag1
glm.fit = glm(Direction ~ Lag2:Lag1, data = Weekly, family = binomial, subset = train)

glm.probs = predict(glm.fit, test, type = "response")

glm.pred = rep("Down", length(glm.probs))

glm.pred[glm.probs > 0.5] = "Up"
Dir = Direction[!train]
table(glm.pred, Dir)

mean(glm.pred == Dir)

# KNN k =10
knn.pred = knn(train.X, test.X, train.Direction, k = 10)

table(knn.pred, Dir)

mean(knn.pred == Dir)

# KNN k = 100
knn.pred = knn(train.X, test.X, train.Direction, k = 100)

table(knn.pred, Dir)

mean(knn.pred == Dir)

#Models have comparable performance, but logistic regression is still best
#KNN w/k=100 had the worst performance 


#Chapter 8 #8#
#8a
rm(list = ls())
library(tree) 
library(randomForest) 
library(ISLR) 
data = Carseats 
attach(data) 
set.seed(1)

n=dim(data)
ind = sample(1:n, size=0.2*nrow(data))
train = data[-ind, ]
test = data[ind, ]

#8b
tree.data = tree(Sales~., data=Carseats)
summary(tree.data)
plot(tree.data)
text(tree.data, pretty=0)

pred.data=predict(tree.data,test)
mean((test$Sales - pred.data)^2)

#Observed test error rate is 2.854017

#8c
cv.data=cv.tree(tree.data, FUN=prune.tree)
par(mfrow=c(1,2))
plot(cv.data$size, cv.data$dev, type='b')
plot(cv.data$k, cv.data$dev, type='b')

pruned.data=prune.tree(tree.data, best=9)
par(mfrow=c(1,1))
plot(pruned.data)
text(pruned.data, pretty=0)
pred.pruned = predict(pruned.data, test)
mean((test$Sales - pred.pruned)^2)
#Pruning increases test error rate to 3.472245

#8d
bag.data=randomForest(Sales~., data=train, mtry=10, ntree=500, importance = TRUE)
bag.pred=predict(bag.data, test)
mean((test$Sales - bag.pred)^2)
#Bagging lowered test error rate to 2.58765

importance(bag.data)
#Price and ShelveLoc are shown to be the most imporant variables

#8e
rf.data=randomForest(Sales~., data=train, mtry=5, ntree=500, importance=TRUE)
rf.pred=predict(rf.data,test)
mean((test$Sales - rf.pred)^2)
#Observed test error rate is 2.58197
importance(rf.data)
#Price and ShelveLoc are again the most important variables
#Changing m varies test MSE 


#Chapter 8 #11#
#11a
rm(list = ls())
library(gbm) 
library(ISLR) 
data = Caravan 
attach(data) 
set.seed(1)

ind = 1:1000
data$Purchase=ifelse(data$Purchase=='Yes', 1, 0)
train=data[ind,]
test=data[-ind,]

#11b
boost.data=gbm(Purchase~., data=train, n.trees=1000, shrinkage=0.01, distribution='bernoulli')
summary(boost.data)
#PPERSAUT, MKOOPKLA, MOPLHOOG and MBERMIDD are shown to be the most important predictors


#11c
boost.prob=predict(boost.data, test, n.trees=1000, type='response')
boost.pred = ifelse(boost.prob>0.2,1,0)
table(test$Purchase, boost.pred)

lm.data=glm(Purchase~., data=train, family=binomial)
lm.prob=predict(lm.data, test, type='response')
lm.pred=ifelse(lm.prob>0.2,1,0)
table(test$Purchase, lm.pred)

#Boosting better than logistic regression in this case


#Problem 1: Beauty Pays#
rm(list = ls())
setwd("~/Desktop/STA S380.17")
beauty_data=read.csv('BeautyData.csv')
attach(beauty_data)

lm.fit = lm(CourseEvals~., data = beauty_data)
summary(lm.fit)

#Problem 2: Housing Price Structure#
rm(list = ls())
setwd("~/Desktop/STA S380.17")
housing_data=read.csv('MidCity.csv')
attach(housing_data)

n=dim(housing_data)[1]

point1 = rep(0,n)
point1[Nbhd==1]=1

point2 = rep(0,n)
point2[Nbhd==2]=1

point3=rep(0,n)
point3[Nbhd==3]=1

br = rep(0,n)
br[Brick=='Yes']=1

Price=Price/1000
Sqft=SqFt/1000

model1 = lm(Price ~ br + point2 + point3 + SqFt + Offers + SqFt + Bedrooms + Bathrooms)
summary(model1)
confint(model1)

model2= lm(Price ~ br + point2 + point3 + SqFt + Bedrooms + Bathrooms + br:point3)
summary(model2)
confint(model2)
confint(model2, level=0.99)


#Problem 4: BART#
setwd("~/Desktop/STA S380.17")
data = read.csv("CAhousing.csv")
attach(data)


avbed=totalBedrooms/households
avrooms=totalRooms/households
avocc=population/households
logMedVal=log(medianHouseValue)
data=data[,-c(4,5,9)] 
data$logMedVal = logMedVal

x=data[,1:6]
y=medianHouseValue
head(cbind(x,y))

install.packages("BART")
library(BART)

nd=200 
burn=50 
bf = wbart(x,y,nskip=burn,ndpost=nd)

lmf = lm(y~.,data.frame(x,y))
fitmat = cbind(y,bf$yhat.train.mean,lmf$fitted.values)
colnames(fitmat)=c("y","BART","Linear")
cor(fitmat)

#setting train & test
n=length(y) 
ind = sample(1:n,floor(.75*n)) 
xtrain=x[ind,]; ytrain=y[ind] 
xtest=x[-ind,]; ytest=y[-ind] 

#set yhat mean (from BART example on course website)
bf_train = wbart(xtrain,ytrain)
yhat = predict(bf_train,as.matrix(xtest))

yhat.mean = apply(yhat,2,mean)

#plot BART 
plot(ytest,yhat.mean)
abline(0,1,col=2)

#Find RMSE
rmse=sqrt((sum((yhat-ytest)^2))/length(ytest))

#BART outperforms boosting & RF

#Problem 5: Neural Nets#
rm(list = ls())
set.seed(500)
library(MASS)
data = Boston
attach(data)

#set train and test data
n=dim(data)
ind = sample(1:n, size=0.2*nrow(data))
train = data[-ind, ]
test = data[ind, ]

#normalize the data
maxs = apply(data,2,max)
mins = apply(data,2,min)
scaled = as.data.frame(scale(data, center=mins, scale=maxs-mins))
train2=scaled[-ind, ]
test2=scaled[ind, ]

#fit the neural net
library(nnet)
n = names(train2)
f=as.formula(paste('crim~', paste(n[!n %in% 'crim'], collapse = '+')))
nn = nnet(f, data=train2, size=1, linout=T)
summary(nn)

#Cross validate for different size & decay parameters
nn2=nnet(f, data=train2, size=3, decay=0.5, linout=T)
nn3=nnet(f, data=train2, size=5, decay=0.5, linout=T)
nn4=nnet(f, data=train2, size=10, decay = 0.1, linout=T)
nn5=nnet(f, data=train2, size=10, decay = 0.001, linout=T)