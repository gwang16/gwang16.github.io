rm(list=ls())

library(xgboost)
library(ggplot2)
library(reshape2)
library(tseries)

datOrig <- read.csv("./data/stock_returns_base150.csv")
datOrig <- datOrig[1:100,]
datFull <- datOrig
rownames(datFull) <- datFull[,1]
datFull <- datFull[,-1]
dat <- datFull[1:50,]

########## Visualizing data
datOrig$date <- as.Date(datOrig$date, "%m/%d/%Y")
dat_melt <- melt(datOrig, id.var="date")
ggplot(data=dat_melt, aes(x=date, y=value, group=variable, color=variable)) +
  geom_line()+
  facet_grid(variable ~ ., scales="free")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


########## stationarity
sapply(dat,adf.test,k=2)
adf.test(dat[,1], k = 10)
########## Feature Engineering
datFull_lag1 <-  datFull
datFull_lag1[2:100,] <- datFull_lag1[1:99,]
datFull_lag1[1,] <- 0

datFull_lag2 <-  datFull
datFull_lag2[3:100,] <- datFull[1:98,]
datFull_lag2[1:2,] <- 0

datFull2 <- data.frame(datFull,datFull_lag1[,-1],datFull_lag2[,-1])
datFull2 <- datFull2[3:100,]
dat <- datFull2[1:48,]

###########Separate into training and testing data set for validation  
train <- dat[1:40,]
test <- dat[41:nrow(dat),]

str(train)
dim(train)
dim(test)

########## Variable selection
# METHOD 1: Granger Causality
library(lmtest)
norder = 4
grangertest(S1 ~ S2 , order = norder, data = train)$"Pr(>F)"[2]
grangertest(S1 ~ S3 , order = norder, data = train)$"Pr(>F)"[2]
grangertest(S1 ~ S4 , order = norder, data = train)$"Pr(>F)"[2]
grangertest(S1 ~ S5 , order = norder, data = train)$"Pr(>F)"[2]
grangertest(S1 ~ S6 , order = norder, data = train)$"Pr(>F)"[2]
grangertest(S1 ~ S7 , order = norder, data = train)$"Pr(>F)"[2]
grangertest(S1 ~ S8 , order = norder, data = train)$"Pr(>F)"[2]
grangertest(S1 ~ S9 , order = norder, data = train)$"Pr(>F)"[2]
grangertest(S1 ~ S10 , order = norder, data = train)$"Pr(>F)"[2]

# METHOD 2: Bestsubset
library(leaps)
exhaustive <- regsubsets(S1~., data=train, nbest=2, method=c("exhaustive"))
summary(exhaustive) #top two models for models with k variables, k=1:8
plot(exhaustive, scale="adjr2")

# METHOD 3: forward stepwise selection
null=lm(S1~1, data=train)
full=lm(S1~., data=train)
forward<-step(null, scope=list(lower=null, upper=full), direction="forward")
summary(forward)

################### XGBoost Decision Tree Model 

#XGBoost data set format
dtrain <- xgb.DMatrix(data = as.matrix(train[,-1]), label=train[,1])
dtest <- xgb.DMatrix(data = as.matrix(test[,-1]), label=test[,1])
ddat <- xgb.DMatrix(data = as.matrix(dat[,-1]), label=dat[,1])

#Train model
watchlist <- list(train=dtrain, test=dtest)
optimizedNRound <- 23 #optimized from cross validation (see end of script)
nCores <- 4 #number of CPU cores
trainingSpeed <- 0.2 #a value from 0 to 1, lower corresponds to slower learning rate, require larger nround
treeDepth <- 2 #depth of tree, optimized from treeDepth search
#for (treeDepth in 1:6){
bst <- xgb.train(data=dtrain, max.depth=treeDepth, eta=trainingSpeed, nthread = nCores, nround=optimizedNRound, watchlist=watchlist, objective = "reg:linear")
#}


#Plot importance of features
importance_matrix <- xgb.importance(colnames(train), model = bst)
print(importance_matrix)
xgb.plot.importance(importance_matrix = importance_matrix)

#################Performance testing

#Insample prediction
insample_xg <- predict(bst, dtrain)
insample_forward <- predict(forward,train)

#Out of sample prediction
pred_xg <- predict(bst, dtest)
pred_forward <- predict(forward,test)

#Ensemble
insample_ensemble <- (insample_xg + insample_forward)/2
pred_ensemble <- (pred_xg + pred_forward)/2


#Visualization
pred<- data.frame(date= rownames(dat), 
                   XG= c(insample_xg,pred_xg),
                   forward= c(insample_forward,pred_forward),
                   #ensemble= c(insample_ensemble,pred_ensemble),
                   actual= c(train[,1],test[,1]))
                    ## only out of sample prediction 
                    # pred<- data.frame(date= rownames(test), 
                    #                   XG= c(pred_xg),
                    #                   forward= c(pred_forward),
                    #                   actual= c(test[,1]))
pred$date <- as.Date(pred$date, "%m/%d/%Y")
pred_melt <- melt(pred, id.var="date")
ggplot(data=pred_melt, aes(x=date, y=value, group=variable, color=variable)) +
  geom_point()+
  geom_line()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#################Error calculation
err <- data.frame(
  Insample=rbind(sqrt(mean((train[,1]-insample_xg)^2)), sqrt(mean((train[,1]-insample_forward)^2)), sqrt(mean((train[,1]-insample_ensemble)^2))),
  OutOfSample=rbind(sqrt(mean((test[,1]-pred_xg)^2)), sqrt(mean((test[,1]-pred_forward)^2)), sqrt(mean((test[,1]-pred_ensemble)^2))),
  row.names = c("XG Boost", "Forward", "Ensemble"))

err #XG Boost is clearly over fit





############## K-fold cross validation, Parameter tuning
minErr <- matrix(0,2,16)
optIter <- matrix(0,2,16)
for (runs in 1:10){ #Using 10 for speed, original tuning done using 100
  for (depth in 2:3){
    for (eta in seq(0.05,0.8,by = 0.05)){
      bst.cv <- xgb.cv(data=ddat, max.depth=depth, eta=eta, nthread = 4, nround=50, watchlist=watchlist, objective = "reg:linear",
                       nfold=5)
      minErr[depth-1,eta*20] <- minErr[depth-1,eta*20] + min(bst.cv[, test.rmse.mean])   
      optIter[depth-1,eta*20] <- optIter[depth-1,eta*20] + which.min(bst.cv[, test.rmse.mean]) 
    }
  }
}
minErr/100 
# [,1]      [,2]      [,3]      [,4]      [,5]      [,6]      [,7]      [,8]
# [1,] 0.3944744 0.3719870 0.3714662 0.3714158 0.3762106 0.3722950 0.3784051 0.3787654
# [2,] 0.4019331 0.3812146 0.3836383 0.3778903 0.3742915 0.3756229 0.3774171 0.3843153
# [,9]     [,10]     [,11]     [,12]     [,13]     [,14]     [,15]     [,16]
# [1,] 0.3851148 0.3900121 0.3965959 0.4034562 0.4146127 0.4238729 0.4424365 0.4492037
# [2,] 0.3913635 0.3867681 0.3930688 0.4008526 0.4103990 0.4196912 0.4293788 0.4392880
optIter/100
# [,1]  [,2]  [,3]  [,4]  [,5]  [,6]  [,7]  [,8]  [,9] [,10] [,11] [,12] [,13] [,14]
# [1,] 49.97 45.67 31.72 23.37 18.65 15.94 14.15 12.52  9.93  9.30  7.90  6.47  5.95  5.63
# [2,] 50.00 46.71 36.57 28.31 23.29 19.24 15.72 13.10 11.76 10.11  8.88  7.04  7.08  6.64
# [,15] [,16]
# [1,]  5.92  4.89
# [2,]  5.50  4.80

#optimal parameters: depth =2; eta = 0.2, nround = 23. Resulting error = 0.3714158
#plot(bst.cv[, test.rmse.mean])

# #K-fold cross validation, modified for time series data
# nfold <- 10
# folds <- cut(seq(1,nrow(dat)),breaks=nfold,labels=FALSE) #Create 10 equally size folds
# 
# for(i in 1:nfold){
#   ind <- which(folds==i,arr.ind=TRUE)
#   test <- dat[ind,]
#   train <- dat[-ind,]
# }



#######################Final output##################
train <- datFull2[1:48,]
dtrain <- xgb.DMatrix(data = as.matrix(train[,-1]), label=train[,1])
test <- datFull2[49:98,]
dtest <- xgb.DMatrix(data = as.matrix(test[,-1]))

#Train model
bst <- xgb.train(data=dtrain, max.depth=treeDepth, eta=trainingSpeed, nthread = nCores, nround=optimizedNRound, objective = "reg:linear")

#forecast
#Insample prediction
insample_xg <- predict(bst, dtrain)
insample_forward <- predict(forward,train)

#Out of sample prediction
pred_xg <- predict(bst, dtest)
pred_forward <- predict(forward,test)

pred<- data.frame(date= rownames(datFull2), 
           XG= c(insample_xg,pred_xg),
           forward= c(insample_forward,pred_forward),
           actual = datFull2[,1])


pred$date <- as.Date(pred$date, "%m/%d/%Y")
pred_melt <- melt(pred, id.var="date")
ggplot(data=pred_melt, aes(x=date, y=value, group=variable, color=variable)) +
  geom_point()+
  geom_line()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#In sample error
err <- data.frame(
  Insample=rbind(sqrt(mean((train[,1]-insample_xg)^2)), sqrt(mean((train[,1]-insample_forward)^2))),
  OutOfSample=rbind(sqrt(mean((test[,1]-pred_xg)^2)), sqrt(mean((test[,1]-pred_forward)^2))),
  row.names = c("XG Boost", "Forward"))

err

output <- data.frame(Date=rownames(test),Value=pred_xg)
write.csv(output,'predictions.csv', row.names = F)
