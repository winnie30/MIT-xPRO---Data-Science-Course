#### Explore the data ####
# In case you need to use the dataset in other software packages
write.csv(data, file = "cps.data.csv" )
cps.data <-read.csv("cps.data.csv", row.names = 1)
 
head(cps.data)
summary(cps.data)
str(cps.data)
table(cps.data[,1:8])

#### train basic & flexible model on complete dataset ####

form1 <- wage ~ female + sc+ cg+ mw + so + we + exp1 + exp2 + exp3
lm.basicFull  <- lm(form1, cps.data)
sm.basicFull  <- summary(lm.basicFull)
r2.basic      <- sm.basicFull$r.squared
r2Adj.basic   <- sm.basicFull$adj.r.squared
mse.basic     <- sum((lm.basicFull$residuals)^2)/lm.basicFull$df.residual

form2 <- wage ~ female + (sc+ cg+ mw + so + we + exp1 + exp2 + exp3)^2
lm.flexFull   <- lm(form2, cps.data)
sm.flexFull   <- summary(lm.flexFull)
r2.flex       <- sm.flexFull$r.squared
r2Adj.flex    <- sm.flexFull$adj.r.squared
mse.flex      <- sum((lm.flexFull$residuals)^2)/lm.flexFull$df.residual

Results.Model <- matrix(,nrow = 2,ncol = 3)
Results.Model[1,] <- c(r2.basic, r2Adj.basic, mse.basic)
Results.Model[2,] <- c(r2.flex, r2Adj.flex, mse.flex)
colnames(Results.Model) <-  c("R2", "R2.Adj", "MSE")
rownames(Results.Model) <-  c("BasicModel", "FlexibleModel")
Results.Model

#### Split data into training and test ####

if(!require("caret")) install.packages("caret"); library("caret") 
set.seed(123)
idx.train <- createDataPartition(cps.data$wage, p = 0.5, list = FALSE)

train.cps <- cps.data[idx.train,] 
test.cps  <- cps.data[-idx.train,] 

#### Train/Fit basic & flexible model ####

lm.basic <- lm(form1, train.cps) 
pred.lm.basic <- predict(lm.basic, newdata = test.cps)

lm.flex <- lm(form2, train.cps)
pred.lm.flex <- predict(lm.flex, newdata = test.cps)

#### Calcualte performance of Basic & flexible model ####

y <- test.cps[,12]

rB <- postResample(pred.lm.basic, y)
rf <- postResample(pred.lm.flex, y)
rB
Results.Prediction      <- matrix(0,2,3)
Results.Prediction[1,]  <- rB
Results.Prediction[2,]  <- rf
colnames(Results.Prediction) <- c('RMSE', 'r2', 'MAE')
rownames(Results.Prediction) <-  c("BasicModel", "FlexibleModel")
Results.Prediction
Results.Model

#MSE.fit1   <- summary(lm((y- pred.lm.basic)^2~1))$coef[1]    #same result
#R2.fit1    <- 1- MSE.fit1/var(y)                             #same result
#mse.basicSplit <- sum((pred.lm.basic - y)^2) / length(y)     #same result
