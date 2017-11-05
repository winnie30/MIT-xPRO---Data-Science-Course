#### Explore the dataset ####
setwd(choose.dir())
cps.data  <- read.csv(choose.files(),row.names = 1)

head(cps.data)
apply(cps.data,2,mean)
summary(cps.data)
str(cps.data)

female.stats  <- as.matrix(apply(cps.data[cps.data$female==1,],2,mean))
male.stats    <- as.matrix(apply(cps.data[cps.data$female==0,],2,mean))

gender.stats <- cbind(female.stats,male.stats)
gender.stats <-  gender.stats[-1,]
colnames(gender.stats) <- c('female averages', 'male averages')

library(xtable)
xtable(gender.stats)

###################  Perform Linear Regression #############################

#Wage regression - basic model, pulling out coef, std erro and 95% confidence

fml.1 <- wage ~ female + cg + sc + mw + so + we + exp1 + exp2 + exp3

lm.full.b   <- lm(fml.1,cps.data)
est.f.b     <- summary(lm.full.b)$coefficients[2,1:2]
ci.f.b      <- confint(lm.full.b)[2,]

#Wage regression - Flex model, pulling out coef, std erro and 95% confidence

fml.2 <- wage ~ female + (cg + sc + mw + so + we + exp1 + exp2 + exp3)^2

lm.full.f   <- lm(fml.2, cps.data)
est.f.f     <- summary(lm.full.f)$coefficients[2,1:2]
ci.f.f      <- confint(lm.full.f)[2,]

# create table of results

wage.table1     <- matrix(0,2,4)
wage.table1[1,] <- cbind(est.f.b, ci.f.b) 
wage.table1[2,] <- cbind(est.f.f, ci.f.f)

colnames(wage.table1) <- c('estimate','Std.Error', '2.5%', '97.5%')
rownames(wage.table1) <- c('basic model', 'flexible model')
wage.table1

##################### Partialling out #########################
####basic model####

#outcome regression on covariates
part.fml.1Y <- wage ~  cg + sc + mw + so + we + exp1 + exp2 + exp3
#treatment regression on covariates
part.fml.1D <- female ~  cg + sc + mw + so + we + exp1 + exp2 + exp3
#residuals of outcome and treatment
res.lm.1Y <- lm(part.fml.1Y, cps.data)$residuals
res.lm.1D <- lm(part.fml.1D, cps.data)$residuals

part.lm.b   <- lm(res.lm.1Y~res.lm.1D)
part.est.b  <- summary(part.lm.b)$coef[2,1:2]
part.ci.b   <- confint(part.lm.b)[2,]


#### Flexible model ####
#outcome regression on covariates
part.fml.2Y <- wage ~  (cg + sc + mw + so + we + exp1 + exp2 + exp3)^2
#treatment regression on covariates
part.fml.2D <- female ~  (cg + sc + mw + so + we + exp1 + exp2 + exp3)^2
#residuals of outcome and treatment

res.lm.2Y <- lm(part.fml.2Y, cps.data)$residuals
res.lm.2D <- lm(part.fml.2D, cps.data)$residuals

part.lm.f   <- lm(res.lm.2Y~res.lm.2D)
part.est.f  <- summary(part.lm.f)$coef[2,1:2]
part.ci.f   <- confint(part.lm.f)[2,]

#### create results table 2 ####

wage.table2     <- matrix(0,4,2)
wage.table2[1,] <- est.f.b
wage.table2[2,] <- est.f.f
wage.table2[3,] <- part.est.b 
wage.table2[4,] <- part.est.f 

colnames(wage.table2) <- c('estimate', 'std.error')
rownames(wage.table2) <- c('basic model', 'flexible model', 'basic - partial out', 'flexible - partial out')
wage.table2
