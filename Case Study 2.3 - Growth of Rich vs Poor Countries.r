################ Set Up ###################
setwd(choose.dir())
write.csv(growth, file = 'Growth.RichvsPoor.Countries.csv')
rm(list = ls())

rpg <- read.csv('Growth.RichvsPoor.Countries.csv', row.names = 1)

############## Explore Dataset ####################
str(rpg)
summary(rpg)
head(rpg)

############# Linear Regression ###################
#Full fit
lm.rpg    <- lm(Outcome ~ . - intercept , rpg)

#Partialing Out
if (!require('hdm')) install.packages('hdm'); library(hdm)
ry    <- rlasso(Outcome ~ . -intercept - gdpsh465, rpg)$res
rd    <- rlasso(gdpsh465 ~ . -Outcome - intercept, rpg)$res

#regress partialed out Y on partialed out d
part.lm.rpg <- lm(ry ~rd - 1)

########## Load Results into a Table #####################
results       <- matrix(0,2,4)
results[1,]   <- c(summary(lm.rpg)$coef[2,1:2], confint(lm.rpg)[2,])
results[2,]   <- c(summary(part.lm.rpg)$coef[,1:2], confint(part.lm.rpg))

rownames(results) <- c('least-squares', 'partialing-out via lasso')
colnames(results) <- c(colnames(summary(lm.rpg)$coef[,1:2]),colnames(confint(lm.rpg)))

results
rm(est.part)
