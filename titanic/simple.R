
setwd("C:/Users/Goki/Desktop/titanic")

tita<-read.table(file=paste("train.csv",sep=""), sep=",", header=TRUE, quote="\"")

color1 = c("blue", "red")

sex1 = c("male", "female")
male = subset(tita, tita$Sex == "male")
female = subset(tita, tita$Sex == "female")


#descriptive stats

table(tita$Sex)
#female   male 
#   314    577

table(tita$Pclass)
#  1   2   3 
#216 184 491 

#age structure
par(mfrow = c(1,3))
hist(tita$Age, xlab = "Age", main="", xlim = c(0,80))
hist(male$Age, xlab = "Male Age", main="", xlim = c(0,80))
hist(female$Age, xlab = "Female Age", main="", xlim = c(0,80))
         
par(mfrow = c(1,1))
table(tita$SibSp)
#  0   1   2   3   4   5   8 
#608 209  28  16  18   5   7 
sibsp.survived = tapply(tita$Survived, tita$SibSp, mean)
barplot(sibsp.survived)

#By Pclass
par(mfrow = c(1,2))
boxplot(tita$Fare ~ tita$Pclass, xlab = "Class", ylab = "Fare")
boxplot(tita$Age ~ tita$Pclass, xlab = "Class", ylab = "Age")

df1 = data.frame(Survived = rep(NA, 2), Sex = sex1)
for (i in 1:2)
{
    tmp = subset(tita, tita$Sex == sex1[i])
    df1$Survived[i] = mean(tmp$Survived)
}

barplot(df1$Survived, names.arg = df1$Sex, col = color1)

####################################################################
##########################      glm       ##########################                    
####################################################################
library(lme4)

tita$PclassCat = as.factor(tita$Pclass)
tita$SibSpCat = as.factor(tita$SibSp)
tita$ParchCat = as.factor(tita$Parch)
mod1 = glm(Survived ~ Sex, data = tita, family = binomial);summary(mod1)
mod2 = glm(Survived ~ PclassCat, data = tita, family = binomial);summary(mod2)
mod3 = glm(Survived ~ Age, data = tita, family = binomial);summary(mod3)
mod4 = glm(Survived ~ SibSpCat, data = tita, family = binomial);summary(mod4)
mod5 = glm(Survived ~ ParchCat, data = tita, family = binomial);summary(mod5)
mod6 = glm(Survived ~ Fare, data = tita, family = binomial);summary(mod6)
mod7 = glm(Survived ~ Embarked, data = tita, family = binomial);summary(mod7)

mod.tot = glm(Survived ~ Sex + PclassCat + Age + SibSpCat + ParchCat + Fare + Embarked, data = tita, family = binomial);summary(mod.tot)
#Deviance Residuals: 
#    Min       1Q   Median       3Q      Max  
#-2.8578  -0.6461  -0.3791   0.6171   2.4375  
#
#Coefficients:
#              Estimate Std. Error z value Pr(>|z|)    
#(Intercept)  1.852e+01  1.659e+03   0.011  0.99109    
#Sexmale     -2.603e+00  2.241e-01 -11.615  < 2e-16 ***
#PclassCat2  -1.207e+00  3.331e-01  -3.624  0.00029 ***
#PclassCat3  -2.259e+00  3.489e-01  -6.474 9.53e-11 ***
#Age         -4.021e-02  8.801e-03  -4.569 4.90e-06 ***
#SibSpCat1   -5.238e-02  2.426e-01  -0.216  0.82903    
#SibSpCat2   -6.962e-01  5.801e-01  -1.200  0.23011    
#SibSpCat3   -2.045e+00  7.872e-01  -2.597  0.00939 ** 
#SibSpCat4   -1.821e+00  7.963e-01  -2.287  0.02221 *  
#SibSpCat5   -1.622e+01  9.639e+02  -0.017  0.98658    
#ParchCat1    3.234e-01  3.000e-01   1.078  0.28110    
#ParchCat2    2.237e-01  4.158e-01   0.538  0.59050    
#ParchCat3    4.717e-01  1.047e+00   0.450  0.65242    
#ParchCat4   -1.585e+01  1.053e+03  -0.015  0.98799    
#ParchCat5   -9.741e-01  1.179e+00  -0.826  0.40878    
#ParchCat6   -1.634e+01  2.400e+03  -0.007  0.99457    
#Fare         1.551e-03  2.644e-03   0.587  0.55745    
#EmbarkedC   -1.449e+01  1.659e+03  -0.009  0.99303    
#EmbarkedQ   -1.514e+01  1.659e+03  -0.009  0.99272    
#EmbarkedS   -1.478e+01  1.659e+03  -0.009  0.99289 
drop1(mod.tot)
#Model:
#Survived ~ Sex + PclassCat + Age + SibSpCat + ParchCat + Fare + 
#    Embarked
#          Df Deviance    AIC
#<none>         620.63 660.63
#Sex        1   786.65 824.65
#PclassCat  2   665.14 701.14
#Age        1   643.31 681.31
#SibSpCat   5   636.10 666.10
#ParchCat   6   627.63 655.63
#Fare       1   621.00 659.00
#Embarked   3   622.83 656.83

mod.tot2 = glm(Survived ~ Sex + PclassCat + Age + SibSpCat + Embarked + Fare, data = tita, family = binomial);summary(mod.tot2)
drop1(mod.tot2)
#Model:
#Survived ~ Sex + Pclass + Age + SibSp + Parch + Fare
#       Df Deviance    AIC
#<none>      635.81 649.81
#Sex     1   815.18 827.18
#Pclass  1   695.26 707.26
#Age     1   667.62 679.62
#SibSp   1   645.25 657.25
#Parch   1   636.07 648.07
#Fare    1   636.62 648.62

mod.tot3 = glm(Survived ~ Sex + PclassCat + Age + SibSpCat + Fare, data = tita, family = binomial);summary(mod.tot3)
drop1(mod.tot3)
#Model:
#Survived ~ Sex + Pclass + Age + SibSp + Parch
#       Df Deviance    AIC
#<none>      636.62 648.62
#Sex     1   817.17 827.17
#Pclass  1   741.96 751.96
#Age     1   669.40 679.40
#SibSp   1   645.60 655.60
#Parch   1   636.72 646.72

mod.tot4 = glm(Survived ~ Sex + PclassCat + Age + SibSpCat, data = tita, family = binomial);summary(mod.tot4)
#Deviance Residuals: 
#    Min       1Q   Median       3Q      Max  
#-2.8900  -0.6501  -0.3760   0.6257   2.4808  
#
#Coefficients:
#              Estimate Std. Error z value Pr(>|z|)    
#(Intercept)   4.249935   0.454708   9.347  < 2e-16 ***
#Sexmale      -2.606773   0.215723 -12.084  < 2e-16 ***
#PclassCat2   -1.428199   0.286114  -4.992 5.98e-07 ***
#PclassCat3   -2.599819   0.288453  -9.013  < 2e-16 ***
#Age          -0.046077   0.008337  -5.527 3.26e-08 ***
#SibSpCat1     0.002792   0.227878   0.012   0.9902    
#SibSpCat2    -0.564842   0.549873  -1.027   0.3043    
#SibSpCat3    -1.887820   0.747922  -2.524   0.0116 *  
#SibSpCat4    -1.686790   0.744599  -2.265   0.0235 *  
#SibSpCat5   -15.010060 585.370306  -0.026   0.9795    

drop1(mod.tot4)
#Model:
#Survived ~ Sex + Pclass + Age + SibSp
#       Df Deviance    AIC
#<none>      636.72 646.72
#Sex     1   823.84 831.84
#Pclass  1   742.29 750.29
#Age     1   669.44 677.44
#SibSp   1   647.29 655.29



#We remove age because of missing data.
mod.totv2 = glm(Survived ~ Sex + PclassCat + SibSpCat + ParchCat + Fare + Embarked, data = tita, family = binomial);summary(mod.totv2)
drop1(mod.totv2)
#Model:
#Survived ~ Sex + PclassCat + SibSpCat + ParchCat + Fare + Embarked
#          Df Deviance     AIC
#<none>         785.79  825.79
#Sex        1  1005.24 1043.24
#PclassCat  2   819.60  855.60
#SibSpCat   6   806.68  834.68
#ParchCat   6   801.26  829.26
#Fare       1   787.18  825.18
#Embarked   3   789.10  823.10   ***

mod.totv2.2 = glm(Survived ~ Sex + PclassCat + SibSpCat + ParchCat + Fare, data = tita, family = binomial);summary(mod.totv2.2)
drop1(mod.totv2.2)
#Model:
#Survived ~ Sex + PclassCat + SibSpCat + ParchCat + Fare
#          Df Deviance     AIC
#<none>         789.10  823.10
#Sex        1  1022.45 1054.45
#PclassCat  2   823.05  853.05
#SibSpCat   6   812.93  834.93
#ParchCat   6   805.91  827.91
#Fare       1   791.21  823.21   ***  

mod.totv2.3 = glm(Survived ~ Sex + PclassCat + SibSpCat + ParchCat, data = tita, family = binomial);summary(mod.totv2.3)
drop1(mod.totv2.3)
#Model:
#Survived ~ Sex + PclassCat + SibSpCat + ParchCat
#          Df Deviance     AIC
#<none>         791.21  823.21
#Sex        1  1029.57 1059.57
#PclassCat  2   859.01  887.01
#SibSpCat   6   814.05  834.05
#ParchCat   6   808.21  828.21



#Stepwise model
library(MASS)
full.model = mod.tot
step.model = stepAIC(full.model, direction = "both", trace = F)
#On arrive sur le meme model que le drop1