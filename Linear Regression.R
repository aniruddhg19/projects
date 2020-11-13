wine <- read.csv(file.choose(),header = TRUE,sep = ';')
head(wine)

str(wine)

dim(wine)
sum(is.na(wine))

library(ggplot2)
corr <- round(cor(wine),2)
ggcorrplot::ggcorrplot(corr)

# to predict density
cols3 <- c('volatile.acidity','free.sulfur.dioxide','total.sulfur.dioxide','sulphates')
wine[cols3] <- NULL

head(wine)

# Multiple Linear Regression

wine_lm <- lm(density~.,data=wine)
plot(wine_lm)

summary(wine_lm)

# Second model created after removing insignificant variables

wine_lm2 <- lm(density~fixed.acidity + residual.sugar + chlorides + pH + alcohol,data = wine )
summary(wine_lm2)

#Test for multicollinearity

round(car::vif(wine_lm2),3)

fixed.acidity residual.sugar      chlorides        pH           alcohol 
     1.931          1.019          1.126          2.094          1.091 

# prediction with confidence interval - 95%

predict(wine_lm2,data.frame(fixed.acidity = 7.9,residual.sugar = 2.6,chlorides = 0.085, pH = 3.7,alcohol = 11),interval = 'confidence')

     fit       lwr       upr
1 0.9978039 0.9976695 0.9979383

# prediction with prediction interval - 95%

predict(wine_lm2,data.frame(fixed.acidity = 17.8,residual.sugar = 17,chlorides = 1, pH = 5,alcohol = 18),interval = 'prediction')

     fit     lwr      upr
1 1.016803 1.01477 1.018837

#Accuracy

AIC(wine_lm2)
[1] -18330.25

#MAE
fit <- wine_lm2$fitted.values

comp_table <- as.data.frame(cbind(Actuals = wine$density,Predicted = fit))
str(comp_table)

MAE <- mean(abs(comp_table$Actuals-comp_table$Predicted))
MAE
[1] 0.0005873531

