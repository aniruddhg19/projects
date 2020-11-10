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

# prediction with confidence interval - 95%

predict(wine_lm2,data.frame(fixed.acidity = 7.9,residual.sugar = 2.6,chlorides = 0.085, pH = 3.7,alcohol = 11),interval = 'confidence')

# prediction with prediction interval - 95%

predict(wine_lm2,data.frame(fixed.acidity = 17.8,residual.sugar = 17,chlorides = 1, pH = 5,alcohol = 18),interval = 'prediction')
