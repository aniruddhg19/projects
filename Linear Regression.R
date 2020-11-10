shear_strength <- c(2158.7,1678.15,2316,2061.3,2207.3,1708.3,1784.7,2575,2357.9,2256.7,2165.7,2399.55,1779.8,2336.75,1765.3,2053.5,2414.4,2200.5,2654.2,1753.7)

age_of_propellant <- c(15.5,23.75,8,17,5.5,19,24,2.5,7.5,11,13,3.75,25,9.75,22,18,6,12.5,2,21.5)

expt_data <- data.frame(shear_strength,age_of_propellant)

head(expt_data)

colnames(expt_data) <- c('Shear Strength (psi) : y ', 'Age of propellant (weeks) : x')

summary(expt_data)

plot(expt_data$`Age of propellant (weeks) : x`,expt_data$`Shear Strength (psi) : y `, xlim = c(1,28) , 
     ylim =c(1650,2700) ,xlab = 'Age of propellant (weeks) : x',ylab = 'Shear Strength (psi) : y',
     pch = 23,main = 'Scatterplot of Data',sub = 'New formulation')

ggplot2::qplot(x = expt_data$`Age of propellant (weeks) : x`, y = expt_data$`Shear Strength (psi) : y `,data = expt_data,
  xlim = c(1,28), ylim=c(1650,2700),xlab = 'Age of Propellant', ylab = 'Shear Strength',geom = 'point',
    main = 'Scatterplot of Data',color = 'red') 


## creating the regression model
lin_model <- lm(expt_data$`Shear Strength (psi) : y `~expt_data$`Age of propellant (weeks) : x`)
lin_model

summary(lin_model)


## model coeffcients
coef(lin_model)

## fitted values
fitted(lin_model)

##  residuals
resid(lin_model) # or residuals(lin_model) or residuals.lm(lin_model)

# Mean square error = 96.14 **2 / 20 = 462.145


## tabulating observed,fit & residuals

fit <- round(fitted(lin_model),2)
res <- round(resid(lin_model),2)

compare_table <- data.frame(c(expt_data$`Shear Strength (psi) : y `),fit,res)
colnames(compare_table) <- c('Shear Strength (y)','Fitted Values','Residuals')
head(compare_table)

## variance of the coefficients

coef(lin_model)**2

## sum of residuals

sum(res)

## sum of observed and fitted values

sum(compare_table$`Shear Strength (y)`) - sum(compare_table$`Fitted Values`)

## regression line


### FOr plot function

abline(lin_model,col='blue')

abline(coef=coef(lin_model)) # Regression line with intercept and slope.same result

abline(a=lin_model$coefficients[1],b=lin_model$coefficients[2],col='green') # Regression line with intercept and slope.same result

## For qplot function

library(ggplot2)

qplot(x = expt_data$`Age of propellant (weeks) : x`, y = expt_data$`Shear Strength (psi) : y `,data = expt_data,
  xlim = c(1,28), ylim=c(1650,2700),xlab = 'Age of Propellant', ylab = 'Shear Strength',geom = 'point',
  main = 'Scatterplot of Data',color = 'red') + geom_abline(slope =lin_model$coefficients[2],intercept = lin_model$coefficients[1],col = 'blue' ) 
  

# dev.off()

## centroid calculation

rsdepth::centroid(expt_data$`Shear Strength (psi) : y `,expt_data$`Age of propellant (weeks) : x`)

## sum of residuals weighted by corresponding regressor

sum(crossprod(expt_data$`Age of propellant (weeks) : x`,compare_table$Residuals))

## sum of residuals weighted by corresponding fitted value

sum(crossprod(compare_table$`Fitted Values`,compare_table$Residuals))

# Hypothesis testing on slope . summary(lin_model) --> Standard error of the slope = 96.14.NUll hypothesis :  
# b=0 is rejected suggesting that there is a linear relationship between y & x

## Sxx, Sxy, Syy ,b1

Sxx <- sum(expt_data$`Age of propellant (weeks) : x`**2) - sum(expt_data$`Age of propellant (weeks) : x`)**2/nrow(expt_data)

Sxy <- sum(crossprod(expt_data$`Shear Strength (psi) : y `,expt_data$`Age of propellant (weeks) : x`)) - sum(expt_data$`Shear Strength (psi) : y `)*sum(expt_data$`Age of propellant (weeks) : x`)/nrow(expt_data)

(b1 <- Sxy/Sxx)                                                                                                          

# Calculating MSres

MSres <- s$sigma**2 # residual standard error from summary output or sigma-hat squared

# Standard error of slope or estimated standard error 

s <- summary(lin_model)

se_b1 <- (MSres/Sxx)**0.5

# T stat calculation

to <- b1 / se_b1
to**2 # F stat value

# confidence interval on the slope & intercept

confint(lin_model,lin_model$coefficients[2],level = 0.95)

# prediction interval
newdata <- expt_data$`Shear Strength (psi) : y `

predict.lm(lin_model,newdata = data.frame(newdata),level =0.95,interval = "confidence")
predict.lm(lin_model,newdata = data.frame(expt_data$`Shear Strength (psi) : y `),interval = "prediction",level = 0.95)
