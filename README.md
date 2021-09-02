# ISLR-Chapter-III
## Upload data
Advertising <- read.csv("F:/FROM F DRIVE HP/R/rlessons/ISLR Master/ISLR-master/dataset/Advertising.csv")
### Plot and fit model
with(data=Advertising, plot(TV, Sales, col="red"))

model <- lm(formula = Sales ~ TV, data = Advertising)
abline(model, lwd=2, col="blue")
#
summary(model)

Advertising$Predicted <- predict(model) # Save the predicted values

Advertising$Residuals <- residuals(model) # save the residual values

head(Advertising)

library(dplyr)
##
Advertising %>% select(Sales,Predicted, Residuals) %>% head()

library(ggplot2)

plot1 <- ggplot(data = Advertising,mapping = aes(x=TV, y= Sales) )

print(plot1)

plot2 <- plot1 + geom_point(color = "red") # Plot actual values

print(plot2)

plot3 <- plot2 + geom_point(aes(y = Predicted), shape = 1)  #Add predicted values

print(plot3)

plot4 <- plot2 + geom_segment(aes(xend = TV, yend = Predicted), alpha = 1/4)  #Connect actual data points with predicted values 

print(plot4)

plot5 <- plot4 + geom_smooth(method = "lm", se = FALSE, lwd = 1, lty = 1, color = "steelblue")    
print (plot5)

plot6 <- plot5 + theme_test() 

print(plot6)

plot7 <- plot6 + labs(title = "3.Liner Regression")

print(plot7)

##############################################################
sales_all <- lm(Sales ~ TV + Radio + Newspaper, data = Advertising)

summary (sales_all)

M <- cor(Advertising[ , 2:5])

library(corrplot)

corrplot(M, method = "number", type = "upper")

corrplot(M, method = "number", type = "upper", order = "hclust")

corrplot(M, method = "color", type = "upper")

corrplot(M, method =  "circle", type = "upper")

corrplot(M, method = "square", order = "hclust", diag = FALSE)

corrplot(M, method = "square", order = "hclust", type = "upper")

############################################################################

#3d Regression Plotting

library("plot3D")

# set the x, y, and z variables

x <- Advertising$Radio

y <- Advertising$TV

z <- Advertising$Sales

# Compute the linear regression 

fit <- lm(z ~ x + y)

# create a grid from the x and y values (min to max) and predict values for every point this will become the regression plane

grid.lines = 40

x.pred <- seq(min(x), max(x), length.out = grid.lines)

y.pred <- seq(min(y), max(y), length.out = grid.lines)

xy <- expand.grid( x = x.pred, y = y.pred)

z.pred <- matrix(predict(fit, newdata = xy), 
                 nrow = grid.lines, ncol = grid.lines)

# create the fitted points for droplines to the surface

fitpoints <- predict(fit)

# scatter plot with regression plane

scatter3D(x, y, z, pch = 19, cex = 1,colvar = NULL, col="red", 
          theta = 20, phi = 10, bty="b",
          xlab = "Radio", ylab = "TV", zlab = "Sales",  
          surf = list(x = x.pred, y = y.pred, z = z.pred,  
                      facets = TRUE, fit = fitpoints, col=ramp.col (col = c("dodgerblue3","seagreen2"), n = 300, alpha=0.9), border="black"), main = "Advertising")

##############################
Credit <- read.csv("F:/FROM F DRIVE HP/R/rlessons/ISLR Master/ISLR-master/dataset/Credit.csv")

## Scatter Matrix

pairs(Credit[, c(12, 6, 5, 7, 2, 3, 4)], cex = .3, col = "steelblue" )

balance_gender <- lm(Balance ~ Gender, data = Credit)
summary(balance_gender)

balance_ethnicity <- lm(Balance ~ Ethnicity, data = Credit)

summary(balance_ethnicity)

sales <- lm(Sales ~ TV + Radio + TV*Radio, data = Advertising)

summary(sales) 

line1 <-  ggplot(data = Advertising,mapping = aes(x=TV, y= Sales) ) + geom_smooth(method = "lm", se = FALSE, lwd = 1, lty = 1, color = "steelblue") + theme_test()

print(line1)

##

ggplot(data = Credit, mapping = aes(x = Income, y=Balance, color = Student)) + geom_point() +
  geom_smooth(method = "lm", se = FALSE, lwd = 1, lty = 1)+ theme_test()


model1 <- lm (Balance ~ Income + Student, data = Credit)

summary(model1)

Credit$Predicted1 <- predict(model1)

model2 <- lm (Balance ~ Income + Student + Income*Student, data = Credit)

summary(model2)

Credit$Predicted2 <- predict(model2)

###########

head(Credit)

ggplot(data = Credit, mapping = aes(x = Income, y = Balance, color = Student)) + 
  geom_line(aes(y = Predicted1)) + theme_test() # Two regression line

ggplot(data = Credit, mapping = aes(x = Income, y = Balance, color = Student)) +
  geom_line(aes(y = Predicted2)) + theme_test()

###################

Auto <- read.table(file = "F:/FROM F DRIVE HP/R/rlessons/ISLR Master/ISLR-master/dataset/Auto.data", header = TRUE, dec = ".")

head(Auto)

Auto$horsepower <- as.numeric(Auto$horsepower)

Auto <- na.omit(Auto)

ggplot(data = Auto, mapping = aes(x = horsepower, y = mpg)) + geom_point(cex = .9, alpha = 0.5) + geom_smooth(method = "lm", se = FALSE, lwd = 1, lty = 1, color = "orange") + 
    theme_test()

ggplot(data = Auto, mapping = aes(x = horsepower, y = mpg)) + geom_point(cex = .9, alpha = 0.5) + geom_smooth(formula = "y ~ x^2", se = FALSE, lwd = 1, lty = 1, color = "steelblue") + 
   theme_test()

ggplot(data = Auto, mapping = aes(x = horsepower, y = mpg)) + geom_point(cex = .9, alpha = 0.5) + geom_smooth(formula = "y ~ poly(x, 2)", se = FALSE, lwd = 1, lty = 1, color = "green") + 
   theme_test()

############
ggplot(data = Auto, mapping = aes(x = horsepower, y = mpg)) + geom_point(cex = .9, alpha = 0.5) + 
  geom_smooth(method = "lm", se = FALSE, lwd = 1, lty = 1, color = "orange") + 
  geom_smooth(formula = "y ~ x^2", se = FALSE, lwd = 1, lty = 1, color = "steelblue") +
  geom_smooth(formula = "y ~ poly(x, 2)", se = FALSE, lwd = 1, lty = 1, color = "green") +
  theme_test()

mpg1 <- lm(data = Auto, mpg ~ horsepower)

mpg2 <- lm(data = Auto, mpg ~ horsepower + I(horsepower^2))

summary(mpg1)

summary(mpg2)

################

Auto$residual1 <- residuals(mpg1)

Auto$fitted1 <- predict(mpg1)

Auto$residual2 <- residuals(mpg2)

Auto$fitted2 <- predict(mpg2)

ggplot(data = Auto, mapping = aes(x = fitted1, y = residual1)) + geom_point() + geom_smooth( color = "red", se = FALSE) + theme_test() 

ggplot(data = Auto, mapping = aes(x = fitted2, y = residual2)) + geom_point() + geom_smooth(color = "red", se = FALSE) + theme_test()

plot(mpg1)

plot(mpg2)

###########

#### Add line touching the outer quantiles of the residual (test for heteroscedasticity)


################## Collinearity
Balance_rating_limit <- lm(Balance ~ Rating + Limit, data = Credit)

summary(Balance_rating_limit)

Balance_age_limit <- lm(Balance ~ Age + Limit, data = Credit)

summary(Balance_age_limit)


