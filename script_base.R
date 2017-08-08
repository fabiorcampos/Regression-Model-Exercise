library(ggplot2)
library(datasets)

### summary the data
summary(mtcars)
plot(mtcars)
cor(mtcars[, -c(9)])[1, ]

### Transpose to a factor
mtcars$am = as.factor(mtcars$am)
levels(mtcars$am) = c("Automatic", "Manual")

### Boxplot of difference of Automatic and Manual
autmanbox = boxplot(mpg ~ am, data=mtcars, main = "Comparison of MPG by type of Transmission",
              xlab = "Type of Gear",
              ylab = "Car consumption (MPG)",
              ylim = c(10, 35),
              col = c("blue", "red"))

### Scatter Plot 
wtplot = scatterplot(mpg ~ wt | am, data=mtcars,
                     xlab="Weight of Car", ylab="Miles Per Gallon",
                     main="MPG by Type of Gear and Weight",
                     col = c("blue", "red"),
                     legend.title = "Type of Transmission",
                     legend.coords = "topright") 

### Models
dot = mtcars[order(mtcars$mpg),] # sort by mpg
dot$am <- factor(dot$am) # it must be a factor
dot$color[dot$am == "Automatic"] <- "red"
dot$color[dot$am == "Manual"] <- "blue"
dotchart(dot$mpg,labels=row.names(dot),cex=.7,groups= dot$am,
         main="Gas Milage for Car Models\ngrouped by Type of Transmission",
         xlab="Miles Per Gallon", gcolor="black", color=dot$color) 

### Regression Model
fit1 = lm(mpg ~ wt, data=mtcars)
fit2 = lm(mpg ~ wt + am, data=mtcars)
fit3 = lm(mpg ~ am + wt, data=mtcars)

par(mfcol = c(1, 3))
plot(mtcars$wt, resid(fit1), main = "Model 1", xlab = "Weight (lbs/1000)", ylab = "Residuals")
plot(mtcars$wt, resid(fit2), main = "Model 2", xlab = "Weight (lbs/1000)", ylab = "Residuals")
plot(mtcars$wt, resid(fit3), main = "Model 3", xlab = "Weight (lbs/1000)", ylab = "Residuals")
