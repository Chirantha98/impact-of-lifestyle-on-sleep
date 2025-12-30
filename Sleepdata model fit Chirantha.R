R Coding

library(ggplot2)
library(dplyr)
library(tidyverse)
library(mice)

sleepdata <- read.csv('sleepdata.csv',header = TRUE)
sleepdata

#find missing values
md.pattern(sleepdata)
is.na(sleepdata)
View(sleepdata)

# Total count of missing values
total_missing <- sum(is.na(sleepdata))
print(total_missing)


#Primilinary analysis

#Sleep duration (Y)

# Calculate Q1, Q3, and IQR
Q1 <- quantile(sleepdata$Sleep.Duration, 0.25)
Q3 <- quantile(sleepdata$Sleep.Duration, 0.75)
IQR <- Q3 - Q1


# Define outlier thresholds
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# Identify outliers
outliers <- sleepdata$Sleep.Duration[sleepdata$Sleep.Duration < lower_bound | sleepdata$Sleep.Duration > upper_bound]
outliers

stress_freq <- table(sleepdata$Sleep.Duration)
stress_freq


BarChart(Sleep.Duration,data=sleepdata,
         main ="Bar chart  of Sleep Duration",
         xlab = "Sleep duration",
         ylab = "Number of persons")


boxplot(sleepdata$Sleep.Duration,ylab="Number of persons",main= "Boxplot of Sleep duration",Col="blue")


plot(x = 1:length(sleepdata$Sleep.Duration),  
     y = sleepdata$Sleep.Duration,            
     main = "Scatterplot of Sleep Duration",   
     xlab = "Data Point Index",              
     ylab = "Sleep Duration",                  
     col = "black",                           
     cex = 0.8 )                           


#Stress level(X_1)

str(sleepdata$Stress.Level)
stress_freq <- table(sleepdata$Stress.Level)

summary(sleepdata$Stress.Level)

describe(sleepdata$Stress.Level)

cor(sleepdata[, c("Stress.Level", "Sleep.Duration")])

plot(x = sleepdata$Sleep.Duration,
     y = sleepdata$Stress.Level,
     main = "Stress Level vs Sleep Duration",
     xlab = "Stress Level",
     ylab = "Sleep Duration",
     col = "blue",
     pch = 16,
     cex = 0.8)

abline(fit, col = "red")



BarChart(Stress.Level,data=sleepdata,
         main ="Bar chart  of Stress Level",
         xlab = "Stress Level",
         ylab = "Number of persons")

boxplot(sleepdata$Stress.Level,ylab="Number of persons",main= "Boxplot of Stress Level",Col="blue")

plot(x = 1:length(sleepdata$Stress.Level),  
     y = sleepdata$Stress.Level,            
     main = "Scatterplot of Stress Level",  
     xlab = "Data Point Index",              
     ylab = "Stress Level",                  
     col = "black",                           
     pch = 16,                               
     cex = 0.8) 


#BMI category(X_2)

BMI_freq <- table(sleepdata$BMI.Category)
BMI_freq

BarChart(BMI.Category,data=sleepdata,
         main ="Bar chart  of BMI Category",
         xlab = "BMI Category",
         ylab = "Number of persons")

#Heart Rate(X_3)

heart_freq <- table(sleepdata$Heart.Rate)
heart_freq


BarChart(Heart.Rate,data=sleepdata,
         main ="Bar chart  of Heart Rate",
         xlab = "Heart Rate",
         ylab = "Number of persons")

describe(sleepdata$Heart.Rate)

cor(sleepdata[, c("Heart.Rate", "Sleep.Duration")])

plot(x = sleepdata$Sleep.Duration,
     y = sleepdata$Heart.Rate,
     main = "Heart Rate vs Sleep Duration",
     xlab = "Heart Rate",
     ylab = "Sleep Duration",
     col = "blue",
     pch = 16,
     cex = 0.8)

abline(fit, col = "red")

boxplot(sleepdata$Heart.Rate,ylab="Number of persons",main= "Boxplot of Heart Rate",Col="blue")

plot(x = 1:length(sleepdata$Heart.Rate),  
     y = sleepdata$Heart.Rate,            
     main = "Scatterplot of Heart Rate",   
     xlab = "Data Point Index",              
     ylab = "Heart Rate",                  
     col = "black",                                                        
     cex = 0.8)
#assigning numerical values for BMI
BMI_num <- c("Overweight"=0,"Normal"=1)
sleepdata$BMI_numeric <- BMI_num[sleepdata$BMI.Category]
View(sleepdata)


# Fitting the model
model<- lm(Sleep.Duration~ Stress.Level + BMI_numeric + Heart.Rate,data = sleepdata)
model

summary(model)

cor(sleepdata[, c("BMI_numeric", "Sleep.Duration")])


#Reduced model
reduced_model <- lm(Sleep.Duration ~ Stress.Level + Heart.Rate,data = sleepdata)
reduced_model

summary(reduced_model)
anova(reduced_model)

anova(model,reduced_model)



# Get the model residuals
model_residuals = model$residuals
model_residuals

#Extract Fitted values
predicted_values <- predict(model)
predicted_values


# Obtain confidence intervals
conf_intervals <- confint(model)
conf_intervals



#Check the assumptions

#normality

# Get the model residuals
model_residuals = model$residuals
model_residuals
# Plot the result
hist(model_residuals)
# Plot the residuals
qqnorm(model_residuals)
# Plot the Q-Q line
qqline(model_residuals,col="red")





#Linearity


# Create scatter plot for independent variable 1
plot_Stress_Level <- ggplot(sleepdata, aes(x= Stress.Level, y= Sleep.Duration)) +
  geom_point() +
  geom_smooth(method="lm", se=FALSE, color="red") +
  labs(title="Scatter Plot of Stress Level vs  Sleep Duration")
#se=FALSE turns off the shading around the line
#indicating confidence intervals.)

# Create scatter plot for independent variable 3
plot_Heart_Rate <- ggplot(sleepdata, aes(x=Heart.Rate, y=Sleep.Duration)) +
  geom_point() +
  geom_smooth(method="lm", se=FALSE, color="red") +
  labs(title="Scatter Plot of BMI Category vs Sleep Duration")

plot_Stress_Level
plot_Heart_Rate


#multicollinearity


# Compute correlation matrix
correlation_matrix <- cor(sleepdata[, c("Sleep.Duration","Stress.Level", "BMI_numeric","Heart.Rate")])
print(correlation_matrix)



#Homoscedasticity

# Extract standardized residuals
standardized_residuals <- rstandard(model)
standardized_residuals
#(identify outliers in a regression model
#is known as a standardized residual.
#In practice, we often consider any standardized
#residual with an absolute value greater than 3 to
#be an outlier.)


# Plot standardized residuals against predicted values
plot(predicted_values, standardized_residuals, 
     xlab = "Predicted Values", ylab = "Standardized Residuals",
     main = "Plot of Predicted Values vs. Standardized Residuals",
     pch = 16, col = "blue")
abline(h = 0, col = "red")  # Add horizontal line at y = 0





