

########################################################
### Step 0.  Load any libraries and format output
########################################################

options(scipen = 5, # discourage scientific notation for better formatting of coefficients
        digits = 4)  # print 3 digits

library(car)  # nice QQ plot for linear regression, vif function
library(pROC)  # makes ROC curves
library(caret) # data partition code, glm diagnostics
library(ResourceSelection)  # contains the Hosmer-Lemeshow test and other glm diagnostics

## Add your own libraries here




########################################################
### Step 1.  Import the dataset 
########################################################

## OPTION 1.  Download the file from Learn and then import the dataset from that folder
# Change this file path to the path on your computer where you saved the data file
#   Notice that the slashes need to be   /  orientation
setwd("C:/Users/You/My Documents/My Favourite Class/Predicting Churn QWE/")
data <- read.csv("ChurnData.csv", header = TRUE)  

## OPTION 2.  Download the file from github
data <- read.csv(url("https://laurencipriano.github.io/IveyBusinessStatistics/Datasets/ChurnData.csv"), 
                 header = TRUE)  

summary(data)


########################################################
### EXHIBIT 2   CORRELATION MATRIX 
########################################################

cor(data)


########################################################
### EXHIBIT 3   LINEAR REGRESSION 
########################################################


RegLM <- lm(data = data, 
            CHI_Month0 ~ CHI_Diff + CustomerAge_months + SupportCases_Month0 +
              SPriority_Month0 + Logins_Diff + Blogs_Diff + Views_Diff)
summary(RegLM)

RegLM2 <- lm(data = data, 
             CHI_Month0 ~ CHI_Diff + CustomerAge_months + SupportCases_Month0 +
               SPriority_Month0 + Logins_Diff +  Views_Diff)
summary(RegLM2)


########################################################
### EXHIBIT 4   LINEAR REGRESSION DIAGNOSTICS
########################################################

plot(x=data$CHI_Diff , y=rstandard(RegLM2))
abline(0, 0, lty=2, col="grey") # draw a straight line at 0 for a visual reference
lines(lowess(x=data$CHI_Diff, rstandard(RegLM2)), col = "red", lwd = 2)

plot(x=data$CustomerAge_months , y=rstandard(RegLM2))
abline(0, 0, lty=2, col="grey") 
lines(lowess(x=data$CustomerAge_months, rstandard(RegLM2)), col = "red", lwd = 2)

plot(x=data$SupportCases_Month0 , y=rstandard(RegLM2))
abline(0, 0, lty=2, col="grey") 
lines(lowess(x=data$SupportCases_Month0, rstandard(RegLM2)), col = "red", lwd = 2)

plot(x=data$SPriority_Month0 , y=rstandard(RegLM2))
abline(0, 0, lty=2, col="grey") 
lines(lowess(x=data$SPriority_Month0, rstandard(RegLM2)), col = "red", lwd = 2)

plot(x=data$Logins_Diff , y=rstandard(RegLM2))
abline(0, 0, lty=2, col="grey") 
lines(lowess(x=data$Logins_Diff, rstandard(RegLM2)), col = "red", lwd = 2)

plot(x=data$Views_Diff , y=rstandard(RegLM2))
abline(0, 0, lty=2, col="grey") 
lines(lowess(x=data$Views_Diff, rstandard(RegLM2)), col = "red", lwd = 2)

plot(x=RegLM2$fitted.values , y=rstandard(RegLM2))
abline(0, 0, lty=2, col="grey") 
lines(lowess(x=RegLM2$fitted.values, rstandard(RegLM2)), col = "red", lwd = 2)

vif(RegLM2)

plot(RegLM2, 4)

plot(RegLM2, 6)

qqPlot(RegLM2, id=FALSE)



########################################################
### EXHIBIT 5   LOGISTIC REGRESSION 
########################################################

Reg.A <- glm(data = data, 
             Churn ~ CHI_Month0 + CHI_Diff + Views_Diff,
             family = 'binomial')
summary(Reg.A)
exp(cbind(Reg.A$coefficients, confint(Reg.A)))



