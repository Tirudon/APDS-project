#Load the relevant package for running regression
library(lmtest)

#Read the data file
#=================================================
NewData <- data.frame(read.csv("New Data.csv"))
#==================================================

#Defining Variables
#==================================================
# Foreign Institutional Investments during the month
FII <- NewData[,2]
FII

#Month end IIP General Index 
IIPG <- NewData[,7]
IIPG

#one month lag value of IIP General Index
IIPGLag <- NewData[,8]
IIPGLag

#Dow Jones Industrial Average for MOnth end
DJIA <- NewData[,9]
DJIA

#Monthly return on DJIA
DJIAReturn <- NewData[,10]
DJIAReturn

#USD/INR exchange rate expressed as INR per USD
FXRate <- NewData[,11]
FXRate

#Monthly change in USD/INR rate 
FXRateChange <- NewData[,12]
FXRateChange

#one month lagged value of FX Rate change
FXRateChangeLag <- NewData[,24]
FXRateChangeLag

#Value of NIFTY Bank Index
NIFTYBank <- NewData[,13]
NIFTYBank

#Monthly return on NIFTY Bank Index
NIFTYBankReturn <- NewData[,14]
NIFTYBankReturn

#M3 money supply (monthly)
M3 <- NewData[,21]
M3

#Change in M3 money supply for each month
M3Change <- NewData[,22]
M3Change

#One month lagged value of Change in M3 money supply
M3ChangeLag <- NewData[,23]
M3ChangeLag

#RBI Foreign Exchange Reserves (Month end in USD)
FER <- NewData[,25]
FER

#MOnthly change in RBI Foreign Exchange Reserves
FERChange <- NewData[,26]
FERChange

#==================================================

# SCATTER PLOTS and CORRELATION Analysis 
#==================================================

# NIFTY Bank Return vs FII 
plot(NIFTYBankReturn ~ FII, data=NewData, xlab="FII", ylab="Return on NIFTY Bank")
abline(lm(NIFTYBankReturn ~ FII), col="red")
cor.test(NIFTYBankReturn, FII, method = "pearson")
#positive correlation between FII and Bank NIFTY returns

# NIFTY Bank Return vs Change in Foreign Exchange Reserves 
plot(NIFTYBankReturn ~ FERChange, data=NewData, xlab="FER Change", ylab="Return on NIFTY Bank")
abline(lm(NIFTYBankReturn ~ FERChange), col="red")
cor.test(NIFTYBankReturn, FERChange, method = "pearson")
#positive correlation between change in FER and returns on Bank NIFTY 

# NIFTY Bank Return vs Change in M3 Money supply 
plot(NIFTYBankReturn ~ M3Change, data=NewData, xlab="Change in M3 money supply", ylab="Return on NIFTY Bank")
abline(lm(NIFTYBankReturn ~ M3Change), col="red")
cor.test(NIFTYBankReturn, M3Change, method = "pearson")
#No correlation between Change in M3 money supply and Bank NIFTY returns

# NIFTY Bank Return vs one period Lagged value of Change in M3 Money supply 
plot(NIFTYBankReturn ~ M3ChangeLag, data=NewData, xlab="Lagged change in M3 Money supply", ylab="Return on NIFTY Bank")
abline(lm(NIFTYBankReturn ~ M3ChangeLag), col="red")
cor.test(NIFTYBankReturn, M3ChangeLag, method = "pearson")
#No significant correlation between Lagged change in M3 Money supply and Bank NIFTY returns

# NIFTY Bank Return vs DJIA return 
plot(NIFTYBankReturn ~ DJIAReturn, data=NewData, xlab="Return on DJIA", ylab="Return on NIFTY Bank")
abline(lm(NIFTYBankReturn ~ DJIAReturn), col="red")
cor.test(NIFTYBankReturn, DJIAReturn, method = "pearson")
#positive correlation between DJIA returns and Bank NIFTY returns

# NIFTY Bank Return vs Change in Fx rate (USD/INR)  
plot(NIFTYBankReturn ~ FXRateChange, data=NewData, xlab="Change in USD/INR (INR per USD)", ylab="Return on NIFTY Bank")
abline(lm(NIFTYBankReturn ~ FXRateChange), col="red")
cor.test(NIFTYBankReturn, FXRateChange, method = "pearson")
#No significant correlation between Change in USD/INR rate and Bank NIFTY returns

# NIFTY Bank Return vs 1 period Lagged Change in Fx rate (USD/INR)  
plot(NIFTYBankReturn ~ FXRateChangeLag, data=NewData, xlab="Lagged Change in USD/INR (INR per USD)", ylab="Return on NIFTY Bank")
abline(lm(NIFTYBankReturn ~ FXRateChangeLag), col="red")
cor.test(NIFTYBankReturn, FXRateChangeLag, method = "pearson")
#Negative correlation between Lagged Change in USD/INR rate and Bank NIFTY returns

# NIFTY Bank Return vs IIP General Index  
plot(NIFTYBankReturn ~ IIPG, data=NewData, xlab="IIP General Index", ylab="Return on NIFTY Bank")
abline(lm(NIFTYBankReturn ~ IIPG), col="red")
cor.test(NIFTYBankReturn, IIPG, method = "pearson")
#No correlation between IIP General Index and Bank NIFTY returns

# NIFTY Bank Return vs 1 period lagged IIP General Index  
plot(NIFTYBankReturn ~ IIPGLag, data=NewData, xlab="Lagged IIP General Index", ylab="Return on NIFTY Bank")
abline(lm(NIFTYBankReturn ~ IIPGLag), col="red")
cor.test(NIFTYBankReturn, IIPGLag, method = "pearson")
#No correlation between IIP General Index and Bank NIFTY returns

#==================================================

#MULTIPLE REGRESSION ANALYSIS
#==================================================

Fit1 <- lm(NIFTYBankReturn ~ FII + FERChange + M3ChangeLag + DJIAReturn + FXRateChangeLag + IIPGLag, data = NewData)
summary(Fit1)
#R-square of 49.84%. The independent variables: "Change in FER", "Lagged change in M3 Money supply" and "Lagged valued of IIP General Index" are insignificant ind etermining the Return on NIFTY BANK Index

# Dropping the insignificant variables and running the MULTIPLE REGRESSION
Fit2 <- lm(NIFTYBankReturn ~ FII + FXRateChangeLag + DJIAReturn, data = NewData)
summary(Fit2)
# R square of 46.5%.
# All the independent variables: "FII", "Lagged change in USD/INR", "and Return on DJIA" are significant in determining the "Return on NIFTY Bank"
# FII and Return on DJIA have positive slope coefficients. This iumples Increase in FII and postive returns on DJIA , leads to higher returns on NIFTY Bank.
# But Lagged change in USD/INR has a negative coefficient. This means when INR depreciates w.r.t. USD, the return on NIFTY Bank decreases.

#==================================================
