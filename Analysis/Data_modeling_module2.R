#------------------------------------------------------------------------------
#    Modeling done in this module
#
#
#
#
#
#------------------------------------------------------------------------------





#------------------------------- Simple expontential smoothing    -------------
cat("SES: This method is more suitable for forecasting data with no trend or seasonal pattern", "\n")
Model_ses <- ses(WugeAsIs, h=12)
plot(Model_ses, plot.conf=FALSE, ylab="Wige Exports", xlab="Year", fcol="white", type="o", main = "SES: Simple exponential smoothing model")
lines(fitted(Model_ses), col="green", type="o")
lines(Model_ses$mean, col="blue", type="o")
legend("topleft",lty=1, col=c(1,"green"), c("data", expression(alpha == 0.671)),pch=1)






# ------------------------------Holt's linear trend method   ------------------
# Holt added to the model in order to forecast using trends as well. For this it
# is necessary to add a beta, which determines the trend. If neither alpha nor beta 
# is stated, both parameters will be optimised using ets(). The trend is exponential 
# if the intercepts(level) and the gradient (slope) are multiplied with eachother. 
# The values are worse. As the Beta was very low in the optimisation, the forecast 
# is very similar to the ses() model. 
#-----------------------------------------------------------------------------------
Model_holt_1 <- holt(WugeAsIs,h=12,ylab="Wuge Exports", xlab="Year")
plot(Model_holt_1)


# ------------------------------Holt's expoential trend method   ------------------
# expoential trend
Model_holt_2<- holt(WugeAsIs, exponential=TRUE,h=12)
plot(Model_holt_2)


# ---------------------Dampened trends ---------------------------------------------
#   As such simple trends tend to forecast the future to positively, we have added 
#  a dampener. This also works for exponential trends. We also plot the level and 
#  slope individually for each model.
#-----------------------------------------------------------------------------------


# ------ Holt's Dampened Linear trends ---------------------------------------------
Model_holt_3 <- holt(WugeAsIs, damped=TRUE,h=12)
plot(Model_holt_3)




# ------ Holt's Dampened expoential trends -----------------------------------------
Model_holt_4 <- holt(WugeAsIs, exponential=TRUE, damped=TRUE,h=12)
plot(Model_holt_4)



cat("Level and slope can be plotted individually for each of Holt's linear trend methods. ", "\n")
# level and slope can be plotted individually for each model. 
plot(Model_holt_1$model$state, main="Level and slope for Holt's linear trend")
plot(Model_holt_2$model$state, main="Level and slope for Holt's expoential trend")
plot(Model_holt_3$model$state, main="Level and slope for Holt's Dampened Linear trend")
plot(Model_holt_4$model$state, main="Level and slope for Holt's Dampened exponential trend")

plot(Model_holt_1, plot.conf=FALSE, ylab="Wuge Exports", xlab="Year", main="All non-seasonal Models", fcol="white", type="o")
lines(fitted(Model_ses), col="purple", type="o")
lines(fitted(Model_holt_1), col="blue", type="o")
lines(fitted(Model_holt_2), col="red", type="o")
lines(fitted(Model_holt_3), col="green", type="o")
lines(fitted(Model_holt_4), col="orange", type="o")
lines(Model_ses$mean, col="purple", type="o")
lines(Model_holt_1$mean, col="blue", type="o")
lines(Model_holt_2$mean, col="red", type="o")
lines(Model_holt_3$mean, col="green", type="o")
lines(Model_holt_4$mean, col="orange", type="o")
legend("topleft",lty=1, col=c(1,"purple","blue","red","green","orange"), c("data", "SES","Holts auto", "Exponential", "Additive Damped", "Multiplicative Damped"),pch=1)



cat("\n Now we look at Models that accommodate seasonality \n")


#--------------------Holt-Winter's seasonal method   -------------------------------
#  Holt and Winters have expanded Holt's model further to include the seasonality 
#  aspect. The parameter gamma, which is for smoothing the seasonality, was added 
#  to achieve this. The values are better than the models without seasonality. 
#  This is logical, since the data is strongly influenced by seasonality.  In the 
#  following model, none of the parameters are given so that they will be optimised 
#  automatically. There are two models: one using an additive error model method and 
#  one using a multiplicative error model. The additive model gives slightly better 
#  results than the multiplicative model.
#------------------------------------------------------------------------------------

#--------------------Holt-Winter's additive method   -------------------------------
Model_hw_1 <- hw(WugeAsIs ,seasonal="additive",h=12)
plot(Model_hw_1)



#--------------------Holt-Winter's multiplicative method   -------------------------
Model_hw_2 <- hw(WugeAsIs ,seasonal="multiplicative",h=12)
plot(Model_hw_2)


plot(Model_hw_1, ylab="Exports ", plot.conf=FALSE, type="o", fcol="white", xlab="Year", main="Forecasts from Holt-Winter's seasonal methods")
lines(fitted(Model_hw_1), col="red", lty=2)
lines(fitted(Model_hw_2), col="green", lty=2)
lines(Model_hw_1$mean, type="o", col="red")
lines(Model_hw_2$mean, type="o", col="green")
legend("topleft",lty=1, pch=1, col=1:3, c("data","Holt Winters' Additive","Holt Winters' Multiplicative"))

# In order to use the results later, they need to be converted into point forcasts.
Model_hw_1_df <-as.data.frame(Model_hw_1) 
Model_hw_1_PointForecast <- ts(Model_hw_1_df$"Point Forecast", start=c(2014,1), end=c(2014,12), frequency=12)
Model_hw_1_PointForecast
Model_hw_2_df <-as.data.frame(Model_hw_2) 
Model_hw_2_PointForecast <- ts(Model_hw_2_df$"Point Forecast", start=c(2014,1), end=c(2014,12), frequency=12)
Model_hw_2_PointForecast



# --- Collect the accuraacy values for the models for decision making later-----------------------
E_ses    <- accuracy(Model_ses)
E_holt_1 <- accuracy(Model_holt_1)
E_holt_2 <- accuracy(Model_holt_2)
E_holt_3 <- accuracy(Model_holt_3)
E_holt_4 <- accuracy(Model_holt_4)
E_hw_1 <- accuracy(Model_hw_1)
E_hw_2 <- accuracy(Model_hw_2)

ME      <- c(E_ses[1,1],E_holt_1[1,1],E_holt_2[1,1],E_holt_3[1,1],E_holt_4[1,1],E_hw_1[1],E_hw_2[1,1])
RMSE    <- c(E_ses[1,2],E_holt_1[1,2],E_holt_2[1,2],E_holt_3[1,2],E_holt_4[1,2],E_hw_1[1,2],E_hw_2[1,2])
MAE     <- c(E_ses[1,3],E_holt_1[1,3],E_holt_2[1,3],E_holt_3[1,3],E_holt_4[1,3],E_hw_1[1,3],E_hw_2[1,3])  
MPE     <- c(E_ses[1,4],E_holt_1[1,4],E_holt_2[1,4],E_holt_3[1,4],E_holt_4[1,4],E_hw_1[1,4],E_hw_2[1,4])       
MAPE    <- c(E_ses[1,5],E_holt_1[1,5],E_holt_2[1,5],E_holt_3[1,5],E_holt_4[1,5],E_hw_1[1,5],E_hw_2[1,5])       
MASE    <- c(E_ses[1,6],E_holt_1[1,6],E_holt_2[1,6],E_holt_3[1,6],E_holt_4[1,6],E_hw_1[1,6],E_hw_2[1,6])        
ACF1    <- c(E_ses[1,7],E_holt_1[1,7],E_holt_2[1,7],E_holt_3[1,7],E_holt_4[1,7],E_hw_1[1,7],E_hw_2[1,7])       
AIC     <- c(Model_ses$model$aic,Model_holt_1$model$aic,Model_holt_2$model$aic,Model_holt_3$model$aic,Model_holt_4$model$aic,Model_hw_1$model$aic,Model_hw_2$model$aic)
BIC     <- c(Model_ses$model$bic,Model_holt_1$model$bic,Model_holt_2$model$bic,Model_holt_3$model$bic,Model_holt_4$model$bic,Model_hw_1$model$bic,Model_hw_2$model$bic) 
AICC     <- c(Model_ses$model$aicc,Model_holt_1$model$aicc,Model_holt_2$model$aicc,Model_holt_3$model$aicc,Model_holt_4$model$aicc,Model_hw_1$model$aicc,Model_hw_2$model$aicc) 

