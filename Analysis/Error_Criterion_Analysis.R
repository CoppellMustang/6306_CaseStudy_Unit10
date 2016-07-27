
#------------------------------------------------------------------------------
#  AIC AICC BIC plot for all Models
#------------------------------------------------------------------------------
g_range <- range(0,AIC,BIC,AICC)
plot(AIC, type="o", y_lim=g_range, col="blue",axes=FALSE, ylab="", main="AIC,BIC and AICc Criterion for the models")
lines(BIC, type="o", pch=22, lty=1, col="green")
lines(AICC, type="o", pch=22, lty=3, col="red")
axis(1, at=1:7, lab=c("SES","Holt1","Holt2","Holt3","Holt4","HW1","HW2"))
legend("topleft",lwd=2, pch=1, col=c('blue','green','red'), c("AIC","BIC","AICC"))


cat("\n Plot shows that Holt-Winters seasonal multiplicative model (HW2) gives the most desirable values.", "\n")

#------------------------------------------------------------------------------
#  ME,RMSE,MAE,MPE,MAPE,MASE,ACF1,AIC,BIC,AICC plot for all Models
#------------------------------------------------------------------------------
g_range <- range(0, ME,RMSE,MAE,MPE,MAPE,MASE,ACF1)
plot(ME, type="o", col="blue",  ylim=c(-8000, 125000) ,axes=FALSE, ylab="",main = "ME, RMSE, MAE and ACF1 for the models")
lines(RMSE, type="o", pch=22, col="red")
lines(MAE, type="o", pch=22, col="green")
lines(ACF1, type="o", pch=22,  col="gray2")
axis(1, at=1:7, lab=c("SES","Holt1","Holt2","Holt3","Holt4","HW1","HW2"))
legend("topleft",lwd=2, pch=1, col=c('blue','red','green',"gray2"), c("ME","RMSE","MAE","ACF1"))



cat("\n The plot above also favors Holt-Winters seasonal multiplicative model (HW2) with lower values.", "\n")
