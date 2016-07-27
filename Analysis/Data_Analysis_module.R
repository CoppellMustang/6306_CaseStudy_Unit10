#----------------------------------------------------------------------------------------
#   1. PLot the As-Is and Plan
#   2. Correlation between As Is and Plan data 
#   3. Linear fit statistics
#   4. decompose using SLT
#----------------------------------------------------------------------------------------


#----------------------------------------------------------------------------------------
#                   1. PLot the As-Is and Plan for Wuge time series data
#----------------------------------------------------------------------------------------
par(mfrow=c(1,2)) 
cat("\n First we plot the As-Is and Plan data side-by-side to observe a relationship", "\n")
p_WugeAsIs <- plot(WugeAsIs, col="blue", main="WugeAsIs",lwd=3)
p_WugePlan <- plot(WugePlan, col="Green", main="WugePlan",lwd=3)
par(mfrow=c(1,1))



#----------------------------------------------------------------------------------------
#                   2. Correlation between As Is and Plan data 
#----------------------------------------------------------------------------------------
cor_WugeAsIs_Plan <- cor(WugeAsIs, WugePlan)


#----------------------------------------------------------------------------------------
#                   3. Linear fit statistics
#----------------------------------------------------------------------------------------
WugeAsIs_lm <- lm(WugeAsIs ~ WugePlan , data = WugeAsIs)




#----------------------------------------------------------------------------------------
#                   4. De compose usinf SLT
#----------------------------------------------------------------------------------------
WugeAsIs_stl <- stl(WugeAsIs, s.window=5)



#----------------------------------------------------------------------------------------
#                   5. Monthly breakdown <<moved>>
#----------------------------------------------------------------------------------------
# <moved to RMD> monthplot(WugeAsIs_stl$time.series[,"seasonal"], main="", ylab="Seasonal")