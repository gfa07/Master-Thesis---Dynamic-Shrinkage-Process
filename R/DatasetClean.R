

ff5 = read.csv("FF_Factors_5_2007_2017.csv")
mom = read.csv("FF_Momentum_2007_2017.csv")
industry = read.csv("Industry_Portfolios_5_2007_2017.csv")

ManufHlthWithoutPercent = industry[ ,2:3] / 100
RF = ff5$RF / 100

yDailiesWithoutPercent = ManufHlthWithoutPercent - RF
yDailiesWithoutPercent = cbind(Dates = industry[ ,"Dates"], yDailiesWithoutPercent)

varOfInterDaily = cbind(ff5[ ,2:6], MOM = mom[ ,2])
varOfInterDailyWithoutPercent = varOfInterDaily / 100
varOfInterDailyWithoutPercent = cbind(Dates = ff5[ ,"Dates"], varOfInterDailyWithoutPercent)


library("lubridate")
varOfInterDailyWithoutPercent$Dates = ymd(varOfInterDailyWithoutPercent$Dates)
varOfInterDailyWithoutPercent$Week = week(varOfInterDailyWithoutPercent$Dates)

varOfInterDailyWithoutPercent$Week = floor_date(varOfInterDailyWithoutPercent$Dates, unit = 'week')
varOfInterDailyWithoutPercent$Week = as.integer(factor(varOfInterDailyWithoutPercent$Week))

yDailiesWithoutPercent$Dates = ymd(yDailiesWithoutPercent$Dates)
yDailiesWithoutPercent$Week = week(yDailiesWithoutPercent$Dates)

yDailiesWithoutPercent$Week = floor_date(yDailiesWithoutPercent$Dates, unit = 'week')
yDailiesWithoutPercent$Week = as.integer(factor(yDailiesWithoutPercent$Week))


#Formula for multi-Period Returns: ((1+R_1) * ... * (1+R_N)) - 1

dailyToWeekly = function(df, variab){
  
  maxNweeks = max(df$Week)
  weeklyReturns = matrix(NA, nrow = maxNweeks, ncol = length(variab), dimnames = list(1:maxNweeks, variab))
  
  for (j in 1:maxNweeks) {
    weekData = df[df$Week == j, ]
    
    for(i in variab){
      weeklyReturns[j, i] = prod(1 + weekData[[i]]) - 1
      
      }
    
  }
  return(weeklyReturns)
}

variables1 = c("Mkt.RF", "SMB", "HML", "RMW", "CMA", "MOM")

weekReturnsVarOfInter = dailyToWeekly(df = varOfInterDailyWithoutPercent, variab = variables1)


variables2 = c("Manuf", "Hlth")

weekReturnsYs = dailyToWeekly(df = yDailiesWithoutPercent, variab = variables2)


dim(weekReturnsVarOfInter)
dim(weekReturnsYs)
#Matches the T = 522!

predictors = weekReturnsVarOfInter
yManuf = weekReturnsYs[ ,1]
yHlth = weekReturnsYs[ ,2]

#Standardize Data -> Unit Variance
library('effectsize')
predictorsStd = standardize(predictors)
apply(predictorsStd, 2, sd)
XStd = cbind(Intercept = 1, predictorsStd)

yManufStd = standardize(yManuf) 
sd(yManufStd)
yHlthStd = standardize(yHlth)
sd(yHlthStd)
#Note that data isn't in percentage anymore! But with standardization that doesn't matter because that would cancel out! 

finalDataSet = cbind(yManufStd, yHlthStd, XStd)
#write.csv(finalDataSet, file = "FinalDataset.csv")



prettycol = c("#6C8EBF", "#c0a34d", "#780000","#007878","#B5C6DF","#EADAAA","#AE6666",
              "#9E4F4F", "#2F4858", "#7A6C5D","#3A7D7D")
origdatset = read.csv("F-F_Research_Data_Factors_weekly_2007_2017.csv") 

par(mfrow = c(3,1))
plot(predictors[,"Mkt.RF"]*100, type = 'l', xlab='TimeStep', ylab = expression(R[M] - R[F]), col = prettycol[1], lwd=1)
lines(origdatset$Mkt.RF, col = prettycol[2], lwd=1)

plot(predictors[,"SMB"]*100, type = 'l', xlab='TimeStep', ylab = 'SMB', col = prettycol[1], lwd=1)
lines(origdatset$SMB, col = prettycol[2], lwd=1)

plot(predictors[,"HML"]*100, type = 'l', xlab='TimeStep', ylab = 'HML', col = prettycol[1], lwd=1)
lines(origdatset$HML, col = prettycol[2], lwd=1)
