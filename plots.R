library("tseries")
library("PerformanceAnalytics")
#get monthly closing prices
rclPrices = get.hist.quote(instrument="RCL", start="2005-11-01",
                            end="2015-10-31", quote="Close",
                            provider="yahoo", origin="1970-01-01",
                            compression="m", retclass="zoo")
ndaqPrices = get.hist.quote(instrument="NDAQ", start="2005-11-01",
                           end="2015-10-31", quote="Close",
                           provider="yahoo", origin="1970-01-01",
                           compression="m", retclass="zoo")
vrtxPrices = get.hist.quote(instrument="VRTX", start="2005-11-01",
                            end="2015-10-31", quote="Close",
                            provider="yahoo", origin="1970-01-01",
                            compression="m", retclass="zoo")
wecPrices = get.hist.quote(instrument="WEC", start="2005-11-01",
                            end="2015-10-31", quote="Close",
                            provider="yahoo", origin="1970-01-01",
                            compression="m", retclass="zoo")
wfcPrices = get.hist.quote(instrument="WFC", start="2005-11-01",
                           end="2015-10-31", quote="Close",
                           provider="yahoo", origin="1970-01-01",
                           compression="m", retclass="zoo")
disPrices = get.hist.quote(instrument="DIS", start="2005-11-01",
                           end="2015-10-31", quote="Close",
                           provider="yahoo", origin="1970-01-01",
                           compression="m", retclass="zoo")
kssPrices = get.hist.quote(instrument="KSS", start="2005-11-01",
                           end="2015-10-31", quote="Close",
                           provider="yahoo", origin="1970-01-01",
                           compression="m", retclass="zoo")
lvltPrices = get.hist.quote(instrument="LVLT", start="2005-11-01",
                           end="2015-10-31", quote="Close",
                           provider="yahoo", origin="1970-01-01",
                           compression="m", retclass="zoo")
srclPrices = get.hist.quote(instrument="SRCL", start="2005-11-01",
                           end="2015-10-31", quote="Close",
                           provider="yahoo", origin="1970-01-01",
                           compression="m", retclass="zoo")
wbaPrices = get.hist.quote(instrument="WBA", start="2005-11-01",
                           end="2015-10-31", quote="Close",
                           provider="yahoo", origin="1970-01-01",
                           compression="m", retclass="zoo")
sp500Prices = get.hist.quote(instrument="^gspc", start="2005-11-01",
                             end="2015-10-31", quote="Close",
                             provider="yahoo", origin="1970-01-01",
                             compression="m", retclass="zoo")
#change the date 
index(rclPrices) = as.yearmon(index(rclPrices))
index(ndaqPrices) = as.yearmon(index(ndaqPrices))
index(vrtxPrices) = as.yearmon(index(vrtxPrices))
index(wecPrices) = as.yearmon(index(wecPrices))
index(wfcPrices) = as.yearmon(index(wfcPrices))
index(disPrices) = as.yearmon(index(disPrices))
index(kssPrices) = as.yearmon(index(kssPrices))
index(lvltPrices) = as.yearmon(index(lvltPrices))
index(srclPrices) = as.yearmon(index(srclPrices))
index(wbaPrices) = as.yearmon(index(wbaPrices))
index(sp500Prices) = as.yearmon(index(sp500Prices))
#merge 
rclSp500Prices = merge(rclPrices, sp500Prices)
#return
rclRetS = Return.calculate(rclPrices, method="simple")
ndaqRetS = Return.calculate(ndaqPrices, method="simple")
vrtxRetS = Return.calculate(vrtxPrices, method="simple")
wecRetS = Return.calculate(wecPrices, method="simple")
wfcRetS = Return.calculate(wfcPrices, method="simple")
disRetS = Return.calculate(disPrices, method="simple")
kssRetS = Return.calculate(kssPrices, method="simple")
lvltRetS = Return.calculate(lvltPrices, method="simple")
srclRetS = Return.calculate(srclPrices, method="simple")
wbaRetS = Return.calculate(wbaPrices, method="simple")
sp500RetS = Return.calculate(sp500Prices, method="simple")


#remove the NA value
rclRetS = rclRetS[-1]*100
ndaqRetS = ndaqRetS[-1] * 100
vrtxRetS = vrtxRetS[-1] * 100
wecRetS = wecRetS[-1] * 100
wfcRetS = wfcRetS[-1] * 100
disRetS = disRetS[-1] * 100
kssRetS = kssRetS[-1] * 100
lvltRetS = lvltRetS[-1] * 100
srclRetS = srclRetS[-1] * 100
wbaRetS = wbaRetS[-1] * 100
sp500RetS = sp500RetS[-1] * 100


#monthly return 
my.panel <- function(...) {
    lines(...)
    abline(h=0)
}
plot(rclSp500RetS, main="", panel=my.panel, lwd=2, col="blue")
#Equity Curves
equityCurveRcl = cumprod(1 + rclRetS)
equityCurvendaq = cumprod(1 + ndaqRetS)
equityCurvevrtx = cumprod(1 + vrtxRetS)
equityCurvewec = cumprod(1 + wecRetS)
equityCurvewfc = cumprod(1 + wfcRetS)
equityCurvedis = cumprod(1 + disRetS)
equityCurvekss = cumprod(1 + kssRetS)
equityCurvelvlt = cumprod(1 + lvltRetS)
equityCurvesrcl = cumprod(1 + srclRetS)
equityCurvewba = cumprod(1 + wbaRetS)
equityCurveSp500 = cumprod(1 + sp500RetS)
dataToPlot = merge(equityCurveRcl, equityCurvendaq, equityCurvevrtx, 
                   equityCurvewec, equityCurvewfc, equityCurvedis, equityCurvekss,
                   equityCurvelvlt, equityCurvesrcl, equityCurvewba, equityCurveSp500)
plot(dataToPlot,plot.type="single",col=topo.colors(n=11, alpha = 1),lwd=2,xlab = "Time" ,ylab="Cumulative Returns", main = "Equity Curve")
legend(x="topleft", legend=names(dataToPlot),
       col=topo.colors(n=11, alpha = 1), lwd=2)
#qqplot
par(mfrow=c(3,4))
#rcl
qqnorm(rclRetS, main = "RCL")
qqline(rclRetS)
#ndaq
qqnorm(ndaqRetS, main = "NDAQ")
qqline(ndaqRetS)
#vrtxRetS
qqnorm(vrtxRetS, main = "VRTX")
qqline(vrtxRetS)
#wecRetS
qqnorm(wecRetS, main = "WEC")
qqline(wecRetS)
#wfcRetS
qqnorm(wfcRetS, main = "WFC")
qqline(wfcRetS)
#disRetS
qqnorm(disRetS, main = "DIS")
qqline(disRetS)
#kssRetS
qqnorm(kssRetS, main = "KSS")
qqline(kssRetS)
#lvltRetS
qqnorm(lvltRetS, main = "LVLT")
qqline(lvltRetS)
#srclRetS
qqnorm(srclRetS, main = "SRCL")
qqline(srclRetS)
#wbaRetS
qqnorm(wbaRetS, main = "WBA")
qqline(wbaRetS)
#s&p500 sp500RetS
qqnorm(sp500RetS, main = "S&P500")
qqline(sp500RetS)

#hist
par(mfrow=c(3,4))
hist(rclRetS, main="RCL", col="cornflowerblue", breaks=15, ylim = c(0,50))
hist(ndaqRetS, main="NDAQ", col="cornflowerblue", breaks=15)
hist(vrtxRetS, main="VRTX", col="cornflowerblue", breaks=15)
hist(wecRetS, main="WEC", col="cornflowerblue", breaks=15)
hist(wfcRetS, main="WFC", col="cornflowerblue", breaks=15)
hist(disRetS, main="DIS", col="cornflowerblue", breaks=15)
hist(kssRetS, main="KSS", col="cornflowerblue", breaks=15)
hist(lvltRetS, main="LVLT", col="cornflowerblue", breaks=15)
hist(srclRetS, main="SRCL", col="cornflowerblue", breaks=15)
hist(wbaRetS, main="WBA", col="cornflowerblue", breaks=15)
hist(sp500RetS, main="SP500", col="cornflowerblue", breaks=15)
#boxplot
par(mfrow=c(3,4))
boxplot(as.vector(rclRetS), main = "RCL")
boxplot(as.vector(ndaqRetS), main = "NDAQ")
boxplot(as.vector(vrtxRetS), main = "VRTX")
boxplot(as.vector(wecRetS), main = "WEC")
boxplot(as.vector(wfcRetS), main = "WFC")
boxplot(as.vector(disRetS), main = "DIS")
boxplot(as.vector(kssRetS), main = "KSS")
boxplot(as.vector(lvltRetS), main = "LVLT")
boxplot(as.vector(srclRetS), main = "SRCL")
boxplot(as.vector(wbaRetS), main = "WBA")
boxplot(as.vector(sp500RetS), main = "S&P500")

#test for stationary
adf.test(rclRetS)
adf.test(ndaqRetS)
adf.test(vrtxRetS)
adf.test(wecRetS)
adf.test(wfcRetS)
adf.test(disRetS)
adf.test(kssRetS)
adf.test(lvltRetS)
adf.test(srclRetS)
adf.test(wbaRetS)
adf.test(sp500RetS)
#test normality 
shapiro.test(as.vector(rclRetS))
shapiro.test(as.vector(ndaqRetS))
shapiro.test(as.vector(vrtxRetS))
shapiro.test(as.vector(wecRetS))
shapiro.test(as.vector(wfcRetS))
shapiro.test(as.vector(disRetS))
shapiro.test(as.vector(kssRetS))
shapiro.test(as.vector(lvltRetS))
shapiro.test(as.vector(srclRetS))
shapiro.test(as.vector(wbaRetS))
shapiro.test(as.vector(sp500RetS))
pairs(dataToPlot, main = "Pairwise Scatter Plots Between Assets Return")
cor(dataToPlot)
