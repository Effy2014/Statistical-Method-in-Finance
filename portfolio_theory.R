library(e1071) 
setwd("/Users/XW/Desktop/4290Project")
dataset <- read.csv("dataset.csv", header = T)
data <- dataset[2:12]
library(ggplot2)
library(scales)
library(reshape2)
library(quadprog)
str(dataset)
dataset$Date <- as.Date(dataset$Date, "%m/%d/%y")
#create variables of the month 
dataset$month <- as.Date(cut(dataset$Date, breaks = "month"))
price <- melt(dataset[2:13], id = ("month"))
ggplot(data = price, aes(x = month, y = value, group = variable, color = variable)) + 
    geom_line() + 
    stat_summary(fun.y = sum, # adds up all observations for the month
                 geom = "line") + # or "line"
    scale_x_date(labels = date_format("%y"), breaks = "1 year") +
    xlab("Time") +
    ylab("Closing Price") +
    ggtitle("10 Stocks' Monthly Closing Prices Over 10 years")

n = dim(dataset)[1]
Return = dataset[2:n,2:12]/dataset[1:(n-1),2:12] - 1
Return$Month <- dataset$month[2:n]
returns <- Return[1:10] * 100

mean <- colMeans(returns)
sd <- apply(returns, 2, sd)
Skew <- apply(returns, 2, skewness)
Kurt <- apply(returns, 2, kurtosis)
gen.info <- rbind(mean, sd, Skew, Kurt)
flag <- data.frame(returns, Return$Month)
ReTurn <- melt(flag, id = ("Return.Month"))
names(ReTurn) = c("Month", "variable", "value")
ggplot(data = ReTurn, aes(x = Month, y = value, group = variable, color = variable)) + 
    geom_line() + 
    stat_summary(fun.y = sum, # adds up all observations for the month
                 geom = "line") + # or "line"
    scale_x_date(labels = date_format("%y"), breaks = "1 year") +
    xlab("Time") +
    ylim(c(-100, 100))+
    ylab("Return") +
    ggtitle("10 Stocks' Return Over 10 years")
# get sp500 monthly closing price 
sp500Prices = get.hist.quote(instrument="^gspc", start="2005-11-01",
                             end="2015-10-31", quote="Close",
                             provider="yahoo", origin="1970-01-01",
                             compression="m", retclass="zoo")
# Returns of S&P 500 Index
index(sp500Prices) = as.yearmon(index(sp500Prices))
sp500RetS = Return.calculate(sp500Prices, method="simple")
# equity curve
#interest rate of risk asset
dat <-  read.csv("FRB_H15.csv", header = F)
dat1 = dat[c(54:172),]
risk_free = sum(dat1[,2])/(119*12)

returns <- Return[1:10] * 100
pairs(returns)
mean_vect = apply(returns, 2, mean)
cov_mat = cov(returns)
sd_vect = sqrt(diag(cov_mat))

Amat = cbind(rep(1,10),mean_vect)  # set the constraints matrix
muP = seq(min(mean_vect),max(mean_vect),length=10000)  # set of 300 possible target values
# for the expect portfolio return
sdP = numeric(length(muP))
weights = matrix(0,nrow=10000,ncol=10) # storage for portfolio weights
for (i in 1:length(muP))  # find the optimal portfolios for
    # each target expected return
{
    bvec = c(1,muP[i])  # constraint vector
    result =
        solve.QP(Dmat=2*cov_mat,dvec=rep(0,10),Amat=Amat,bvec=bvec,meq=2)
    sdP[i] = sqrt(result$value)
    weights[i,] = result$solution
}
plot(sdP,muP,type="l",xlim= c(0,20),ylim=c(0,4),lty=3)
points(0,risk_free,cex=4,pch="*") # show risk-free asset
sharpe =(muP-risk_free)/sdP # compute Sharpe’s ratios
ind = (sharpe == max(sharpe)) # Find maximum Sharpe’s ratio
options(digits=3)
weights[ind,] #  print the weights of the tangency portfolio short sale
lines(c(0,7),risk_free+c(0,7)*(muP[ind]-risk_free)/sdP[ind],lwd=4,lty=2)
# show line of optimal portfolios
points(sdP[ind],muP[ind],cex=4,pch="*") # show tangency portfolio
ind2 = (sdP == min(sdP)) # find the minimum variance portfolio
points(sdP[ind2],muP[ind2],cex=2,pch="+") # show min var portfolio
ind3 = (muP > muP[ind2])
lines(sdP[ind3],muP[ind3],type="l",xlim=c(0,.25),
      ylim=c(0,.3),lwd=2)  #  plot the efficient frontier
#title("Efficient Portfolio Frontier With Short Sell")
text(sd_vect[1],mean_vect[1],"RCL",cex=0.5)
text(sd_vect[2],mean_vect[2],"NDAQ",cex=0.5)
text(sd_vect[3],mean_vect[3],"VRTX",cex=0.5)
text(sd_vect[4],mean_vect[4],"WEC",cex=0.5)
text(sd_vect[5],mean_vect[5],"WFC",cex=0.5)
text(sd_vect[6],mean_vect[6],"DIS",cex=0.5)
text(sd_vect[7],mean_vect[7],"KSS",cex=0.5)
text(sd_vect[8],mean_vect[8],"LVLT",cex=0.5)
text(sd_vect[9],mean_vect[9],"SRCL",cex=0.5)
text(sd_vect[10],mean_vect[10],"WBA",cex=0.5)
title("Tangency Portfolio With Short Sell")
#no short 
Amat = cbind(rep(1,10),mean_vect, diag(1, nrow = 10))  # set the constraints matrix
muP = seq(min(mean_vect) + 0.0001 ,max(mean_vect) - 0.0001, length=10000)  # set of 300 possible target values
# for the expect portfolio return
sdP = numeric(length(muP))
weights = matrix(0,nrow=10000,ncol=10) # storage for portfolio weights
for (i in 1:length(muP))  # find the optimal portfolios for
    # each target expected return
{
    bvec = c(1,muP[i], rep(0, 10))  # constraint vector
    result =
        solve.QP(Dmat=2*cov_mat,dvec=rep(0,10),Amat=Amat,bvec=bvec,meq=2)
    sdP[i] = sqrt(result$value)
    weights[i,] = result$solution
}
plot(sdP,muP,type="l",xlim= c(0,30),ylim=c(0,4),lty=3)
points(0,risk_free,cex=4,pch="*") # show risk-free asset
sharpe =(muP-risk_free)/sdP # compute Sharpe’s ratios
ind = (sharpe == max(sharpe)) # Find maximum Sharpe’s ratio
options(digits=3)
weights[ind,] #  print the weights of the tangency portfolio no short sale
lines(c(0,7),risk_free+c(0,7)*(muP[ind]-risk_free)/sdP[ind],lwd=4,lty=2)
# show line of optimal portfolios
points(sdP[ind],muP[ind],cex=4,pch="*") # show tangency portfolio
ind2 = (sdP == min(sdP)) # find the minimum variance portfolio
points(sdP[ind2],muP[ind2],cex=2,pch="+") # show min var portfolio
ind3 = (muP > muP[ind2])
lines(sdP[ind3],muP[ind3],type="l",xlim=c(0,.25),
      ylim=c(0,.3),lwd=2)  #  plot the efficient frontier
#title("Efficient Portfolio Frontier Without Short Sell")
text(sd_vect[1],mean_vect[1],"RCL",cex=.5)
text(sd_vect[2],mean_vect[2],"NDAQ",cex=.5)
text(sd_vect[3],mean_vect[3],"VRTX",cex=.5)
text(sd_vect[4],mean_vect[4],"WEC",cex=.5)
text(sd_vect[5],mean_vect[5],"WFC",cex=.5)
text(sd_vect[6],mean_vect[6],"DIS",cex=.5)
text(sd_vect[7],mean_vect[7],"KSS",cex=.5)
text(sd_vect[8],mean_vect[8],"LVLT",cex=.5)
text(sd_vect[9],mean_vect[9],"SRCL",cex=.5)
text(sd_vect[10],mean_vect[10],"WBA",cex=.5)
title("Tangency Portfolio Without Short Sell")
#calculate the mean and var
#for tangency
weights[ind, ]
weights[ind, ]%*%mean_vect
weights[ind, ]%*%cov_mat%*%weights[ind, ]
#for mvp
weights[ind2, ]
weights[ind2, ]%*%mean_vect
weights[ind2, ]%*%cov_mat%*%weights[ind2, ]
#sharp ratio
(mean_vect-risk_free)/sd_vect
