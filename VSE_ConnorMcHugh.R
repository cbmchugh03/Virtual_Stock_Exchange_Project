#First Transaction Code
install.packages("quantmod")
library(quantmod)

getSymbols('SPY', from="2020-10-10",to="2024-11-04") #Pulling SPY Data

tail(SPY)

SPY_Return <- dailyReturn(Ad(SPY)) #Computing daily return
tail(SPY_Return)

RTSCORE <- 15
A <- 0.5 + (10/RTSCORE) #Calculating my risk aversion

Invest <- 100000 #Setting Investment Amount
rf <- .0493/252  #Finding daily return

SPYm <- mean(SPY_Return)
SPYsd <- sd(SPY_Return)

y<- ((SPYm-rf)/(A*(SPYsd^2))) #Calculating y0*
y1 <- 2

Invest_SPY <- Invest*y1 #Determining what portion of wealth to invest in SPY

SPYPrice <- 575.28 #Setting SPYPrice
  
PropShare_SPY <- Invest_SPY/SPYPrice #Determining amount of shares of SPY to buy

#Code for EF Graph
library(quantmod)

nmlist<-c('XLK','XLV','XLF','XLY', 'XLP',
          'XLU','XLE','XLC','XLI','XLRE','XLB') #Downloading ETF data 
## Check the 1st element in nmlist
nmlist[1]

Returns<-list()
for (i in 1: length(nmlist)){
  getSymbols(nmlist[i], from='2018-07-31',to='2024-10-23') #Creating a loop for pulling data
  ret<-dailyReturn(Ad(get(nmlist[i])))
  Returns[[i]]<-ret
}
rt <- cbind(Returns[[1]], Returns[[2]])

returns <- do.call(cbind, Returns) #Combine multiple xts into 1 xts

class(returns)

returns <- round(returns, digits = 4) #Cleaning up returns data(rounding)
colnames(returns) <- nmlist
View(returns)

install.packages("fPortfolio")  #installing fPortfolio
library(fPortfolio)

returns_series<-as.timeSeries(returns) #Making the returns a time series so its compatible with fPortfolio

specEF<-portfolioSpec()
setRiskFreeRate(specEF)<-0.0488/252 #specifying risk free rate

frontier<-portfolioFrontier(returns_series,specEF) #starting to form efficient frontier
View(frontier)

frontier_points<-frontierPlot(frontier, pch = 19) #Plotting efficient frontier
grid()

View(frontier_points)
class(frontier_points)

EF_point<-singleAssetPoints(frontier, pch=19,cex=1.5, col="orange")#Add single asset points and labels
class(EF_point)
View(EF_point)

text( y=EF_point[,"targetReturn"],       #Adding more labels
      x=EF_point[,"targetRisk"],
      labels=row.names(EF_point),
      pos=4,cex=1)
minvariancePoints(frontier,pch = 19,cex=1.5, col = "red")
tangencyLines(frontier,lty=1,lwd=4,col = "blue")
tangencyPoints(frontier, pch = 19, cex=1.5, col = "green") #Adding minimum variance point and tangency line

install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
m_xlk<-mean(returns[,'XLK'])
s_xlk<-sd(returns[,'XLK'])
SR_xlk<-m_xlk/s_xlk
varisk_xlk<-quantile(returns[,'XLK'],probs = c(0.05))
es_xlk<-ES(returns[,'XLK'],p=0.05,method = 'historical') #Previous 6 lines compute financial indicators for XLK

colname<-c("mean", "std", "SR","VaR","ES")

stats<-matrix(NA,ncol = 5,nrow = 11,dimnames = list(nmlist, colname)) #Computes financial indicators for the rest of the ETFs
for(i in 1:length(nmlist))
{value<-returns[,nmlist[i]]
m = mean(value)
s = sd(value)
SR = m/s
varisk = quantile(value,probs = c(0.05))
es = ES(value,p=0.05,method = 'historical')

stats[i,1]<-m
stats[i,2]<-s
stats[i,3]<-SR
stats[i,4]<-varisk
stats[i,5]<-es
}
View(stats)
ETF8<-returns[,-which(colnames(returns) %in% c("XLK","XLE","XLY"))] #Removing 3 riskiest assets
myETF<-ETF8[,which(colnames(ETF8) %in% c("XLP","XLV","XLB"))] #Choosing assets for portfolio

library(fPortfolio)
myETF_Series<-as.timeSeries(myETF)
specPort<-portfolioSpec()
setRiskFreeRate(specPort)<-0.0488/252
minvar<-minvariancePortfolio(myETF_Series,specPort) #Finding minimum global variance portfolio

maxSharpe<-tangencyPortfolio(myETF_Series,specPort) #Finding tangency portfolio

summary(maxSharpe)   #View a summary of tangency portfolio
---

#Second Transaction Code
meanoptimal <- .0005
stdoptimal <- 0.0102
rf1029 <- .0488/252
y0tangency <- (meanoptimal-rf1029)/(A*stdoptimal^2) #Calculating y0* for the tangency portfolio
y0tangency1 <- 2                   #Setting y0* to 2 so max investment amount is 200,000
mrktXLV <- 148.12                  #Setting Market Price for the stocks in portfolio
mrktXLP <- 80.28
mrktXLB <- 94.64
networth <- 102137.36
XLVw <- 0.5228                     #Setting weights for portfolio
XLPw <- 0.3847
XLBw <- 0.0925
XLVshares <- (networth * y0tangency1 * XLVw)/mrktXLV           #Calculating how many shares of each ETF I should buy
XLPshares <- (networth * y0tangency1 * XLPw)/mrktXLP
XLBshares <- (networth * y0tangency1 * XLBw)/mrktXLB


#Code for Third Transaction
library("quantmod")
getSymbols('SPY', from="2018-07-31",to="2024-11-11") #Setting what date range for data
SPY_Return <- dailyReturn(Ad(SPY))

nmlist<-c('XLK','XLV','XLF','XLY', 'XLP',
          'XLU','XLE','XLC','XLI','XLRE','XLB') #Creating list with 11 sector ETFs
nmlist[1]

Returns<-list()
for (i in 1: length(nmlist)){
  getSymbols(nmlist[i], from='2018-07-31',to='2024-11-11') #Creating a loop for pulling data
  ret<-dailyReturn(Ad(get(nmlist[i])))
  Returns[[i]]<-ret
}
rt <- cbind(Returns[[1]], Returns[[2]])

returns <- do.call(cbind, Returns)
returns <- round(returns, digits = 4)
colnames(returns) <- nmlist

getSymbols('DGS3MO',src='FRED')
View(DGS3MO)
RF<-DGS3MO/100/252

CAPM<-merge(returns, RF, SPY_Return, all=FALSE)  #Merging data into one object
colnames(CAPM)[13] <- "Market"
View(CAPM)

marketEXERT<-CAPM$Market-CAPM$DGS3MO   #Calculating market excess return
XLVmodel<-lm((XLV-DGS3MO)~marketEXERT, data=CAPM)  #Creating our linear model
summary(XLVmodel)

XLVmodel$coefficients  #Exploring parts on our linear model and saving various components as different names
Intercept<-XLVmodel$coefficients[1]
Slope<-XLVmodel$coefficients[2]
alpha<-as.numeric(Intercept) #Turning the alpha into a numeric value

beta<-as.numeric(Slope)   #Turning the beta into a numeric value

Mdlsum<-summary(XLVmodel) #lM summary
sigma<- Mdlsum$sigma
Ratio<-alpha/sigma   #Calculating the information ratio

colname<-c("Alpha" , "Sigma" , "Beta" ," Ratio ")   #Starting to create table with all these metrics for the 11 ETFs

reg_results<-matrix(NA, nrow = 11, ncol = 4, dimnames = list(nmlist, colname))  #Creating the matrix
View(reg_results)   #Viewing the matrix

for (i in 1:length(nmlist)){
  marketEXERT<-CAPM$Market-CAPM$DGS3MO
  
  Model<-lm((get(nmlist[i])-DGS3MO)~marketEXERT,data = CAPM)
  alpha<-as.numeric(Model$coefficients[1])
  sigma<-summary(Model)$sigma
  beta<-as.numeric(Model$coefficients[2])
  ratio<-alpha/sigma
  reg_results[i,1]<-alpha
  reg_results[i,2]<-sigma
  reg_results[i,3]<-beta
  reg_results[i,4]<-ratio
}
View(reg_results)
#Filling matrix with all the necessary data and information, and analyzing to determine which ETFs to buy
XLK_P <- 235.85  #Determining the amount of shares of XLK, XLU, and XLV to buy 
  XLU_P <- 79.40
  XLV_P <- 149.30
  TotalCapital <- 14000
  Shares_Ratio <- TotalCapital /(XLK_P + XLU_P + XLV_P)

XLY_P <- 219.46   #Determining the amount of XLY, XLRE, and XLB to short sell
  XLRE_P <- 43.53
  XLB_P <- 94.06
  TotalCapital_Short <- 14000
Shares_Shortsell <- TotalCapital /(XLY_P + XLRE_P + XLB_P)