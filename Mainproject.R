library(Ecdat)
library(quadprog)
library(tseries)
library(PerformanceAnalytics)
library(zoo)


SP500 <- get.hist.quote("^GSPC",start="2007-03-23",end="2016-03-01",quote="AdjClose",provider="yahoo",compression="d")


##------------------------60 days --------------------------------
##1st step


setwd("/Users/quya/Google Drive/quant portfolio mgt/result/data")
price <- read.csv('price.csv',header=TRUE)
priceMatrix <- as.matrix(price[,2:14])
return <- read.csv('returndaily.csv',header=TRUE)
returnMatrix <- as.matrix(return[,2:14])
FB <- read.csv('betaMkt-60.csv',header=TRUE)
FBM <- as.matrix(FB[,2:14])
SMB <- read.csv('betaSMB-60.csv',header=TRUE)
SMBM <- as.matrix(SMB[,2:14])
HML <- read.csv('betaHML-60.csv',header=TRUE)
HMLM <- as.matrix(HML[,2:14])
Factors <- read.csv('quandl.csv',header=TRUE)
FactorsMatrix <- as.matrix(Factors[,2:5])
bet <- read.csv('betaCAPM-60.csv',header=TRUE)
Bet <- as.matrix(bet[,2:14])

rownames(priceMatrix) <- price[,1]
rownames(returnMatrix) <- return[,1]
rownames(FBM) <- FB[,1]
rownames(SMBM) <- SMB[,1]
rownames(HMLM) <- HML[,1]
rownames(FactorsMatrix) <- Factors[,1]
rownames(Bet) <- bet[,1]

##2nd: 60days opt 

source('/Users/quya/Google Drive/quant portfolio mgt/Result/RefeFunctions.R')
Targetbeta <- c(0.5,1,1.5)
N <- 2250
lamda <- 0.03
RR60 <- matrix(0,nrow=N-60,ncol=3)
for (i in 1:3){
RR60[,i] <- PortfolioReturn(priceMatrix,returnMatrix,FactorsMatrix,60,Targetbeta[i])
}


colnames(RR60) <- paste0("beta=",Targetbeta)
rownames(RR60) <- rownames(returnMatrix[-(1:60),])

bm60 <- matrix(0,nrow=N-60,ncol=1)
bm60 <- bm2opt(60)
bm60 <- matrix(bm60)
rownames(bm60) <- rownames(RR60)

Pnl60 <- tail(apply(RR60+1,2,cumprod),1)

PlotPrice(RR60,2,60)
PlotBeta(RR60)


abline(v=175,col="red")
PlotComp(RR60[,2],SP500,bm60,60)


## -------------------------------90days------------------------------------
##2nd step


price <- read.csv('price.csv',header=TRUE)
priceMatrix <- as.matrix(price[,2:14])
return <- read.csv('returndaily.csv',header=TRUE)
returnMatrix <- as.matrix(return[,2:14])
FB <- read.csv('betaMkt-90.csv',header=TRUE)
FBM <- as.matrix(FB[,2:14])
SMB <- read.csv('betaSMB-90.csv',header=TRUE)
SMBM <- as.matrix(SMB[,2:14])
HML <- read.csv('betaHML-90.csv',header=TRUE)
HMLM <- as.matrix(HML[,2:14])
Factors <- read.csv('quandl.csv',header=TRUE)
FactorsMatrix <- as.matrix(Factors[,2:5])
bet <- read.csv('betaCAPM-90.csv',header=TRUE)
Bet <- as.matrix(bet[,2:14])

rownames(priceMatrix) <- price[,1]
rownames(returnMatrix) <- return[,1]
rownames(FBM) <- FB[,1]
rownames(SMBM) <- SMB[,1]
rownames(HMLM) <- HML[,1]
rownames(FactorsMatrix) <- Factors[,1]
rownames(Bet) <- bet[,1]


##:90 days opt


RR90 <- matrix(0,nrow=N-90,ncol=3)
for (i in 1:3){
  RR90[,i] <- PortfolioReturn(priceMatrix,returnMatrix,FactorsMatrix,90,Targetbeta[i])
}

colnames(RR90) <- paste0("beta=",Targetbeta)
rownames(RR90) <- rownames(returnMatrix[-(1:90),])


Pnl90 <- tail(apply(RR90+1,2,cumprod),1)

PlotPrice(RR90,2,90)
PlotBeta(RR90)

bm90 <- matrix(0,nrow=N-90,ncol=1)
bm90 <- bm2opt(90)
PlotComp(RR90[,2],SP500,bm90,90)


##-------------------------------------120 days------------------
##3rd: 120 days opt


price <- read.csv('price.csv',header=TRUE)
priceMatrix <- as.matrix(price[,2:14])
return <- read.csv('returndaily.csv',header=TRUE)
returnMatrix <- as.matrix(return[,2:14])
FB <- read.csv('betaMkt-120.csv',header=TRUE)
FBM <- as.matrix(FB[,2:14])
SMB <- read.csv('betaSMB-120.csv',header=TRUE)
SMBM <- as.matrix(SMB[,2:14])
HML <- read.csv('betaHML-120.csv',header=TRUE)
HMLM <- as.matrix(HML[,2:14])
Factors <- read.csv('quandl.csv',header=TRUE)
FactorsMatrix <- as.matrix(Factors[,2:5])
bet <- read.csv('betaCAPM-120.csv',header=TRUE)
Bet <- as.matrix(bet[,2:14])

rownames(priceMatrix) <- price[,1]
rownames(returnMatrix) <- return[,1]
rownames(FBM) <- FB[,1]
rownames(SMBM) <- SMB[,1]
rownames(HMLM) <- HML[,1]
rownames(FactorsMatrix) <- Factors[,1]
rownames(Bet) <- bet[,1]



RR120 <- matrix(0,nrow=N-120,ncol=3)
for (i in 1:3){
  RR120[,i] <- PortfolioReturn(priceMatrix,returnMatrix,FactorsMatrix,120,Targetbeta[i])
}

colnames(RR120) <- paste0("beta=",Targetbeta)
rownames(RR120) <- rownames(returnMatrix[-(1:120),])

Pnl120 <- tail(apply(RR120+1,2,cumprod),1)

PlotPrice(RR120,2,120)
PlotBeta(RR120)
bm120 <- matrix(0,nrow=N-120,ncol=1)
bm120 <- bm2opt(120)
PlotComp(RR120[,2],SP500,bm120,120)



##Pnl table

Pnl <- rbind(Pnl60,Pnl90,Pnl120)
rownames(Pnl) <- c("60days","90days","120days")
colnames(Pnl) <- c("Beta=0.5","Beta=1","Beta=1.5")




##-------------------Difference in the length of days--------------------

#beta=0.5

Ndays <- N-120
Ret <- RR60[(61:2190),1]
Price <- 100*(cumprod(Ret+1))
PriceM <- matrix(Price,Ndays,1)
plot(PriceM,type='l',xlim=range(0:2250),ylim=range(0:400),main="Different Length",sub="lamda=0.03,beta=0.5",xlab="days",ylab="price",col="red")
Ret <- RR90[(31:2160),1]
Price <- 100*(cumprod(Ret+1))
PriceM <- matrix(Price,Ndays,1)
lines(PriceM,type='l',col="blue")
Ret <- RR120[,1]
Price <- 100*(cumprod(Ret+1))
PriceM <- matrix(Price,Ndays,1)
lines(PriceM,type='l',col="green")
legend(0,400,legend=c("60days","90days","120days"),col=c("red","blue","green"),lwd=3, lty=1,bty="n")



#beta=1
Ndays <- N-120
Ret <- RR60[(61:2190),2]
Price <- 100*(cumprod(Ret+1))
PriceM <- matrix(Price,Ndays,1)
plot(PriceM,type='l',xlim=range(0:2250),ylim=range(0:400),main="Different Length",sub="lamda=0.03,beta=1",xlab="days",ylab="price",col="red")
Ret <- RR90[(31:2160),2]
Price <- 100*(cumprod(Ret+1))
PriceM <- matrix(Price,Ndays,1)
lines(PriceM,type='l',col="blue")
Ret <- RR120[,2]
Price <- 100*(cumprod(Ret+1))
PriceM <- matrix(Price,Ndays,1)
lines(PriceM,type='l',col="green")
legend(0,400,legend=c("60days","90days","120days"),col=c("red","blue","green"),lwd=3, lty=1,bty="n")

#beta=1.5

Ndays <- N-120
Ret <- RR60[(61:2190),3]
Price <- 100*(cumprod(Ret+1))
PriceM <- matrix(Price,Ndays,1)
plot(PriceM,type='l',xlim=range(0:2250),ylim=range(0:400),main="Different Length",sub="lamda=0.03,beta=1.5",xlab="days",ylab="price",col="red")
Ret <- RR90[(31:2160),3]
Price <- 100*(cumprod(Ret+1))
PriceM <- matrix(Price,Ndays,1)
lines(PriceM,type='l',col="blue")
Ret <- RR120[,3]
Price <- 100*(cumprod(Ret+1))
PriceM <- matrix(Price,Ndays,1)
lines(PriceM,type='l',col="green")
legend(0,400,legend=c("60days","90days","120days"),col=c("red","blue","green"),lwd=3, lty=1,bty="n")



#Using RR120 as the final comparision
#Construct the perfomance matrix

return_SP500 <- Return.calculate(SP500)[-1,]
return_SP500 <- return_SP500[-(1:120),]
SP2 <- matrix(return_SP500)

performance_matrix <- cbind(returnMatrix[-(1:120),],RR120,bm120,SP2)
colnames(performance_matrix) <-c(colnames(returnMatrix),colnames(Pnl),"Benchmark 2","S&P 500") 

#Get the summary table of performance 
performance <- summary_table(performance_matrix)


##Different market enviorment analysis
##before the crises

ReturnBC <- cbind(RR60[61:115,],RR90[31:85,],RR120[1:55,])
colnames(ReturnBC) <- c(paste0("60days ",colnames(RR60)),paste0("90days ",colnames(RR90)),paste0("120days ",colnames(RR120)))
performanceBC <- summary_table(ReturnBC)

#max cumulative return
which(performanceBC[1,]==max(performanceBC[1,]))
#min volatility
which(performanceBC[5,]==min(performanceBC[5,]))
#max daily min return
which(performanceBC[3,]==max(performanceBC[3,]))
#min max drawdown
which(performanceBC[4,]==max(performanceBC[4,]))
#max Sharpe ratio
which(performanceBC[6,]==max(performanceBC[6,]))

#to plot the cumulative return
CumReturnBC <- apply((ReturnBC+1),2,cumprod)
CumReturnBC <-  as.zoo(CumReturnBC)
plot(CumReturnBC,main="Return before Crises",plot.type="single",col=c("black","red","blue","green","yellow","orange","brown","grey","purple"))
legend(x=0,y=1.3,legend=colnames(ReturnBC),col=c("black","red","blue","green","yellow","orange","brown","grey","purple"),lty=1,cex = 0.5)


#During the Crises

ReturnDC <- cbind(RR60[116:490,],RR90[86:460,],RR120[56:430,])
colnames(ReturnDC) <- c(paste0("60days ",colnames(RR60)),paste0("90days ",colnames(RR90)),paste0("120days ",colnames(RR120)))
performanceDC <- summary_table(ReturnDC)

#max cumulative return
which(performanceDC[1,]==max(performanceDC[1,]))
#min volatility
which(performanceDC[5,]==min(performanceDC[5,]))
#max daily min return
which(performanceDC[3,]==max(performanceDC[3,]))
#min max drawdown
which(performanceDC[4,]==max(performanceDC[4,]))
#max Sharpe ratio
which(performanceDC[6,]==max(performanceDC[6,]))

#to plot the cumulative return
CumReturnDC <- apply((ReturnDC+1),2,cumprod)
CumReturnDC <-  as.zoo(CumReturnDC)
plot(CumReturnDC,main="Return during Crises",plot.type="single",col=c("black","red","blue","green","yellow","orange","brown","grey","purple"))
legend(x=0,y=0.6,legend=colnames(ReturnDC),col=c("black","red","blue","green","yellow","orange","brown","grey","purple"),lty=1,cex = 0.5)


##Afer Crises

ReturnAC <- cbind(RR60[491:2190,],RR90[461:2160,],RR120[431:2130,])
colnames(ReturnAC) <- c(paste0("60days ",colnames(RR60)),paste0("90days ",colnames(RR90)),paste0("120days ",colnames(RR120)))
performanceAC <- summary_table(ReturnAC)

#max cumulative return
which(performanceAC[1,]==max(performanceAC[1,]))
#min volatility
which(performanceAC[5,]==min(performanceAC[5,]))
#max daily min return
which(performanceAC[3,]==max(performanceAC[3,]))
#min max drawdown
which(performanceAC[4,]==max(performanceAC[4,]))
#max Sharpe ratio
which(performanceAC[6,]==max(performanceAC[6,]))

#to plot the cumulative return
CumReturnAC <- apply((ReturnAC+1),2,cumprod)
CumReturnAC <-  as.zoo(CumReturnAC)
plot(CumReturnAC,main="Return after Crises",plot.type="single",col=c("black","red","blue","green","yellow","orange","brown","grey","purple"))
legend(x=0,y=6,legend=colnames(ReturnAC),col=c("black","red","blue","green","yellow","orange","brown","grey","purple"),lty=1,cex = 0.5)


write.csv(performanceBC,file = "performanceBC.csv")
write.csv(performanceDC,file = "performanceDC.csv")
write.csv(performanceAC,file = "performanceAC.csv")

#SP500 performance subsetting

performance_SP <- apply(SP2+1,2,cumprod)

rownames(SP2) <- rownames(RR120)
SPBC<- as.matrix(SP2[1:55,])
SPDC <- as.matrix(SP2[56:430,])
SPAC <- as.matrix(SP2[431:2130,])

perform_SPBC <- summary_table(SPBC)
perform_SPDC <- summary_table(SPDC)
perform_SPAC <- summary_table(SPAC)

SP_perform <- cbind(perform_SPBC,perform_SPDC,perform_SPAC)
colnames(SP_perform) <- c("Before Crises","During Crises","After Crise")

write.csv(SP_perform,file="SPperform.csv")

plot(SP2,type="l")
abline(v=c(55,430),col="red")


##Same beta comparison

beta0.5<- cbind(RR60[-(1:60),1],RR90[-(1:30),1],RR120[,1])
beta0.5return <- apply((beta0.5+1),2,cumprod)
beta0.5return <- as.zoo(beta0.5return)
plot(beta0.5return,main="Best estimator beta=0.5",type="l", plot.type="single",col=c("black","red","blue"))
legend(x=0,y=2.5,legend=c("60days","90days","120days"),col=c("black","red","blue"),lty=1)

beta1<- cbind(RR60[-(1:60),2],RR90[-(1:30),2],RR120[,2])
beta1return <- apply((beta1+1),2,cumprod)
beta1return <- as.zoo(beta1return)
plot(beta1return,main="Best estimator beta=1",type="l", plot.type="single",col=c("black","red","blue"))
legend(x=0,y=3.0,legend=c("60days","90days","120days"),col=c("black","red","blue"),lty=1)

beta1.5<- cbind(RR60[-(1:60),3],RR90[-(1:30),3],RR120[,3])
beta1.5return <- apply((beta1.5+1),2,cumprod)
beta1.5return <- as.zoo(beta1.5return)
plot(beta1.5return,main="Best estimator beta=1.5",type="l", plot.type="single",col=c("black","red","blue"))
legend(x=0,y=3.5,legend=c("60days","90days","120days"),col=c("black","red","blue"),lty=1)



write.csv(Pnl,file="Pnl.csv")
write.csv(performance,file="performance.csv")
