#this file is used to store reference functions in the main project.
#1.benchmark 2 portfolio optimization
i=38
bm2opt <- function(estim){
N <- nrow(returnMatrix)
bm2 <- rep(0,N-estim)
begIndex <- estim
mkt <- mean(FactorsMatrix[(1:begIndex),1])
smb <- mean(FactorsMatrix[(1:begIndex),2])
hml<- mean(FactorsMatrix[(1:begIndex),3])
rf <- mean(FactorsMatrix[(1:begIndex),4])
EReturn <- 250*(mkt*FBM[1,]+smb*SMBM[1,]+hml*HMLM[1,]+rf)/100
wp <- c(rep(1/13,13))
retdata <- 250*returnMatrix[(1:begIndex),]
covmat <- cov(retdata)
dvec_bench_2 <- 2*lamda*wp
return_bench_2 <- EReturn
bvec_bench_2 <- c(1,15/100,rep(-2,26))
cov_bench_2 <- covmat+2*lamda*diag(1,13)
Amat <- cbind(1,return_bench_2,diag(1,13),diag(-1,13))
Opt <- solve.QP(cov_bench_2,dvec_bench_2,Amat,bvec_bench_2,meq=2)
solu<- Opt$solution
rvec <- returnMatrix[(begIndex+1),]
bm2[1] <- t(rvec)%*%solu
for (i in (2:(N-estim))){
  mkt <- mean(FactorsMatrix[i:(begIndex+i-1),1])
  smb <- mean(FactorsMatrix[i:(begIndex+i-1),2])
  hml<- mean(FactorsMatrix[i:(begIndex+i-1),3])
  rf <- mean(FactorsMatrix[i:(begIndex+i-1),4])
  EReturn <- 250*(mkt*FBM[i,]+smb*SMBM[i,]+hml*HMLM[i,]+rf)/100
  retdata <- 250*returnMatrix[i:(begIndex+i-1),]
  covmat <- cov(retdata)
  dvec_bench_2 <- 2*lamda*solu
  return_bench_2 <- EReturn
  bvec_bench_2 <- c(1,15/100)
  cov_bench_2 <- covmat+2*lamda*diag(1,13)
  Amat <- cbind(1,return_bench_2)
  opt_sol <- solve.QP(cov_bench_2,dvec_bench_2,Amat,bvec_bench_2,meq=2)
  solu <- opt_sol$solution
  rvec <- returnMatrix[(begIndex+i),]
  bm2[i] <- t(rvec)%*%solu
} 
return(bm2)
}


#2. setting beta, make optimization
betaopt <- function(beta,EReturn,Targetbeta,solu,lamda,covmat){
  Amat <- cbind(rep(1,13),beta,diag(1,13),diag(-1,13))
  bvec <- c(1,Targetbeta,rep(-2,26))
  m <- 1
  n <- 1
  # 1st:m=1,n=0; 2nd:m=0,n=1; 3rd: m=1,n=1
  dvec <- m*EReturn+2*lamda*solu
  Dmat <- 2*lamda*(diag(1,13)+n*covmat)
  opt_sol <- solve.QP(Dmat, dvec, Amat, bvec, meq=2, factorized=FALSE)
  return(opt_sol)
}



#3.This function is used to generate optimal portoflio price
PortfolioReturn <- function(priceMatrix,returnMatrix,FactorsMatrix,estim,Tbeta){
begIndex <- estim
RollTime <- N -estim
RR=c(rep(0,RollTime))
#1st ret
mkt <- mean(FactorsMatrix[(1:begIndex),1])
smb <- mean(FactorsMatrix[(1:begIndex),2])
hml<- mean(FactorsMatrix[(1:begIndex),3])
rf <- mean(FactorsMatrix[(1:begIndex),4])
EReturn <- 250*(mkt*FBM[1,]+smb*SMBM[1,]+hml*HMLM[1,]+rf)/100
wp <- c(rep(1/13,13))
retdata <- 250*returnMatrix[(1:begIndex),]
covmat <- cov(retdata)
opt_sol <- betaopt(Bet[1,],EReturn,Tbeta,wp,lamda,covmat)
solu <- opt_sol$solution
rvec <- returnMatrix[(begIndex+1),]
RR[1] <- t(rvec)%*%solu
for (i in 2:RollTime){
  mkt <- mean(FactorsMatrix[i:(begIndex+i-1),1])
  smb <- mean(FactorsMatrix[i:(begIndex+i-1),2])
  hml<- mean(FactorsMatrix[i:(begIndex+i-1),3])
  rf <- mean(FactorsMatrix[i:(begIndex+i-1),4])
  EReturn <- 250*(mkt*FBM[i,]+smb*SMBM[i,]+hml*HMLM[i,]+rf)/100
  retdata <- 250*returnMatrix[i:(begIndex+i-1),]
  covmat <- cov(retdata)
  opt_sol <- betaopt(Bet[i,],EReturn,Tbeta,solu,lamda,covmat)
  solu <- opt_sol$solution
  rvec <- returnMatrix[(begIndex+i),]
  RR[i] <- t(rvec)%*%solu
  
}

return(RR)
}

#4. this function is used to plot the curve.
PlotPrice <- function(RR,n,estim){
  Ndays <- nrow(RR)
  Ret <- RR[,n]
  Price <- 100*(cumprod(Ret+1))
  PriceM <- matrix(Price,Ndays,1)
  ts <- read.csv("/Users/quya/Google Drive/quant portfolio mgt/result/data/returndaily.csv",header=T)[,1][-(1:estim)]
  rownames(PriceM) <- ts
  plot(PriceM,type='l',main="Best OPT",sub="beta=1.5,lambda=0.03",ylab="price") 
}

#5. this function is used to plot the 3 beta curves in one graph
PlotBeta <- function(RR){
  Ndays <- nrow(RR)
  Ret <- RR[,1]
  Price <- 100*(cumprod(Ret+1))
  PriceM <- matrix(Price,Ndays,1)
  plot(PriceM,type='l',xlim=range(0:2150),ylim=range(0:400),main="Best OPT",sub="lamda=0.03",ylab="price",col="red")
  Ret <- RR[,2]
  Price <- 100*(cumprod(Ret+1))
  PriceM <- matrix(Price,Ndays,1)
  lines(PriceM,type='l',col="blue")
  Ret <- RR[,3]
  Price <- 100*(cumprod(Ret+1))
  PriceM <- matrix(Price,Ndays,1)
  lines(PriceM,type='l',col="green")
  legend(0,400,legend=c("beta=0.5","beta=1","beta=1.5"),col=c("red","blue","green"),lwd=3, lty=1,bty="n")
}

#6. this function is used to plot comparison of OPT,S&P and Min(volatility)
PlotComp <- function(opt,SP,bm2,estim){
  return_SP500 <- Return.calculate(SP500)[-1,]
  return_SP500 <- return_SP500[-(1:estim),]
  benchmarkSP500 <- 100*(cumprod(return_SP500+1))
  SP <- matrix(benchmarkSP500)
  Ndays <- 2250-estim
  Price <- 100*(cumprod(opt+1))
  PriceM <- matrix(Price,Ndays,1)
  plot(PriceM,type='l',xlim=range(0:2150),ylim=range(0:400),main="Performance",sub="lamda=0.03",ylab="price",col="red")
  lines(SP,type='l',col="blue")
  Price <- 100*(cumprod(bm2+1))
  PriceM <- matrix(Price,Ndays,1)
  lines(PriceM,type='l',col="green")
  legend(0,400,legend=c("MaxRet","S&P","MinVol"),col=c("red","blue","green"),lwd=3, lty=1,bty="n")
}

#7 summary table


summary_table <- function(ret_urn){
  n_col <- ncol(ret_urn)
  summary_matrix <- matrix(0,10,n_col)
  colnames(summary_matrix) <- colnames(ret_urn)
  rownames(summary_matrix) <- c("Cumulated PnL","Daily Mean Geometric Return","Daily Min Return","Max 10 days Drawdown","Volatility","Sharpe Ratio","Skewness","Kurtosis","Modified VaR","CVaR")
  
  ret_urn2 <- ret_urn+1
  a <- apply(ret_urn2,2,cumprod)
  summary_matrix[1,]<- tail(a,1)
  
  summary_matrix[2,] <- apply(ret_urn2,2,mean.geometric)-1
  summary_matrix[3,] <- apply(ret_urn,2,min)
  
  a1 <- a[1:(nrow(a)-9),]
  get_low<-function(a){rollapply(a,width=10,FUN=min)}
  a2 <- apply(a,2,get_low)
  a3 <- (a2-a1)/a1
  summary_matrix[4,] <- apply(a3,2,min)
  
  
  summary_matrix[5,] <- apply(ret_urn,2,sd)*sqrt(250)
  
  mean_rf <- mean(Factors[,5])/100
  summary_matrix[6,] <- SharpeRatio.annualized(ret_urn,Rf=mean_rf,scale=250)
  
  summary_matrix[7,] <- apply(ret_urn,2,skewness)
  summary_matrix[8,] <- apply(ret_urn,2,kurtosis)
  
  summary_matrix[9,] <- apply(ret_urn,2,VaR,method="modified")
  summary_matrix[10,] <- apply(ret_urn,2, ETL)
  
  return (summary_matrix)
}
