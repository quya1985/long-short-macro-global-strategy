library(zoo)
library(tseries)
library(Quandl)
library(quadprog)
library(PerformanceAnalytics)
library(quantmod)

#import daily data as pri_ce from 2007-03-23
#QQQQ can't be found use QQQ instead

invest_uni <- c("FXE","EWJ","GLD","QQQ","SPY","SHV","DBA","USO","XBI","ILF","GAF","EPP","FEZ")
a <- lapply(invest_uni,get.hist.quote,start="2007-03-23",end="2016-03-01",quote="AdjClose",provider="yahoo",compression="d")
pri_ce <- as.data.frame.list(a)
names(pri_ce) <- invest_uni


setwd("/Users/quya/Google Drive/quant portfolio mgt")
write.csv(pri_ce,file="price.csv")

#get return from 2007-03-26
#delete last row to match the Fench Fama data

ret_urn <- matrix(0,nrow(pri_ce),ncol(pri_ce))
ret_urn <- apply(pri_ce,2,diff)/pri_ce[-nrow(pri_ce),]
rownames(ret_urn) <- rownames(pri_ce)[-1]
ret_urn <-ret_urn*250
ret_urn <- ret_urn[-nrow(ret_urn),]

write.csv(ret_urn,file="returndaily.csv")



#get French Fama data as quandl_data 
#delete fisrt row to match the ret_urn

quandl_data <- Quandl("KFRENCH/FACTORS_D",start_date="2007-03-23",end_date="2016-03-01", order="asc")
quandl_data <- quandl_data[-1,]

write.csv(quandl_data,file="quandl.csv",row.names = F)


#get_beta for 60 days period

n_row <- nrow(ret_urn)


betaMkt <- matrix(1,nrow=(n_row-59),ncol=13)
betaSMB <- matrix(1,nrow=(n_row-59),ncol=13)
betaHML <- matrix(1,nrow=(n_row-59),ncol=13)
betaCAPM <- matrix(1,nrow=(n_row-59),ncol=13)


  for (i in 1:13){    
    for (j in 1:(n_row-59)) {
      beta_matrix <- cbind(ret_urn[j:(j+59),i]/250*100,quandl_data[j:(j+59),2:5])
      colnames(beta_matrix) <- c("Return","Mkt","SMB","HML","Rf")
      beta_lm <- lm((Return-Rf)~Mkt+SMB+HML,data=beta_matrix)
      Capm_beta_lm <- lm(Return-Rf~Mkt,data=beta_matrix)
      betaMkt[j,i]<- unname(beta_lm$coefficients["Mkt"])
      betaSMB[j,i] <-unname(beta_lm$coefficients["SMB"])
      betaHML[j,i] <-unname(beta_lm$coefficients["HML"])
      betaCAPM[j,i] <- unname(Capm_beta_lm$coefficients["Mkt"])
    }        
}

colnames(betaCAPM) <- invest_uni
colnames(betaHML) <- invest_uni
colnames(betaSMB) <- invest_uni
colnames(betaMkt) <- invest_uni


rownames(betaCAPM) <- rownames(ret_urn)[-(1:59)]
rownames(betaHML) <- rownames(ret_urn)[-(1:59)]
rownames(betaSMB) <- rownames(ret_urn)[-(1:59)]
rownames(betaMkt) <- rownames(ret_urn)[-(1:59)]


write.csv (betaMkt,file="betaMkt-60.csv")
write.csv (betaSMB,file="betaSMB-60.csv")
write.csv (betaHML,file="betaHML-60.csv")
write.csv(betaCAPM,file="betaCAPM-60.csv")

#get beta for 90 days

betaMkt <- matrix(1,nrow=(n_row-89),ncol=13)
betaSMB <- matrix(1,nrow=(n_row-89),ncol=13)
betaHML <- matrix(1,nrow=(n_row-89),ncol=13)
betaCAPM <- matrix(1,nrow=(n_row-89),ncol=13)


for (i in 1:13){    
  for (j in 1:(n_row-89)) {
    beta_matrix <- cbind(ret_urn[j:(j+89),i]/250*100,quandl_data[j:(j+89),])
    colnames(beta_matrix) <- c("Return","Mkt","SMB","HML","Rf")
    beta_lm <- lm((Return-Rf)~Mkt+SMB+HML,data=beta_matrix)
    Capm_beta_lm <- lm(Return-Rf~Mkt,data=beta_matrix)
    betaMkt[j,i]<- unname(beta_lm$coefficients["Mkt"])
    betaSMB[j,i] <-unname(beta_lm$coefficients["SMB"])
    betaHML[j,i] <-unname(beta_lm$coefficients["HML"])
    betaCAPM[j,i] <- unname(Capm_beta_lm$coefficients["Mkt"])
  }        
}

colnames(betaCAPM) <- invest_uni
colnames(betaHML) <- invest_uni
colnames(betaSMB) <- invest_uni
colnames(betaMkt) <- invest_uni


rownames(betaCAPM) <-rownames(as.matrix((ret_urn)[-(1:89),]))
rownames(betaHML) <- rownames(as.matrix((ret_urn)[-(1:89),]))
rownames(betaSMB) <- rownames(as.matrix((ret_urn)[-(1:89),]))
rownames(betaMkt) <- rownames(as.matrix((ret_urn)[-(1:89),]))


write.csv (betaMkt,file="betaMkt-90.csv")
write.csv (betaSMB,file="betaSMB-90.csv")
write.csv (betaHML,file="betaHML-90.csv")
write.csv(betaCAPM,file="betaCAPM-90.csv")

#get beta for 120 days


betaMkt <- matrix(1,nrow=(n_row-119),ncol=13)
betaSMB <- matrix(1,nrow=(n_row-119),ncol=13)
betaHML <- matrix(1,nrow=(n_row-119),ncol=13)
betaCAPM <- matrix(1,nrow=(n_row-119),ncol=13)


for (i in 1:13){    
  for (j in 1:(n_row-119)) {
    beta_matrix <- cbind(ret_urn[j:(j+119),i]/250*100,quandl_data[j:(j+119),])
    colnames(beta_matrix) <- c("Return","Mkt","SMB","HML","Rf")
    beta_lm <- lm((Return-Rf)~Mkt+SMB+HML,data=beta_matrix)
    Capm_beta_lm <- lm(Return-Rf~Mkt,data=beta_matrix)
    betaMkt[j,i]<- unname(beta_lm$coefficients["Mkt"])
    betaSMB[j,i] <-unname(beta_lm$coefficients["SMB"])
    betaHML[j,i] <-unname(beta_lm$coefficients["HML"])
    betaCAPM[j,i] <- unname(Capm_beta_lm$coefficients["Mkt"])
  }        
}

colnames(betaCAPM) <- invest_uni
colnames(betaHML) <- invest_uni
colnames(betaSMB) <- invest_uni
colnames(betaMkt) <- invest_uni


rownames(betaCAPM) <- rownames(as.matrix((ret_urn)[-(1:119),]))
rownames(betaHML) <- rownames(as.matrix((ret_urn)[-(1:119),]))
rownames(betaSMB) <- rownames(as.matrix((ret_urn)[-(1:119),]))
rownames(betaMkt) <- rownames(as.matrix((ret_urn)[-(1:119),]))


write.csv (betaMkt,file="betaMkt-120.csv")
write.csv (betaSMB,file="betaSMB-120.csv")
write.csv (betaHML,file="betaHML-120.csv")
write.csv(betaCAPM,file="betaCAPM-120.csv")



