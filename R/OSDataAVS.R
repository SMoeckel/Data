BvDdata  <- function (Sfile){
 dinput <-   read.csv2(Sfile, header = TRUE, sep=";", dec=",", na.strings="n.a.")
 dinput
}
BvDTxt  <- function(Sfile){
  dinput  <- read.csv2(Sfile, header=FALSE, sep=";", colClasses = "character", fill = FALSE)
  dinput  <- VectorSource(dinput)
  dinput  <- VCorpus(dinput)
  dinput
}
FacMean  <- function(SFrame, Fac, fun){
  index  <- 1:ncol(SFrame)
  row  <- 1:nrow(SFrame)
  numCols  <- vapply(SFrame, is.numeric, logical(1))
  facCols  <- vapply(SFrame, is.factor, logical(1))
  index  <-  index[numCols]
  VarName  <- c(names(SFrame[facCols]),paste("P", names(SFrame[index]),sep=""))
  d  <- SFrame[,facCols]
  d  <- d[-row[duplicated(Fac)],]
 for (i in 1: length(index)){
  d <- cbind(d,tapply(SFrame[[index[i]]],Fac,fun))
  }
 colnames(d) <- VarName
 d
}
ChkIntegrity <- function(SFrame){
  TotAsset  <- SFrame[,"Fixed.Assets"]+SFrame[,"CurrAss"]
  TotELiab  <- SFrame[,"Equity"]+SFrame[,"NCLiabs"]+SFrame[,"CLiabs"]
  d  <- abs(TotAsset - TotELiab)
  is.na(SFrame[,c("Turnover","CoS","OthOpEx")])
  EBIT  <- SFrame[,"Turnover"]-SFrame[,"CoS"]-SFrame[,"OthOpEx"]
  d <- cbind(d,abs(SFrame[,"Ebit"]-EBIT))
  d
}
SetUp <- function(){
  options(prompt = "R > ", digits = 4, show.signif.stars=FALSE) # layout des workspace
  library(KernSmooth)
  library(SparseM)
  library(survival)
  library(strucchange)
  library(lmtest)
  library(quantreg)
  library(splines)
  library(sandwich)
  library(zoo)
  library(dynlm)
  library(tm)# End loading packages
}