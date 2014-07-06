BvDdata  <- function (Sfile){
 dinput <-   read.csv2(Sfile, header = TRUE, sep=";", dec=",", na.strings="n.a.")
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