
if( !("quantmod" %in% installed.packages()) )
{
  install.packages("quantmod")  
}

Merge.TwoStocks <- function(stock1,stock2){
  minStock1 <- dim(stock1)[1]-min(dim(stock1)[1],dim(stock2)[1])+1
  maxStock1 <- dim(stock1)[1]
  minStock2 <- dim(stock2)[1]-min(dim(stock1)[1],dim(stock2)[1])+1
  maxStock2 <- dim(stock2)[1]
  myNewData <- cbind(stock1[minStock1:maxStock1,],stock2[minStock2:maxStock2,])
  return(myNewData)
}

#testmerge <- Merge.TwoStocks(AAPL,SNAP)