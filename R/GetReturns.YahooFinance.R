#' This function uses the function getsymbols from the package "quantmod" to download the adjusted price of a chosen stock and calulates the returns.
#'
#' @param stockName A string that represents a firm's alphanumeric characters of the exchange
#' @return The function create directly a dataframe inside the Global Environment named as the chosen "stockName".
#' This new dataframe contains three column:
#' - The first column contains the dates
#' - The second column contains the prices
#' - The third column contains the returns
#' @export
#' @usage 
#' GetReturns.YahooFinance(stockName)
#' @examples
#' GetReturns.YahooFinance("AAPL")
#' GetReturns.YahooFinance("MSFT")
#' GetReturns.YahooFinance("SNAP") 


GetReturns.YahooFinance <- function(stockName){


  getSymbols(stockName,auto.assign = TRUE)
  stockData <- get(stockName)
  stockData <- as.data.frame(stockData)
  stockData <- stockData[6]
  stockReturn <- rep(0,dim(stockData)[1])
  for (k in 2:dim(stockData)[1]) {
    stockReturn[k] <- (stockData[k,1]-stockData[k-1,1])/stockData[k-1,1]
  }
  stockData <- cbind(stockData,stockReturn)
  colnames(stockData) <- c(colnames(stockData)[1],paste0(stockName,".Return"))

  assign(stockName,stockData,envir = .GlobalEnv)

  return("Your Dataframe with the adjusted Prices and Returns is now in the Global Environment")

}
