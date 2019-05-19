#' This function converts some given data in a format that is readable for the others package's functions
#'
#' @param dataSet A dataframe that has to contain the following column: Dates, Prices, Return
#' @param stockName A string that will become the name of the variable created by the function
#' @param columnData A number that correspond to the column of the dates in the given "dataSet"
#' @param columnPrice A number that correspond to the column of the prices in the given "dataSet"
#' @param columnReturn A number that correspond to the column of the returns in the given "dataSet"
#' @return The function create directly a dataframe inside the Global Environment named as "stockName".
#' This new dataframe contains three column:
#' - The first column contains the dates
#' - The second column contains the prices
#' - The third column contains the returns
#' @export
#' @usage 
#' GetReturns.MyData(dataSet,stockName,columnData,columnPrice,columnReturn)
#' @examples 
#' # We create a dataframe as examples that contains also other terms that we don't need and then we use the function
#' dates <- as.Date(c("2019/01/01","2019/01/02","2019/01/03","2019/01/04"))
#' prices <- c(189.43,190.50,186.97,188.59)
#' returns <- NULL
#' for (k in 2:4) { returns[k] <- (prices[k]-prices[k-1])/prices[k-1] }
#' randomData <- rnorm(4)
#' 
#' ourDataFrameExample <- data.frame(prices,randomData,dates,returns)
#' 
#' GetReturns.MyData(ourDataFrameExample,"PIPPO",3,1,4)


GetReturns.MyData <- function(dataSet,stockName,columnData,columnPrice,columnReturn){
  dataSet <- as.data.frame(dataSet)
  stockData <- cbind(dataSet[columnPrice],dataSet[columnReturn])
  stockData[1,2] <- 0
  row.names(stockData) <- t(dataSet[columnData])
  colnames(stockData) <- c(paste0(stockName,".Adjusted"),paste0(stockName,".Return"))
  assign(stockName,stockData,envir = .GlobalEnv)
  return("Your DataFrame with the adjusted Prices and Returns is now in the Global Environment")
}