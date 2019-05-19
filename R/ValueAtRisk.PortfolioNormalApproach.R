#' This function calculates the VaR (Value at Risk) of a portfolio created by the user based on the portfolio-normal approach.
#' 
#' @param confidenceLevel a number from 0 to 1 (usually 0.95 or 0.99) that represents the confidence level (default:confidenceLevel=0.95) 
#' @param timePeriod an integer (usually 1 or 10) that represents the period on which the VaR is based (default: timePeriod=1)
#' @param daysForVariance an integer that represents the number of days we want to use to compute the variance (default: alldata)
#' @param stock1,stock2,... a dataframe for every stock that is created by one of the two following functions GetReturns.YahooFinance or GetReturns.MyData 
#' @param weight1,weight2,... a number associated with every stock that represents the capital we want to invest in each
#' @return The function returns the Value at Risk of your portfolio based on your data. It also gives you some information about the position that is printed.
#' 
#' @export
#' @usage 
#' ValueAtRisk.PortfolioNormalApproach(confidenceLevel=0.95,timePeriod=1,daysForVariance=TRUE,stock1,weight1,stock2,weight2, ... )
#' @examples
#' GetReturns.YahooFinance("AAPL")
#' GetReturns.YahooFinance("MSFT")
#' GetReturns.YahooFinance("SNAP")
#' ValueAtRisk.PortfolioNormalApproach(,,,AAPL,3000,MSFT,5000,SNAP,2000)
#' ValueAtRisk.PortfolioNormalApproach(0.99,,,AAPL,3000,MSFT,5000,SNAP,2000)
#' ValueAtRisk.PortfolioNormalApproach(,10,,AAPL,3000,MSFT,5000,SNAP,2000)
#' ValueAtRisk.PortfolioNormalApproach(,,250,AAPL,3000,MSFT,5000,SNAP,2000)


ValueAtRisk.PortfolioNormalApproach <- function(confidenceLevel=0.95,timePeriod=1,daysForVariance=TRUE,stock1,weight1,
                                                stock2=TRUE,weight2=0,stock3=TRUE,weight3=0,stock4=TRUE,weight4=0,stock5=TRUE,weight5=0,
                                                stock6=TRUE,weight6=0,stock7=TRUE,weight7=0,stock8=TRUE,weight8=0,stock9=TRUE,weight9=0,stock10=TRUE,weight10=0,
                                                stock11=TRUE,weight11=0,stock12=TRUE,weight12=0,stock13=TRUE,weight13=0,stock14=TRUE,weight14=0,stock15=TRUE,weight15=0,
                                                stock16=TRUE,weight16=0,stock17=TRUE,weight17=0,stock18=TRUE,weight18=0,stock19=TRUE,weight19=0,stock20=TRUE,weight20=0,
                                                stock21=TRUE,weight21=0,stock22=TRUE,weight22=0,stock23=TRUE,weight23=0,stock24=TRUE,weight24=0,stock25=TRUE,weight25=0,
                                                stock26=TRUE,weight26=0,stock27=TRUE,weight27=0,stock28=TRUE,weight28=0,stock29=TRUE,weight29=0,stock30=TRUE,weight30=0){
# Preparation of data ---------------------------------------------------------------------------------------------------------------------------------------------------
  # The following part of the code creates the following things:
  # - myPortfolio: a dataframe that contains all the stocks with their weight (capital) in one big dataframe
  # - nameStocks: a vector that contains the names of the stocks
  # - priceToday: a vector that contains the last prices of the stocks dataframe
  # - numberStocks: a vector that contains how many stocks we can buy with the given capital
  # - usedWeight: a vector that contains the part of the capital that we effectively used
  # - allNotUsedWeight: a vector that contains the part of the capital that we didn't use  myPortfolio <- stock1
  myPortfolio <- stock1
  priceToday <- stock1[dim(stock1)[1],1]
  numberStocks <- floor(weight1/priceToday)
  capitalInvested <- numberStocks*priceToday
  capitalNotInvested <- weight1-capitalInvested
  usedWeight <- capitalInvested
  allNotUsedWeight <- capitalNotInvested
  nameStocks <- gsub(".Adjusted","",colnames(stock1)[1])
  for (k in 2:30) {
    if (get(paste0("weight",k))!=0) {
      myPortfolio <- Merge.TwoStocks(myPortfolio,get(paste0("stock",k)))
      priceToday <- c(priceToday,get(paste0("stock",k))[dim(get(paste0("stock",k)))[1],1])
      numberStocks <- c(numberStocks,floor(get(paste0("weight",k))/priceToday[k]))
      capitalInvested <- numberStocks[k]*priceToday[k]
      capitalNotInvested <- get(paste0("weight",k))-capitalInvested
      usedWeight <- c(usedWeight,capitalInvested)
      allNotUsedWeight <- c(allNotUsedWeight,capitalNotInvested)
      nameStocks <- c(nameStocks,gsub(".Adjusted","",colnames(get(paste0("stock",k)))[1]))
    }
  }
  numberOfFirms <- dim(myPortfolio)[2]/2
# Simulation of your portfolio in the past ------------------------------------------------------------------------------------------------------------------------------
  # The following part of the code creates the following thing:
  # - simulationReturnsPortfolio: a vector of returns that simulates a portfolio with todays weight
  simulationPortfolio <- NULL
  for (j in 1:numberOfFirms) {
    simulationPortfolio <- cbind(simulationPortfolio,numberStocks[j]*myPortfolio[,j*2-1])
  }

  simulationTotalCapitalPortfolio <- rep(0,dim(simulationPortfolio)[1])
  for (x in 1:numberOfFirms) {
    simulationTotalCapitalPortfolio <- simulationTotalCapitalPortfolio + simulationPortfolio[,x]
  }

  simulationReturnsPortfolio <- NULL
  for (y in 2:length(simulationTotalCapitalPortfolio)) {
    simulationReturnsPortfolio[y-1] <- (simulationTotalCapitalPortfolio[y]-simulationTotalCapitalPortfolio[y-1])/simulationTotalCapitalPortfolio[y-1]
  }
# Calculation of VaR ----------------------------------------------------------------------------------------------------------------------------------------------------
  # The following part of the code calculates the VaR based on an Portfolio-Normal Approach

  if (daysForVariance==TRUE) {
    volatility <- sd(simulationReturnsPortfolio)
  } else {
    volatility <- sd(simulationReturnsPortfolio[(length(simulationReturnsPortfolio)-daysForVariance+1):length(simulationReturnsPortfolio)])
  }

  alpha <- qnorm(confidenceLevel)
  valueAtRiskResult <- alpha*volatility*sum(usedWeight)*sqrt(timePeriod)
# Results ---------------------------------------------------------------------------------------------------------------------------------------------------------------
  # The following part of the code prints the data of your portfolio and the VaR
  totalUsedWeight <- sum(usedWeight)
  totalNotUsedWeight <- sum(allNotUsedWeight)
  totalCapital <- totalUsedWeight + totalNotUsedWeight
  print(paste0("You have a total of ",totalCapital," and your transactions are the following:"))
  print(cbind(nameStocks,priceToday,numberStocks,usedWeight))
  print(paste0("You have a total of ",totalNotUsedWeight," to use for other transactions."))
  print(paste0("The VaR(",confidenceLevel,";",timePeriod, ") based on an Portfolio-Normal Approach of your portfolio is:"))
  return(valueAtRiskResult)
#------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
}
