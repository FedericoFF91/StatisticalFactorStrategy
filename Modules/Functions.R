###############################################################################
###############################################################################
###### This file contains the methods which are used on the Main script ######
###############################################################################
###############################################################################

# Function to download stock price from Yahoo finance
DownloadTickerConstantDates <- function(StartDate,EndDate,Ticker){
  # catch exceptions
  out <- tryCatch(
    {
      # download stock price 
      Stock <- stocks::load_prices(tickers = Ticker,
                                   from = StartDate,
                                   to = EndDate)
      return(Stock)
    },
    error = function(cond){
      
      Stock <- NA  
      return(Stock)
    }
  )
  return(out)
}

# Function to remove outliers
RemoveOutliers <- function(x, na.rm = TRUE, ...) {
  # calculate the 25% and 75% quantile
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  # calculate the interquantile distance
  H <- 1.5 * IQR(x, na.rm = na.rm)
  # Assign to y the old series in order to clean the outliers
  y <- copy(x)
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  return(y)
}

# Function to clean the data
CleaningData <- function(StockDataframeFinancials){
  
  # find log return dataframe
  StockDataframeFinancialsLogRet <- StockDataframeFinancials[,.(Date,
                                                                LogRet = c(NA,log(Price[-1]/Price[-length(Price)]))),
                                                             by=Ticker]
  
  # remove log ret equal to 0, NAs 
  StockFinancialsLogRetClean <- StockDataframeFinancialsLogRet[LogRet != 0]
  StockFinancialsLogRetClean <- StockFinancialsLogRetClean[!is.na(LogRet)]
  
  # flip the dataset and remove stock where we have more than 1% of NAs
  FinancialsLogRetDcast <- as.data.frame(dcast.data.table(StockFinancialsLogRetClean,Date~Ticker,value.var = "LogRet"))
  # take only column with a % of NAs less than 1%
  FinancialsLogRetDcastClean <- FinancialsLogRetDcast[, -which(colMeans(is.na(FinancialsLogRetDcast)) > 0.01)]
  # replace NAs with 0
  FinancialsLogRetDcastClean[is.na(FinancialsLogRetDcastClean)] <- 0
  
  return(FinancialsLogRetDcastClean)
}

# Function to find the factor loadings using factor analysis
GetFactorLoadings <- function(FinancialsLogRetTrainSet,LookBackPeriod){
  
  PCAFinancials <- prcomp(FinancialsLogRetTrainSet[,-1], center = T,scale. = T,retx = TRUE)
  summary(PCAFinancials)
  
  ## 10 PCA explain 42% of the variance, try to apply a factor analysis 
  ## Now performing factor analysis in order to find the factors
  ## please note that the selection of 10 factors is arbitrary
  FactorAnalysis <- factanal(FinancialsLogRetTrainSet[,-1], factors = 10, rotation = "none")
  Factorloadings <- data.table(FactorAnalysis$loadings[,])
  Factorloadings[,Stock:= attr(FactorAnalysis$loadings,"dimnames")[[1]]]
  
  return(Factorloadings)
}

# Function to find the time series of returns
GetFactorsTimeSeries <- function(Row,Factorloadings){
  
  # melt the dataframe
  RowMelt <- melt.data.table(Row,id.vars = "Date",measure.vars = colnames(Row)[-1],
                             variable.name = "Stocks",value.name = "LogRet")
  
  # match the returns into the Factorloadings dataset
  CrossSectionalDf <- merge(Factorloadings,RowMelt,by.x = "Stock",by.y = "Stocks")
  # remove the dates and the name of the stocks
  CrossSectionalDfClean <- CrossSectionalDf[,-c("Date","Stock")]
  setcolorder(CrossSectionalDfClean, c("LogRet", setdiff(names(CrossSectionalDfClean), "LogRet")))
  # perform regression
  FactorsforTheDay <- lm(formula = LogRet ~ . +0,data = CrossSectionalDfClean)
  
  # create the dataframe with the factors
  FactorsDf <- data.table(Date = unique(CrossSectionalDf$Date),
                          FactorValue = FactorsforTheDay$coefficients,
                          FactorName = names(FactorsforTheDay$coefficients))
  # dcast the table and sort the columns
  FactorFinal <- dcast.data.table(FactorsDf,Date ~ FactorName, value.var = "FactorValue")
  setcolorder(FactorFinal, c(setdiff(names(FactorFinal), "Factor10"),"Factor10"))
  
  return(FactorFinal)
}

# Function for predicting regression
FindPredictingRegression <- function(Df,FactorDataframe){
  
  # Build up the dataframe for the regression, please remember that is a predictive time series regression
  # so I am removing the last factor observation and first return observation because I am running
  # the regression on the factor for the day t and the return on the day t+1
  NewDf <- data.frame(data.table(Df[-1,-1]),FactorDataframe[-nrow(FactorDataframe),-1])
  # regression
  RegressionDf <- lm(formula = V1 ~ . +0,data = NewDf)
  # predicted return -- I use the last factor observation in order to predict the return for this stock
  # for the next day
  PredRet <- sum(RegressionDf$coefficients * FactorDataframe[(nrow(FactorDataframe)-1),-1])
  # predicted return dataframe
  PredRetDf <- data.table(Date = Df$Date[length(Df$Date)],
                          Stock = colnames(Df)[2],
                          PredRet = PredRet)
  
  return(PredRetDf)
}

# Get the strategy return for that day
GetStrategyReturn <- function(ListPredictedRet,FinancialsLogRetDcastClean,StockDataframeFinancials){
  
  # dataframe predicted returns for all the stocks 
  PredRetFinalDf <- rbindlist(ListPredictedRet)
  # sort based on the return
  PredRetFinalDf <- PredRetFinalDf[order(PredRet,decreasing = T)]
  # Find the top 10 and last 10 stocks based on our predictive dataframe
  Top10 <- PredRetFinalDf[,head(.SD,10)]$Stock
  Last10 <- PredRetFinalDf[,tail(.SD,10)]$Stock
  
  FinancialsLogRetDcastClean <- data.table(FinancialsLogRetDcastClean)
  FinancialsLogClean <- melt.data.table(FinancialsLogRetDcastClean,id.vars = "Date",
                                        measure.vars = colnames(FinancialsLogRetDcastClean)[-1],
                                        variable.name = "Stocks",value.name = "LogRet")
  # calculate returns for the stocks
  StockDataframeFinancialsRet <- StockDataframeFinancials[,.(Date,
                                                             Ret = c(NA,(Price[-1] - Price[-length(Price)])/Price[-length(Price)])),
                                                          by=Ticker]
  
  # Get the actual returns based on the top and last 10 selected by the model
  # the idea is to go long the top 10 and short the last 10 (AQR style)
  LongReturn <- sum(StockDataframeFinancialsRet[Date == unique(PredRetFinalDf$Date)& Ticker%in%Top10]$Ret,na.rm = T)/length(Top10)
  ShortReturn <- sum(StockDataframeFinancialsRet[Date == unique(PredRetFinalDf$Date)& Ticker%in%Last10]$Ret,na.rm = T)/length(Last10)
  
  # total return for the day
  Total <- LongReturn - ShortReturn
  # build the dataframe and append to the list
  TotalDf <- data.table(Date = unique(PredRetFinalDf$Date),StrategyReturn = Total)
  
  return(TotalDf)
  
}