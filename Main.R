###########################################################################
## Author: Federico Finocchietti                                         ##
## Date : 31/07/2019                                                     ##
## Description: This the main for the statistical factor                 ##
##              model strategy                                           ##
##                                                                       ##
###########################################################################


# libraries ---------------------------------------------------------------

library(data.table)
library(stringr)
library(quantmod)
library(dplyr)
library(stocks)
library(psych)
library(ggplot2)
library(corrplot)


# please ensure to insert here the path of the current folder
PathFolder <- "C:/Users/Federico/Documents/Strategy/"
setwd(PathFolder)


# load the methods --------------------------------------------------------

source("./Modules/Functions.R")

# main --------------------------------------------------------------------



# disabling scientific notation
options(scipen = 999)

# Upload all the tickers based on a list that I scraped on the web
# the csv contains the ticker, name, industry group and the correspondent
# sic code
StockTickers <- fread("./Static/TickersList.csv")

# Select only NASDAQ stocks
Financials <- StockTickers[exchange == 'NASDAQ']
# remove duplicates
ListTickersFinancials <- unique(Financials$ticker)


################## ################### ####
########## Download Stock Prices ##########
################## ################### ####

# use the library stocks in order to download
# stock prices from yahoo finance


# loop to iterate across tickers and dates
StockListFinancials <- list()
# get the starting time
start <- Sys.time()

for(ticker in ListTickersFinancials){
  
  # paste out which ticker and date is processing
  cat(paste0("PROCESSING **** Ticker ",ticker))
  
  # call the price
  StockPrices <- DownloadTickerConstantDates(StartDate = "2014-01-01",EndDate = "2019-04-30",Ticker = ticker)
  
  # format the dataset and append to the list
  if(!is.na(StockPrices)){
    StockPricesDfFin <- data.table(Date = attr(x = StockPrices,"dimnames")[[1]],
                                   Price = StockPrices,
                                   Ticker = attr(x = StockPrices,"dimnames")[[2]])
    colnames(StockPricesDfFin) <- c("Date","Price","Ticker")
  }
  if(is.na(StockPrices)){
    
    StockPricesDfFin <- NA
  }
  # append to the list
  StockListFinancials <- append(StockListFinancials,list(StockPricesDfFin))
}

Sys.time() - start

# removing NAs from list
StockListFinancialsClean <- StockListFinancials[!sapply(StockListFinancials, function(x) all(is.na(x)))]
# rbind the dataframe with the prices
StockDataframeFinancials <- rbindlist(StockListFinancialsClean)
#StockDataframeFinancials <- fread(paste0(Path,"Static/nasdaq.csv"))

################## ################### ####################### ##################
########## Clean the data and prepare the dataset for the PCA analysis ##########
################## ################### ####################### ##################

# cleaning the data
FinancialsLogRetDcastClean <- CleaningData(StockDataframeFinancials)

################## ################### ##############
########## Perform PCA and factor analysis ##########
################## ################### ##############

# In order to perform PCA and factor analysis (and later on cross sectional regression)
# I picked up a Lookbackperiod of 600 days 
LookBackPeriod <- 600
FinancialsLogRetDcastClean <- as.data.frame(FinancialsLogRetDcastClean)
StrategyList <- list()

for (j in 1:(nrow(FinancialsLogRetDcastClean)-LookBackPeriod)){
  
  cat(paste0("Processing ",j,"\n"))
  
  FinancialsLogRetTrainSet <- FinancialsLogRetDcastClean[j:(j+LookBackPeriod),]
  
  # find the factor loadings
  Factorloadings <- GetFactorLoadings(FinancialsLogRetDcastClean,LookBackPeriod)
  
  ################## ################### ############## ################### ############## ###
  ########## Cross Sectional Regression to find the factors for that particular day ##########
  ################## ################### ############## ################### ############## ###
  
  # perform cross sectional regression in order to find the time series of the hidden factors.
  # In this case, the factor loadings (the betas of the regression) are the factors themselves
  # The idea is to build up the time series of the factors in order to use it on the next step
  # to do a predictive regression
  FactorsList <- list()
  
  for(g in 1:nrow(FinancialsLogRetTrainSet)){
    
    # pick the returns for the specific date
    Row <- data.table(FinancialsLogRetTrainSet[g,])
    
    FactorFinal<- GetFactorsTimeSeries(Row,Factorloadings)
    # append the dataframe to the factor list
    FactorsList <- append(FactorsList,list(FactorFinal))
    
  }  
  
  # rbind the dataframe
  FactorDataframe <- rbindlist(FactorsList)

  ################## ################### ############## ################### ############## ### #########
  ########## Perform predictive time series regression in order to find the predicted returns ##########
  ################## ################### ############## ################### ############## ### #########
  
  # Perform a predictive regression using the time series of the factors which I just found on
  # the previous step. In order to make it predictive, I regress the factor at the day t 
  # on the return at the the day t+1
  ListPredictedRet <- list()
  
  for(i in 2:ncol(FinancialsLogRetTrainSet[,-1])){
    
    # find the dataframe with the stock to use (Use returns plus one and the factors dataframe)
    FinancialsLogRetTrainSet <- as.data.frame(FinancialsLogRetTrainSet)
    Df <- FinancialsLogRetTrainSet[,c(1,i)]
    
    # get the predictive return dataframe for the stock
    PredRetDf <- FindPredictingRegression(Df,FactorDataframe)
    
    # append the return for this stock to the list
    ListPredictedRet <- append(ListPredictedRet,list(PredRetDf))
  }  
  
  # find the return of the strategy for this specific day
  TotalDf <- GetStrategyReturn(ListPredictedRet,FinancialsLogRetDcastClean,StockDataframeFinancials)
  
  StrategyList <- append(StrategyList,list(TotalDf))
}

# rbind the list
StrategyReturn <- rbindlist(StrategyList)
# calculate the cumulative return of the strategy
StrategyReturn[,Cumulative := cumsum(StrategyReturn)]
# plot the equity curve
ggplot(StrategyReturn,aes(as.Date(Date),Cumulative)) + geom_line()


