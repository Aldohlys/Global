#### 04.09.2023
#### FOr all programs
###############   General purpose utility programs  ###################

library(reticulate)
library(lubridate)
library(readr)
library(DescTools)
library(quantmod)
library(stringr)
library(scales)
library(RQuantLib) ## for isBusinessDay function

################  Display error message

display_error_message = function(error_msg) {
  showModal(modalDialog(
    title = "Error message",
    error_msg,
    easyClose = TRUE,
    footer = NULL
  ))
}


pgcd = function(x) {
  x.na=na.omit(x)
  if (length(x.na)<2) return(as.numeric(x.na))
  else return(GCD(as.numeric(x.na)))
}

##########################  General purposes utilities ##########

##### Yahoo-based symbol lookup
### getSym is based upon quantmod getSymbols function


getSym = function(sym){
  lookup_yahoo = c("ESTX50","MC","SPX","XSP")
  new_value= c("^STOXX50E","MC.PA","^SPX","^XSP")
  if (sym %in% lookup_yahoo) sym=new_value[which(sym == lookup_yahoo)]
  return(getSymbols(sym,from=ymd("2023-01-03"),auto.assign = FALSE,warnings=FALSE))
}

getAdjReturns = function(sym) {
  returns=data.frame(date=as.Date(index(sym)), 
                   return=c('NA',round(diff(log(coredata(coredata(sym[,6])))),3)))
  colnames(price)=c("date","return")
  return(returns)
}

getPrice= function(sym) {
    price=data.frame(date=as.Date(index(sym)), value=coredata(sym[,6]))
    colnames(price)=c("date","value")
    return(price)
}


findNearestNumberOrDate = function(numbers, target) {
  nearest = numbers[1]
  diff = abs(as.numeric(nearest - target))
  
  for (number in as.list(numbers)) {
    current_diff = abs(as.numeric(number - target))
    if (current_diff < diff) {
      diff = current_diff
      nearest = number
    }
  }
  return(nearest)
}

#### Utilities - no real relation with options computation 
getDTE = function(c_datetime,exp_date) {
  if_else(is.na(exp_date),NA, {
    exp_datetime=as_datetime(exp_date)
    hour(exp_datetime) = 16  ### Option end is at 16:00 EST
    tz(exp_datetime) = "EST" 
    
    difftime=exp_datetime - c_datetime
    
    ### DTE cannot be negative
    difftime = if_else (difftime < 0, 
                        0,
                        { 
                          difftime=as.numeric(as.duration(difftime)) ### Convert to duration in seconds and then to a number
                          difftime=round(difftime/(24*3600),2) ### and then convert duration from seconds duration to days duration - keep only 2 digits
                        })  
    
    difftime
  })
}

## Compare symbol, expiration date and position - 
## price cannot be used as combined spread (vertical) may exist in portfolio files
### dt contains symbol, date, avgCost, pos
getUPrice = function(dt) {
  portf1= data.frame(read.csv("C:\\Users\\aldoh\\Documents\\NewTrading\\U1804173.csv",sep=";"))
  portf2= data.frame(read.csv("C:\\Users\\aldoh\\Documents\\NewTrading\\DU5221795.csv",sep=";"))
  portf=rbind(portf1,portf2)
  
  ### Convert from European date format to internal R date format
  portf$date=as.Date(portf$date,format="%d.%m.%Y") 
  
  portf = right_join(portf,dt,by=c("symbol"="sym","date"))
  portf %>% replace(is.na(.), 0) %>% group_by(date,symbol) %>% summarize(uPrice=max(undPrice)) %>% ungroup
}

getInstrumentName=function(sym,expdate,strike,type) {
  if_else(type=="Stock", "",{
    ## In case type already equals P or C, keep current value - otherwise replace by P for Put and C for Call
    right = case_match(type, "Put" ~"P","Call" ~"C",.default = type)
    loc= Sys.getlocale("LC_TIME")
    Sys.setlocale("LC_TIME","English")
    expdate=str_to_upper(format(expdate,"%d%b%y"))
    Sys.setlocale("LC_TIME",loc)
    paste(sym,expdate,strike,right)
  } )
}

getTradeNr = function(v_instrument) {
  message("getTradeNr")
  if(length(v_instrument)==0) return(NA)
  if (is.unsorted(v_instrument)) stop("Instrument must be sorted - prog. error")
  ### Read Trades.csv file and extract open/adjusted trades, to select all instruments present in dt argument
  trades= data.frame(read.csv("C:\\Users\\aldoh\\Documents\\NewTrading\\Trades.csv",sep=";"))
  ###trades=data.frame(read.csv("C:\\Users\\martinale\\Documents\\RProjects\\RAnalysis\\Trading\\Trades.csv",sep=";"))
  
  trades %<>% filter(Statut=="Ouvert" | Statut=="Ajusté")
  trades %<>% select(Instrument,TradeNr)
 
  ### Retrieve dates in trades.csv corresponding to instruments of dt
  ### If there are several dates for the same instrument, take the oldest one (min)
  trade_nr=left_join(as.data.frame(list(Instrument=v_instrument)),trades)  %>% 
    group_by(Instrument) %>% 
    pull(TradeNr)
  
  ### If all instrument are NA -> trade is not present - not yet recorded in Trades.csv
  if (length(trade_nr)==0) {
    display_error_message("Trade not present in Trades.csv file!")
    print(v_instrument)
    return(NA)
  }
  
  ### Retrieve the common Trade Nr
  trade_nr=unique(trade_nr)
  
  ### If at least one then retrieve corresponding trade nr
  ### And get the original trade date of the trade nr
  if (length(trade_nr) >1) {
    message("There is more than one trade in Instrument argument!")
    return(NA)
  }
  
  print(trade_nr)
  return(as.numeric(trade_nr))
}


getOpenDate = function(trade_nr) {
  message("getOpenDate")
  
  trades= data.frame(read.csv("C:\\Users\\aldoh\\Documents\\NewTrading\\Trades.csv",sep=";"))
  ###trades=data.frame(read.csv("C:\\Users\\martinale\\Documents\\RProjects\\RAnalysis\\Trading\\Trades.csv",sep=";"))
  
  trades %<>% filter(TradeNr==trade_nr)
  if (nrow(trades)==0) {
    display_error_message("Trade does not exist!")
    return(NA)
  }
  if (unique(trades$Statut)=="Fermé") {
    display_error_message("Trade is closed in Trades.csv file!")
    return(NA)
  }
  
  trades %<>% select(TradeDate)
  trade_dates=dmy(trades$TradeDate)
  
  ### If there are several dates for the trade nr (adjusted case), take the oldest one (min)
  orig_trade_date=suppressWarnings(min(trade_dates,na.rm=T))
  
  print(orig_trade_date)
  return(as.Date(orig_trade_date))
}

### Returns a number - sum of rewards (non-NA) for all given instruments that are Open/adjusted
getRnR = function(trade_nr) {
  message("getRnR - Reward and Risk")
  ##if (is.unsorted(v_instrument)) stop("Instrument must be sorted - prog. error")
  ### Read Trades.csv file and extract open/adjusted trades, to select all instruments present in dt argument
  trades= data.frame(read.csv("C:\\Users\\aldoh\\Documents\\NewTrading\\Trades.csv",sep=";"))
  ### For office laptop
  ###trades=data.frame(read.csv("C:\\Users\\martinale\\Documents\\RProjects\\RAnalysis\\Trading\\Trades.csv",sep=";"))
  trades %<>% filter(TradeNr==trade_nr)
  if (nrow(trades)==0) {
    display_error_message("Trade does not exist!")
    return(NA)
  }
  if (unique(trades$Statut)=="Fermé") {
    display_error_message("Trade is closed in Trades.csv file!")
    return(NA)
  }
  
  ### Retrieve instruments in trades.csv corresponding to instruments of dt
  ### If there are several records for the same instrument, take the oldest one (min)
  RnR=  trades %>% summarize(reward= sum(as.double(Reward),na.rm=T),risk= sum(as.double(Risk),na.rm=T))
  #print(RnR)
  return(RnR)
}

### Tries first on Yahoo (close price) - this works only for previous days, not for today
### THen on IBKR and if not available returns NA
### report_date can be a closed day -> then takes nearest day in the list
getsymPrice = function(sym,currency,report_date){
  prices_list=getPrice(getSym(sym))
  
  ### First case - requested date is an holiday
  ### Get last close price in this case
  if ((!isBusinessDay("UnitedStates",report_date)) | report_date < today()) {
    report_date = findNearestNumberOrDate(prices_list$date, report_date)
    price = filter(prices_list,date==report_date)
    return(price$value)
  }
    
  #### If report_date is today and is not an holiday
  ####  then tries to retrieve current market price and finally asks the user
  #### 
  ### SMART works fine in all cases except for SPX and XSP cases
  exchange = switch(sym, ESTX50= "EUREX", SPX =, XSP = "CBOE", "SMART")
  sec=switch(sym, SPX=,XSP=,ESTX50="IND","STK")
  return(stock_price(sec,sym,currency,exchange,reqType=4))
  
}
  
################################################################
getLastTickerData = function(ticker) {
  if (is.null(ticker) |
      ticker %in% c("","All")) return(list(last=NA,change=NA))
  
  ### Retrieve data from Yahoo Finance - no need to launch IBKR TWS
  ### Get last price and last change (J/J-1)
  tryCatch({
    ticker=getSymbols(ticker,auto.assign=FALSE,from=today()-10,warnings=FALSE) ## Case Tuesday morning and US market not yet opened + Monday and Friday were off -> Get Wed and THur data
    names(ticker)[length(names(ticker))]="Adjusted" ### Last column is "ticker.Adjusted" -> to be renamed as Adjusted
    last_data=as.numeric(ticker[[nrow(ticker),"Adjusted"]])
    p_last_data=as.numeric(ticker[[nrow(ticker)-1,"Adjusted"]])
    return(list(
      last=round(last_data,2),
      change=label_percent(accuracy=0.01)(last_data/p_last_data-1)
    ))
  }, error = function(e) {
    print(paste("Error:", e))
    return(list(last="Non disponible",change=NA))
  })
}

lastSPY=getLastTickerData("SPY")  ### Mkt value


py_run_file("getContractValue.py")


getVal=function(sym) {
  cat("No value for ",sym,"\n Enter new price: ")
  if (interactive()) val=readline(prompt="(interactive) ")
  else val= readLines(con="stdin", n=1)[[1]]
  as.double(val)
}

#### Used by Gonet.R script and RAnalysis
stock_price = function(sec="STK",sym,currency,exchange="SMART",reqType=4) {
  val=py$getStockValue(sec=sec,sym=sym,currency=currency,exchange=exchange,reqType=reqType)
  print(paste("Stock value",sym,":",val))
  
  #### readline works only in interactive mode, 
  #### readLines works only in non-interactive mode
  ### Case where no IBKR connection exists (NULL) or no value returned
  if (is.null(val)) val=getVal(sym)
  else if (is.na(val)) val=getVal(sym)
  return(val)
}

#### CURRENCY management ##################

### The 2 lines below will not work with "from=today()" between 00:00 and 6:00am 
### as Europe is one day after the US between this time period and Yahoo server is located in the US

# euro_usd = xts(1.10,order.by = today())
# chf_usd = xts(1.12,order.by=today())

# euro_usd = xts({
#   message("Enter 1 euro in USD")
#   readline(prompt=">> ")}
#   ,order.by = today())
# 
# chf_usd = xts({
#   message("Enter 1 CHF in USD")
#   readline(prompt=">> ")}
#   ,order.by=today())


# suppressWarnings({
#   euro=getSymbols("EURUSD=X",from=s_date,warnings=FALSE, auto.assign = FALSE)[,6]
#   euro=data.frame(date=as.Date(index(euro)),EUR=as.numeric(euro))
# 
#   ### Suppress strange things if the last 2 dates are equal then remove last line
#   test_euro=tail(euro,2)
#   if (nrow(test_euro) >=2) {if (test_euro[1,]$date == test_euro[2,]$date) euro=head(euro,-1)}
# 
#   chf=getSymbols("CHFUSD=X",from=s_date,warnings=FALSE, auto.assign = FALSE)[,6]
#   chf=data.frame(date=as.Date(index(chf)),CHF=as.numeric(chf))
# 
#   ### Suppress strange things if the last 2 dates are equal then remove last line
#   test_chf=tail(chf,2)
#   if (nrow(test_chf) >=2) {if (test_chf[1,]$date == test_chf[2,]$date) chf=head(chf,-1)}
# })

getCurrencyPairs = function() {
  message("getCurrencyPairs")
  ### euro_usd and chf_usd data frames - values for the day- are already retrieved
  usd=read.csv("C:/Users/aldoh/Documents/NewTrading/CurrencyPairs.csv",sep=";")
  ### Convert string to date
  usd$date=ymd(usd$date)
  
  if (usd$date == today())  {
    #print(usd)
    return(usd)
  }
  
  EUR = py$getCurrencyPairValue("EURUSD",reqType=2)
  if (is.null(EUR)) EUR=getVal("EUR/USD")
  else if (is.na(EUR)) EUR=getVal("EUR/USD")
  
  CHF = py$getCurrencyPairValue("CHFUSD",reqType=2)
  if (is.null(CHF)) CHF=getVal("CHF/USD")
  else if (is.na(CHF)) CHF=getVal("CHF/USD")
  
  usd = data.frame(date=today(),EUR=EUR,CHF=CHF)
  #print(usd)
  
  write.table(usd,"C:/Users/aldoh/Documents/NewTrading/CurrencyPairs.csv",sep=";",dec=".",row.names=F)
  return(usd)
}

#Returns the amount values formatted with their respective currency sign, based on the currency argument
## Amounts are rounded to 0.01

euro <- label_dollar(
  prefix = "",
  suffix = " \u20ac",
  accuracy=0.01
)
chf <- label_dollar(
  prefix = "",
  suffix = " CHF",
  accuracy=0.01
)
dollar <- label_dollar(
  prefix = "",
  suffix = " $",
  accuracy=0.01
)

### Returns a string with amount and currency symbol
currency_format = function(amount,currency){
  if_else (is.na(amount), "", {
    case_match(currency, "EUR"~euro(amount),
           "CHF"~chf(amount),
           "USD"~dollar(amount))
  })
}

# currency_convert(100,"CHF")
# currency_convert(110,"EUR")
# currency_convert(120,"USD")
# as.numeric(map2(c(100,110,120),c("CHF","EUR","USD"),currency_convert))


### Converts the amount of currency into USD, using getCurrencyPairs
currency_convert = function(amount,currency) {
  ### Suppress warning that close is only current close and not final one for today
  usd=getCurrencyPairs()
  cur_convert= switch(currency, "EUR"=usd$EUR,
                      "CHF"=usd$CHF,
                      "USD"=1)
  
  cur_convert*amount
}

### Same but with EUR and CHF variables
convert_to_usd = function(amount,currency,EUR,CHF) {
  round(case_match(currency,
                   "EUR" ~amount*EUR,
                   "CHF" ~amount*CHF,
                   "USD" ~amount),2)
}



###########  Compute option price based on combo ############
#### This has been replaced by getBSComboPrice

# positions=data.frame(type="P",expiration=ymd("2023-02-17"),strike=80.0)
# positions=add_row(positions,type="P",expiration=ymd("2023-02-17"),strike=90.0)
# positions=add_row(positions,type="P",expiration=ymd("2023-04-21"),strike=100.0)
# positions=add_row(positions,type="C",expiration=ymd("2023-03-21"),strike=100.0)

# compute_opt_price = function(positions, price, vol,days){
#   mutate(positions,p_price= if_else(
#     type=="P", getBSPutPrice(S=price,K=positions$strike,r=interest_rate,
#                              DTE=as.numeric(ymd(positions$expiration)-today()-days),
#                              sig=vol),
#     getBSCallPrice(S=price,K=positions$strike,r=interest_rate,
#                    DTE=as.numeric(ymd(positions$expiration)-today()-days),sig=vol)))
# }

