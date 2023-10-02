#### 04.09.2023
#### FOr all programs
###############   General purpose utility programs  ###################

library(reticulate)
library(lubridate)
library(readr)
library(DescTools)
library(quantmod)
library(stringr)

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
  
  for (number in numbers) {
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

getOpenDate = function(v_instrument) {
  message("getOpenDate")
  if (is.unsorted(v_instrument)) stop("Instrument must be sorted - prog. error")
  ### Read Trades.csv file and extract open/adjusted trades, to select all instruments present in dt argument
  trades= data.frame(read.csv("C:\\Users\\aldoh\\Documents\\NewTrading\\Trades.csv",sep=";"))
  ###trades=data.frame(read.csv("C:\\Users\\martinale\\Documents\\RProjects\\RAnalysis\\Trading\\Trades.csv",sep=";"))
  trades %<>% filter(Statut=="Ouvert" | Statut=="Ajusté")
  trades %<>% select(Instrument,TradeDate)
  trades$TradeDate=dmy(trades$TradeDate)
  ### Retrieve dates in trades.csv corresponding to instruments of dt
  ### If there are several dates for the same instrument, take the oldest one (min)
  orig_trade_dates=left_join(as.data.frame(list(Instrument=v_instrument)),trades)  %>% 
      group_by(Instrument) %>% 
      summarize(dates= (if_else (is.na(TradeDate),NA,suppressWarnings(min(TradeDate,na.rm=T))))) %>% 
      pull(dates)
  
  ### If all instrumenst are NA -> trade is not present - not yet recorded in Trades.csv
  if (all(is.na(orig_trade_dates))) {
    message("trade not present")
    print(v_instrument)
    return(NA)
  }
  
  ### If at least one
  if (any(is.na(orig_trade_dates))) {
    old_orig_date=min(orig_trade_dates,na.rm=T)
    orig_trade_dates=replace(orig_trade_dates,is.na(orig_trade_dates),old_orig_date)
  }
  return(orig_trade_dates)
}

### Returns a number - sum of rewards (non-NA) for all given instruments that are Open/adjusted
getReward = function(v_instrument) {
  message("getReward")
  ##if (is.unsorted(v_instrument)) stop("Instrument must be sorted - prog. error")
  ### Read Trades.csv file and extract open/adjusted trades, to select all instruments present in dt argument
  trades= data.frame(read.csv("C:\\Users\\aldoh\\Documents\\NewTrading\\Trades.csv",sep=";"))
  ###trades=data.frame(read.csv("C:\\Users\\martinale\\Documents\\RProjects\\RAnalysis\\Trading\\Trades.csv",sep=";"))
  trades %<>% filter(Statut=="Ouvert" | Statut=="Ajusté")
  trades %<>% select(Instrument,Reward)
 
  ### Retrieve dates in trades.csv corresponding to instruments of dt
  ### If there are several dates for the same instrument, take the oldest one (min)
  reward=left_join(as.data.frame(list(Instrument=v_instrument)),trades)  %>% 
    summarize(reward= sum(as.double(Reward),na.rm=T)) %>% pull(reward) 
  print(reward)
  return(reward)
}

### Tries first on Yahoo (close price) - this works only for previous days, not for today
### THen on IBKR and if not available returns NA
getsymPrice = function(sym,currency,orig_date){
  prices_list=getPrice(getSym(sym))
  price=filter(prices_list,date==orig_date)
  if (nrow(price)!=0) price$value else {
    ### SMART works fine in all cases except for SPX and XSP cases
    exchange = switch(sym, ESTX50= "EUREX", SPX =, XSP = "CBOE", "SMART")
    sec=switch(sym, SPX=,XSP=,ESTX50="IND","STK")
    py$getStockValue(sec=sec,sym=sym,currency=currency,exchange=exchange,reqType=4)
  }
}
  
py_run_file("getContractValue.py")

getCurrencyPairs = function() {
  EUR = py$getCurrencyPairValue("EURUSD",reqType=2)
  if(is.na(EUR)) {
    cat("No value for EUR-USD pair\n ")
    EUR=readline(prompt="Enter value: ")
  }
  EUR=as.double(EUR)
  euro_usd <<- data.frame(date=as.Date(today()),EUR=EUR)
  
  CHF = py$getCurrencyPairValue("CHFUSD",reqType=2)
  if(is.na(CHF)) {
    cat("No value for CHF-USD pair\n ")
    CHF=readline(prompt="Enter value: ")
  }
  CHF= as.double(CHF)
  chf_usd <<- data.frame(date=as.Date(today()),CHF=CHF)
}


currency_format = function(amount,currency){
  if (is.na(amount)) "" else {
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
    
    switch(currency, "EUR"=euro(amount),
           "CHF"=chf(amount),
           "USD"=dollar(amount))
  }
}

# positions=data.frame(type="P",expiration=ymd("2023-02-17"),strike=80.0)
# positions=add_row(positions,type="P",expiration=ymd("2023-02-17"),strike=90.0)
# positions=add_row(positions,type="P",expiration=ymd("2023-04-21"),strike=100.0)
# positions=add_row(positions,type="C",expiration=ymd("2023-03-21"),strike=100.0)

compute_opt_price = function(positions, price, vol,days){
  mutate(positions,p_price= if_else(
    type=="P", getBSPutPrice(S=price,K=positions$strike,r=interest_rate,
                             DTE=as.numeric(ymd(positions$expiration)-today()-days),
                             sig=vol),
    getBSCallPrice(S=price,K=positions$strike,r=interest_rate,
                   DTE=as.numeric(ymd(positions$expiration)-today()-days),sig=vol)))
}

