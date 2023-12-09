#### 04.09.2023
#### For all programs
###############   General purpose utility programs  ###################

library(lubridate) ### for today() function
library(readr)
library(DescTools) ### for pgcd
library(quantmod) ## to retrieve data from Yahoo - getSymbols
library(stringr) ### for str_to_upper function
library(scales) #### for label_percent() and label_dollar() function
library(RQuantLib) ## for isBusinessDay function

library(reticulate)
py_run_file("C:/Users/aldoh/Documents/Global/getContractValue.py")
print("Python functions loaded!")


##########################  General purposes utilities ##########

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
  else return(DescTools::GCD(as.numeric(x.na)))
}


##### reformat function is to be used to convert a CSV file 
##### separated by commas into CSV separated by semi-columns
reformat = function() {
  file="C:\\Users\\aldoh\\Documents\\NewTrading\\Trades.csv"
  new_file="C:\\Users\\aldoh\\Documents\\NewTrading\\NewTrades.csv"
  trade=read.delim(file,check.names = TRUE,sep=",")
  col_file=c("TradeNr","Account","TradeDate","Thème","Instrument","Ssjacent","Pos","Prix","Comm.",
             "Total","Exp.Date","Risk","Reward","PnL","Statut","Currency","Remarques")
  trade=select(trade,all_of(col_file))
  write.table(trade,new_file,sep=";",row.names=F,fileEncoding = "UTF-8")
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

##### Utilities to work on Trades.csv file or options (getDTE)  ###################
#### Utilities - no real relation with options computation 
getDTE = function(c_datetime,exp_date) {
  if_else(is.na(exp_date),NA, {
    exp_datetime=as_datetime(exp_date)
    hour(exp_datetime) = 16  ### Option end is at 16:00 EST
    tz(exp_datetime) = "EST" 
    
    difftime=exp_datetime - c_datetime
    
    ### If difftime i.e. DTE is negative then call and put prices are equal to 0 - delta equal to 0 
    difftime=as.numeric(as.duration(difftime)) ### Convert to duration in seconds and then to a number
    difftime=round(difftime/(24*3600),2) ### and then convert duration from seconds duration to days duration - keep only 2 digits
    
    difftime
  })
}


buildInstrumentName=function(sym,expdate,strike,type) {
  ### If type is neither P,C, Put or Call then returns sym name
  if_else(type %in% c("P","C","Put","Call"), {
    #### expdate must be a date
    stopifnot("expdate must be a date!" = is.Date(expdate))
    
    ## In case type already equals P or C, keep current value - otherwise replace by P for Put and C for Call
    right = case_match(type, "Put" ~"P","Call" ~"C",.default = type)
    
    #### Instrument string is built using English - i.e. non locale date names
    #### This is necessary as Instrument name must follow IBKR format (to match strings in Trades.csv file)
    #### First retrieve current locale and save it
    loc= Sys.getlocale("LC_TIME")
    ### Apply locale used by IBKR to build instrument expiration date
    Sys.setlocale("LC_TIME","English")
    
    expdate=stringr::str_to_upper(format(expdate,"%d%b%y"))
    ### Re-load previous locale
    Sys.setlocale("LC_TIME",loc)
    paste(sym,expdate,strike,right)
  },sym )
}

### For office laptop
###file="C:\\Users\\martinale\\Documents\\RProjects\\RAnalysis\\Trading\\Trades.csv"

### This function work only for IBKR accounts not for Gonet account
getAllTrades = function() {
  suppressMessages(read_delim(file="C:\\Users\\aldoh\\Documents\\NewTrading\\Trades.csv",
                                     delim=";",locale=locale(date_names="en",decimal_mark=".",
                                                             grouping_mark="",encoding="UTF-8")))
}

### This function work only for IBKR accounts not for Gonet account
getAllCurrencyPairs = function() {
  suppressMessages(read_delim(file="C:\\Users\\aldoh\\Documents\\NewTrading\\CurrencyPairs.csv",
                                     delim=";",locale=locale(date_names="en",decimal_mark=".",
                                                             grouping_mark="",encoding="UTF-8")))
}

getTradeNr = function(v_instrument) {
  if(length(v_instrument)==0) return(NA)
  if (is.unsorted(v_instrument)) stop("Instrument must be sorted - prog. error")
  ### Read Trades.csv file and extract open/adjusted trades, to select all instruments present in dt argument
  trades=getAllTrades()
  trades %<>% filter(Statut=="Ouvert" | Statut=="Ajusté")
  trades %<>% select(Instrument,TradeNr)
 
  ### Retrieve dates in trades.csv corresponding to instruments of dt
  ### If there are several dates for the same instrument, take the oldest one (min)
  ### Suppress join by message
  trade_nr=suppressMessages(left_join(as.data.frame(list(Instrument=v_instrument)),trades)  %>% 
    group_by(Instrument) %>% 
    pull(TradeNr))
  
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
    display_error_message("There is more than one trade in Instrument argument! Display oldest trade nr")
    return(NA)
  }
  
  message("getTradeNr: ",trade_nr)
  return(as.integer(trade_nr))
}


getOpenDate = function(trade_nr) {
  
  stopifnot("trade_nr must be a numeric" = is.numeric(trade_nr))
  trades=getAllTrades()
  
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
  
  message("getOpenDate: ",trade_nr," orig_date: ",format(orig_trade_date,"%d-%m-%Y"))
  return(orig_trade_date)
}

### Returns a number - sum of rewards (non-NA) for all given instruments that are Open/adjusted
getRnR = function(trade_nr) {
  message("getRnR - Reward and Risk")
  stopifnot("trade_nr must be a numeric" = is.numeric(trade_nr))
  
  ##if (is.unsorted(v_instrument)) stop("Instrument must be sorted - prog. error")
  ### Read Trades.csv file and extract open/adjusted trades, to select all instruments present in dt argument
  trades=getAllTrades()
  
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

###################  Retrieve prices functions #######################

##### Yahoo-based symbol lookup
### getSym is based upon quantmod getSymbols function

#### auto.assign=TRUE is necessary if multiple symbols at the same time
getSymFromDate = function(sym, date) {
  lookup_yahoo = c("ESTX50"="^STOXX50E","MC"="MC.PA","OR"="OR.PA","TTE"="TTE.PA","AI"="AI.PA",
                   "SPX"="^SPX","XSP"="^XSP","NESN"="NESN.SW","HOLN"="HOLN.SW","SLHN"="SLHN.SW")
  sym=if_else(sym %in% names(lookup_yahoo), lookup_yahoo[sym], sym)
  lapply(sym, function(x) { quantmod::getSymbols(x,from=date,auto.assign = F,warnings=FALSE)})
}

getSym = function(sym){
  getSymFromDate(sym,ymd("2023-01-03"))
}

# getAdjReturns = function(sym) {
#   returns=data.frame(date=as.Date(index(sym)), 
#                      return=c('NA',round(diff(log(coredata(coredata(sym[,6])))),3)))
#   colnames(price)=c("date","return")
#   return(returns)
# }

getPrice= function(sym_list) {
  ### Takes only the first element of the sym list
  ### This is for compatibility with getSymFromDate
  sym=sym_list[[1]]
  price=data.frame(date=as.Date(index(sym)), value=coredata(sym[,6]))
  colnames(price)=c("date","value")
  return(price)
}


### Tries first on Yahoo (close price) - this works only for previous days, not for today
### THen on IBKR and if not available returns NA
### report_date can be a closed day -> then takes nearest day in the list
### prices_list goes back only to begin of 2023 - so not suited for Gonet account
getsymPrice = function(sym,currency,report_date){
  
  ### First case - requested date is an holiday
  ### Get last close price in this case
  if ((!RQuantLib::isBusinessDay("UnitedStates",report_date)) | report_date < today()) {
    prices_list=getPrice(getSymFromDate(sym,ymd("2023-01-03")))
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

### Takes last open date if current date provided does not work
getLastAdjustedPrice = function(ticker) {
  if_else( (is.null(ticker) | ticker %in% c("","All","STOCK")),
      NA,
      {
        ## Case date is Tuesday morning and US market not yet opened + Monday and Friday were off -> Get THur data
        ### This returns all last data available
        ticker=getSymFromDate(ticker,today()-5) 
        ### Last column is "ticker.Adjusted"
        sapply(ticker, function(x) round(x[[nrow(x),6]],2))
      }
  )      
}


getLastPriceDate = function(ticker) {
  if_else( (is.null(ticker) | ticker %in% c("","All","STOCK")),
           NA,
           {ticker=getSymFromDate(ticker,today()-5) 
           ### Last column is "ticker.Adjusted" -> to be renamed as Adjusted
           sapply(ticker,function(x) format(index(x[nrow(x)]),"%d.%m.%Y"))
           })
}

### Retrieve data from Yahoo Finance - no need to launch IBKR TWS
### Get last price and last change (J/J-1)
getLastTickerData = function(ticker) {
  if (is.null(ticker) |
      ticker %in% c("","All","STOCK")) return(list(last=NA,change=NA))
  tryCatch({
    ticks=getSymFromDate(ticker,today()-5)[[1]] ## Case Tuesday morning and US market not yet opened + Monday and Friday were off -> Get Wed and THur data
    ##names(ticks)[length(names(ticks))]="Adjusted" ### Last column is "ticker.Adjusted" -> to be renamed as Adjusted
    last_data=ticks[[nrow(ticks),6]]
    p_last_data=ticks[[nrow(ticks)-1,6]]
    return(list(
      last=round(last_data,2),
      change=scales::label_percent(accuracy=0.01)(last_data/p_last_data-1)
    ))
  }, error = function(e) {
    print(paste("Error:", e))
    return(list(last="Non disponible",change=NA))
  })
}

lastSPY=getLastTickerData("SPY")  ### Mkt value

getVal=function(sym) {
  cat("No value for ",sym,"\n Enter new price: ")
  if (interactive()) {
    ## display_error_message(paste0("No value for",sym," You need to enter a new price"))
    # showModal(modalDialog(
    #   tags$h2('No value for Please enter your personal information'),
    #   numericInput('val', 'Value'),
    #   footer=tagList(
    #     actionButton('submit', 'Submit'),
    #     modalButton('cancel')
    #   )
    # ))
  val=readline(prompt="(interactive) ")
  }
  else val= readLines(con="stdin", n=1)[[1]]
  as.double(val)
}

#### Used by Gonet.R script and RAnalysis
stock_price = function(sec="STK",sym,currency,exchange="SMART",reqType=4) {
  #### Default value for security type is Stock
  #### Default value for exchange is SMART
  message("stock_price")
  ### Special case for CSBGU0 stock I won in Gonet portfolio
  if (sym == "CSBGU0") reqType=3
  line=py$getStockValue(sec=sec,sym=sym,currency=currency,exchange=exchange,reqType=reqType)
  
  #### readline works only in interactive mode, 
  #### readLines works only in non-interactive mode
  ### Case where no IBKR connection exists (NULL) or no value returned
  ### isTRUE let is.na test works also if val=NULL
  ### length(val) is TRUE when val=numeric(0)
  if (is.null(line)) {
    val=getVal(sym)
    #### Write data to CSV file as data input by end user or Yahoo
    line=tibble(datetime=format(now(),"%e %b %Y %Hh%M"),sym=sym)
    line$price=val
    write.table(line,"C:/Users/aldoh/Documents/Global/prices.csv",sep=";",
                row.names = FALSE,quote=F,col.names = FALSE,append=TRUE)
  }
  val=line[["price"]]
  print(paste0("DateTime:",line[["datetime"]], " Value: ",val))
  return(val)
} 

#### CURRENCY management ##################

### The 2 lines below will not work with "from=today()" between 00:00 and 6:00am 
### as Europe is one day after the US between this time period and Yahoo server is located in the US

getCurrencyPairs = function() {
  message("getCurrencyPairs")
  ### euro_usd and chf_usd data frames - values for the day- are already retrieved
  usd=getAllCurrencyPairs()

  ### Convert string to date
  last_usd=last(usd)
  last_date=ymd(last_usd$date)
  
  if (last_date == today())  {
    #print(usd)
    return(last_usd)
  }
  
  EUR = py$getCurrencyPairValue("EURUSD",reqType=2)
  if (is.null(EUR)) EUR=getVal("EUR/USD")
  else if (is.na(EUR)) EUR=getVal("EUR/USD")
  
  CHF = py$getCurrencyPairValue("CHFUSD",reqType=2)
  if (is.null(CHF)) CHF=getVal("CHF/USD")
  else if (is.na(CHF)) CHF=getVal("CHF/USD")
  
  usd = data.frame(date=today(),EUR=EUR,CHF=CHF)
  #print(usd)
  
  write.table(usd,"C:/Users/aldoh/Documents/NewTrading/CurrencyPairs.csv",sep=";",dec=".",row.names=F,append=T,col.names=F)
  return(usd)
}


### Returns a string with amount and currency symbol
currency_format = function(amount,currency){
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
  
  if_else (is.na(amount), "", {
    case_match(currency, "EUR"~euro(amount),
           "CHF"~chf(amount),
           "USD"~dollar(amount))
  })
}

### Converts the amount of currency into USD, using getCurrencyPairs
currency_convert = function(amount,currency) {
  
  ### Suppress warning that close is only current close and not final one for today
  usd=getCurrencyPairs()
  cur_convert= case_match(currency, "EUR"~usd$EUR,
                      "CHF"~usd$CHF,
                      "USD"~1)
  
  cur_convert*amount
}

### Same but with EUR and CHF variables
convert_to_usd = function(amount,currency,EUR,CHF) {
  round(case_match(currency,
                   "EUR" ~amount*EUR,
                   "CHF" ~amount*CHF,
                   "USD" ~amount),2)
}

convert_to_usd_date = function(amount,currency,date) {
  usd=usd=getAllCurrencyPairs()
  usd$date=ymd(usd$date)
  
  input=tibble(amount,currency,date)
  
  ### Retrieve at a given date
  data=left_join(input,usd)
  convert_to_usd(data$amount,data$currency,data$EUR,data$CHF)
  
}

### Test data
# amount=c(100,120,110)
# currency=c("CHF","USD","EUR")
# date=ymd(c(20231110,20231109,20231115))
# 


## Compare symbol, expiration date and position - 
## price cannot be used as combined spread (vertical) may exist in portfolio files
### dt contains symbol, date, avgCost, pos
# getUPrice = function(dt) {
#   portf1= data.frame(read.csv("C:\\Users\\aldoh\\Documents\\NewTrading\\U1804173.csv",sep=";"))
#   portf2= data.frame(read.csv("C:\\Users\\aldoh\\Documents\\NewTrading\\DU5221795.csv",sep=";"))
#   portf=rbind(portf1,portf2)
#   
#   ### Convert from European date format to internal R date format
#   portf$date=as.Date(portf$date,format="%d.%m.%Y") 
#   
#   portf = right_join(portf,dt,by=c("symbol"="sym","date"))
#   portf %>% replace(is.na(.), 0) %>% group_by(date,symbol) %>% summarize(uPrice=max(undPrice)) %>% ungroup
# }