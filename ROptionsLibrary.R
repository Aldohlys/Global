
##########  30.08.2022
##########  Alexis Martin

library(derivmkts)
library(quantmod)
library(lubridate)

#########  General purposes utilities

getSym = function(sym){
  return(getSymbols(sym,auto.assign = FALSE,warnings=FALSE))
}


getPrice = function(sym) {
  price=data.frame(date=as.Date(index(sym)), value=coredata(sym[,6]),volume=coredata(sym[,5]),return=c('NA',round(diff(log(coredata(coredata(sym[,6])))),3)))
  colnames(price)=c("date","value","volume","return")
  return(price)
}

interest_rate=0.0384 #### Eurodollar 3 month as of 19.03.2023

#### This is a utility - no real relation with options computation
getDTE = function(c_datetime,exp_date) {
  exp_datetime=as_datetime(exp_date)
  hour(exp_datetime) = 16  ### Option end is at 16:00 EST
  tz(exp_datetime) = "EST" 
  
  difftime=exp_datetime - c_datetime
  
  if (difftime < 0) return(0)  ### DTE cannot be negative
  difftime=as.numeric(as.duration(difftime)) ### Convert to duration in seconds and then to a number
  difftime=round(difftime/(24*3600),2) ### and then convert duration from seconds duration to days duration - keep only 2 digits
  
  return(difftime)
}


### https://www.theglobeandmail.com/investing/markets/commodities/GE*1/

##### https://rdocumentation.org/packages/derivmkts/versions/0.2.5
##########  Black Scholes option pricing model

##### S = Spot price
##### K = exercise price
##### r = Annual continuously-compounded risk-free interest rate
##### DTE = nb of days to expiration. 0 = same day. 0.5 = 12 hours to expiration
##### tt =  Time to maturity in years
##### v,sig = Volatility of the asset price, defined as the annualized standard deviation of the continuously-compounded return
##### d, div	= Dividend yield, annualized, continuously-compounded

# Usage library derivmkts
# bscall(s, k, v, r, tt, d)
# bsput(s, k, v, r, tt, d)
# assetcall(s, k, v, r, tt, d)
# cashcall(s, k, v, r, tt, d)
# assetput(s, k, v, r, tt, d)
# cashput(s, k, v, r, tt, d)
# Arguments

# !!!! bscall(s=410,k=410,v=0.17,r=0,tt=0,d=0) returns NaN - basically not possible to price the call -> equals 0
# Same issue for bsput

### If missing r then r= interest_rate, missing div then div= 0
getBSOptPrice = function(type,S,K,r=interest_rate,DTE,sig,div=0) {
  if (type=="Put") getBSPutPrice(S, K, r, DTE, div, sig)
  else getBSCallPrice(S, K, r, DTE, div, sig)
}

getBSCallPrice <- function(S, K, r=interest_rate, DTE, div=0, sig){
  if(any(missing(S), missing(K), missing(DTE),missing(sig))) stop("getBSCallPrice: one of arguments missing!!")

  ## Handle special cases not handled correctly by bsput/bscall
  if ((DTE==0)&(S==K)) return(0)
  if (DTE<0) return(0)
  
  T=DTE/365
  
  # d1 <- (log(S/K) + (r + sig^2/2)*T) / (sig*sqrt(T))
  # d2 <- d1 - sig*sqrt(T)
  # value <- S*pnorm(d1) - K*exp(-r*T)*pnorm(d2)
  
  bscall(s=S, k=K, v=sig, r=r, tt=T, d=div)
}

getBSPutPrice <- function(S, K, r=interest_rate, DTE, div=0, sig){
  if(any(missing(S), missing(K), missing(DTE),missing(sig))) stop("getBSPutPrice: one of arguments missing!!")
  
  # print("getBSPutPrice: ")
  # argg <- c(as.list(environment()))
  #View(argg)
  
    ## Handle special cases not handled correctly by bsput/bscall
    if ((DTE==0)&(S==K)) return(0)
    if (DTE<0) return(0)
  
    T=DTE/365
    # d1 <- (log(S/K) + (r + sig^2/2)*T) / (sig*sqrt(T))
    # d2 <- d1 - sig*sqrt(T)
    # value <-  (K*exp(-r*T)*pnorm(-d2) - S*pnorm(-d1))
    
    bsput(s=S, k=K, v=sig, r=r, tt=T, d=div)
}

getBSOptDelta <- function(type,S,K,DTE,sig,r=interest_rate,div=0){
  ## Handle special cases not handled correctly by bsput/bscall
  ### DTE=0 cannot be handled correctly
  if (DTE<=0) return(0)
  T=DTE/365
  
  print(paste("getBSOptDelta: argts S:",S," K:",K," sig:",sig," r:",r," T:",T," div:",div))
  if (type=="Put") bsopt(S,K,sig,r,T,div)$Put["Delta",]
  else bsopt(S,K,sig,r,T,div)$Call["Delta",]
}


#### Implied vol computation
#### Also for different combo options

getImpliedVol = function(price,max_vol,getPrice,...) {
  # print("getImpliedVol: ")
  # argg <- c(as.list(environment()), list(...))
  # print(argg)
  
  ## Handle case where no price received
  if (is.na(price)) return(0)
  ## If price is 0, then vol=0
  if (price==0) return(0)
  
  ### Price is increasing with volatility
  ### Volatility is >0
  
  max_price=getPrice(...,max_vol)
  if (abs(max_price-price)<0.01) {
    return(max_vol)
  }
  if (abs(price)>abs(max_price)) {
    print(paste("Price:",price,"Max price:",max_price))
    stop("Max volatility is too low!")
  }
  
  calc_vol=max_vol/2
  min_vol=0
  
  for (i in 1:100) {
    value= getPrice(...,sig=calc_vol)
    # print(paste0("Calc vol:",round(calc_vol,3),"   Min vol:",round(min_vol,3),"   Max vol:",round(max_vol,3),
    #            "   Price approx.: ",round(value,3),"   Real price:",price))
    diffPrice=value-price
    if (abs(diffPrice)<0.00005) {break}
    
    if (sign(diffPrice*price)>0) {
      max_vol=calc_vol
      calc_vol=calc_vol/2
    }
    else {
      min_vol=calc_vol
      calc_vol=(min_vol+max_vol)/2
    }
    
  }
  return(calc_vol)
}

### 800% max_vol
getImpliedVolOpt = function(type,S,K,r,DTE,div=0,price,max_vol=8) {
  if_else (type=="Call", getImpliedVol(price,max_vol,getBSCallPrice,S,K,r,DTE,div),
      getImpliedVol(price,max_vol,getBSPutPrice,S,K,r,DTE,div))
}




#### Forward volatility #############
#### As per Option's book formula
forwardVol = function(vol1,t1,vol2,t2) {
  return(sqrt((vol2^2*t2-vol1^2*t1)/(t2-t1)))
}

getCalendarPrice = function(S,K,r,DTE1,DTE2,vol,div=0) {
  getBSCallPrice(S,K,r,DTE2,vol,div)-
    getBSCallPrice(S,K,r,DTE1,vol,div)
}

getCallSpreadPrice = function(S,K1, K2,r,DTE,vol,div=0) {
  getBSCallPrice(S,K2,r,DTE,vol,div)-
    getBSCallPrice(S,K1,r,DTE,vol,div)
}

getPutSpreadPrice = function(S,K1, K2,r,DTE,vol,div=0) {
  getBSPutPrice(S,K2,r,DTE,vol,div)-
    getBSPutPrice(S,K1,r,DTE,vol,div)
}

getPutDiagSpreadPrice = function(S,K1, K2,r,DTE1, DTE2,vol,div=0) {
  getBSPutPrice(S,K1,r,DTE1,vol,div)-
    getBSPutPrice(S,K2,r,DTE2,vol,div)
}

###################  Binomial tree option computation
# binomopt(s, k, v, r, tt, d, nstep = 10, american = TRUE,
#          putopt=FALSE, specifyupdn=FALSE, crr=FALSE, jarrowrudd=FALSE,
#          up=1.5, dn=0.5, returntrees=FALSE, returnparams=FALSE,
#          returngreeks=FALSE)
# 
# nstep
# Number of binomial steps. Default is nstep = 10
# 
# american
# Boolean indicating if option is American
# 
# putopt
# Boolean TRUE is the option is a put
# 
# specifyupdn
# Boolean, if TRUE, manual entry of the binomial parameters up and down. This overrides the crr and jarrowrudd flags
# 
# crr
# TRUE to use the Cox-Ross-Rubinstein tree
# 
# jarrowrudd
# TRUE to use the Jarrow-Rudd tree
# 
# up, dn
# If specifyupdn=TRUE, up and down moves on the binomial tree
# 
# returntrees
# If returntrees=TRUE, the list returned by the function includes four trees: for the price of the underlying asset (stree), the option price (oppricetree), where the option is exercised (exertree), and the probability of being at each node. This parameter has no effect if returnparams=FALSE, which is the default.
# 
# returnparams
# Return the vector of inputs and computed pricing parameters as well as the price
# 
# returngreeks
# Return time 0 delta, gamma, and theta in the vector greeks

getBinomCallPrice = function(S, K, r=interest_rate, DTE, sig, div=0) {
  nsteps=200
  binomopt(S,K,sig,r,
           DTE/365,
           div,nsteps,
           american=TRUE,
           putopt=FALSE)[["price"]]
}

getBinomPutPrice = function(S, K, r=interest_rate, DTE, sig, div=0) {
  nsteps=200
  binomopt(S,K,sig,r,DTE/365,div,nsteps,
           american=TRUE,
           putopt=TRUE)[["price"]]
}

getImpliedVolCalendar = function(S,K,r,DTE1,DTE2,div=0,price,max_vol=8) {
  getImpliedVol(price,max_vol,getCalendarPrice,S,K,r,DTE1,DTE2,div)
}

getImpliedVolCallSpread = function(S,K1, K2, r,DTE,div=0,price,max_vol=8) {
  getImpliedVol(price,max_vol,getCallSpreadPrice,S,K1, K2,r,DTE,div)
}

getImpliedVolPutSpread = function(S,K1, K2, r,DTE,div=0,price,max_vol) {
  getImpliedVol(price,max_vol,getPutSpreadPrice,S,K1, K2,r,DTE,div)
}

getImpliedVolPutDiagSpread = function(S,K1, K2, r,DTE1, DTE2, div=0,price,max_vol) {
  getImpliedVol(price,max_vol,getPutDiagSpreadPrice,S,K1, K2,r,DTE1, DTE2,div)
}



##### This comes from Option as Strategic Investment's book advanced maths chapter
getProbFwd = function(P,Q,IV,DTE) {
  # P is current price
  # Q is target price
  # IV is implied vol for the stock considered
  
  v=IV*sqrt(DTE/365)
  # P Norm gives lower tail value by default lower.tail = FALSE otherwise to be given
  return(pnorm(log(Q/P)/v))
}


########  To get IV
#### Retrieve option chain for strikes where volume is > 10

#####################################################################################################################
#####################  Following Market Chameleon idea, compute forward price based upon previous underlying behaviour
################  Deduce call and put price

getCallPrice = function(spot,strike,DTE,end_date,sample_size) {
  distribCallPrice=sapply(getFwdPriceList(spot,eqRatioList(end_date,DTE,sample_size)),(function(x) getOneCallPrice(x,strike)))
  return(round(mean(distribCallPrice)/(1+DTE*interest_rate/365),2))
}

getRandomCallPrice = function(spot,strike,DTE,sample_size) {
  distribCallPrice=sapply(getFwdPriceList(spot,eqRatioRandomList(DTE,sample_size)),(function(x) getOneCallPrice(x,strike)))
  return(round(mean(distribCallPrice)/(1+DTE*interest_rate/365),2))
}


getPutPrice = function(spot,strike,DTE,end_date,sample_size) {
  distribPutPrice=sapply(getFwdPriceList(spot,eqRatioList(end_date,DTE,sample_size)),(function(x) getOnePutPrice(x,strike)))
  return(round(mean(distribPutPrice)/(1+DTE*interest_rate/365),2))
}

getRandomPutPrice = function(spot,strike,DTE,sample_size) {
  distribPutPrice=sapply(getFwdPriceList(spot,eqRatioRandomList(DTE,sample_size)),(function(x) getOnePutPrice(x,strike)))
  return(round(mean(distribPutPrice)/(1+DTE*interest_rate/365),2))
}

getFwdPrice = function(spot,DTE,end_date,sample_size) {
  distribFwdPrice=getFwdPriceList(spot,eqRatioList(end_date,DTE,sample_size))
  return(round(mean(distribFwdPrice),2))
}

