#### General purpose options library
#### Alexis Martin 16.05.2023
####
####
#### getBSCallPrice : get BS price for Call option
#### getBSPutPrice : get BS price for Put option
#### getBSOptPrice : get BS price for any option
#### getBSOptDelta : get BS price for any option or 1 for stock
#### getVol : get implied vol given a price, a price function

library(derivmkts) ### for basic bsput, bscall and Greeks computation
library(quantmod) ### To retrieve stock quotes from Yahoo src
library(lubridate) ### to manage dates and time
library(dplyr) ### Data manipulation
library(purrr) ### Functional programing (e.g. pmap)
library(tidyr)

interest_rate=0.05472 ### 3 months Treasury Bill

############################## Black-Scholes computation ##################
### https://www.theglobeandmail.com/investing/markets/commodities/GE*1/

##### https://rdocumentation.org/packages/derivmkts/versions/0.2.5
##########  Black Scholes option pricing model
# !!!! bscall(s=410,k=410,v=0.17,r=0,tt=0,d=0) returns NaN - basically not possible to price the call -> equals 0
# Same issue for bsput
### If missing r then r= interest_rate, missing div then div= 0

######### getBSCallPrice ######################
getBSCallPrice <- function(S, K, r=interest_rate, DTE, sig, div=0){
  if(any(missing(S), missing(K), missing(DTE),missing(sig))) {
    message(c(as.list(environment())))
    stop("getBSCallPrice: one of arguments missing!!")
  } 
  # d1 <- (log(S/K) + (r + sig^2/2)*T) / (sig*sqrt(T))
  # d2 <- d1 - sig*sqrt(T)
  # value <- S*pnorm(d1) - K*exp(-r*T)*pnorm(d2)  
  ## Handle special cases not handled correctly by bsput/bscall
  if_else( ((DTE==0)&(S==K) | (DTE<0) | ((S==0)&(K==0))), 0, 
           {
        T=DTE/365
        bscall(s=S, k=K, v=sig, r=r, tt=T, d=div)})
}

########### getBSPutPrice ###########
getBSPutPrice <- function(S, K, r=interest_rate, DTE, sig, div=0){
  if(any(missing(S), missing(K), missing(DTE),missing(sig))) stop("getBSPutPrice: one of arguments missing!!")
  
  # print("getBSPutPrice: ")
  # argg <- c(as.list(environment()))
  #View(argg)
  # d1 <- (log(S/K) + (r + sig^2/2)*T) / (sig*sqrt(T))
  # d2 <- d1 - sig*sqrt(T)
  # value <-  (K*exp(-r*T)*pnorm(-d2) - S*pnorm(-d1))
  
  ## Handle special cases not handled correctly by bsput/bscall
  if_else( ((DTE==0)&(S==K) | (DTE<0) | ((S==0)&(K==0))), 0, 
           {
             T=DTE/365
             bsput(s=S, k=K, v=sig, r=r, tt=T, d=div)})
}

########### Straddle Price Put + Call
getBSStraddlePrice = function(S,K,r,DTE,sig,div=0) {
  getBSCallPrice(S,K,r,DTE,sig,div)+getBSPutPrice(S,K,r,DTE,sig,div)
}

### getBSOptPrice ##############
getBSOptPrice = function(type,S,K,r=interest_rate,DTE,sig,div=0) {
  ##print(paste("getBSOptPrice Type:",type," S:",S," K:",K," r:",r," DTE:",DTE," sig:",sig, "div:",div))
  if_else(type=="Put", getBSPutPrice(S, K, r, DTE, sig, div),
          getBSCallPrice(S, K, r, DTE, sig, div))
}

### getBSPrice ##############
getBSPrice = function(type,S,K,r=interest_rate,DTE,sig,div=0) {
  ##print(paste("getBSOptPrice Type:",type," S:",S," K:",K," r:",r," DTE:",DTE," sig:",sig, "div:",div))
  if_else(type=="Put", getBSPutPrice(S, K, r, DTE, sig, div),
          if_else(type=="Call", getBSCallPrice(S, K, r, DTE, sig, div),
              if_else(type=="Stock", S, NA)))
}


##### Combo price ################
### data data frame with columns: pos,type,strike,DTE - sig provided by calling function
### Ratioed price - dividing by PGCD of positions
### But multiplier should be the same for all lines - otherwise it does not make sense at all
### Either implied volatility is provided as part of the combo itself for each branch, or it is provided only for the whole combo
### In all cases, if provided externally then volatility and underlying prices must be sorted, 
## otherwise the prices list will not correspond to the set of underlying prices and implied vol - 
### because summarize sorts values by grouped data (here S and sig)

### Be aware that combo price is divided by PGCD of positions!!!!

getBSComboPrice = function(data,S, r=interest_rate, sig,div=0) { 
  
  ######  data contains pos type strike DTE / sig is either in data, or independant - S is independant
  if (any(missing(data),missing(S)) | (missing(sig) & !("sig" %in% names(data)))) {
    display_error_message("Missing data in getBSComboPrice!!")
    return(NA)
  }
  
  if (is.unsorted(S)) {
    display_error_message("Underlying prices list must be sorted !")
    return(NA)
  }

  ### Case where sig is to be found in data dataframe, as absent from command line
  if (missing(sig)) {
    data=crossing(data,S=S) %>% group_by(S)
    price=data %>% summarize(price=sum(pos*getBSOptPrice(type=type,S=S,K=strike,r=r,DTE=DTE,sig=sig))/pgcd(pos))
  }
  
  else {
    if (is.unsorted(sig)) {
      display_error_message("Implied vol lists must be sorted")
      return(NA)
    }
    ### In case sig also present in the data frame
    data$sig=NULL
    data=crossing(data,S=S,sig=sig) %>% group_by(S,sig)
    price=data %>% summarize(price=sum(pos*getBSPrice(type=type,S=S,K=strike,r=r,DTE=DTE,sig=sig))/pgcd(pos))
  }
  return(price$price)
}



#######  getBSOptDelta ###############
getBSOptDelta <- function(type,S,K,DTE,sig,r=interest_rate,div=0){
  ## Handle special cases not handled correctly by bsput/bscall
  ### DTE=0 cannot be handled correctly
  ### Test first that DTE exists (Stock case)
  
  if_else (type=="Stock", 1, {
    if_else (DTE<=0, 0, {
      T=DTE/365
      ##print(paste("getBSOptDelta: argts S:",S," K:",K," sig:",sig," r:",r," T:",T," div:",div))
      if_else(type=="Put", bsopt(S,K,sig,r,T,div)$Put["Delta",],
               bsopt(S,K,sig,r,T,div)$Call["Delta",])
    })
  })
}

#### getVol Implied vol computation #########################
#### Also valid for different combo options

### getVol deduces implied volatility from an option price (called price)
### TO do this it computes option prices with a series of volatility numbers (starting from 400%)
#### THen dochotomia algorithm : this works ONLY if fPrice is monotonous
getVol = function(price,min_vol=0,max_vol=2,iter=0,fPrice,...) {
  if (missing(price)) return(NA)
  if (iter>20) return(NA)
  sig=(min_vol+max_vol)/2
  cur_price=fPrice(...,sig=sig)
  ##message("Iter: ",iter," cur_price: ",cur_price, " min_vol: ",min_vol," max_vol: ",max_vol)
  if (is.na(cur_price)) return(NA)
  if (abs(sig-max_vol)<0.0001) {
    #  message("Iter: ",iter,"cur_price: ",cur_price, "min_vol: ",min_vol,"max_vol: ",max_vol)
      round(sig,3) 
  }
  else {
      if (sign(price*(cur_price-price))<0)  getVol(price=price,min_vol=sig,max_vol=max_vol,iter=iter+1,fPrice,...)
      else getVol(price=price,min_vol=min_vol,max_vol=sig,iter=iter+1,fPrice,...)  
  }
}

getVol2 = function(price,fPrice,...) {
  if (missing(price)) return(NA)
  sample_size=0.5  ### Slices of 50% each
  sample_increment=0.0001 ### to get volatility at 0.05% precision - 5'000 samples each time
  for (sig1 in seq(from=0,to=2,by=sample_size)) {
    prices_list=fPrice(...,sig=seq(sig1,sig1+sample_size,by=sample_increment))
    min_price_index=which.min(abs(prices_list-price))
    min_price=prices_list[min_price_index]
    if (abs(min_price-price)<0.001) {
      vol=sig1+(min_price_index-1)*sample_increment
      ### Limit to xx.y% format
      return(round(vol,3))
    }
  }
  display_error_message("Could not compute a volatility from price function !!")  
}

getImpliedVolOpt = function(type,S,K,r=interest_rate,DTE,div=0,price) {
  if_else(is.na(price),NA,
  switch(type,
    "Stock" = 0,
    "Put" = getVol(price=price,fPrice=getBSPutPrice,S=S,K=K,r=r,DTE=DTE,div=div),
    "Call"=getVol(price=price,fPrice=getBSCallPrice,S=S,K=K,r=r,DTE=DTE,div=div)
  ))
}

getImpliedVolOpt2 = function(df) {
  return(unlist(pmap(df,getImpliedVolOpt)))
}

#### getVOl cannot be vectorized so this cannot be used as vectorized function
getImpliedVolOpt = function(type,S,K,r=interest_rate,DTE,div=0,price) {
  if_else(is.na(price),NA,
          switch(type,
                 "Stock" = 0,
                 "Put" = getVol(price=price,fPrice=getBSPutPrice,S=S,K=K,r=r,DTE=DTE,div=div),
                 "Call"= getVol(price=price,fPrice=getBSCallPrice,S=S,K=K,r=r,DTE=DTE,div=div)
          ))
}


getImpliedVolStraddle = function(S,K,r=interest_rate,DTE,div=0,price) {
  getVol(price=price,fPrice=getBSStraddlePrice,S=S,K=K,r=r,DTE=DTE,div=div)
}

getImpliedVolCombo = function(S,data,r=interest_rate,div=0,price) {
  getVol2(price=price,fPrice=getBSComboPrice,data=data,S=S,r=r,div=div)
}

