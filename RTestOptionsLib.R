#### 16.05.2023
#### test functions for options calculations

library(magrittr)
library(dplyr)
options(dplyr.summarise.inform = FALSE)
library(derivmkts)
library(tidyr)
library(purrr)
library(DescTools)

interest_rate=0.05035 #### Eurodollar 3 month as of 01.09.2023

test_foo = function(number=0, foo, input, output) {
  if (!is.function(foo)) stop("This is not a function")
  if (norm(as.matrix(unlist(pmap(input,foo))-output),type="O") <= 0.0001)
    message("TEST RESULT ",number,"  ",as.character(substitute(foo))," is Ok")
  else message("TEST RESULT ",number,"  ",as.character(substitute(foo)), " is KO!!!!")
}

test_num=1

#### test getBSCallPrice ####
test_input = list(S=100,K=100,r=0.04,DTE=5,sig=0.25,div=0)
test_output = 1.19455
test_foo(test_num,getBSCallPrice,test_input,test_output)
test_num = test_num + 1

test_input = list(S=22,K=22.5,r=0.04,DTE=5,sig=0.3,div=0)
test_output = 0.1265669
test_foo(test_num,getBSCallPrice,test_input,test_output)
test_num = test_num + 1

#### test getBSPutPrice ####
test_input = list(S=100,K=100,r=0.04,DTE=5,sig=0.25,div=0)
test_output =  1.13977
test_foo(test_num,getBSPutPrice,test_input,test_output)
test_num = test_num + 1

test_input = list(S=22,K=22.5,r=0.04,DTE=5,sig=0.3,div=0)
test_output = 0.6142415
test_foo(test_num,getBSPutPrice,test_input,test_output)
test_num = test_num + 1

#### test getBSStraddlePrice ####
test_input = list(S=100,K=100,r=0.04,DTE=5,sig=0.25,div=0)
test_output = 1.19455+1.13977
test_foo(test_num,getBSStraddlePrice,test_input,test_output)
test_num = test_num + 1

test_input = list(S=22,K=22.5,r=0.04,DTE=5,sig=0.3,div=0)
test_output = 0.1265669+0.6142415
test_foo(test_num,getBSStraddlePrice,test_input,test_output)
test_num = test_num + 1


### Test getBSComboPrice
#### getBSComboPrice = function(data,S, r=interest_rate, sig,div=0)
df1=data.frame(pos=c(2,-2),type=c("Put","Put"),strike=c(100,95),DTE=c(5,10))
df2=data.frame(pos=c(1,-2),type=c("Call","Put"),strike=c(100,95),DTE=c(5,10))
df3=data.frame(pos=c(200,2),type=c("Stock","Put"),strike=c(NA,127),DTE=c(NA,18))
dg=rbind(cbind(df1,sig=0.3,S=100,r=0.05035),cbind(df2,sig=0.4,S=90,r=0.05035),cbind(df3,sig=0.37,S=130,r=0.05))
test_input = dg %>% group_by(S,sig) %>% nest()
test_output= c(1.007629,-11.19979,13002.76)
test_foo(test_num,getBSComboPrice,test_input,test_output)
test_num = test_num + 1


df1=data.frame(pos=c(2,-2),type=c("Put","Put"),strike=c(100,95),DTE=c(5,10),sig=c(0.2,0.4))
df2=data.frame(pos=c(1,-2),type=c("Put","Call"),strike=c(80,90),DTE=c(15,1),sig=c(0.1,0.6))
df4=data.frame(pos=c(200,2),type=c("Stock","Put"),strike=c(0,127),DTE=c(NA,11.99),sig=c(0,0.37))
dg=rbind(cbind(df1,S=100),cbind(df2,S=90))
(test_input = dg %>% group_by(S) %>% nest())
test_output=c(0.1184202, -2.2674)  ### Be aware that price is reduced by PGCD of positions!!!!
test_foo(test_num,getBSComboPrice,test_input,test_output)
test_num = test_num + 1

#### test getBSOptPrice ####
test_input=data.frame(type=c("Call","Call","Put","Put"), S=c(100,100,22,100),K=c(100,100,22.5,100),
                      r=c(0.04,0.04,0.04,0.04),DTE=c(5,5,5,10),div=c(0,0,0,0),
                      sig=c(0.3,0.147,0.3,0.25))

test_output=c(1.4278845, 0.7139234, 0.6142415, 1.5956271)
test_foo(test_num,getBSOptPrice,test_input,test_output)
test_num = test_num + 1

#### test getBSDeltaPrice ####
test_input %<>% add_row(type="Stock",S=100,K=NA,r=NA,DTE=NA,div=NA,sig=NA)
test_output=c(0.5132272,  0.5161330, -0.7280299, -0.4811875,1)
test_foo(test_num, getBSOptDelta,test_input,test_output)
test_num = test_num + 1

##############  getVol test functions #####################

test_input=data.frame(price=c(1.427885, 1.595627, 0.6142415), 
                      S=c(100,100,22),K=c(100,100,22.5),
                      r=c(0.04,0.04,0.04),
                      DTE=c(5,10,5),div=c(0,0,0))
test_input$fPrice=  c(getBSCallPrice,getBSPutPrice,getBSPutPrice)
test_output=c(0.30, 0.25, 0.30)
test_foo(test_num, getVol,test_input,test_output)
test_num = test_num + 1

### Works with default values for r and div
test_input=data.frame(price=c(getBSCallPrice(S=100,K=105,DTE=5,sig=0.25),
                              getBSPutPrice(S=90,K=100,DTE=10,sig=0.35),
                              getBSCallPrice(S=95,K=102,DTE=10,sig=0.3)),
                      S=c(100,90,95),K=c(105,100,102),DTE=c(5,10,10))
test_input$fPrice=  c(getBSCallPrice,getBSPutPrice,getBSCallPrice)
test_output=c(0.25,0.35,0.3)
test_foo(test_num, getVol,test_input,test_output)
test_num = test_num + 1

####################### getImpliedVolOpt test functions

getImpliedVolOpt(type="Call",S=100,K=105,DTE=5,price=0.06303)
getImpliedVolOpt(type="Put",S=20.85,K=21,DTE=28.88,price= 0.578326)
getImpliedVolOpt(type="Put",S=20.85,K=21,DTE=7.88,price=0.322445)

test_input=data.frame(type=c("Call","Put","Call","Stock","Put","Put"),
                      S=c(100,90,95,NA,20.85,20.85),K=c(105,100,102,NA,21,21),DTE=c(5,10,10,NA,28.88,7.88),
                      price=c(getBSCallPrice(S=100,K=105,DTE=5,sig=0.25),
                              getBSPutPrice(S=90,K=100,DTE=10,sig=0.35),
                              getBSCallPrice(S=95,K=102,DTE=10,sig=0.3),
                              0,
                              0.578326,
                              0.322445))
test_output=c(0.25,0.35,0.3,0,0.232,0.207)
test_foo(test_num, getImpliedVolOpt,test_input,test_output)
test_num = test_num + 1

##################  getImpliedVolStraddle test function
test_input = list(S=100,K=100,r=0.04,DTE=5,div=0,price=1.19455+1.13977)
test_output = 0.25
test_foo(test_num,getImpliedVolStraddle,test_input,test_output)
test_num = test_num + 1

test_input = list(S=22,K=22.5,r=0.04,DTE=5,div=0,price=0.1265669+0.6142415)
test_output = 0.3
test_foo(test_num,getImpliedVolStraddle,test_input,test_output)
test_num = test_num + 1

#################  getImpliedVolCombo tests
df1=data.frame(pos=c(2,-2),type=c("Put","Put"),strike=c(100,95),DTE=c(5,10))
df2=data.frame(pos=c(1,-2),type=c("Call","Put"),strike=c(100,95),DTE=c(5,10))
df3=data.frame(pos=c(1,-2,1),type=c("Call","Put","Put"),strike=c(4500,4400,4300),DTE=c(10,50,20))
df4=data.frame(pos=c(1,-1,100),type=c("Call","Put","Stock"),strike=c(22.5,21,NA),DTE=c(10,50,NA))
df5=data.frame(pos=c(1,-1),type=c("Call","Put"),strike=c(22.5,21),DTE=c(10,50))

### Removing 
dg=rbind(cbind(df1,price=0.5848232,S=100),cbind(df2,price=0.7181855,S=100),
         cbind(df3,price=getBSComboPrice(data=df3,S=4350,sig=0.2),S=4350), ### -211.5959
         cbind(df4,price=getBSComboPrice(data=df4,S=21.5,sig=0.236),S=21.5), ### 2149.596
         cbind(df5,price=getBSComboPrice(data=df5,S=21.5,sig=0.634),S=21.5))  ###  -1.156967

(test_input = dg %>% group_by(S,price) %>% nest())
(test_output=c(0.134,0.3,0.2,0.236,0.634))

test_foo(test_num,getImpliedVolCombo,test_input,test_output)
test_num = test_num + 1

##### Ne fonctionne pas #################
# getVol = function(price,min_vol=0,max_vol=8,iter=0,fPrice,...) {
#   if_else (iter>20, NA, {
#     sig=(min_vol+max_vol)/2
#     cur_price=fPrice(...,sig=sig)
#     if_else(abs(sig-max_vol)<0.00001, round(sig,4), {
#       message(paste("Iter: ",iter,"cur_price: ",cur_price, "min_vol: ",min_vol,"max_vol: ",max_vol,collapse=","))
#       if_else(sign(price*(cur_price-price))<0,  
#               getVol(price=price,min_vol=sig,max_vol=max_vol,iter=iter+1,fPrice,...),
#               getVol(price=price,min_vol=min_vol,max_vol=sig,iter=iter+1,fPrice,...))})
#   })
# }


