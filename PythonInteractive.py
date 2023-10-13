# library(reticulate)
# repl_python()
# 

#### Python interactive code
from ib_insync import *

util.startLoop()

ib = IB()
ib.connect('127.0.0.1', 7496, clientId=25)

ib.disconnect()

expdate='20231117'
strikes=[19,20,21,22,23]
strike=21
exchange='SMART'
tradingClass='SLV'

contracts=[Contract(secType='OPT',symbol=sym,lastTradeDateOrContractMonth=expdate,
                    strike=strike_c,right='Put',exchange=exchange,tradingClass=tradingClass) for strike_c in strikes]
ib.qualifyContracts(*contracts)
tickers = ib.reqTickers(*contracts)

tickers[0].marketPrice()
tickers[0].modelGreeks

