library(reticulate)
repl_python()
# 

py_run_file("C:/Users/aldoh/Documents/Global/getContractValue.py")

#### Python interactive code
from ib_insync import *


#### For a normal python script there is no need for util.startLoop, in fact it would throw an error. 
#### It is only needed when working interactively in Jupyter. 
#### Or to be more precise any console that utilizes IPythonKernel, such as the one in Spyder.
## util.startLoop()

ib = IB()
ib.connect('127.0.0.1', 7496, clientId=25)

ib.disconnect()

expdate='20231117'
strikes=[19,20,21,22,23]
strike=21

reqType=4
sec="STK"
sec="IND"
secType="STK"
secType="IND"
sym="SPY"
sym="ESTX50"
sym="SLV"
sym="USO"
sym="MC"
exchangeSec="SMART"
exchange='SMART'
exchange='EUREX'
exchangeSec='CBOE'
exchangeSec='EUREX'
exchangeOpt="EUREX"
exchangeOpt="SMART"

currency="USD"
currency="EUR"

tradingClass='USO'
tradingClass='SPXW'
tradingClass='SPX'
tradingClass='SLV'
tradingClass="OESX"
tradingClass="OEXP"

contracts=[Contract(secType='OPT',symbol=sym,lastTradeDateOrContractMonth=expdate,
                    strike=strike_c,right='Put',exchange=exchange,tradingClass=tradingClass) for strike_c in strikes]
ib.qualifyContracts(*contracts)
tickers = ib.reqTickers(*contracts)

tickers[0].marketPrice()
tickers[0].modelGreeks

