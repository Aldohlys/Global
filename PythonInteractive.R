library(reticulate)
repl_python()




#### Python interactive code
from ib_insync import *

util.startLoop()

ib = IB()
ib.connect('127.0.0.1', 7496, clientId=25)

ib.disconnect()

sym='SLV'
expdate='20230922'
strikes=[19,19.5,20,20.5,21,21.5,22,22.5,23,23.5]
strike=21
exchange='SMART'
tradingClass='SLV'

contracts=[Contract(secType='OPT',symbol=sym,lastTradeDateOrContractMonth=expdate,
                    strike=strike_c,right='Put',exchange=exchange,tradingClass=tradingClass) for strike_c in strikes]
ib.qualifyContracts(*contracts)