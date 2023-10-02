from ib_insync import *
import math

def is_port_in_use(port):
    import socket
    with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
        return s.connect_ex(('localhost', port)) == 0

def getPort():
  import random
  port_id=random.randint(1,9990)
  if (is_port_in_use(port_id)):
    port_id=port_id+1
    print("Port id:",port_id)
  return port_id

def getOptValue(sym,expiration,strike,right,currency,exchange,tradingClass):
  ib = IB()
  try:
    ib.connect('127.0.0.1', 7496, clientId=getPort())    # use this one for TWS (Traders Workstation) acct mgt
  except ConnectionError:
    return None
   
  ##### INDIVIDUAL CONTRACTS
  #contract = Contract(symbol=sym,secType="STK",currency=currency,exchange=exchange)
  contract = Contract(symbol=sym,secType="OPT",lastTradeDateOrContractMonth=expiration,
                      strike=strike,right=right,exchange=exchange,currency=currency,tradingClass=tradingClass) # Simple contract
  print("Contract:",contract)
  if(ib.qualifyContracts(contract)):
    ib.reqMarketDataType(int(2))
    [ticker] = ib.reqTickers(contract)
    #print("\nTicker:",ticker)
    ib.sleep(1)
    value= ticker.marketPrice()
    # greeks=ticker.modelGreeks  ### Another way to retrieve impliedVol
    # print("\nValue:",value)
    # print("\nImpliedVol:",greeks.impliedVol)
  else:
    value = float('nan')  
  
  ib.disconnect()
  return(value)


def find_nearest_number(numbers, target):
    if not numbers:
        raise ValueError("The list of numbers is empty.")
    
    nearest = numbers[0]
    diff = abs(nearest - target)
    
    for number in numbers:
        current_diff = abs(number - target)
        
        if current_diff < diff:
            diff = current_diff
            nearest = number
    
    return nearest

def getOptExchangeList(sym,secType,currency,exchange):
  ib = IB()
  try:
    ib.connect('127.0.0.1', 7496, clientId=getPort())    # use this one for TWS (Traders Workstation) acct mgt
  except ConnectionError:
    return None
  
  underlying= Contract(symbol=sym,secType=secType,
                      exchange=exchange,currency=currency) # Simple contract may be an index or stock
  if (ib.qualifyContracts(underlying)):
    chains = ib.reqSecDefOptParams(sym, '', underlying.secType, underlying.conId)
    ib.sleep(1)
    opt_exchange_list = [c.exchange for c in chains]
  else: opt_exchange_list=float('nan')

  ib.disconnect()

  return opt_exchange_list

def getTradingClassList(sym,secType,currency,exchangeSec,exchangeOpt):
  ib = IB()
  try:
    ib.connect('127.0.0.1', 7496, clientId=getPort())    # use this one for TWS (Traders Workstation) acct mgt
  except ConnectionError:
    return None
    
  underlying= Contract(symbol=sym,secType=secType,
                      exchange=exchangeSec,currency=currency) # Simple contract may be an index or stock
  if (ib.qualifyContracts(underlying)):
    chains = ib.reqSecDefOptParams(sym, '', underlying.secType, underlying.conId)
    ib.sleep(1)
    tradingClass_list = [c.tradingClass for c in chains if c.exchange==exchangeOpt]
  else: tradingClass_list=float('nan')
  
  ib.disconnect()
  return tradingClass_list

def getChain(sym,secType,currency,exchangeSec,exchangeOpt,tradingClass):
  ib = IB()
  try:
    ib.connect('127.0.0.1', 7496, clientId=getPort())    # use this one for TWS (Traders Workstation) acct mgt
  except ConnectionError:
    return None
  
  underlying= Contract(symbol=sym,secType=secType,
                      exchange=exchangeSec,currency=currency) # Simple contract may be an index or stock
  if(ib.qualifyContracts(underlying)):
    chains = ib.reqSecDefOptParams(sym, '', underlying.secType, underlying.conId)
    ib.sleep(1)
    chain=[c for c in chains if c.exchange==exchangeOpt and c.tradingClass==tradingClass]
    ch=chain[0]
  else: ch=float('nan')  
  
  ib.disconnect()
  return ch
  
def getExpDates(sym,secType,currency,exchange,tradingClass):
  ib = IB()
  try:
    ib.connect('127.0.0.1', 7496, clientId=getPort())    # use this one for TWS (Traders Workstation) acct mgt
  except ConnectionError:
    return None
  
  underlying= Contract(symbol=sym,secType=secType,
                      exchange=exchange,currency=currency,tradingClass=tradingClass) # Simple contract may be an index or stock
  if(ib.qualifyContracts(underlying)):
    chains = ib.reqSecDefOptParams(sym, '', underlying.secType, underlying.conId)
    ib.sleep(1)
    print("Chains: ",chains)
    chain = next(c for c in chains if c.exchange == exchange)
    ### chain = next(c for c in chains)
    print("ExpDates:",chain.expirations)
    ch_expirations=chain.expirations
  else: ch_expirations=float('nan')
  
  ib.disconnect()
  return(ch_expirations)


#py$getStrikesfromExpDate(sym="HD",secType="STK",currency="USD", exchange="SMART",expdate='20231006',strikes=strikes_list)


def getStrikesfromExpDate(sym,currency,exchange,tradingClass,expdate,strikes):
  ib = IB()
  try:
    ib.connect('127.0.0.1', 7496, clientId=getPort())    # use this one for TWS (Traders Workstation) acct mgt
  except ConnectionError:
    return None
    
  contracts=[Contract(secType='OPT',symbol=sym,lastTradeDateOrContractMonth=expdate,
              strike=strike_c,right='Put',exchange=exchange,tradingClass=tradingClass) for strike_c in strikes]
  updated_strikes=[]
  print("Contracts:",contracts)
  
  for i in range(len(contracts)):
   #### Iterate over each contract
   if(ib.qualifyContracts(contracts[i])):
      updated_strikes.append(contracts[i].strike)
  print("Strikes:",updated_strikes)
  ib.sleep(1)
  ib.disconnect()
  return(updated_strikes)



def getStockValue(sec,sym,currency,exchange,reqType):
  ib = IB()
  try:
    ib.connect('127.0.0.1', 7496, clientId=getPort())    # use this one for TWS (Traders Workstation) acct mgt
  except ConnectionError:
    return None
  
  ##### INDIVIDUAL CONTRACTS
  contract = Contract(symbol=sym,secType=sec,exchange=exchange,currency=currency) # Simple contract
  print("Contract:",contract)
  if(ib.qualifyContracts(contract)):
    ib.qualifyContracts(contract)
    ib.reqMarketDataType(int(reqType)) ### Request type - Should be 2 or 4
    [ticker] = ib.reqTickers(contract)
    #print("\nTicker:",ticker)
    value= ticker.marketPrice()
  else:
    value = float('nan')  
  print("\nValue:",value)
  
  ib.disconnect()
  return(value)

def getCurrencyPairValue(currency_pair,reqType):
  ib = IB()
  try:
    ib.connect('127.0.0.1', 7496, clientId=getPort())    # use this one for TWS (Traders Workstation) acct mgt
  except ConnectionError:
    return None

  ##### INDIVIDUAL CONTRACTS
  contract = Forex(currency_pair) # Simple contract
  print("Contract:",contract)
  if(ib.qualifyContracts(contract)):
    ib.reqMarketDataType(int(reqType)) ### Request type - Should be 2 or 4
    [ticker] = ib.reqTickers(contract)
    ib.sleep(1)
    print("\nTicker:",ticker)
    value= ticker.marketPrice()
    print("\nValue:",value)
  else: 
    value=float('nan')
  
  ib.disconnect()
  return(value)
  
# def getimpliedVol(sym,secType,date,price,currency,exchange,reqType):
#   print("getimpliedVol: ",sym,secType,date,price,currency,exchange,reqType)
#   ib = IB()
#   ib.connect('127.0.0.1', 7496, clientId=getPort())    # use this one for TWS (Traders Workstation) acct mgt
#   ib.sleep(1)
#   
#   underlying= Contract(symbol=sym,secType=secType,
#                       exchange=exchange,currency=currency) # Simple contract may be an index or stock
#   ib.qualifyContracts(underlying)
#   chains = ib.reqSecDefOptParams(sym, '', underlying.secType, underlying.conId)
#   chain = next(c for c in chains if c.exchange == exchange)
#   print("Chain IV:",chain)
#   
#   tradClass=chain.tradingClass
#   strikes=chain.strikes
#   expirations= [int(num) for num in chain.expirations]
#   strike= find_nearest_number(strikes, price)
#   
#   expiration=find_nearest_number(expirations,date)
#   
#   ##### INDIVIDUAL CONTRACTS
#   contract = Contract(symbol=sym,secType="OPT",tradingClass=tradClass,
#                       lastTradeDateOrContractMonth=str(expiration),
#                       strike=strike,
#                       ### At the money put and call are likely to have very near impliedVol
#                       right="P",
#                       exchange=exchange,currency=currency) # Simple contract
#   print("Contract:",contract)
#   ib.qualifyContracts(contract)
#   ib.reqMarketDataType(int(reqType)) ### Request type - Should be 1 or 2 - 1=Live, 2=Frozen(closed)
#   ticker=ib.reqMktData(contract, genericTickList='106', snapshot=False, regulatorySnapshot=False, mktDataOptions=[])
#   ib.sleep(1)
#   value= ticker.impliedVolatility
#   print("\nImpliedVol:",value)
#   ib.disconnect()
#   return(value)




