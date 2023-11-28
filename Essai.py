import locale
import datetime
import pandas as pd

locale.setlocale(locale.LC_ALL, '')

string=datetime.datetime.now().strftime('%d %b %Y %H:%M')

du=pd.DataFrame([[string,"ESSAI",100.3]],columns=["datetime","sym","price"])
du.to_csv("C:/Users/aldoh/Documents/Global/prices.csv",header=False, index=False, mode='a', sep=';')
