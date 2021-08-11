import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
# if __name__ == "__main__":
import os
os.chdir('D:/data-vietquant/daily')

#CTG
#HPG
#AAA
#HT1
#BID
#VIC
#NVL
AAA = pd.read_csv("HT1.csv")
AAA['Date'] = [i.split(' ')[0][:] for i in AAA['Date']]
AAA['Date'] = AAA['Date'].apply(pd.Timestamp) #very important: convert string to date

AAA = AAA.set_index(['Date'], inplace = False)
start_date = '2017-01-01'
end_date = '2021-07-30'

AAA_data = AAA[start_date:]
print(AAA_data[10:20])

Lows = AAA_data['Low']
Highs = AAA_data['High']

fig = plt.figure()
ax1 = fig.add_subplot(111,  ylabel = "AAA prices")
Highs.plot(ax=ax1,  color = 'g', lw = 2)
Lows.plot(ax= ax1, color = 'c', lw = 2)
plt.hlines(Highs.head(50).max(),Lows.index.values[0],Lows.index.values[-1]
,linewidth=2, color='g')
plt.hlines(Lows.head(50).min(),Lows.index.values[0],Lows.index.values[-1],
linewidth=2, color='r')
plt.axvline(linewidth=2,color='b',x=Lows.index.values[50],linestyle=':')
plt.show()

AAA_signal = pd.DataFrame(index = AAA.index)
AAA_signal["Price"] = AAA["Close"]


#simple strategy:
#create dataframe:
def trading_support_resistance(data, bin_width= 20):
    data['sup_tolerance'] = pd.Series(np.zeros(len(data)))
    data['res_tolerance'] = pd.Series(np.zeros(len(data)))
    #number of times reach support:
    data['sup_count'] = pd.Series(np.zeros(len(data)))
    #number of times reach resistance:
    data['res_count'] = pd.Series(np.zeros(len(data)))
    #support in 1 period:
    data['sup'] = pd.Series(np.zeros(len(data)))
    #resistance in 1 period:
    data['res'] = pd.Series(np.zeros(len(data)))
    #signal to buy and purchase:
    data['signal'] = pd.Series(np.zeros(len(data)))
    ###
    ini_support = 0
    ini_resistance = 0
    #look back period
    for x in range((bin_width-1)+bin_width, len(data)):
        data_period = data[(x-bin_width):(x+1)]
        #in each sub period:
        support_level = np.min(data_period['Price'])
        resistance_level = np.max(data_period['Price'])
        range_level = resistance_level - support_level
        data['res'][x] = resistance_level
        data['sup'][x] = support_level
        data['sup_tolerance'][x] = support_level + 0.2*range_level
        data['res_tolerance'][x] = resistance_level - 0.2*range_level

        #run the algorithm:
        #counting date in upper res zone
        if data['Price'][x] >= data['res_tolerance'][x] and \
            data['Price'][x] <= data['res'][x]:
            ini_resistance +=1
            data['res_count'] = ini_resistance
        #counting data in lower sup zone
        elif data['Price'][x] <= data['sup_tolerance'][x] and \
            data['Price'][x] >= data['sup'][x]:
            ini_support +=1
            data['sup_count'] = ini_support
        else:
            ini_support = 0
            ini_resistance = 0
        #if in the resistant zone over 2 days, have signal:
        if ini_resistance > 2:
            data['signal'][x] = 1
        elif ini_support > 2:
            data['signal'][x] = 0
        else:
            data['signal'][x] = data['signal'][x-1]

    #-1 means sell, +1 means buy
    data['position'] = data['signal'].diff()

trading_support_resistance(AAA_signal)
print(AAA_signal.tail(50))


#plot signal:

fig = plt.figure()
ax1 = fig.add_subplot(111,  ylabel = "AAA price")
AAA_signal['sup'].plot(lw=2, color='g')
AAA_signal['res'].plot(lw=2 , color = 'b')
AAA_signal['Price'].plot(lw=2 , color = 'r')
# print(AAA_signal['position'])
ax1.plot(AAA_signal.loc[AAA_signal.position == 1].index\
        ,AAA_signal.Price[AAA_signal.position == 1], '^', markersize = 7, label= 'buy' )

ax1.plot(AAA_signal.loc[AAA_signal.position == -1].index\
        ,AAA_signal.Price[AAA_signal.position == -1], 'v', markersize = 7, label= 'sell' )

plt.legend()
plt.show()


#building FAST AND SLOW EMA
#FAST EMA:
fast_num = 30
mu_fast = 2/(fast_num+1)
EMA_fast_list = []
old_fast_EMA = 0
#slow EMA
slow_num  = 50
mu_slow = 2/(slow_num+1)
EMA_slow_list = []
old_slow_EMA = 0


#MACD: EMA fast - EMA slow:
MACD_list = []

#EMA_MACD_fast:
EMA_fMACD_num = 10
mu_fMACD = 2 / (EMA_fMACD_num + 1)
EMA_fMACD_list = []
EMA_fMACD_value = 0

#EMA_MACD_slow:
EMA_sMACD_num = 20
mu_sMACD = 2 / (EMA_sMACD_num +1)
EMA_sMACD_list = []
EMA_sMACD_value = 0


#MACD_hist:
MACD_hist =[]


#MACD_lv2
MACD_lv2_hist =[]


AAA_price = pd.DataFrame(index = AAA_data.index)
AAA_price['Price'] = AAA_data['Close']


for price in AAA_price['Price']:
    #fast EMA:
    if old_fast_EMA == 0:
        old_fast_EMA = price
        EMA_fast_list.append(old_fast_EMA)
    else:
        old_fast_EMA = (price- old_fast_EMA)*mu_fast + old_fast_EMA
        EMA_fast_list.append(old_fast_EMA)
    #slow EMA
    if old_slow_EMA == 0:
        old_slow_EMA = price
        EMA_slow_list.append(old_slow_EMA)
    else:
        old_slow_EMA = (price - old_slow_EMA)*mu_slow +old_slow_EMA
        EMA_slow_list.append(old_slow_EMA)
    #MACD:
    MACD = old_fast_EMA - old_slow_EMA #MACD
    MACD_list.append(MACD)

    #EMA_sMACD:
    if EMA_sMACD_value == 0:
        EMA_sMACD_value = MACD
        EMA_sMACD_list.append(EMA_sMACD_value)
    else:
        EMA_sMACD_value = (MACD - EMA_sMACD_value) * mu_sMACD + EMA_sMACD_value
        EMA_sMACD_list.append(EMA_sMACD_value)

    #MACD histogram: MACD - EMA_MACD:
    hist  = MACD - EMA_sMACD_value
    MACD_hist.append(hist)


    #EMA_fMACD:
    if EMA_fMACD_value == 0:
        EMA_fMACD_value = hist
        EMA_fMACD_list.append(EMA_fMACD_value)
    else:
        EMA_fMACD_value =(hist - EMA_fMACD_value) * mu_fMACD + EMA_fMACD_value
        EMA_fMACD_list.append(EMA_fMACD_value)

    MACD_l2 = hist - EMA_fMACD_value
    MACD_lv2_hist.append(MACD_l2)


AAA_price['FEMA'] = EMA_fast_list
AAA_price['SEMA'] = EMA_slow_list
AAA_price['MACD'] = MACD_list
AAA_price['EMA_fMACD'] = EMA_fMACD_list
AAA_price['EMA_sMACD'] = EMA_sMACD_list
AAA_price['MACD_hist'] = MACD_hist
AAA_price['MACD_lv2_hist'] = MACD_lv2_hist
print(AAA_price)



def EMA_MACD_algorithm(data):
    data['long_signal'] = np.zeros(len(data))
  # data['long_signal']= np.zeros(len(data))
    for x in range(len(data)):
        if data['MACD'][x] > 0:
            data['long_signal'][x] = 1
        elif (data['MACD'][x] - 100) < 0:
            data['long_signal'][x] = 0
        else:
            data['long_signal'][x] = data['long_signal'][x-1]
    data['long_position'] = data['long_signal'].diff()

    data['short_signal'] = np.zeros(len(data))
    for x in range(len(data)):
        if data['MACD_lv2_hist'][x] > 0:
            data['short_signal'][x] = 1
        elif data['MACD_lv2_hist'][x] < 0:
            data['short_signal'][x] = 0
        else:
            data['short_signal'][x] = data['short_signal'][x - 1]
    data['short_position'] = data['short_signal'].diff()


EMA_MACD_algorithm(AAA_price)
print(AAA_price['short_position'][-50:])
# print(AAA_price['2021-01-20':'2021-01-25'])
fig = plt.figure()
ax1 = fig.add_subplot(311, ylabel = 'AAA prices')
AAA_price['Price'].plot(ax = ax1,lw=2, color= 'g', legend = True)
AAA_price['FEMA'].plot(ax = ax1,lw=2 , color= 'b', legend= True)
AAA_price['SEMA'].plot(ax = ax1,lw= 2, color = 'r', legend = True)
ax1.plot(AAA_price.loc[(AAA_price.short_position == 1)].index\
        ,AAA_price.Price[(AAA_price.short_position == 1)], '^', markersize = 7, label= 'buy' )
ax1.plot(AAA_price.loc[AAA_price.short_position == -1].index\
        ,AAA_price.Price[AAA_price.short_position == -1], 'v', markersize = 7, label= 'sell' )
ax2 = fig.add_subplot(312, ylabel= 'Absolute price oscillator')
# AAA_price['EMA_fMACD'].plot(ax=ax2, lw=2 , color = 'g', legend = True)
AAA_price['EMA_sMACD'].plot(ax=ax2, lw=2 , color = 'b', legend = True)
AAA_price['MACD'].plot(ax = ax2, lw= 2 , color= 'black', legend= True)
plt.axhline( linewidth=2,color='b',y= 0,linestyle=':')
ax3 = fig.add_subplot(313, ylabel= 'MACD hist')
AAA_price['MACD_lv2_hist'].plot(ax = ax3, lw = 2, color = 'r', legend = True)
plt.axhline( linewidth=2,color='b',y= 0,linestyle=':')
plt.show()
