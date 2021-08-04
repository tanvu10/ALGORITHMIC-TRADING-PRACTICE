# data processing

# importing data
# STEEL GROUP:

import pandas as pd
import math
import numpy as np
import matplotlib.pyplot as plt


def data_processing(dataframe, startdate):
    dataframe['Date'] = [i.split(' ')[0][:] for i in dataframe['Date']]
    dataframe['Date'] = dataframe['Date'].apply(pd.Timestamp)  # very important: convert string to date
    dataframe = dataframe.set_index(['Date'], inplace=False)
    # calculate daily log return:
    log_ret_df = pd.DataFrame()
    for stock in dataframe.columns:
        dataframe[stock + 'lag_1'] = dataframe[stock].shift(1).fillna(1)
        dataframe[stock + 'log_ret'] = dataframe.apply(lambda x: math.log(x[stock] / x[stock + 'lag_1']), axis=1)
        log_ret_df[stock] = dataframe[stock + 'log_ret']

    dataframe = dataframe.iloc[:, :len(log_ret_df.columns)]
    log_ret_df.index = dataframe.index
    log_ret_df = log_ret_df.iloc[1:, :]
    log_ret_df = log_ret_df[startdate:]
    return dataframe, log_ret_df


def high_cor_filter(dataframe):
    high_corr_stocks_pairs = pd.DataFrame(columns=['stock1', 'stock2'])
    corr_matrix = dataframe.corr()
    print(corr_matrix)
    stock_list = dataframe.columns
    # print(stock_list[1])
    pair_num = 0
    for i in range(len(stock_list)):
        # corr of each columns stock:
        stock = stock_list[i]
        corr_list = list(corr_matrix[stock])
        for j in range(len(corr_list)):
            if corr_list[j] > 0.7:
                if j > i:
                    high_corr_stocks_pairs.loc[pair_num, 'stock1'] = stock_list[i]
                    high_corr_stocks_pairs.loc[pair_num, 'stock2'] = stock_list[j]
                    pair_num += 1
    return high_corr_stocks_pairs



if __name__ == "__main__":
    import os
    os.chdir('D:/data-vietquant/pair-trading')
    steel = pd.read_csv('BANK-GROUP.csv')
    price, ln_return = data_processing(steel, '2017-01-01')
    print(ln_return)
    print(price)
    stock_pair = high_cor_filter(price)
    print(stock_pair)
    print(type(price))
    plt.plot(price['MBB'] - price['ACB'])
    plt.plot(price['MBB'])
    plt.plot(price['ACB'])
    plt.show()
    plt.plot(np.cumsum(ln_return.iloc[:, 2]))
    plt.show()
    plt.hist(np.cumsum(ln_return.iloc[:, 1]))
    plt.show()
