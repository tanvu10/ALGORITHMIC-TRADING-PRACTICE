
import pandas as pd
import math
import numpy as np
import matplotlib.pyplot as plt
from rpy2.robjects import FloatVector
from rpy2.robjects.packages import importr
import rpy2.robjects as robjects
from fitter import Fitter, get_common_distributions, get_distributions


copula = importr('copula', lib_loc= 'C:/Users/Tan Vu/Documents/R/win-library/4.1')

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
    steel = pd.read_csv('REAL-ESTATE-GROUP.csv')
    price, ln_return = data_processing(steel, '2017-01-01')
    # print(ln_return)
    # stock_pair = high_cor_filter(price)

    train1 = ln_return[:'2018-01-04']
    test1 = ln_return['2018-01-05':'2018-07-04']

    train2 = ln_return['2017-07-04':'2018-07-04']
    test2 = ln_return['2018-07-05':'2019-01-04']

    train3 = ln_return['2018-01-04':'2019-01-04']
    test3 = ln_return['2019-01-05': '2019-07-05']

    train4 = ln_return['2018-07-04':'2019-07-04']
    test4 = ln_return['2019-07-05':'2020-01-05']

    train5 = ln_return['2019-01-04':'2020-01-04']
    test5 = ln_return['2020-01-05':'2020-07-05']

    train6 = ln_return['2019-07-04': '2020-07-04']
    test6 = ln_return['2020-07-05':'2021-01-05']

    train7 = ln_return['2020-01-04':'2021-01-04']
    test7 = ln_return['2021-01-05': '2021-07-05']

    train8 = ln_return['2020-07-04':'2021-07-04']
    test8 = ln_return['2021-07-05':]


    train1.to_csv('train1.csv')
    test1.to_csv('test1.csv')

    train2.to_csv('train2.csv')
    test2.to_csv('test2.csv')

    train3.to_csv('train3.csv')
    test3.to_csv('test3.csv')

    train4.to_csv('train4.csv')
    test4.to_csv('test4.csv')

    train5.to_csv('train5.csv')
    test5.to_csv('test5.csv')

    train6.to_csv('train6.csv')
    test6.to_csv('test6.csv')

    train7.to_csv('train7.csv')
    test7.to_csv('test7.csv')

    train8.to_csv('train8.csv')
    test8.to_csv('test8.csv')

    r_script = ''' 
    
    library('mistr')
    library('copula')
    library('EnvStats')
    setwd('D:/data-vietquant/pair-trading')
    
    
   fit_GNG<-function(ret)
      {
      ret<-na.omit(ret)
      # Find the break 1 & 2
      num1=quantile(ret,0.05,names=FALSE)
      num2=quantile(ret,0.95,names=FALSE)
      # 2 tails data and normal parts
      x<-ret[(ret>num1)&(ret<num2)]
      x1<-ret[ret<=num1]; x2<-ret[ret>=num2]
      # Find the shape parameter
      s1<-as.numeric(epareto(-x1,method='mle')$parameters[2])
      s2<-as.numeric(epareto(x2,method='mle')$parameters[2])
      # Fitting the dataset
      fit_ret<-GNG_fit(ret, start = c(break1 = num1, break2 =num2, 
                                      mean = mean(x) , sd = sd(x) ,
                                      shape1 = s1, shape2 = s2))
      cdf<-function(x){p(distribution(fit_ret),x)}
      icdf<-function(x){q(distribution(fit_ret),x)}
      return (list(cdf,icdf))
      }
      
    COPULA<- function(ret,list_cdf)
    {
      n<- ncol(ret); m<-nrow(ret); U<-matrix(0,m,n)
      # Turn return to Uniform data
      for (i in 1:n)
        {
            U[,i]<-list_cdf[[i]](ret[,i])
        }
      
      # Gaussian Copula
      fit_Gaussian  <-fitCopula(normalCopula(dim=n), U, method = 'ml')
      Gaussian_model<-normalCopula(coef(fit_Gaussian) ,dim=n)
     
      return(Gaussian_model) 
    }



    #FIND CONDITIONAL PROBABILITY
    con_proba<-function(Copula_model,u)
    {
      # u is a vector of all given value at a point in time. u= (u1,u2,..,ud)
      # This function provide the output of a vector containing con-proba for each stock
      n<-10000; d=length(u); con_proba<-numeric(d)
      U<-matrix(0,n,d); for (i in 1:d){U[,i]<-runif(n)}
      index<-1:d
      for (i in index)
        { 
            sim<-U ; 
            for (j in index[index!=i]) 
                {
                    sim[,j]<-u[j]
                }
            U_con <- cCopula(sim, copula = Copula_model, inverse = TRUE)
            con_proba[i]<-sum(U_con[,i]<=u[i])/n
        }
      return(con_proba)
    }



    #conditional probability for train and test
    train_test_P<-function(train_path,test_path)
    {
      # Train dataset
      # ret<-read.csv(train_path)
      train_path <-subset(train_path ,select= -c(Date)); train_path <-as.matrix(train_path); m1<-nrow(train_path)
      
      # Test dataset
      # test_ret<-read.csv(test_path)
    test_path <-subset(test_path,select= -c(Date)); test_path<-as.matrix(test_path); m2<-nrow(test_path)
    
    # Find the CDF functions
    list_cdf<-list() ; list_icdf<-list(); n<-ncol(train_path)
    for (i in 1:n)
    {func_list<-fit_GNG(train_path[,i])
      list_cdf[[i]]<-func_list[[1]]}
    
    # Find Copula model
    ret_COPULA<-COPULA(train_path,list_cdf)
    
    # Turn return to Uniform data
    m<-nrow(test_path); U1<-matrix(0,m1,n); U2<-matrix(0,m2,n)
      for (i in 1:n){
        U1[,i]<-list_cdf[[i]](train_path[,i])
        U2[,i]<-list_cdf[[i]](test_path[,i])}
    
    # Turn to conditional probability 
    P1<-matrix(0,m1,n); P2<-matrix(0,m2,n)
    for (i in 1:m1)
    {P1[i,]<-con_proba(ret_COPULA,U1[i,])}
    for (i in 1:m2)
    {P2[i,]<-con_proba(ret_COPULA,U2[i,])}
    
    dimnames(P1)[[2]]=dimnames(P2)[[2]]<-dimnames(test_path)[[2]]
    return(list(P1,P2))
    }

      
    setwd('D:/data-vietquant/pair-trading')
    train1 <- read.csv('train1.csv')
    test1 = read.csv('test1.csv')
    
    train2 <- read.csv('train2.csv')
    test2 = read.csv('test2.csv')

    train3 <- read.csv('train3.csv')
    test3 = read.csv('test3.csv')

    train4 <- read.csv('train4.csv')
    test4 = read.csv('test4.csv')

    train5 <- read.csv('train5.csv')
    test5 = read.csv('test5.csv')

    train6 <- read.csv('train6.csv')
    test6 = read.csv('test6.csv')

    train7 <- read.csv('train7.csv')
    test7 = read.csv('test7.csv')

    train8 <- read.csv('train8.csv')
    test8 = read.csv('test8.csv')
    
            

    A1 = train_test_P(train1, test1)
    conditional_train1 = A1[[1]]
    conditional_test1 = A1[[2]]

    A2 = train_test_P(train2, test2)
    conditional_train2 = A2[[1]]
    conditional_test2 = A2[[2]]

    A3 = train_test_P(train3, test3)
    conditional_train3 = A3[[1]]
    conditional_test3 = A3[[2]]

    A4 = train_test_P(train4, test4)
    conditional_train4 = A4[[1]]
    conditional_test4 = A4[[2]]

    A5 = train_test_P(train5, test5)
    conditional_train5 = A5[[1]]
    conditional_test5 = A5[[2]]
    
    A6 = train_test_P(train6, test6)
    conditional_train6 = A6[[1]]
    conditional_test6 = A6[[2]]
    
    A7 = train_test_P(train7, test7)
    conditional_train7 = A7[[1]]
    conditional_test7 = A7[[2]]

    A8 = train_test_P(train8, test8)
    conditional_train8 = A8[[1]]
    conditional_test8 = A8[[2]]
    
    
    '''

    robjects.r(r_script)
    con_proba_test1 = robjects.r('conditional_test1')
    con_proba_train1 = robjects.r('conditional_train1')
    print(con_proba_test1)
    print(test1)
    con_proba_test2 = robjects.r('conditional_test2')
    con_proba_train2 = robjects.r('conditional_train2')

    con_proba_test3 = robjects.r('conditional_test3')
    con_proba_train3 = robjects.r('conditional_train3')

    con_proba_test4 = robjects.r('conditional_test4')
    con_proba_train4 = robjects.r('conditional_train4')

    con_proba_test5 = robjects.r('conditional_test5')
    con_proba_train5 = robjects.r('conditional_train5')

    con_proba_test6 = robjects.r('conditional_test6')
    con_proba_train6 = robjects.r('conditional_train6')

    con_proba_test7 = robjects.r('conditional_test7')
    con_proba_train7 = robjects.r('conditional_train7')

    con_proba_test8 = robjects.r('conditional_test8')
    con_proba_train8 = robjects.r('conditional_train8')
    #conditional train
    con_proba_train1 = np.array(con_proba_train1)
    con_proba_train1 = pd.DataFrame(con_proba_train1, columns =steel.columns[1:])

    con_proba_train2 = np.array(con_proba_train2)
    con_proba_train2 = pd.DataFrame(con_proba_train2, columns =steel.columns[1:])

    con_proba_train3 = np.array(con_proba_train3)
    con_proba_train3 = pd.DataFrame(con_proba_train3, columns =steel.columns[1:])

    con_proba_train4 = np.array(con_proba_train4)
    con_proba_train4 = pd.DataFrame(con_proba_train4, columns =steel.columns[1:])

    con_proba_train5 = np.array(con_proba_train5)
    con_proba_train5 = pd.DataFrame(con_proba_train5, columns =steel.columns[1:])

    con_proba_train6 = np.array(con_proba_train6)
    con_proba_train6 = pd.DataFrame(con_proba_train6, columns =steel.columns[1:])

    con_proba_train7 = np.array(con_proba_train7)
    con_proba_train7 = pd.DataFrame(con_proba_train7, columns =steel.columns[1:])

    con_proba_train8 = np.array(con_proba_train8)
    con_proba_train8 = pd.DataFrame(con_proba_train8, columns =steel.columns[1:])

    #conditional test
    con_proba_test1 = np.array(con_proba_test1)
    con_proba_test1 = pd.DataFrame(con_proba_test1, columns =steel.columns[1:])

    con_proba_test2 = np.array(con_proba_test2)
    con_proba_test2 = pd.DataFrame(con_proba_test2, columns =steel.columns[1:])

    con_proba_test3 = np.array(con_proba_test3)
    con_proba_test3 = pd.DataFrame(con_proba_test3, columns =steel.columns[1:])

    con_proba_test4 = np.array(con_proba_test4)
    con_proba_test4 = pd.DataFrame(con_proba_test4, columns =steel.columns[1:])

    con_proba_test5 = np.array(con_proba_test5)
    con_proba_test5 = pd.DataFrame(con_proba_test5, columns =steel.columns[1:])

    con_proba_test6 = np.array(con_proba_test6)
    con_proba_test6 = pd.DataFrame(con_proba_test6, columns =steel.columns[1:])

    con_proba_test7 = np.array(con_proba_test7)
    con_proba_test7 = pd.DataFrame(con_proba_test7, columns =steel.columns[1:])

    con_proba_test8 = np.array(con_proba_test8)
    con_proba_test8 = pd.DataFrame(con_proba_test8, columns =steel.columns[1:])



    def weight_shifting(dataframe):
        stock = dataframe.columns
        ws = pd.DataFrame()
        for stock in stock:
            dataframe[stock+ 'lag_1'] = dataframe[stock].shift(1).fillna(1)
            dataframe[stock + 'ws'] = dataframe.apply(lambda x: (x[stock] - x[stock + 'lag_1']), axis=1)
            ws[stock] = dataframe[stock + 'ws']
            ws[stock][0] = 0
            for x in range(ws[stock].shape[0]):
                if ws[stock][x]== 0 and x != 0:
                    ws[stock][x] = ws[stock][x-1]
        ws.iloc[0, :] = 0
        return ws

    #algorithm 0: signal based on log_return
    def dynamic0(con_test):
        con_test.replace(0, 0.0001, inplace = True)
        count = 0
        dynamic_weight = []
        ini = [1/con_test.shape[1] for i in range(con_test.shape[1])]
        dynamic_weight.append(ini)
        # signal base on return
        for i in range(con_test.shape[0]):
            a = [i >= 0.95 for i in con_test.iloc[i,:]]
            b = [i <= 0.05 for i in con_test.iloc[i,:]]

            if np.sum(a) >=1 or np.sum(b) >= 1:
                param =np.sum(np.fromiter((1/j for j in con_test.iloc[i,:]),float))
                param = 1/param
                weight = [param/j for j in con_test.iloc[i,:]]
                dynamic_weight.append(weight)
                count +=1
            else:
                if i == 0:
                    dynamic_weight.append(ini)
                else:
                    dynamic_weight.append(dynamic_weight[i])
        dynamic_weight = pd.DataFrame(dynamic_weight, columns=con_test.columns)
        print('algo1')
        print(count)
        print((dynamic_weight))
        return dynamic_weight

    #algorithm 1: cumulative mispricing index period updating
    def dynamic1(con_train,con_test):
        con_test.replace(0, 0.0001,inplace = True)
        #processing with train set:
        #take std of cum_MI of train
        train_std_list = []
        stock = con_test.columns
        ut = 50
        #columns check
        for i in range(con_train.shape[1]):
            con_train1 = con_train
            count = 0
            #row check
            for j in range(int(con_test.shape[0]/ut)+1):
                try:
                    if j == 0:
                        con_train2 = con_train1.iloc[ut*j:,:].append(con_test.iloc[:(ut * j), :], ignore_index=True)
                        minus = con_train2.iloc[:, i] - 0.5
                        mean = 0
                        stdi = np.std(np.cumsum(minus))
                        bound = [mean - 2*stdi, mean + 2*stdi]
                        train_std_list.append(bound)
                        count +=1
                        # plt.plot(np.cumsum(minus))
                        # plt.show()
                    else:
                        con_train2 = con_train1.iloc[:,:].append(con_test.iloc[:(ut*j),:],ignore_index=True)
                        minus  = con_train2.iloc[:,i] - 0.5
                        minus = np.cumsum(minus)
                        mean = np.mean(minus[ut*j:])
                        stdi = np.std(minus[ut*j:])
                        bound = [mean - 2*stdi, mean+ 2*stdi]
                        train_std_list.append(bound)
                        count+=1
                        # plt.plot(np.cumsum(minus))
                        # plt.show()
                except:
                    con_train2 = con_train1.iloc[:,:].append(con_test, ignore_index= True)
                    minus = con_train2.iloc[:, i] - 0.5
                    minus = np.cumsum(minus)
                    mean = np.mean(minus[ut*j:])
                    stdi = np.std(minus[ut*j:])
                    bound = [mean -2*stdi, mean+2*stdi]
                    train_std_list.append(bound)
                    count+=1
                    # plt.plot(np.cumsum(minus))
                    # plt.show()
        # print(train_std_list)
        # print(len(train_std_list))
        # print(train_std_list[0][1])



        #take cum_MI of test
        cum_MI = pd.DataFrame()
        stock = con_test.columns
        for i in range(con_test.shape[1]):
            MI = con_test.iloc[:,i] - 0.5
            MI = np.cumsum(MI)
            cum_MI[stock[i] + 'cum_MI'] = MI
        # print(cum_MI)

        #algorithm dynamic weight base on cum_MI
        weight_init = [1/con_test.shape[1] for i in range(con_test.shape[1])]
        update_times = 0
        dynamic_weight = []
        dynamic_weight.append(weight_init)
        for i in range(con_test.shape[0]):
            int_part = i//ut
            sub_part = con_test.shape[0]//ut
            a = [cum_MI.iloc[i,j] >= (train_std_list[int_part+sub_part*j][1]) for j in range(con_test.shape[1])]
            # print(a)
            b = [cum_MI.iloc[i,j] <= (train_std_list[int_part+sub_part*j][0]) for j in range(con_test.shape[1])]
            # print(b)
            #if have signal
            if np.sum(a) >=1 or np.sum(b) >=1:
                param = np.sum(np.fromiter((1 / j for j in con_test.iloc[i, :]), float))
                param = 1 / param
                weight = [param / j for j in con_test.iloc[i, :]]
                dynamic_weight.append(weight)
                update_times +=1
            #if not have signal
            else:
                if i == 0:
                    dynamic_weight.append(weight_init)
                else:
                    dynamic_weight.append(dynamic_weight[i])

        dynamic_weight = pd.DataFrame(dynamic_weight, columns= con_test.columns)
        print('algo2')
        print(update_times)
        print(dynamic_weight)
        return dynamic_weight

    #algorithm 2: normal cumulative mispricing index
    def dynamic2(con_train, con_test):
        con_test.replace(0, 0.0001,inplace = True)
        count = 0
        std_list =[]
        #train processing:
        for i in range(con_train.shape[1]):
            minus = con_train.iloc[:,i] -0.5
            minus = np.cumsum(minus)
            std = np.std(minus)
            std_list.append(std)

        MI_df = pd.DataFrame()
        stock = con_test.columns
        #test processing:
        for i in range(con_test.shape[1]):
            MI = con_test.iloc[:,i] - 0.5
            MI = np.cumsum(MI)
            MI_df[stock[i]+'MI'] = MI

        #condition:
        weight_list =[]
        ini = [1/con_test.shape[1] for i in range(con_test.shape[1])]
        weight_list.append(ini)
        for i in range(con_test.shape[0]):
            a = [MI_df.iloc[i,j] >= 2*std_list[j] for j in range(con_test.shape[1])]
            b = [MI_df.iloc[i,j] <= -2*std_list[j] for j in range(con_test.shape[1])]
            if np.sum(a) >= 1 or np.sum(b) >=1:
                param = np.sum(np.fromiter((1 / j for j in con_test.iloc[i, :]), float))
                param = 1 / param
                weight = [param / j for j in con_test.iloc[i, :]]
                weight_list.append(weight)
                count+=1
            else:
                if i == 0:
                    weight_list.append(ini)
                else:
                    weight_list.append(weight_list[i])

        weight_list = pd.DataFrame(weight_list, columns=con_test.columns)
        print('algo3')
        print(count)
        print(weight_list)
        return weight_list

    #calculating return:
    def return_calculator(weight, dataframe_return):
        return_vect =[]
        for i in range(dataframe_return.shape[0]):
            a = np.sum(np.fromiter((weight.iloc[i, j] * dataframe_return.iloc[i, j] for j in range(dataframe_return.shape[1])), float))
            return_vect.append(a)
        return_vect = pd.DataFrame(return_vect)
        return return_vect

    def whole_ret_calculator(w1, test1, w2, test2, w3,test3, w4, test4, w5,test5, w6,test6, w7,test7, w8, test8):
        p1 = return_calculator(w1, test1)
        p2 = return_calculator(w2, test2)
        p3 = return_calculator(w3, test3)
        p4 = return_calculator(w4, test4)
        p5 = return_calculator(w5, test5)
        p6 = return_calculator(w6, test6)
        p7 = return_calculator(w7, test7)
        p8 = return_calculator(w8, test8)
        whole = pd.concat([p1,p2,p3,p4,p5,p6,p7,p8], axis = 0, ignore_index=True)
        sharpe = np.mean(whole)/np.std(whole)
        print(sharpe)
        whole = np.cumsum(whole)
        plt.plot(whole)

        return whole


    #weight of algorithm
    w1_algo1 = dynamic0(con_proba_test1)
    w1_algo2 = dynamic1(con_proba_train1, con_proba_test1)
    w1_algo3 = dynamic2(con_proba_train1, con_proba_test1)
    w1_ave = np.full((test1.shape[0], test1.shape[1]), 1/len(test1.columns))
    w1_ave = pd.DataFrame(w1_ave)

    w2_algo1 = dynamic0(con_proba_test2)
    w2_algo2 = dynamic1(con_proba_train2, con_proba_test2)
    w2_algo3 = dynamic2(con_proba_train2, con_proba_test2)
    w2_ave = np.full((test2.shape[0], test2.shape[1]), 1 / test2.shape[1])
    w2_ave = pd.DataFrame(w2_ave)

    w3_algo1 = dynamic0(con_proba_test3)
    w3_algo2 = dynamic1(con_proba_train3, con_proba_test3)
    w3_algo3 = dynamic2(con_proba_train3, con_proba_test3)
    w3_ave = np.full((test3.shape[0], test3.shape[1]), 1 / test3.shape[1])
    w3_ave = pd.DataFrame(w3_ave)

    w4_algo1 = dynamic0(con_proba_test4)
    w4_algo2 = dynamic1(con_proba_train4, con_proba_test4)
    w4_algo3 = dynamic2(con_proba_train4, con_proba_test4)
    w4_ave = np.full((test4.shape[0], test4.shape[1]), 1 / test4.shape[1])
    w4_ave = pd.DataFrame(w4_ave)

    w5_algo1 = dynamic0(con_proba_test5)
    w5_algo2 = dynamic1(con_proba_train5, con_proba_test5)
    w5_algo3 = dynamic2(con_proba_train5, con_proba_test5)
    w5_ave = np.full((test5.shape[0], test5.shape[1]), 1 / test5.shape[1])
    w5_ave = pd.DataFrame(w5_ave)

    w6_algo1 = dynamic0(con_proba_test6)
    w6_algo2 = dynamic1(con_proba_train6, con_proba_test6)
    w6_algo3 = dynamic2(con_proba_train6, con_proba_test6)
    w6_ave = np.full((test6.shape[0], test6.shape[1]), 1 / test6.shape[1])
    w6_ave = pd.DataFrame(w6_ave)

    w7_algo1 = dynamic0(con_proba_test7)
    w7_algo2 = dynamic1(con_proba_train7, con_proba_test7)
    w7_algo3 = dynamic2(con_proba_train7, con_proba_test7)
    w7_ave = np.full((test7.shape[0], test7.shape[1]), 1 / test7.shape[1])
    w7_ave = pd.DataFrame(w7_ave)

    w8_algo1 = dynamic0(con_proba_test8)
    w8_algo2 = dynamic1(con_proba_train8, con_proba_test8)
    w8_algo3 = dynamic2(con_proba_train8, con_proba_test8)
    w8_ave = np.full((test8.shape[0], test8.shape[1]), 1 / test8.shape[1])
    w8_ave = pd.DataFrame(w8_ave)




    w1_algo1_ret = return_calculator(w1_algo1,test1)
    w1_algo2_ret = return_calculator(w1_algo2,test1)
    w1_algo3_ret = return_calculator(w1_algo3,test1)
    w1_ave_ret = return_calculator(w1_ave,test1)


    w2_algo1_ret = return_calculator(w2_algo1,test2)
    w2_algo2_ret = return_calculator(w2_algo2,test2)
    w2_algo3_ret = return_calculator(w2_algo3,test2)
    w2_ave_ret = return_calculator(w2_ave,test2)


    w3_algo1_ret = return_calculator(w3_algo1,test3)
    w3_algo2_ret = return_calculator(w3_algo2,test3)
    w3_algo3_ret = return_calculator(w3_algo3,test3)
    w3_ave_ret = return_calculator(w3_ave,test3)


    w4_algo1_ret = return_calculator(w4_algo1,test4)
    w4_algo2_ret = return_calculator(w4_algo2,test4)
    w4_algo3_ret = return_calculator(w4_algo3,test4)
    w4_ave_ret = return_calculator(w4_ave,test4)


    w5_algo1_ret = return_calculator(w5_algo1,test5)
    w5_algo2_ret = return_calculator(w5_algo2,test5)
    w5_algo3_ret = return_calculator(w5_algo3,test5)
    w5_ave_ret = return_calculator(w5_ave,test5)


    w6_algo1_ret = return_calculator(w6_algo1,test6)
    w6_algo2_ret = return_calculator(w6_algo2,test6)
    w6_algo3_ret = return_calculator(w6_algo3,test6)
    w6_ave_ret = return_calculator(w6_ave,test6)


    w7_algo1_ret = return_calculator(w7_algo1,test7)
    w7_algo2_ret = return_calculator(w7_algo2,test7)
    w7_algo3_ret = return_calculator(w7_algo3,test7)
    w7_ave_ret = return_calculator(w7_ave,test7)


    w8_algo1_ret = return_calculator(w8_algo1,test8)
    w8_algo2_ret = return_calculator(w8_algo2,test8)
    w8_algo3_ret = return_calculator(w8_algo3,test8)
    w8_ave_ret = return_calculator(w8_ave,test8)


    plt.plot(np.cumsum(w1_algo1_ret))
    plt.plot(np.cumsum(w1_algo2_ret))
    plt.plot(np.cumsum(w1_algo3_ret))
    plt.plot(np.cumsum(w1_ave_ret))
    plt.show()


    plt.plot(np.cumsum(w2_algo1_ret))
    plt.plot(np.cumsum(w2_algo2_ret))
    plt.plot(np.cumsum(w2_algo3_ret))
    plt.plot(np.cumsum(w2_ave_ret))
    plt.show()

    plt.plot(np.cumsum(w3_algo1_ret))
    plt.plot(np.cumsum(w3_algo2_ret))
    plt.plot(np.cumsum(w3_algo3_ret))
    plt.plot(np.cumsum(w3_ave_ret))
    plt.show()

    plt.plot(np.cumsum(w4_algo1_ret))
    plt.plot(np.cumsum(w4_algo2_ret))
    plt.plot(np.cumsum(w4_algo3_ret))
    plt.plot(np.cumsum(w4_ave_ret))
    plt.show()

    plt.plot(np.cumsum(w5_algo1_ret))
    plt.plot(np.cumsum(w5_algo2_ret))
    plt.plot(np.cumsum(w5_algo3_ret))
    plt.plot(np.cumsum(w5_ave_ret))
    plt.show()

    plt.plot(np.cumsum(w6_algo1_ret))
    plt.plot(np.cumsum(w6_algo2_ret))
    plt.plot(np.cumsum(w6_algo3_ret))
    plt.plot(np.cumsum(w6_ave_ret))
    plt.show()

    plt.plot(np.cumsum(w7_algo1_ret))
    plt.plot(np.cumsum(w7_algo2_ret))
    plt.plot(np.cumsum(w7_algo3_ret))
    plt.plot(np.cumsum(w7_ave_ret))
    plt.show()

    plt.plot(np.cumsum(w8_algo1_ret))
    plt.plot(np.cumsum(w8_algo2_ret))
    plt.plot(np.cumsum(w8_algo3_ret))
    plt.plot(np.cumsum(w8_ave_ret))
    plt.show()

    whole_ret_calculator(w1_algo1, test1, w2_algo1, test2, w3_algo1, test3,
                         w4_algo1, test4, w5_algo1, test5, w6_algo1, test6, w7_algo1, test7, w8_algo1, test8)
    whole_ret_calculator(w1_algo2, test1, w2_algo2, test2, w3_algo2, test3,
                         w4_algo2, test4, w5_algo2, test5, w6_algo2, test6, w7_algo2, test7, w8_algo2, test8)
    whole_ret_calculator(w1_algo3, test1, w2_algo3, test2, w3_algo3, test3,
                         w4_algo3, test4, w5_algo3, test5, w6_algo3, test6, w7_algo3, test7, w8_algo3, test8)
    whole_ret_calculator(w1_ave, test1, w2_ave, test2, w3_ave, test3,
                         w4_ave, test4, w5_ave, test5, w6_ave, test6, w7_ave, test7, w8_ave, test8)
    plt.show()