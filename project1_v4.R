setwd("C:/Tina Liu/Quant Investment/HW1")
library(reshape2)
#load("final4.RData")  #uncomment it to use the output data directly
################################################################
#read data
stock=read.csv('HW1 Data.csv')

#clean stock data
stock$date=as.Date(stock$date, format="%d%b%Y") #formate date variable
stock=stock[order(stock$stock_ID,stock$date,stock$shrout_adj_factor),] #order data by id,date and split
stock=stock[!duplicated(stock[,1:2]),] #remove record with same stock id and same date
stock=stock[!is.na(stock$return_incl_divs),]#delete record with return = NA

#create flag variables
stock$div_flag=rep(0,dim(stock)[1])
stock$div_port_flag=rep(0,dim(stock)[1])
stock$split_port_flag=rep(0,dim(stock)[1])

date=sort(unique(stock$date))
ticker=unique(stock$stock_ID)

#create flag for dividend
stock$div_flag[stock$return_incl_divs!=stock$return_ex_divs]=1

#create flag for divdend and split portfolio on stock data
for (id in ticker){
  start=min(which((stock$stock_ID==id)==TRUE))
  end=max(which((stock$stock_ID==id)==TRUE))
  
  for (flag in start:end){
    #flag for div_port
    if (any(stock$div_flag[start:end]>0)){
      p=min(which((stock$div_flag[start:end])>0))#find out the first intiated dividend for this stock
      if (flag==start+p-1){
        hold_period=min(12,(end-flag)) #find the holding period for that stock
        if (hold_period>0){
          stock$div_port_flag[(flag+1):(flag+hold_period)]=rep(1,hold_period)
        }
      }
      #if there are 12 no dividend periods before a dividend, we consider it as a new intiation
      else if (!any(stock$div_flag[(flag-12):(flag-1)]>0) & (stock$div_flag[flag]>0)){
        hold_period=min(12,(end-flag)) #find the holding period for that stock
        if (hold_period>0){
          stock$div_port_flag[(flag+1):(flag+hold_period)]=rep(1,hold_period)  #flag the time to include this stock into the porfolio
        }
        
      }      
    }
    #flag for split_port
    if (!is.na(stock$shrout_adj_factor[flag]) & stock$shrout_adj_factor[flag]>0){
      hold_period=min(12,(end-flag)) #find the holding period for that stock
      if (hold_period>0){
        stock$split_port_flag[(flag+1):(flag+hold_period)]=rep(1,hold_period)
      }
      
    }
  }
}

############################################
#format date variable in stock data
stock$date=as.Date(stock$date, format="%d%b%Y")

#create pivot table from date,stock_ID and div_port_flag columns(create div_port)
div_port=stock[,c(1,2,15)]
div_port=dcast(div_port,date~stock_ID,value='div_port_flag',mean)
div_port[sapply(div_port,is.na)] = 0
row.names(div_port)=div_port$date
div_port=div_port[,-1]


#create pivot table from date,stock_ID and split_port_flag columns(create split_port)
split_port=stock[,c(1,2,16)]
split_port=dcast(split_port,date~stock_ID,value='split_port_flag',mean)
split_port[sapply(split_port,is.na)] = 0
row.names(split_port)=split_port$date
split_port=split_port[,-1]


#create combined portfolio
combine_port=div_port+split_port-div_port*split_port


#calculate weight for equally weighted portfolio
n_div=rowSums(div_port)
n_split=rowSums(split_port)
n_combine=rowSums(combine_port) 
div_equal_weight=div_port
split_equal_weight=split_port
combine_equal_weight=combine_port
div_equal_weight[-1,]=div_equal_weight[-1,]/n_div[-1]
split_equal_weight[-1,]=split_equal_weight[-1,]/n_split[-1]
combine_equal_weight[-1,]=combine_equal_weight[-1,]/n_combine[-1]



#create market cap matrix
stock$market_cap=(stock$price)*(stock$shrout)
cap_mat=dcast(stock,date~stock_ID,value='market_cap',mean)
cap_mat[sapply(cap_mat,is.na)] = 0
row.names(cap_mat)=cap_mat$date
cap_mat=cap_mat[,-1]


#calculate weigh for value weighted portfolio(use last month market cap to calculate weight for this month)
div_value_weight=div_port[-1,]*cap_mat[-dim(cap_mat)[1],]
div_value_weight=rbind(div_port[1,],(div_value_weight/rowSums(div_value_weight)))
split_value_weight=split_port[-1,]*cap_mat[-dim(cap_mat)[1],]
split_value_weight=rbind(split_port[1,],(split_value_weight/rowSums(split_value_weight)))
combine_value_weight=combine_port[-1,]*cap_mat[-dim(cap_mat)[1],]
combine_value_weight=rbind(combine_port[1,],(combine_value_weight/rowSums(combine_value_weight)))



#create return matrix
return_mat=stock[,c(1,2,5)]
return_mat=dcast(return_mat,date~stock_ID,value='return_incl_divs',mean)
return_mat[sapply(return_mat,is.na)] = 0
row.names(return_mat)=return_mat$date
return_mat=return_mat[,-1]


#calculate portfolio return
div_equal_return=div_equal_weight*return_mat
split_equal_return=split_equal_weight*return_mat
combine_equal_return=combine_equal_weight*return_mat
div_value_return=div_value_weight*return_mat
split_value_return=split_value_weight*return_mat
combine_value_return=combine_value_weight*return_mat
div_equal=rowSums(div_equal_return)
div_value=rowSums(div_value_return)
split_equal=rowSums(split_equal_return)
split_value=rowSums(split_value_return)
combine_equal=rowSums(combine_equal_return)
combine_value=rowSums(combine_value_return)
port_return=cbind(div_equal,div_value,split_equal,split_value,combine_equal,combine_value)
 

write.csv(port_return, "C:/Tina Liu/Quant Investment/HW1/port_return.csv")
