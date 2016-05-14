#临时函数
f3 <- function(i){
  if(is.na(i[4])){
    i[4] <- i[3]
  }
  return(i[4])
}


#---------------------线上效果不行！！！！！！！！！！！！！！----------------------------------------------------------------------

#deal day data 
ts_pre_day <- function(i){
  i <- i[i>-1]
  i_ts <- ts(as.vector(unlist(i)))
  i_arima <- auto.arima(i_ts)
  i_pre <- forecast.Arima(i_arima,h=14)
  pre <- sum(i_pre$mean[1:14])
  return(pre)
}
predict_fun_day <- function(data,type,len){ #线下预测集上效果明显比直接用前两周预测效果好
  newdata <- pre_deal(data,type)
  newdata <- newdata[,1:len]
  newdata$cum_day <- apply(newdata,1,how_day)
  data_less_14 <- newdata[newdata$cum_day<14,]
  data_less_28 <- newdata[newdata$cum_day<28 & newdata$cum_day>=14,]
  data_more_28 <- newdata[newdata$cum_day>=28,]
  
  data_less_14$pre <- apply(data_less_14[,-c(1,ncol(data_less_14))],1,function(i) return(mean(i[i>0])*14))
  data_less_28$pre <- apply(data_less_28[,-c(1,ncol(data_less_28))],1,function(i) return(sum(i[(length(i)-6):length(i)])*2))
  data_more_28$pre <- apply(data_more_28[,-c(1,ncol(data_more_28))],1,ts_pre)
  
  x <- data_less_14[,c(1,ncol(data_less_14)-1,ncol(data_less_14))]
  y <- data_less_28[,c(1,ncol(data_less_28)-1,ncol(data_less_28))]
  z <- data_more_28[,c(1,ncol(data_more_28)-1,ncol(data_more_28))]
  
  pred <- rbind(x,y)
  pred <- rbind(pred,z)
  pred$pre <- floor(pred$pre)
  pred$pre[is.na(pred$pre)] <- 0
  pred$pre[pred$pre < 0] <- 0
  
  return(pred)
}

#deal week data
ts_pre_week <- function(i){ #还未测试
  i <- i[i>-1]
  i <- i[-1] #有可能一周未满，去除这种情况
  i_ts <- ts(as.vector(unlist(i)))
  i_arima <- auto.arima(i_ts)
  i_pre <- forecast.Arima(i_arima,h=2)
  pre <- sum(i_pre$mean[1:2])
  return(pre)
}
predict_fun_week <- function(data,data_week,type,len,len_week){
  newdata <- pre_deal(data,"day")
  newdata <- newdata[,1:len]
  data_week <- pre_deal(data_week,type)
  data_week <- data_week[,1:len_week]
  data_week$cum_day <- apply(newdata,1,how_day)
  data_less_14 <- data_week[data_week$cum_day<14,]
  data_less_28 <- data_week[data_week$cum_day<28 & data_week$cum_day>=14,]
  data_more_28 <- data_week[data_week$cum_day>=28,]
  
  data_less_14$pre <- apply(data_less_14[,-c(1,ncol(data_less_14))],1,function(i) return(i[length(i)]*2))
  data_less_28$pre <- apply(data_less_28[,-c(1,ncol(data_less_28))],1,function(i) return(sum(i[(length(i)-1):length(i)])))
  data_more_28$pre <- apply(data_more_28[,-c(1,ncol(data_more_28))],1,ts_pre_week)
  
  x <- data_less_14[,c(1,ncol(data_less_14)-1,ncol(data_less_14))]
  y <- data_less_28[,c(1,ncol(data_less_28)-1,ncol(data_less_28))]
  z <- data_more_28[,c(1,ncol(data_more_28)-1,ncol(data_more_28))]
  
  pred <- rbind(x,y)
  pred <- rbind(pred,z)
  pred$pre <- floor(pred$pre)
  pred$pre[is.na(pred$pre)] <- 0
  pred$pre[pred$pre < 0] <- 0
  
  return(pred)
}

pre_all_ts <- predict_fun_day(item_ts,item_week_ts,"week",431,63)
#----------------------------------------------------------------------------------
#calculate cost
pre_all_week <- predict_fun_week(item_ts,item_week_ts,"week",445,65)
pre_all_week <- data.table(item_id=pre_all_week$item_id,type="all",pre=pre_all_week$pre)
colnames(pre_all_week) <- c("V2","cum_day","pre")
temp <- merge(item_3_2week_ts,pre_3_ts_2,by="V2",all.x=T)
temp$real <- temp[,33]
temp$real[is.na(temp$real)] <- 0
costFun2(data.frame(temp),item_config[item_config$V2==1,])

colnames(pre_1_ts) <- c("V2","cum_day","pre","real")

pre_1_ts <- predict_fun_day(item_1_ts,"day",431)
pre_1_ts <- data.table(item_id=pre_1_ts$item_id,type="all",pre=pre_1_ts$pre)
pre_2_ts <- predict_fun_day(item_2_ts,"day",431)
pre_2_ts <- data.table(item_id=pre_2_ts$item_id,type="all",pre=pre_2_ts$pre)
pre_3_ts <- predict_fun_day(item_3_ts,"day",431)
pre_3_ts <- data.table(item_id=pre_3_ts$item_id,type="all",pre=pre_3_ts$pre)
pre_4_ts <- predict_fun_day(item_4_ts,"day",431)
pre_4_ts <- data.table(item_id=pre_4_ts$item_id,type="all",pre=pre_4_ts$pre)
pre_5_ts <- predict_fun_day(item_5_ts,"day",431)
pre_5_ts <- data.table(item_id=pre_5_ts$item_id,type="all",pre=pre_5_ts$pre)


get_label_data <- function(data_2week,len_2week){
  data_2week <- pre_deal(data_2week,"two_week")
  label <- data_2week[,c(1,len_2week)]
  
  return(label)
}


