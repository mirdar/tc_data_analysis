item_summary <- function(i,flag,bit,len){
  if(flag == 1){
    z <- rev(sort(as.numeric(i[(len-27):len])))
    x <- as.vector(summary(z))
    y <- paste(x[1],"_",x[2],"_",x[3],"_",x[4],"_",x[5],"_",x[6])
    x[7] <- z[2]
  }
  if(flag == 2){
    z <-  rev(sort(as.numeric(i[(len-3):len])))
    x <- as.vector(summary(z))
    y <- paste(x[1],"_",x[2],"_",x[3],"_",x[4],"_",x[5],"_",x[6])
    x[7] <- z[2]
  }
  if(flag == 3){
    z <-  rev(sort(as.numeric(i[(len-1):len])))
    x <- as.vector(summary(z))
    y <- paste(x[1],"_",x[2],"_",x[3],"_",x[4],"_",x[5],"_",x[6])
    x[7] <- z[2]
  }
  #return(y)
  return(x[bit])
}

item_summary_day <- function(i,flag,bit,len){
  if(flag == 1){
    z <- rev(sort(as.numeric(i[(len-6):len])))
    x <- as.vector(summary(z))
    y <- paste(x[1],"_",x[2],"_",x[3],"_",x[4],"_",x[5],"_",x[6])
    x[7] <- z[2]
  }
  if(flag == 2){
    z <-  rev(sort(as.numeric(i[(len-13):len])))
    x <- as.vector(summary(z))
    y <- paste(x[1],"_",x[2],"_",x[3],"_",x[4],"_",x[5],"_",x[6])
    x[7] <- z[2]
  }
  if(flag == 3){
    z <-  rev(sort(as.numeric(i[(len-20):len])))
    x <- as.vector(summary(z))
    y <- paste(x[1],"_",x[2],"_",x[3],"_",x[4],"_",x[5],"_",x[6])
    x[7] <- z[2]
  }
  if(flag == 4){
    z <-  rev(sort(as.numeric(i[(len-27):len])))
    x <- as.vector(summary(z))
    y <- paste(x[1],"_",x[2],"_",x[3],"_",x[4],"_",x[5],"_",x[6])
    x[7] <- z[2]
  }
  #return(y)
  return(x[bit])
}


feature_by_day <- function(data,len){
  x <- data[,1:len]
  
  x$median_1bit <- apply(x,1,item_summary,1,2,len)
  x$medians <- apply(x,1,item_summary,1,3,len)
  x$means <- apply(x,1,item_summary,1,4,len)
  x$median_3bit <- apply(x,1,item_summary,1,5,len)
  x$maxs <- apply(x,1,item_summary,1,6,len)
  x$second_max <- apply(x,1,item_summary,1,7,len)
  
  #--------------------------------------------------------这是day以7作为时间扩展窗口提取特征
  x$median_1bit2 <- apply(x,1,item_summary_day,1,2,len)
  x$medians2 <- apply(x,1,item_summary_day,1,3,len)
  x$means2 <- apply(x,1,item_summary_day,1,4,len)
  x$median_3bit2 <- apply(x,1,item_summary_day,1,5,len)
  x$maxs2 <- apply(x,1,item_summary_day,1,6,len)
  x$second_max2 <- apply(x,1,item_summary_day,1,7,len)
  
  x$median_1bit3 <- apply(x,1,item_summary_day,2,2,len)
  x$medians3 <- apply(x,1,item_summary_day,2,3,len)
  x$means3 <- apply(x,1,item_summary_day,2,4,len)
  x$median_3bit3 <- apply(x,1,item_summary_day,2,5,len)
  x$maxs3 <- apply(x,1,item_summary_day,2,6,len)
  x$second_max3 <- apply(x,1,item_summary_day,2,7,len)
  
  x$median_1bit4 <- apply(x,1,item_summary_day,3,2,len)
  x$medians4 <- apply(x,1,item_summary_day,3,3,len)
  x$means4 <- apply(x,1,item_summary_day,3,4,len)
  x$median_3bit4 <- apply(x,1,item_summary_day,3,5,len)
  x$maxs4 <- apply(x,1,item_summary_day,3,6,len)
  x$second_max4 <- apply(x,1,item_summary_day,3,7,len)
  
  
  x <- x[,c(1,(len-27):(len+24))]
  colnames(x) <- c("item_id",paste("V",1:52,sep=""))
  return(x)
}

#以周为基准
feature_by_week <- function(data,len){
  x <- data[,1:len]
  
  x$median_1bit <- apply(x,1,item_summary,2,2,len)
  x$medians <- apply(x,1,item_summary,2,3,len)
  x$means <- apply(x,1,item_summary,2,4,len)
  x$median_3bit <- apply(x,1,item_summary,2,5,len)
  x$maxs <- apply(x,1,item_summary,2,6,len)
  x$second_max <- apply(x,1,item_summary,2,7,len)
  
  
  x <- x[,c(1,(len-3):(len+6))]
  colnames(x) <- c("item_id",paste("V",53:62,sep=""))
  return(x)
}

#以两周为基准
feature_by_2week <- function(data,len){
  x <- data[,1:len]

  x$median_1bit <- apply(x,1,item_summary,3,2,len)
  x$medians <- apply(x,1,item_summary,3,3,len)
  x$means <- apply(x,1,item_summary,3,4,len)
  x$median_3bit <- apply(x,1,item_summary,3,5,len)
  x$maxs <- apply(x,1,item_summary,3,6,len)
  x$second_max <- apply(x,1,item_summary,3,7,len)
  
  x <- x[,c(1,(len-1):(len+6))]
  colnames(x) <- c("item_id",paste("V",63:70,sep=""))
  return(x)
}

excuteFeatures_for_regression <- function(a,b,c,a_n,b_n,c_n){
  #提取特征
  x <- feature_by_day(a,a_n)
  y <- feature_by_week(b,b_n)
  z <- feature_by_2week(c,c_n)

  x <- data.table(x)
  y <- data.table(y)
  z <- data.table(z)
  
  train <- merge(x,y,by="item_id",all.x=T)
  train <- merge(train,z,by="item_id",all.x=T)
  
  return(train)
}

#归一化
normalization_data <- function(data){
  x <- data.frame(item_id=data[,1])
  for(j in 2:72){
    y <- apply(data,1,function(i) return((i[j]-mean(data[,j]))/sd(data[,j])))
    x <- cbind(x,y)
  }
  x$real <- data$real
  return(x)
}

#获得训练集,len是数据截止哪一天，len_week，len_2week类似len
get_feature_data <- function(data,data_week,data_2week,len,len_week,len_2week){
  data <- pre_deal(data,"day")
  data <- data[,1:len]
  data_week <- pre_deal(data_week,"week")
  data_week <- data_week[,1:len_week]
  data_2week <- pre_deal(data_2week,"two_week")
  data_2week <- data_2week[,1:len_2week]
  
  data$cum_day <- apply(data,1,how_day)
  data_week$cum_day <- apply(data,1,how_day)
  data_2week$cum_day <- apply(data,1,how_day)
  
  train <- excuteFeatures_for_regression(data[data$cum_day>=28,-ncol(data)],
                                         data_week[data_week$cum_day>=28,-ncol(data_week)],
                                         data_2week[data_2week$cum_day>=28,-ncol(data_2week)],
                                         len,len_week,len_2week)
  return(train)
}
  
#得到标签数据,len_2week是label日期
get_label_data <- function(data,data_2week,len,len_2week){
  data <- pre_deal(data,"day")
  data <- data[,1:len]
  data_2week <- pre_deal(data_2week,"two_week")
  data_2week$cum_day <- apply(data,1,how_day)
  label <- data_2week[data_2week$cum_day>=28,c(1,len_2week)]
  
  return(label)
}
#未使用模型预测的数据
get_other_data <- function(data,data_week,len,len_week){
  data <- pre_deal(data,"day")
  data <- data[,1:len]
  data_week <- pre_deal(data_week,"week")
  data_week <- data_week[,1:len_week]
  
  data_week$cum_day <- apply(newdata,1,how_day)
  data_less_14 <- data_week[data_week$cum_day<14,]
  data_less_28 <- data_week[data_week$cum_day<28 & data_week$cum_day>=14,]
  
  data_less_14$pre <- apply(data_less_14[,-c(1,ncol(data_less_14))],1,function(i) return(i[length(i)]*2))
  data_less_28$pre <- apply(data_less_28[,-c(1,ncol(data_less_28))],1,function(i) return(sum(i[(length(i)-1):length(i)])))
  
  x <- data_less_14[,c(1,ncol(data_less_14)-1,ncol(data_less_14))]
  y <- data_less_28[,c(1,ncol(data_less_28)-1,ncol(data_less_28))]

  
  pred <- rbind(x,y)
  pred$pre <- floor(pred$pre)
  pred$pre[is.na(pred$pre)] <- 0
  pred$pre[pred$pre < 0] <- 0
  
  return(pred)
}
#得到全部数据的标签
get_all_label_data <- function(data_2week,len_2week){
  data_2week <- pre_deal(data_2week,"two_week")
  label <- data_2week[,c(1,len_2week)]
  
  return(label)
}
#先预处理得到的数据，进行异常点去除（未处理）
#randomForest回归模型,gbm回归
#lasso
#发现基于特征划分的randomForest效果较其他回归效果好，可以再结果上进行修改

predict_Fun <- function(data,data_week,data_2week){
  train1 <- get_feature_data(data,data_week,data_2week,431,63,32)
  train1_x <- as.matrix(train1[,2:71,with=F])
  train1_y <- get_label_data(data,data_2week,431,33)
  xgbst <- xgboost(data=train1_x,label=train1_y,verbose=0,nrounds = 250)
  
  test1 <- get_feature_data(data,data_week,data_2week,445,65,33)
  test1_x <- as.matrix(test1[,2:71,with=F])
  test1$pre <- predict(xgbst,test1_x)
  pred <- data.table(item_id=test1$item_id,pre=test1$pre)
  return(pred)
}

