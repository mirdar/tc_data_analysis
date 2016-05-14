pre_store <- function(data,len){
  item_store_feature <- data %>% group_by(V2) %>% arrange(V1)
  a <- item_store_feature[,c(2,1,31),with=F]
  md <- melt(a,id=(c("V2","V1")))
  b <- cast(md,V2+variable~V1)
  item_store_ts <- b[,-2]
  ts_1 <- item_store_ts
  ts_1[is.na(ts_1)] <- 0
  x <- predict_by_median(ts_1,len)
  y <- x[,c(1,(len+1):(len+14))]
  y$new <- apply(y,1,rule1)
 # y$new <- apply(y,1,function(i) return(max(i[11],i[13])))
  predict <- data.table(item_id=y$V2,pre=y$new)
  return(predict)
}

cost <- function(data){
  if(as.numeric(data[2]) >= 0){
    a <- abs(as.numeric(data[2])*as.numeric(data[3]))
    return(a)
  }
  if(data[2] < 0){
    a <- abs(as.numeric(data[2])*as.numeric(data[4]))
    return(a)
  }
}

costFun <- function(data,co){
  x1 <- data.table(item_id=data[,1],pre=(data$real-data$pre))
  y <- data.table(item_id=co$V1,less=co$V3,more=co$V4)
  xy1 <- merge(x1,y,by="item_id",all.x=T)
  xy1$cost <- apply(xy1,1,cost)
  print(sum(xy1$cost))
  return(xy1)
}
#计算cost
pred_valid_all <- pre_store(item_feature_2,431)
pred_valid_all$real <- c[,33]
pred_valid_all <- data.frame(pred_valid_all)
costFun(pred_valid_all,item_config[item_config$V2=="all",])

#构造ts变量
ts_day <- function(data){
  a <- data[,c(2,1,31),with=F] #按天
  md <- melt(a,id=(c("V2","V1")))
  b <- cast(md,V2+variable~V1)
  item_i_ts <- b[,-2]
  return(item_i_ts)
}

ts_week <- function(data){
  date_week <- data.table(date_week)#按两周
  data <- data.table(data)
  data$V1 <- as.character(data$V1)
  item_store_i_t <- merge(data,date_week,by="V1",all.x=T) #按一周
  item_i_week <- item_store_i_t %>% group_by(V2,label) %>% summarise(qty_sum=sum(V31)) %>% select(V2,label,qty_sum) %>% arrange(V2,label)
  md <- melt(item_i_week,id=(c("V2","label")))
  b <- cast(md,V2+variable~label)
  item_i_week_ts <- b[,-2]
  return(item_i_week_ts)
}

ts_2week <- function(data){
  date_d <- data.table(date_d)#按两周
  data <- data.table(data)
  data$V1 <- as.character(data$V1)
  item_store_i_t <- merge(data,date_d,by="V1",all.x=T)
  item_i_2week <- item_store_i_t %>% group_by(V2,label) %>% summarise(qty_sum=sum(V31)) %>% select(V2,label,qty_sum) %>% arrange(V2,label)
  md <- melt(item_i_2week,id=(c("V2","label")))
  b <- cast(md,V2+variable~label)
  item_i_2week_ts <- b[,-2]
  return(item_i_2week_ts)
}

#---------------------------------------------------------------------------------

#根据分仓数据求出全国数据
caculate_all <- function(data){
  data_all <- data %>% group_by(item_id) %>% summarise(pre_all=sum(pre)) %>% select(item_id,pre_all) %>% arrange(item_id)
  return(data_all)
}





