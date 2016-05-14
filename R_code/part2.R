replace_na <- function(data){
  if(is.na(data[4])){
    data[4] <- data[2]/data[3]
  }
  return(data[4])
}
# item_feature_2 <- item_feature %>% group_by(V2) %>% arrange(V1)] 
a <- item_feature_2[,c(2,1,30),with=F]
md <- melt(a,id=(c("V2","V1")))
b <- cast(md,V2+variable~V1)

rm_zero_na <- function(i){
    i <- i[2:445]
  i <- i[!is.na(i)]
#  i <- i[i>0]
  return(median(i))
}


#按一周进行统计
#item_feature_2_one_week2 <- item_feature_2_one_week 
#                 %>% group_by(V2,label) %>% summarise(qty_sum=sum(V30)) 
#                  %>% select(V2,label,qty_sum) %>% arrange(V2,label)

rm_zero_na_week <- function(i){
  i <- i[60:64]
  i <- i[!is.na(i)]
  #  i <- i[i>0]
  return(median(i))
}

rm_zero_na_2week <- function(i){
  i <- i[30:32]
  i <- i[!is.na(i)]
  #  i <- i[i>0]
  return(median(i))
}


#为每一行进行summary统计，len用来设置断点（后面的都是预测点）
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

#以天为基准，根据百分位进行预测
predict_by_median <- function(data,len){
  x <- data[,1:len]
  x$median_1bit <- apply(x,1,item_summary,1,2,len)
  x$medians <- apply(x,1,item_summary,1,3,len)
  x$means <- apply(x,1,item_summary,1,4,len)
  x$median_3bit <- apply(x,1,item_summary,1,5,len)
  x$maxs <- apply(x,1,item_summary,1,6,len)
  x$second_max <- apply(x,1,item_summary,1,7,len)
  
  x$a1 <- (x$median_1bit-0)/2
  x$a2 <- (x$median_3bit-x$median_1bit)/2
  x$a3 <- (x$second_max-x$median_3bit)/2
  
  x$pre_by_median <- 14*(0.25*x$a1+0.5*x$a2+0.25*x$a3)
  x$pre_by_medians <- 14*(0.25*x$a1+0.5*x$medians+0.25*x$a3)
  x$pre_just_median <- 14*x$medians
  x$pre_just_mean <- 14*x$means
  x$pre_by_1bit_3bit <- 14*(x$a2)
  x <- x[,c(1,(len+1):(len+14))]
  return(x)
}

#以周为基准
predict_by_median_week <- function(data,len){
  x <- data[,1:len]
  x$median_1bit <- apply(x,1,item_summary,2,2,len)
  x$medians <- apply(x,1,item_summary,2,3,len)
  x$means <- apply(x,1,item_summary,2,4,len)
  x$median_3bit <- apply(x,1,item_summary,2,5,len)
  x$maxs <- apply(x,1,item_summary,2,6,len)
  x$second_max <- apply(x,1,item_summary,2,7,len)
  
  x$a1 <- (x$median_1bit-0)/2
  x$a2 <- (x$median_3bit-x$median_1bit)/2
  x$a3 <- (x$second_max-x$median_3bit)/2
  
  x$pre_by_median <- 2*(0.25*x$a1+0.5*x$a2+0.25*x$a3)
  x$pre_by_medians <- 2*(0.25*x$a1+0.5*x$medians+0.25*x$a3)
  x$pre_just_median <- 2*x$medians
  x$pre_just_mean <- 2*x$means
  x$pre_by_1bit_3bit <- 2*(x$a2)
  x <- x[,c(1,(len+1):(len+14))]
  return(x)
}

#以两周为基准
predict_by_median_2week <- function(data,len){
  x <- data[,1:len]
  x$median_1bit <- apply(x,1,item_summary,3,2,len)
  x$medians <- apply(x,1,item_summary,3,3,len)
  x$means <- apply(x,1,item_summary,3,4,len)
  x$median_3bit <- apply(x,1,item_summary,3,5,len)
  x$maxs <- apply(x,1,item_summary,3,6,len)
  x$second_max <- apply(x,1,item_summary,3,7,len)
  
  x$a1 <- (x$median_1bit-0)/2
  x$a2 <- (x$median_3bit-x$median_1bit)/2
  x$a3 <- (x$second_max-x$median_3bit)/2
  
  x$pre_by_median <- 2*(0.25*x$a1+0.5*x$a2+0.25*x$a3)
  x$pre_by_medians <- 2*(0.25*x$a1+0.5*x$medians+0.25*x$a3)
  x$pre_just_median <- 2*x$medians
  x$pre_just_mean <- 2*x$means
  x$pre_by_1bit_3bit <- 2*(x$a2)
  x <- x[,c(1,(len+1):(len+14))]
  return(x)
}

#以周为基准

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
#pre_by_median与pre_just_median结合的规则
rule1 <- function(i){
  if(i[13]==0){
    i[13] <- i[11]
  }
  return(i[13])
}

costFun <- function(data,co){
  #x1 <- data.table(item_id=data[,1],pre=(data$real-data$pre_just_mean))
  #x1 <- data.table(item_id=data[,1],pre=data[,33]-(data[,32]+data[,31])/2)
  data$new <- apply(data,1,rule1)
  x1 <- data.table(item_id=data[,1],pre=(data$real-data$new))
  y <- data.table(item_id=co$V1,less=co$V3,more=co$V4)
  xy1 <- merge(x1,y,by="item_id",all.x=T)
  xy1$cost <- apply(xy1,1,cost)
  print(sum(xy1$cost))
  return(xy1)
}

costFun2 <- function(data,co){
  x1 <- data.table(item_id=data[,1],pre=(data$real-data$pre))
  y <- data.table(item_id=co$V1,less=co$V3,more=co$V4)
  xy1 <- merge(x1,y,by="item_id",all.x=T)
  xy1$cost <- apply(xy1,1,cost)
  print(sum(xy1$cost))
  return(xy1)
}

#提取特征，前面按天，一周，两周，提取了一些特征，
train$V_43 <- c[,32] #前两周
train$V_44 <- b[,63] #前一周
train$V_45 <- b[,63]-b[,62] #前两周之差
train$V_46 <- apply(b,1,function(i) return(min(i[60:63]))) #前面4周销量最低的的周
f1 <- function(i){ 
  i <- i[-1]
  i <- i[i>0]
  return(median(i))
}
f2 <- function(i){ 
  i <- i[-1]
  i <- i[i>0]
  return(mean(i))
}
train$V_47 <- apply(b,1,f1) #平均每周的销量mean
train$V_48 <- apply(b,1,f1) #中位数median
train$V_49 <- train$V_44-train$V_47
train$V_50 <- train$V_44-train$V_48

#a_n是要预测的单位的前一天的编号，b_n,c_n类似a_n
excuteFeatures <- function(a,b,c,a_n,b_n,c_n){
  #提取特征
  x <- predict_by_median(a,a_n)
  y <- predict_by_median_week(b,b_n)
  z <- predict_by_median_2week(c,c_n)
  colnames(x) <- c("item_id",paste(rep("V",14),1:14,sep=""))
  colnames(y) <- c("item_id",paste(rep("V",14),15:28,sep=""))
  colnames(z) <- c("item_id",paste(rep("V",14),29:42,sep=""))
  x <- data.table(x)
  y <- data.table(y)
  z <- data.table(z)
  train <- merge(x,y,by="item_id",all.x=T)
  train <- merge(train,z,by="item_id",all.x=T)
  train$V_43 <- c[,c_n]
  train$V_44 <- b[,b_n] #前一周
  train$V_45 <- b[,b_n]-b[,b_n-1] #前两周之差
  train$V_46 <- apply(b,1,function(i) return(min(i[(b_n-2):b_n]))) #前面4周销量最低的的周
  train$V_47 <- apply(b,1,f2) #平均每周的销量mean
  train$V_48 <- apply(b,1,f1) #中位数median
  train$V_49 <- train$V_44-train$V_47
  train$V_50 <- train$V_44-train$V_48
  
  return(train)
}

#归一化
normalization_data <- function(data){
  x <- data.frame(item_id=data[,1])
  for(j in 2:51){
    y <- apply(data,1,function(i) return((i[j]-mean(data[,j]))/sd(data[,j])))
    x <- cbind(x,y)
  }
  x$real <- data$real
  return(x)
}
#remove some strong effect points,replace thest value that is lower than 31 with before value
fit <- lm(real~V6+V29+V_43+V28+V34+V37+V38+V_45,data=valid_normal[-c(686,172,318,844,831,305,862,929,657,469,122,961,361,24),])

#从预测的结果看，分仓的结果明显不对，应该重新考虑分仓的模型！！！
