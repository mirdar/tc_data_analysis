# 数据预处理------------------------------------------------------------------------------

#进行异常点发现，并平滑异常点！！！！！！！！

#处理缺失数据
deal_row_na <- function(i){
  j <- 1
  while(is.na(i[j])){
    i[j] <- -1
    j <- j+1
    if(j == length(i)){
      return(i)
    }
  }
  m <- i[j:length(i)]
  m <- m[!is.na(m)]
  m <- median(m)
  for(n in j:length(i)){
    if(is.na(i[n])){
      i[n] <- m
    }
  }
  return(i)
}
deal_na <- function(data){
  a <- data.frame()
  for(i in 1:nrow(data)){
    b <- deal_row_na(data[i,2:ncol(data)])
    a <- rbind(a,b)
  }
  a <- cbind(item_id=data[,1],a)
  return(a)
}

#统计每件商品出现的天数
how_day <- function(i){
  j <- 2
  while(i[j] == -1)
  {
    if(j == length(i)){
      return(0)
    }
    j <- j+1
  }
  return(length(i)-j)
}
y$cum_day <- apply(y,1,how_day)

#一些特殊日期
#20151212 429
#20151111 398
#20141111 33
#20141212 64
#20150618 252
deal_sp_point <- function(data,type){
  if(type == "day"){
    data[,430] <- apply(data[,c(429,431)],1,mean)
    data[,399] <- apply(data[,c(398,400)],1,mean)
    data[,253] <- apply(data[,c(252,254)],1,mean)
    data[,34] <- apply(data[,c(33,35)],1,mean)
    data[,65] <- apply(data[,c(64,66)],1,mean)
  }
  if(type == "week"){
    data[,63] <- apply(data[,c(62,64)],1,mean)
    data[,59] <- apply(data[,c(58,60)],1,mean)
    data[,7] <- apply(data[,c(6,8)],1,mean)
    data[,11] <- apply(data[,c(10,12)],1,mean)
    data[,38] <- apply(data[,c(37,39)],1,mean)
  }
  if(type == "two_week"){
    data[,32] <- apply(data[,c(31,33)],1,mean)
    data[,30] <- apply(data[,c(29,31)],1,mean)
    data[,4] <- apply(data[,c(3,5)],1,mean)
    data[,6] <- apply(data[,c(5,7)],1,mean)
    data[,20] <- apply(data[,c(19,21)],1,mean)
  }
  return(data)
}

#数据预处理，去缺少，修改特殊日期的值
pre_deal <- function(data,type){
  data <- deal_na(data)
  data <- deal_sp_point(data,type)
  return(data)
}
