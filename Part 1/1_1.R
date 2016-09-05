
setwd("C:/Users/Adrian/Dropbox/Universidad/1º de Master/2. Second semester/Network Traffic Measurement and Analysis/Assignments/Final Assignment/Part 2")

load(paste("C:/Users/Adrian/Documents/trace/1_1_results.RData", sep="", collapse = NULL))  

res = c(res22, res23, res24, res25, res26, res27, res28)
x = seq(1,length(res))

jpeg("../Pictures/1_1_hourly.jpg", 1266, 484)
plot(x,res, "l", col = "blue", lwd = 1, xlab = "Time", ylab = "Data volume [Bytes]", main = "Data volume per 60 minutes")
dev.off()


m = 0

for(i in seq(22,28)){
  print(i)
  n = 1
  load(paste("C:/Users/Adrian/Documents/trace/Day_", i, "_timedata.RData", sep="", collapse = NULL))
  
  data = data[order(data$V10),]
  data$V10 = data$V10 - data$V10[1]
  
  while(n <= 48){
    print(n)
    m = m + 1
    data$V10 = data$V10 - 1800
    index = max(which(data$V10 < 0))
    
    if(is.infinite(index)){
      res[m] = 0
    }else{
      res[m] = sum(data$V8[seq(1,index)])
      data = data[-(seq(1,index)),]
    }
    
    n = n + 1 
  }
}


x = seq(1,length(res))

jpeg("../Pictures/1_1_halfhourly.jpg", 1266, 484)
plot(x, res, "l", lwd = 1, col = "blue", xlab = "Time", ylab = "Data volume [Bytes]", main = "Data volume per 30 minutes")
dev.off()

m = 0

for(i in seq(22,28)){
  print(i)
  n = 1
  load(paste("C:/Users/Adrian/Documents/trace/Day_", i, "_timedata.RData", sep="", collapse = NULL))
  
  data = data[order(data$V10),]
  data$V10 = data$V10 - data$V10[1]
  
  while(n <= (4*24)){
    print(n)
    m = m + 1
    data$V10 = data$V10 - 600
    index = max(which(data$V10 < 0))
    
    if(is.infinite(index)){
      res[m] = 0
    }else{
      res[m] = sum(data$V8[seq(1,index)])
      data = data[-(seq(1,index)),]
    }
    
    n = n + 1 
  }
}


x = seq(1,length(res))

jpeg("../Pictures/1_1_quarterhourly.jpg", 1266, 484)
plot(x,res, "l", lwd = 2, col = "blue", xlab = "Time", ylab = "Data [GB]", main = "Data volume per quarter of hour")
dev.off()
