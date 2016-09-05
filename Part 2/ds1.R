
setwd("C:/Users/Adrian/Dropbox/Universidad/1º de Master/2. Second semester/Network Traffic Measurement and Analysis/Assignments/Final Assignment/Part 2")


#############################---- Part 1 ----#############################

data = read.table("ports311.txt", sep="\t", header = FALSE)
total = length(data$V1)

data = data.frame(table(data))

data = data[order(-data$Freq),]

percentage = data$Freq / total * 100

jpeg("../Pictures/2_1_1.jpg", 1266, 424)
barplot(percentage[1:5], names.arg = data$data[1:5], xlab = "Source ports", ylab = "Percentage of total packets", main = "Distribution of packets by source port")
dev.off()

#############################---- Part 2 ----#############################


min = c(2,1,0.5)

for(i in seq(1,length(min))){
  data = read.table("time312.txt", sep="\t", header = FALSE)
  data = data[order(data$V2),] 
  m = 0
  n = 1  
  res = vector()
  flag = 1
  
while(flag){
  print(n)  
  m = m + 1
  data$V2 = data$V2 - min[i]*60
  index = max(which(data$V2 < 0))
  
  if(!is.infinite(index)){
    res[m] = sum(data$V1[seq(1,index)])
    data = data[-(seq(1,index)),]
  }else{
    flag = 0
  }
  n = n + 1 
}

x = seq(1,length(res))
jpeg(paste("../Pictures/2_1_2_",min[i],"min.jpg", sep = ""),  1266, 424)
plot(x,res, "l", col = "blue", xaxt = "n", xlab = "Samples", ylab = "Data [Bytes]", main = paste("Data volume per ",min[i]*60, " seconds"), lwd = 2)
dev.off()
}


#############################---- Part 3 ----#############################

data = read.table("bytes313.txt", sep="\t", header = FALSE)

summary(data$V1)

#Histogram
jpeg("../Pictures/2_1_3_hist.jpg", 1266, 484)
hist(data$V1, breaks = max(data$V1), col = "blue", xlab = "Packet length [Bytes]", ylab = "Frequency", 
     main = "Packets length distribution" )
dev.off()

#Empirical Cumulative Distribution Function
P = ecdf(data$V1)

jpeg("../Pictures/2_1_3_ecdf.jpg", 1266, 484)
plot(P, pch='.',main="ECDF of flows' length",xlab="log10( Flow length [Bytes]  )",
     lwd=3)
dev.off()


