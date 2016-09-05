
library(plyr)
library(MASS)
library(vcd)

setwd("C:/Users/Adrian/Dropbox/Universidad/1º de Master/2. Second semester/Network Traffic Measurement and Analysis/Assignments/Final Assignment/Part 2")
data = read.table("flows60sec_2.t2", sep="\t", header = FALSE, col.names = c("sip","dip","pro","OK","sport","dport","packets","bytes","flows","first","last"))

data[,c(1,2,3,4,5,6)][1:10,]
data[,c(7,8,9,10,11)][1:10,]

#############################---- Part 1 ----#############################

##Source Ips
sports = aggregate(data$flows, list(data$sport), sum)
sports = rename(sports, c("Group.1" = "sport","x" = "flows"))

sports = sports[order(-sports$flows),]
percentages = sports$flows / sum(sports$flows) * 100 

jpeg("../Pictures/2_2_1_sports.jpg", 1266, 484)
barplot(percentages[seq(1,5)], names.arg = sports$sport[seq(1,5)], xlab = "Source ports", ylab = "Percentage", main = "Flow distribution by source ports")
dev.off()

##Destination Ips
dports = aggregate(data$flows, list(data$dport), sum)
dports = rename(dports, c("Group.1" = "dport","x" = "flows"))

dports = dports[order(-dports$flows),]
percentages = dports$flows / sum(dports$flows) * 100 

jpeg("../Pictures/2_2_1_dports.jpg", 1266, 484)
barplot(percentages[seq(1,5)], names.arg = dports$dport[seq(1,5)], xlab = "Destination ports", ylab = "Percentage", main = "Flow distribution by destination ports")
dev.off()

#############################---- Part 2 ----#############################

countries = read.table("sortedcountries", sep="\t")
countries = data.frame(table(countries$V1))
countries = countries[order(-countries$Freq),]
total = sum(countries$Freq)
percentage = countries$Freq / total * 100

jpeg("../Pictures/2_2_2_countries.jpg", 1266, 484)
barplot(percentage[1:10], names.arg = countries$Var1[1:10], cex.names = 0.6, col = "blue", xlab = "Country", ylab = "Percentage", main="Flow Distribution by country")
dev.off()

#############################---- Part 3 ----#############################

###By flow
pairs = do.call(paste, c(data[c("sip", "dip")], sep = "-"))
pairs = data.frame(table(pairs))
pairs = pairs[order(-pairs$Freq),]

x = seq(1,length(pairs$Freq))

jpeg("../Pictures/2_2_3_flows.jpg", 1266, 484)
plot(x, pairs$Freq, log = "xy", xlab = "Rank", ylab = "Flows", main = "Zipf plot -> IP pairs by flows")
dev.off()

#By data volume
pairs = do.call(paste, c(data[c("sip", "dip")], sep = "-"))
pairs = data.frame(pairs = pairs)
pairs = cbind(pairs, bytes = data$bytes)
pairs = aggregate(pairs$bytes, list(pairs$pairs), sum)
pairs = pairs[order(-pairs$x),]

jpeg("../Pictures/2_2_3_traffic.jpg", 1266, 484)
plot(x, pairs$x, log = "xy", xlab = "Rank", ylab = "Data volume [bytes]", main = "Zipf plot -> IP pairs by data volume")
dev.off()

#############################---- Part 4 ----#############################

#Histogram
jpeg("../Pictures/2_2_4_hist.jpg", 1266, 484)
histogram = hist(log10(data$bytes), breaks = 200, col = 4, xlab = "log10( Flow length [Bytes] )",
                 ylab = "Frequency", main = "Flows length distribution")
dev.off()

#Empirical Cumulative Distribution Function
P = ecdf(log10(data$bytes))

jpeg("../Pictures/2_2_4_ecdf.jpg", 1266, 484)
plot(P, pch='.',main="ECDF of flows' length",xlab="log10( Flow length [Bytes]  )",
     lwd=3)
dev.off()

#Summary statistics
summary(data$bytes)

#############################---- Part 5 ----#############################



#Fitting the distribution using "log-normal" distribution
parameters = fitdistr(log(data$bytes),"lognormal") 

print(parameters)

jpeg("../Pictures/2_2_5_hist_lnorm.jpg", 1266, 484)
histogram = hist(log(data$bytes), freq = FALSE, breaks = 200, col = 4, xlab = "log( Flow length [Bytes] )",
                             ylab = "Frequency", main = "Flows length distribution and fitted ''log-normal'' distribution")

curve(dlnorm(x, meanlog =  parameters[[1]][1], sdlog = parameters[[1]][2]), col = "red", add = TRUE)
dev.off()

jpeg("../Pictures/2_2_5_qq_lnorm.jpg", 1266, 484)
qqplot(qlnorm(ppoints(100), meanlog = parameters[[1]][1], sdlog = parameters[[1]][2]),
       log(data$bytes), main = "Q-Q plot for ''log-normal'' distribution", col = 'blue', pch=3)

abline(0,1)
dev.off()

#Fitting the distribution using "gamma" distribution
parameters = fitdistr(log(data$bytes),"gamma")

print(parameters)

jpeg("../Pictures/2_2_5_hist_gamma.jpg", 1266, 484)
histogram = hist(log(data$bytes), freq = FALSE, breaks = 200, col = 4, xlab = "log( Flow length [Bytes] )",
                 ylab = "Frequency", main = "Flows length distribution and fitted ''gamma'' distribution")

curve(dgamma(x, shape =  parameters[[1]][1], rate = parameters[[1]][2]), col = "red", add = TRUE)
dev.off()

jpeg("../Pictures/2_2_5_qq_gamma.jpg", 1266, 484)
qqplot(qgamma(ppoints(100), shape =  parameters[[1]][1], rate = parameters[[1]][2]),
       log(data$bytes), main="Q-Q plot for ''gamma'' distribution",ylab="samples",col='blue',pch=3)

abline(0,1)
dev.off()

#Fitting the distribution using "weibull" distribution
parameters = fitdistr(log(data$bytes),"weibull")

print(parameters)

jpeg("../Pictures/2_2_5_hist_weibull.jpg", 1266, 484)
histogram = hist(log(data$bytes), freq = FALSE, breaks = 200, col = 4, xlab = "log( Flow length [Bytes] )",
                 ylab = "Frequency", main = "Flows length distributionand fitted ''weibull'' distribution")

curve(dweibull(x, shape =  parameters[[1]][1], scale = parameters[[1]][2]), col = "red", add = TRUE)
dev.off()

jpeg("../Pictures/2_2_5_qq_weibull.jpg", 1266, 484)
qqplot(qweibull(ppoints(100), shape =  parameters[[1]][1], scale = parameters[[1]][2]),
       log(data$bytes), main="Q-Q plot for ''weibull'' distribution",ylab="samples",col='blue',pch=3)

abline(0,1)
dev.off()


#############################---- Part 6 ----#############################


data = read.table("flows1sec_2.t2", sep="\t", header = FALSE, col.names = c("sip","dip","pro","OK","sport","dport","packets","bytes","flows","first","last"))
flows1sec = length(data$flows)

data = read.table("flows10sec_2.t2", sep="\t", header = FALSE, col.names = c("sip","dip","pro","OK","sport","dport","packets","bytes","flows","first","last"))
flows10sec = length(data$flows)

data = read.table("flows60sec_2.t2", sep="\t", header = FALSE, col.names = c("sip","dip","pro","OK","sport","dport","packets","bytes","flows","first","last"))
flows60sec = length(data$flows)

data = read.table("flows120sec_2.t2", sep="\t", header = FALSE, col.names = c("sip","dip","pro","OK","sport","dport","packets","bytes","flows","first","last"))
flows120sec = length(data$flows)

data = read.table("flows1800sec_2.t2", sep="\t", header = FALSE, col.names = c("sip","dip","pro","OK","sport","dport","packets","bytes","flows","first","last"))
flows1800sec = length(data$flows)

x = c(flows1sec, flows10sec, flows60sec, flows120sec, flows1800sec)

jpeg("../Pictures/2_2_6.jpg", 1266, 484)
plot(seq(1,length(x)), x, "l", xaxt = "n", xlab = "Flows time extension [seconds]", ylab = "Number of flows", lwd = 2)
axis(1, at=1:length(x), labels = c("1", "10", "60", "120", "1800"))
dev.off()
