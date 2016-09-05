
library(MASS)

setwd("C:/Users/Adrian/Dropbox/Universidad/1º de Master/2. Second semester/Network Traffic Measurement and Analysis/Assignments/Final Assignment/Part 2")


load(paste("C:/Users/Adrian/Documents/trace/data_bytes.RData", sep="", collapse = NULL))
bytes = data
rm(data)
gc()  

#Histogram
jpeg("../Pictures/1_5_hist.jpg", 1266, 484)
hist(log(bytes$V8), freq = FALSE, breaks = 200, col = "blue", main = "Flow length distribution", xlab = "log( length [Bytes] )", ylab = "Frequency")     
dev.off()

#Empirical CDF
P = ecdf(log(bytes$V8))

jpeg("../Pictures/1_5_ecdf.jpg", 1266, 484)
plot(P, pch=',', lwd = 2, col = "blue", main="ECDF of flows length distribution", xlab = "log( length [B] )", ylab = "Probability")
dev.off()

#Summary of flows length distribution
summary(bytes$V8)

#Fitting using log-normal distribution
parameters = fitdistr(log(bytes$V8),"lognormal") 

jpeg("../Pictures/1_5_qq_lnorm.jpg", 1266, 484)
qqplot(qlnorm(ppoints(100), meanlog =  parameters[[1]][1], sdlog = parameters[[1]][2]),
       log(bytes$V8), main="Q-Q plot fitting log-normal distribution",ylab="Samples",col='blue',pch=3)

abline(0,1)
dev.off()

jpeg("../Pictures/1_5_hist_lnorm.jpg", 1266, 484)
hist(log(bytes$V8), freq = FALSE, breaks = 200, col = "blue", main = "Flow length distribution and fitted log-normal distribution", xlab = "log( length [Bytes] )", ylab = "Frequency")     
curve(dlnorm(x, meanlog =  parameters[[1]][1], sdlog = parameters[[1]][2]), col = "red", add = TRUE)
dev.off()

# Fitting using gamma distribution
parameters = fitdistr(log(bytes$V8),"gamma") 

jpeg("../Pictures/1_5_qq_gamma.jpg", 1266, 484)
qqplot(qgamma(ppoints(100), shape =  parameters[[1]][1], rate = parameters[[1]][2]),
       log(bytes$V8), main="Q-Q plot fitting gamma distribution", ylab = "Samples",col = 'blue', pch=3)

abline(0,1)
dev.off()

jpeg("../Pictures/1_5_hist_gamma.jpg", 1266, 484)
hist(log(bytes$V8), breaks = 200, col = "blue", main = "Flow length distribution and fitted gamma distribution", xlab = "log( length [B] )", ylab = "Frequency")     
curve(dgamma(x, lwd = 2, shape =  parameters[[1]][1], rate = parameters[[1]][2]), col = "red", add = TRUE)
dev.off()


# Fitting using weibull distribution
parameters = fitdistr(log(bytes$V8),"weibull") 

jpeg("../Pictures/1_5_qq_weibull.jpg", 1266, 484)
qqplot(qweibull(ppoints(100), shape =  parameters[[1]][1], scale = parameters[[1]][2]),
       log(bytes$V8), main="Q-Q plot fitting weibull distribution", ylab = "samples",col='blue',pch=3)

abline(0,1)
dev.off()

jpeg("../Pictures/1_5_hist_weibull.jpg", 1266, 484)
hist(log(bytes$V8), breaks = 200, col = "blue", main = "Flow length distribution and fitted weibull distribution", xlab = "log( length [B] )", ylab = "Frequency")     
curve(dweibull(x, lwd = 2, shape =  parameters[[1]][1], scale = parameters[[1]][2]), col = "red", add = TRUE)
dev.off()
