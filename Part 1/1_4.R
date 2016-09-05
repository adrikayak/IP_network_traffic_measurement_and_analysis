
setwd("C:/Users/Adrian/Dropbox/Universidad/1º de Master/2. Second semester/Network Traffic Measurement and Analysis/Assignments/Final Assignment/Part 2")


load("C:/Users/Adrian/Documents/trace/data_ipss&bytes.RData")

ipss = aggregate(data$V8, list(data$V1), sum)

load("C:/Users/Adrian/Documents/trace/data_ipsd&bytes.RData")

ipsd = aggregate(data$V8, list(data$V2), sum)

ips = rbind(ipss,ipsd)
rm(ipss)
rm(ipsd)
gc()

ips = aggregate(ips[,2], list(ips[,1]), sum)

jpeg("../Pictures/1_4_hist.jpg", 1266, 484)
hist(log10(ips[,2]), breaks = 200, col = "blue", xlab = "log10( Data Volume [Bytes] )", ylab = "Frequency", main = "User aggregated data")
dev.off()

users = ips[order(-ips[,2]),]

percentage = users[,2][seq(1,20)]/sum(users[,2])*100

jpeg("../Pictures/1_4_barplot.jpg", 1266, 484)
barplot(percentage, names.arg = users[,1][seq(1,20)], xlab = "Users", ylab = "Percentage")
dev.off()