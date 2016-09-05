  
setwd("C:/Users/Adrian/Dropbox/Universidad/1º de Master/2. Second semester/Network Traffic Measurement and Analysis/Assignments/Final Assignment/Part 2")


load(paste("C:/Users/Adrian/Documents/trace/data_ports.RData", sep="", collapse = NULL))
ports = data
rm(data)
gc()

#Source ports
sports = data.frame(table(ports$V5))
sports = sports[order(-sports$Freq),]

spercentage = sports$Freq[seq(1,5)]/sum(sports$Freq)*100

jpeg("../Pictures/1_2_sports.jpg", 1266, 484)
barplot(spercentage, names.arg = sports$Var1[seq(1,5)], xlab = "Ports", ylab = "Percentage", main = "Percentage of total flows by source port")
dev.off()

#Destination ports
dports = data.frame(table(ports$V6))
dports = dports[order(-dports$Freq),]

dpercentage = dports$Freq[seq(1,5)]/sum(dports$Freq)*100

jpeg("../Pictures/1_2_dports.jpg", 1266, 484)
barplot(dpercentage, names.arg = dports$Var1[seq(1,5)], xlab = "Ports", ylab = "Percentage", main = "Percentage of total flows by destination port")
dev.off()
 