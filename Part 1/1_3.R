
setwd("C:/Users/Adrian/Dropbox/Universidad/1º de Master/2. Second semester/Network Traffic Measurement and Analysis/Assignments/Final Assignment/Part 2")


load(paste("C:/Users/Adrian/Documents/trace/data_ips.RData", sep="", collapse = NULL))
ips = data
rm(data)
gc()

load(paste("C:/Users/Adrian/Documents/trace/data_bytes.RData", sep="", collapse = NULL))
bytes = data
rm(data)
gc()

load(paste("C:/Users/Adrian/Documents/trace/data_flows.RData", sep="", collapse = NULL))
flows = data
rm(data)
gc()

###By flow
pairs = do.call(paste, c(ips[c("V1", "V2")], sep = "-"))
pairs1 = data.frame(table(pairs))
pairs1 = pairs[order(-pairs1$Freq),]

x = seq(1,length(pairs1$Freq))

jpeg("../Pictures/1_3_flows.jpg", 1266, 484)
plot(x, pairs1$Freq, log = "xy", xlab = "Rank", ylab = "Flows", main = "Zipf plot -> IP pairs by flows")
dev.off()

#By data volume
pairs = data.frame(pairs = pairs)
pairs = cbind(pairs, bytes = bytes$V8)
pairs = aggregate(pairs$bytes, list(pairs$pairs), sum)
pairs = pairs[order(-pairs$x),]

jpeg("../Pictures/1_3_traffic.jpg", 1266, 484)
plot(x, pairs$x, log = "xy", xlab = "Rank", ylab = "Data volume [Bytes]", main = "Zipf plot -> IP pairs by data volume")
dev.off()