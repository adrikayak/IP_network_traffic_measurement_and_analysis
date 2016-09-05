
library(plyr)

#setwd("/u/90/demiga1/unix/Part2")
setwd("C:/Users/Adrian/Dropbox/Universidad/1º de Master/2. Second semester/Network Traffic Measurement and Analysis/Assignments/Final Assignment/Part 2")


data = read.table("tcptrace.txt", header = TRUE, sep = "\t", dec = ".", fill = TRUE, comment.char = "#")

#############################---- Part 1 ----#############################
RTT_avg = c(data$RTT_avg_a2b, data$RTT_avg_b2a)
RTT_sd = c(data$RTT_stdev_a2b, data$RTT_stdev_b2a)
RTT_var = RTT_sd^2

retransmissions = c(data$rexmt_data_pkts_a2b, data$rexmt_data_pkts_b2a)

jpeg("../Pictures/2_3_RTTavg.jpg", 1266, 484)
plot(RTT_avg, retransmissions, "p", col = "blue", cex = 0.5, xlab = "Connection's RTT average [ms]", 
     ylab = "Retransmissions", main = "Connection's RTT average vs. Retransmissions")
dev.off()

jpeg("../Pictures/2_3_RTTvar.jpg", 1266, 484)
plot(RTT_var, retransmissions, "p", col = "red",, cex = 0.5, xlab = "Connection's RTT variance [ms]",
     ylab = "Retransmissions", main = "Connection's RTT variance vs. Retransmissions")
dev.off()

#############################---- Part 2 ----#############################

data_volume = c(data$actual_data_bytes_a2b, data$actual_data_bytes_b2a)

jpeg("../Pictures/2_3_traffic.jpg", 1266, 484)
plot(data_volume, retransmissions, log = "x", "p", col = "dark green", cex = 0.5, xlab = "Connection's Data volume [Bytes]", 
     ylab = "Retransmissions", main = "Connection's Data volume vs. Retransmissions")
dev.off()
