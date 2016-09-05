

for (k in seq(22,28, by=1)){  
  
  addr = paste ("C:/Users/Adrian/Documents/trace/Day_", k, "/2013-04-", k, "T0000.t2", sep = "", collapse = NULL)
  data = read.table(addr, sep="\t", header=FALSE)
  keeps = c("V8")
  data = data[keeps]
  
  for (i in seq(100, 2300, by=100)){
    
    print(i)
    
    if (i > 0 && i < 1000){
      addr = paste ("C:/Users/Adrian/Documents/trace/Day_", k, "/2013-04-", k, "T", 0, i, ".t2", sep = "", collapse = NULL)
      aux = read.table(addr, sep="\t", header=FALSE)  
      aux = aux[keeps]
      data = rbind(data,aux)
    }
    
    if (i >= 1000){
      addr = paste ("C:/Users/Adrian/Documents/trace/Day_", k, "/2013-04-", k, "T", i, ".t2", sep = "", collapse = NULL)
      aux = read.table(addr, sep="\t", header=FALSE)  
      aux = aux[keeps]
      data = rbind(data,aux)
    }
  }
  rm(aux)
  save.image(paste("~/trace/Day_", k, "_data.RData", sep="", collapse = NULL))
  rm(list = ls(all = TRUE))
  gc()
}

k = 22
n = 0
load(paste("~/trace/Day_", k, "_data.RData", sep="", collapse = NULL))
keeps = c("V8")
data = data[keeps]
aux = data
rm(data)
gc()

for (k in seq(23,28,by=1)){
  if(n > 0){
    load("~/trace/aaa.RData")
  }
  
  n = n+1
  print(k)
  load(paste("~/trace/Day_", k, "_data.RData", sep="", collapse = NULL))
  keeps = c("V8")
  data = data[keeps]
  aux = rbind(aux,data)  
  rm(data)
  gc()
  k = k+1
  save.image("~/trace/aaa.RData")
  rm(aux)
  gc()  
}

load("~/trace/aaa.RData")
data = aux
rm(aux)

save.image(paste("~/trace/data_bytes.RData", sep="", collapse = NULL))

