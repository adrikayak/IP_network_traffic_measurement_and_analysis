


for (k in seq(22,28, by=1)){  
  
  addr = paste ("C:/Users/Adrian/Documents/trace/Day_", k, "/2013-04-", k, "T0000.t2", sep = "", collapse = NULL)
  data = read.table(addr, sep="\t", header=FALSE)
  keeps = c("V8","V10")
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
  save.image(paste("~/trace/Day_", k, "_timedata.RData", sep="", collapse = NULL))
  rm(data)
  gc()
}

