library("rstudioapi")
library(DescTools)
library(foreach)
library(forecast)
library(reshape2)
current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path))

##### Não estacionarizar

source("Data.R")

df = raw_data %>% 
  select(Date,PIB)

w_size = round(0.8*(dim(df)[1]-1)) # Training data
n_windows = (nrow(df)-1) - w_size # Test data
h = 12

y = as.vector(df$PIB[-1])

nr = length(y)

y.f = matrix(NA, nrow = n_windows, ncol = h)

y.r = matrix(NA, nrow = n_windows, ncol = h)

rw.e = matrix(NA,ncol = h,nrow = 2)

for(i in 1:h){
  y.r[,i] = y[(nr-n_windows+1):nr]
  y.f[,i] = y[(nr-n_windows+1-i):(nr-i)]
  rw.e[1,i] = sqrt(mean((y.f[,i] - y.r[,i])^2))
  rw.e[2,i] = mean(abs(y.f[,i] - y.r[,i]))
}

colnames(rw.e)=paste("h=", 1:h, sep="")
rownames(rw.e)= c("rmse","mae")
print(round(rw.e,2))

rw.e = as.data.frame(rw.e)

RW.result = rw.e

save(RW.result,file=paste0("RW",".Rda"))

