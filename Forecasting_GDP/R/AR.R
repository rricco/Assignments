library("rstudioapi")
library(DescTools)
library(foreach)
library(forecast)
library(reshape2)
current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path))


source("Data.R")

df = as.data.frame(cbind(dates,x10))
df$dates = as.Date(df$dates)

# = Number of windows and window size
w_size = round(0.8*dim(df)[1]) # Training data
n_windows = nrow(df) - w_size # Test data
h.max=12
p.max = 24


y = as.vector(df$x10) #Gets inflation data

nr = length(y) #Gets data length

y.f = p.s = matrix(NA, nrow = n_windows, ncol = h.max) #Initialize a nf x h.max matrix

dff = raw_data %>% 
  filter(Date >= AddMonths(df[((nr-n_windows) + 1), 1], -1) & Date <= AddMonths(df[nrow(df),1],-1)) %>% 
  select(Date,PIB)


#Implementation of a (nr-nf-h+1)-month rowlling-window scheme  
pb = txtProgressBar(min = 0, max = n_windows*h.max, style = 3)
for (i in 1:n_windows){
  y.w = y[i:(nr-n_windows+i-1)]
  l.yw = length(y.w)
  for (h in 1:h.max) {
    fitAr = fitArOls(y = y.w[1:(l.yw-h+1)], h = h, p.max = p.max)

    y.f[i,h] = exp(fitAr$y.h)*dff$PIB[i]
    p.s[i,h] = fitAr$p
    setTxtProgressBar(pb, (i-1)*h.max+h)
  }
}
close(pb)




y.r = matrix(tail(raw_data$PIB,n_windows) , ncol=h.max, nrow = n_windows) #Gets realized data

#Computes RMSE(h) and MAE(h)
rmse = sqrt(colMeans((y.f - y.r)^2, na.rm = T))
mae = colMeans(abs(y.f - y.r), na.rm = T)

#Binds RMSE and MAE values
ar.e = rbind(rmse,mae)
colnames(ar.e)=paste("h=", 1:h.max, sep="") #Sets column names

#Prints RMSE and MAE data
print(p.s)
print(round(ar.e,2))

AR.result = ar.e

save(AR.result,file=paste0("AR",".Rda"))

