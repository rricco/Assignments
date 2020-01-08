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
p.max = 12

fitArOls = function(y, h, p.max){
  #y - one-dimensional time series
  #h - forecast horizon
  #p.max - max order of AR models for selection
  
  bic = rep(NA,p.max) #vector to store bic(p)
  models = list() #list to store all fitted models
  
  #Fits {AR(1),...,AR(p.max)} models
  for (p in 1:p.max ) {
    
    aux = embed(y,h+p) #Apply h+p lags
    
    y.aux = aux[,1] #y
    x.aux = aux[,-1] #x
    
    if(h > 1){
      x.aux = x.aux[,-c(1:(h-1))] #Removes unused data
    }
    x.aux2 = cbind(1,x.aux)
    coeff=solve(t(x.aux2)%*%x.aux2)%*%t(x.aux2)%*%y.aux
    fit = list("coeff"=coeff)#lm(y.aux ~ 1 + x.aux) #Fits an OLS model with intercept
    models[[p]] = fit #Store the fitted model
    y.hat = x.aux2%*%coeff
    SSR = sum((y.aux - y.hat)^2)
    n = length(y.aux)
    k=length(coeff)
    bic[p] = log(SSR)+(log(n)*(k))/n
  }
  
  s = which.min(bic) #Gets the index of the model  with the lowest BIC
  
  fit = models[[s]] #Retrieves the selected model
  
  coeff = as.vector(fit$coeff) #as.vector(fit$coefficients) #Gets the linear coefficients
  
  aux = embed(y,s) #Apply s lags over y
  
  n = nrow(aux) #Gets the number of rows
  
  x.vaux = as.vector(c(1,aux[n,])) #Converts to vector
  
  y.h = sum(coeff*x.vaux) #Calculates y(t+h)
  
  return(list("bic" = bic[s], "p" = s,"y.h"= y.h))
}

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

