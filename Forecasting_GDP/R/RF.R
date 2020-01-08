library("rstudioapi")
library(DescTools)
library(reshape2)
# install.packages("gtools")
library(gtools)
library("randomForest")

current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path))
source("Data.R")

n_features = 20


fitRF = function(data, h, l.max){
  
  
  
  
  d.aux.col = ncol(data)
  
  aux = as.matrix(embed(data,h+l.max)) #Apply h+p lags
  
  y.aux = as.vector(aux[,1]) #y
  
  x.aux = aux[,-(1:(d.aux.col*h))] #x
  
  nx = nrow(x.aux)
  
  
  
  
  x.aux = as.matrix(x.aux)
  
  
  aux2 = as.matrix(embed(data,l.max)) #Apply s lags over y
  
  n = nrow(aux2) #Gets the number of rows
  
  
  aux3 = aux2[n,]
  
  aux4 = (c(aux3))
  
  ncx = ncol(x.aux)
  
  colnames(x.aux)=paste("V",(1:ncx)+1,sep="")
  
  aux4 = matrix(aux4,nrow=1)
  colnames(aux4) = colnames(x.aux)
  
  
  # x.tot = rbind(x.aux,aux4)
  # mx = colMeans(x.tot)
  # mx.aux = matrix(mx,nrow=nrow(x.aux),ncol=length(mx),byrow = T)
  
  # my = mean(y.aux)
  
  ntree = 500
  
  fitRFTune = tuneRF((x.aux), (y.aux),ntreeTry = ntree, 
                     plot=F,
                     mtryStart = max(floor(ncol(x.aux)/3), 1),
                     stepFactor=2, 
                     do.trace = F, 
                     trace = F, 
                     doBest = T,
                     improve=0.05,
                     replace=T,
                     importance=T,
                     nodesize=5)
  
  #nrt = nrow(fgl.res)
  
  #mtry = fgl.res[nrt,1] 
  #mtry = min(5,floor((ncol(x.aux))/3))
  fitBag <- randomForest((x.aux), (y.aux),mtry=ncol(x.aux),importance=TRUE,na.action=na.omit,ntree = ntree,nodesize=5)
  fit <- randomForest((x.aux), (y.aux),mtry=max(floor(ncol(x.aux)/3), 1),importance=TRUE,na.action=na.omit,ntree = ntree, nodesize=5)
  #fit <- randomForest((x.aux-mx.aux), (y.aux-my),mtry=mtry,importance=TRUE,
  #na.action=na.omit,ntree = ntree)
  
  
  coeff=importance(fit)
  
  aux5 = as.data.frame(aux4)
  y.h = predict(fit, matrix(aux5,nrow=1),type="response")
  
  y.h.bag = predict(fitBag, matrix(aux5,nrow=1),type="response")
  
  y.h.RFTune = predict(fitRFTune, matrix(aux5,nrow=1),type="response")
  
  
  
  
  return(list("y.h"= y.h, "y.h.bag"= y.h.bag, "y.h.RFTune"= y.h.RFTune, "mtry"=fitRFTune$mtry))
}


mylist <- list()
#joining the series
for(i in 10:(10+n_features-1)) {
  mylist[[i-9]] <- get(paste0("x",i))
  names(mylist)[i-9] <- paste0("x",i)
} 
#Transforming 
df <- data.frame(t(matrix(unlist(mylist), nrow=length(mylist), byrow=T)))
colnames(df) = c(names(mylist))
X <-cbind(dates,df)
X$dates = as.Date(X$dates)
data = as.matrix(X[,-1])
nf = nrow(data) - round(0.8*nrow(data))   #Number of forecasts
nr = nrow(data) #Gets data length

l.max = 24 #Max order 
h.max = 12 #Max number of forecasting steps ahead
y.f.bag = y.f.rf = y.f.rfTune = mtry = matrix(NA, nrow = nf, ncol = h.max) #Initialize a nf x h.max matrix

dff = raw_data %>% 
  filter(Date >= AddMonths(X[((nr-nf) + 1), 1], -1) & Date <= AddMonths(X[nrow(X),1],-1)) %>% 
  select(Date,PIB)



pb = txtProgressBar(min = 0, max = nf*h.max, style = 3)
for (i in 1:nf){
  data.w = data[i:(nr-nf+i-1),]
  l.dw = nrow(data.w)
  for (h in 1:h.max) {
    # top5.h = data[(nr-nf+i-h+1),(72+h-1)]
    # focus.h = data[(nr-nf+i-h+1),(59+h-1)]
    # focus.h = data[(nr-nf+i-h+1),(59+h-1)]
    #dummies = data[(nr-nf+i-h+1),(94:103)]
    fit = fitRF(data = data.w[(1:(l.dw-h+1)),], h = h, l.max = l.max)
    

    y.f.rf[i,h] = exp(fit$y.h)*dff$PIB[i]
    
    y.f.rfTune[i,h] = exp(fit$y.h.RFTune)*dff$PIB[i]
    
    y.f.bag[i,h] = exp(fit$y.h.bag)*dff$PIB[i]
    
    mtry[i,h] = fit$mtry
    #print((i-1)*h.max+h)
    setTxtProgressBar(pb, (i-1)*h.max+h)
  }
}
close(pb)

y.r = matrix(tail(raw_data$PIB,nf) , ncol=h.max, nrow = nf) #Gets realized data

#Computes RMSE(h) and MAE(h)
rmse = sqrt(colMeans((y.f.rf - y.r)^2, na.rm = T))
mae = colMeans(abs(y.f.rf - y.r), na.rm = T)

rmse.Tune = sqrt(colMeans((y.f.rfTune - y.r)^2, na.rm = T))
mae.Tune = colMeans(abs(y.f.rfTune - y.r), na.rm = T)

rmse.bag = sqrt(colMeans((y.f.bag - y.r)^2, na.rm = T))
mae.bag = colMeans(abs(y.f.bag - y.r), na.rm = T)

#Binds RMSE and MAE values
RF.e = rbind(rmse,mae)
colnames(RF.e)=paste("h=", 1:h.max, sep="") #Sets column names

RFTune.e = rbind(rmse.Tune,mae.Tune)
colnames(RFTune.e)=paste("h=", 1:h.max, sep="") #Sets column names

Bag.e = rbind(rmse.bag,mae.bag)
colnames(Bag.e)=paste("h=", 1:h.max, sep="") #Sets column names

#Prints RMSE and MAE data
print(round(RF.e,2))

print(round(RFTune.e,2))

print(round(Bag.e,2))

