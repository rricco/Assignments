library("rstudioapi")
library(DescTools)
library(reshape2)
# install.packages("gtools")
library(gtools)
library("gbm")

current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path))
source("Data.R")

n_features = 20




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

l.max = 18 #Max order 
h.max = 12 #Max number of forecasting steps ahead
y.f.boost = Ntrees = matrix(NA, nrow = nf, ncol = h.max) #Initialize a nf x h.max matrix

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
    fit = fitBoost(data = data.w[(1:(l.dw-h+1)),], h = h, l.max = l.max)
    
    
    y.f.boost[i,h] = exp(fit$y.h)*dff$PIB[i]
    
    Ntrees[i,h] = fit$ntrees
    #print((i-1)*h.max+h)
    setTxtProgressBar(pb, (i-1)*h.max+h)
  }
}
close(pb)

y.r = matrix(tail(raw_data$PIB,nf) , ncol=h.max, nrow = nf) #Gets realized data

#Computes RMSE(h) and MAE(h)
rmse = sqrt(colMeans((y.f.boost - y.r)^2, na.rm = T))
mae = colMeans(abs(y.f.boost - y.r), na.rm = T)


#Binds RMSE and MAE values
Boost.e = rbind(rmse,mae)
colnames(Boost.e)=paste("h=", 1:h.max, sep="") #Sets column names


#Prints RMSE and MAE data
print(round(Boost.e,2))


Boost.result = Boost.e

save(Boost.result,file=paste0("Boost_lag",l.max,".Rda"))

# load("RF_lag1.Rda")
