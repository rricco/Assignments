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
