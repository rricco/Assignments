fitBoost = function(data, h, l.max){
  
  
  
  
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
  
  fit=gbm.fit(x=(x.aux), y=(y.aux), offset = NULL, misc = NULL, distribution = "gaussian",
              w = NULL, var.monotone = NULL, n.trees = 500,#trees[h],
              interaction.depth = 1, n.minobsinnode = 1, shrinkage = 0.01,
              bag.fraction = 0.5, nTrain = NULL, train.fraction = NULL,
              keep.data = TRUE, verbose = TRUE, var.names = NULL,
              response.name = "y", group = NULL)
  

  
  best.iter <- gbm.perf(fit, method = "OOB",plot.it = F)
  
  
  aux5 = as.data.frame(aux4)
  y.h = predict(fit, newdata = matrix(aux5,nrow=1), n.trees = best.iter, type = "link")
  

  
  return(list("y.h"= y.h, "ntrees" = best.iter))
}
