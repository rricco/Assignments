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
