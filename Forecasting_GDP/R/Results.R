library("rstudioapi")
library(xtable)
current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path))

RW = load("RW.Rda")
AR = load("AR.Rda")
rownames(RW.result) = c("rmse.RW","mae.RW")
rownames(AR.result) = c("rmse.AR","mae.AR")


load("RF_lag1.Rda")
load("Boost_lag1.Rda")

RF_lag1 = RF.result
Boost_lag1 = Boost.result
rownames(RF_lag1) = c("rmse.RF_lag1","mae.RF_lag1","rmse.RF_Tune_lag1","mae.RF_Tune_lag1","rmse.Bag_lag1","mae.Bag_lag1")
rownames(Boost_lag1) = c("rmse.Boost_lag1","mae.Boost_lag1")


load("RF_lag6.Rda")
load("Boost_lag6.Rda")

RF_lag6 = RF.result
Boost_lag6 = Boost.result

rownames(RF_lag6) = c("rmse.RF_lag6","mae.RF_lag6","rmse.RF_Tune_lag6","mae.RF_Tune_lag6","rmse.Bag_lag6","mae.Bag_lag6")
rownames(Boost_lag6) = c("rmse.Boost_lag6","mae.Boost_lag6")

load("RF_lag12.Rda")
load("Boost_lag12.Rda")

RF_lag12 = RF.result
Boost_lag12 = Boost.result

rownames(RF_lag12) = c("rmse.RF_lag12","mae.RF_lag12","rmse.RF_Tune_lag12","mae.RF_Tune_lag12","rmse.Bag_lag12","mae.Bag_lag12")
rownames(Boost_lag12) = c("rmse.Boost_lag12","mae.Boost_lag12")


Result = rbind(RW.result,AR.result,RF_lag1,RF_lag6,RF_lag12,Boost_lag1,Boost_lag6,Boost_lag12)

Result = as.matrix(Result)

ffff = as.vector(apply(Result, 1, mean))

Result = cbind(Result,ffff)

colnames(Result)[13] = "Average"



rmse_rw = Result[1,]
mae_rw = Result[2,]


raw_rmse = Result[seq(nrow(Result)) %% 2 != 0,]
raw_mae =  Result[seq(nrow(Result)) %% 2 == 0,]

final_rmse = matrix(NA,nrow = nrow(raw_rmse),ncol = ncol(raw_rmse))
for(i in 1:nrow(raw_rmse)){
  
  final_rmse[i,] = raw_rmse[i,]/rmse_rw
  
}

colnames(final_rmse) = colnames(raw_rmse)
rownames(final_rmse) = rownames(raw_rmse)

final_mae = matrix(NA,nrow = nrow(raw_mae),ncol = ncol(raw_mae))
for(i in 1:nrow(raw_mae)){
  
  final_mae[i,] = raw_mae[i,]/mae_rw
  
}

colnames(final_mae) = colnames(raw_mae)
rownames(final_mae) = rownames(raw_mae)

final_rmse = as.data.frame(final_rmse)
final_mae = as.data.frame(final_mae)


xtable(final_rmse, type = "latex", file = "filename1.tex")
xtable(final_mae, type = "latex", file = "filename2.tex")




