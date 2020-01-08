library(readr)
library(timetk)
library(tidyquant)
library(tibbletime)
library("rstudioapi")

current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path))
print(getwd())




#Raw Data
gdp = ipeadata_monthly_import_csv("PIB.csv")
FA = ipeadata_monthly_import_csv("fin_acc.csv")
BK = import_monthly_bcb("Y_BK.csv")
BCons = import_monthly_bcb("Y_BCons.csv")
Ibov = ipeadata_monthly_import_csv("retorno_ibovespa.csv")
energia = ipeadata_monthly_import_csv("consumo_energia.csv")
monetary_base = ipeadata_monthly_import_csv("monetary_base.csv")
nominal_er = ipeadata_monthly_import_csv("exchange_rate.csv")
selic = ipeadata_monthly_import_csv("Selic.csv")
ipca = ipeadata_monthly_import_csv("IPCA.csv")
inpc = ipeadata_monthly_import_csv("INPC.csv")
TC = ipeadata_monthly_import_csv("Saldo_TC.csv")
BI = import_monthly_bcb("Y_BI.csv")
EB = ipeadata_monthly_import_csv("employes_balance.csv")
CU = import_monthly_bcb("capac_util.csv")
MW = ipeadata_monthly_import_csv("min_wage.csv")
AI = import_monthly_bcb("Agriculture_Index.csv")
MI = import_monthly_bcb("Metal_Index.csv")
M1 = ipeadata_monthly_import_csv("M1.csv")
Debt = ipeadata_monthly_import_csv("divida.csv")


  
raw_data = Reduce(function(x, y) merge(x, y, by="Date"), list(gdp, FA, BK, BCons, Ibov, energia, monetary_base,
                                                         nominal_er,selic,ipca,inpc,TC,BI,EB,CU,MW,AI,MI,M1,Debt))


colnames(raw_data) = c("Date", "PIB","FA",
                   "BK","BCons","Ibov", "Energia", "m_base","dollar",
                   "Selic","IPCA","INPC",
                   "TC","BI","EB","CU","MW","AI","MI","M1","Debt")

dates = raw_data$Date



#Inspecting Variables

# test = diff(log(raw_data$Debt),1)
# serie = raw_data$FA
# ts.plot(serie)
# #
# #
# # #Stationarity
# aTSA::adf.test(serie)
# #
# # #ACF
# acf(serie)






#Séries estacionarizadas
x10 = diff(log(raw_data$PIB),1)
x11 = diff(raw_data$FA,1)/abs(raw_data$FA[-length(raw_data$FA)])
x12 = diff(log(raw_data$BK),1)
x13 = diff(log(raw_data$BCons),1)
x14 = (raw_data$Ibov/100)[-1]
x15 = diff(log(raw_data$Energia),1)
x16 = diff(log(raw_data$m_base),1)
x17 = diff(log(raw_data$dollar),1)
x18 = diff(log(raw_data$Selic),1)
x19 = diff(log(raw_data$IPCA),1)
x20 = diff(log(raw_data$INPC),1)
x21 = diff(raw_data$TC,1)/abs(raw_data$TC[-length(raw_data$TC)])
x22 = diff(log(raw_data$BI),1)
x23 = (raw_data$EB/1000000)[-1]
x24 = diff(log(raw_data$CU),1)
x25 = diff(log(raw_data$MW),1)
x26 = diff(log(raw_data$AI),1)
x27 = diff(log(raw_data$MI),1)
x28 = diff(log(raw_data$M1),1)
x29 = diff(log(raw_data$Debt),1)




# x21 = diff(raw_data$debt,1)

#Ordem de integração (maxima)

oi = 1

dates = dates[-oi]


de_para_var = as.data.frame(cbind(x = c("x10","x11","x12","x13","x14","x15","x16","x17","x18","x19","x20","x21",
                                        "x22","x23","x24","x25","x26","x27","x28","x29"),
                            Var = c("PIB Mensal R$ (milhões)", "Fin Acc Balance (US$ milhoes)",
                                    "Produção de Bens de Capital (Indice Base 100)",
                                    "Produção de Bens de Consumo (Indice Base 100)",
                                    "Retorno Ibovespa (%a.m)",
                                    "Consumo Energia Elétrica Mensal (GWh)",
                                    "Base Monetaria(R$ milhoes) (Média Mensal)",
                                    "Cambio Nominal (Média Mensal)",
                                    "Selic(%a.m)","IPCA","INPC",
                                    "Saldo em Transações correntes US$ (milhoes) Saldo Mensal",
                                    "Produção de Bens Intermediarios (Indice Base 100)",
                                    "Saldo de Empregados mensal (milhoes pessoas)",
                                    "Utilização da Capacidade Instalada (industria)",
                                    "Salario Minimo Real","Agriculture Price Index",
                                    "Metal Price Index","M1 PMPP (R$ milhoes) Media dias uteis",
                                    "Divida Liq Setor Publico (%PIB)"),
                            Method = c("diff(log(x))", "diff(x)/(x_-1)","diff(log(x))",
                                       "diff(log(x))","/100","diff(log(x))",
                                       "diff(log(x))","diff(log(x))","diff(log(x))",
                                       "diff(log(x))","diff(log(x))",
                                       "diff(x)/(x_-1)","diff(log(x))"," /1000000",
                                       "diff(log(x))","diff(log(x))",
                                       "diff(log(x))","diff(log(x))",
                                       "diff(log(x))","diff(log(x))")))
