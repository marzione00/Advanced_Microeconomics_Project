library(readxl)
library(ggplot2)

Dati_cassa_integrazione <- read_excel("GitHub/Advanced_Microeconomics_Project/Analisi_cassa_integrazione/Dati_cassa_integrazione.xlsx")


reg<-lm(Ore *3.28 /1000000000 ~ Time+ T1+T2+T3,data=Dati_cassa_integrazione)
summary(reg)

plot(Dati_cassa_integrazione$Ore*3.28 /1000000000 )