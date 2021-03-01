library(readxl)
library(xlsx)
library(fpp)
library(ggplot2)
library(zoo)


pallete = c('red', 'blue', 'green', 'orange')

Destagionalizzazione <- read_excel("Destagionalizzazione.xlsx")

TS_Alimentare = ts(Destagionalizzazione$alimentare,frequency = 12)

TS_Alimentare_dec<-decompose(TS_Alimentare,type="additive")
TS_Alimentare_dec$random[42:48]=0

TS_Alimentare_dec$trend=TS_Alimentare_dec$x-TS_Alimentare_dec$random-TS_Alimentare_dec$seasonal



autoplot(TS_Alimentare_dec)


TS_Abbigliamento = ts(Destagionalizzazione$`abbigliamento e pellicce`,frequency = 12)

TS_Abbigliamento_dec<-decompose(TS_Abbigliamento,type="additive")

#TS_Abbigliamento_dec$trend[30:48]=TS_Abbigliamento_dec$trend[30:48]+TS_Abbigliamento_dec$random[30:48]

TS_Abbigliamento_dec$random[30:48]=0

TS_Abbigliamento_dec$trend=TS_Abbigliamento_dec$x-TS_Abbigliamento_dec$random-TS_Abbigliamento_dec$seasonal

autoplot(TS_Abbigliamento_dec)


autoplot(TS_Abbigliamento_dec$trend)+ geom_line(aes(y=TS_Alimentare_dec$trend))



TS_Calzature = ts(Destagionalizzazione$Calzature,frequency = 12)

TS_Calzature_dec<-decompose(TS_Calzature,type="additive")

#TS_Calzature_dec$trend[30:48]=TS_Calzature_dec$trend[30:48]+TS_Calzature_dec$random[30:48]

TS_Calzature_dec$random[31:48]=0

TS_Calzature_dec$trend=TS_Calzature_dec$x-TS_Calzature_dec$random-TS_Calzature_dec$seasonal

autoplot(TS_Calzature_dec$trend)+ geom_line(aes(y=TS_Alimentare_dec$trend))

autoplot(TS_Calzature_dec)


TS_Elettrodomestici = ts(Destagionalizzazione$Elettrodomestici,frequency = 12)

TS_Elettrodomestici_dec<-decompose(TS_Elettrodomestici,type="additive")

#TS_Elettrodomestici_dec$trend[30:48]=TS_Elettrodomestici_dec$trend[30:48]+TS_Elettrodomestici_dec$random[30:48]

TS_Elettrodomestici_dec$random[30:48]=0

TS_Elettrodomestici_dec$trend=TS_Elettrodomestici_dec$x-TS_Elettrodomestici_dec$random-TS_Elettrodomestici_dec$seasonal

autoplot(TS_Elettrodomestici_dec$trend)+ geom_line(aes(y=TS_Alimentare_dec$trend))

autoplot(TS_Elettrodomestici_dec)



TS_Mobili = ts(Destagionalizzazione$Mobili,frequency = 12)

TS_Mobili_dec<-decompose(TS_Mobili,type="additive")

#TS_Mobili_dec$trend[30:48]=TS_Mobili_dec$trend[30:48]+TS_Mobili_dec$random[30:48]

TS_Mobili_dec$random[30:48]=0

TS_Mobili_dec$trend=TS_Mobili_dec$x-TS_Mobili_dec$random-TS_Mobili_dec$seasonal

autoplot(TS_Mobili_dec$trend)+ geom_line(aes(y=TS_Alimentare_dec$trend))

autoplot(TS_Mobili_dec)





TS_Informatica = ts(Destagionalizzazione$Informatica,frequency = 12)

TS_Informatica_dec<-decompose(TS_Informatica,type="additive")

#TS_Informatica_dec$trend[30:48]=TS_Informatica_dec$trend[30:48]+TS_Informatica_dec$random[30:48]

TS_Informatica_dec$random[30:48]=0

TS_Informatica_dec$trend=TS_Informatica_dec$x-TS_Informatica_dec$random-TS_Informatica_dec$seasonal

autoplot(TS_Informatica_dec$trend)+ geom_line(aes(y=TS_Alimentare_dec$trend))

autoplot(TS_Informatica_dec)




TS_Fotoottica = ts(Destagionalizzazione$Fotoottica,frequency = 12)

TS_Fotoottica_dec<-decompose(TS_Fotoottica,type="additive")

#TS_Fotoottica_dec$trend[30:48]=TS_Fotoottica_dec$trend[30:48]+TS_Fotoottica_dec$random[30:48]

TS_Fotoottica_dec$random[30:48]=0

TS_Fotoottica_dec$trend=TS_Fotoottica_dec$x-TS_Fotoottica_dec$random-TS_Fotoottica_dec$seasonal

autoplot(TS_Fotoottica_dec$trend)+ geom_line(aes(y=TS_Alimentare_dec$trend))

autoplot(TS_Fotoottica_dec)



TS_Casalinghi = ts(Destagionalizzazione$Casalinghi,frequency = 12)

TS_Casalinghi_dec<-decompose(TS_Casalinghi,type="additive")

TS_Casalinghi_dec$trend[30:48]=TS_Casalinghi_dec$trend[30:48]+TS_Casalinghi_dec$random[30:48]

TS_Casalinghi_dec$random[30:48]=0

TS_Casalinghi_dec$trend=TS_Casalinghi_dec$x-TS_Casalinghi_dec$random-TS_Casalinghi_dec$seasonal

autoplot(TS_Casalinghi_dec$trend)+ geom_line(aes(y=TS_Alimentare_dec$trend))

autoplot(TS_Casalinghi_dec)



TS_Utilenseria = ts(Destagionalizzazione$Utilenseria,frequency = 12)

TS_Utilenseria_dec<-decompose(TS_Utilenseria,type="additive")

TS_Utilenseria_dec$trend[30:48]=TS_Utilenseria_dec$trend[30:48]+TS_Utilenseria_dec$random[30:48]

TS_Utilenseria_dec$random[30:48]=0

TS_Utilenseria_dec$trend=TS_Utilenseria_dec$x-TS_Utilenseria_dec$random-TS_Utilenseria_dec$seasonal

autoplot(TS_Utilenseria_dec$trend)+ geom_line(aes(y=TS_Alimentare_dec$trend))

autoplot(TS_Utilenseria_dec)




TS_Cartoleria = ts(Destagionalizzazione$Cartoleria,frequency = 12)

TS_Cartoleria_dec<-decompose(TS_Utilenseria,type="additive")

TS_Cartoleria_dec$trend[30:48]=TS_Cartoleria_dec$trend[30:48]+TS_Cartoleria_dec$random[30:48]

TS_Cartoleria_dec$random[30:48]=0

TS_Cartoleria_dec$trend=TS_Cartoleria_dec$x-TS_Cartoleria_dec$random-TS_Cartoleria_dec$seasonal

autoplot(TS_Cartoleria_dec$trend)+ geom_line(aes(y=TS_Alimentare_dec$trend))

autoplot(TS_Cartoleria_dec)





TS_Giocattoli = ts(Destagionalizzazione$Giocattoli,frequency = 12)

TS_Giocattoli_dec<-decompose(TS_Giocattoli,type="additive")

TS_Giocattoli_dec$trend[30:48]=TS_Giocattoli_dec$trend[30:48]+TS_Giocattoli_dec$random[30:48]

TS_Giocattoli_dec$random[30:48]=0

autoplot(TS_Giocattoli_dec)



Abbigliamento_vs_Alimentare<-(TS_Abbigliamento_dec$trend) - (TS_Alimentare_dec$trend)

autoplot(Abbigliamento_vs_Alimentare)

write.xlsx(Abbigliamento_vs_Alimentare, file = "Vendite_destag_outcome.xlsx",sheetName='Abbigliamento', append = FALSE)




Calzature_vs_Alimentare<-TS_Calzature_dec$trend-TS_Alimentare_dec$trend

autoplot(Calzature_vs_Alimentare)

write.xlsx(Calzature_vs_Alimentare, file = "Vendite_destag_outcome.xlsx",sheetName='Calzature', append = TRUE)



Elettrodomestici_vs_Alimentare<-TS_Elettrodomestici_dec$trend-TS_Alimentare_dec$trend

autoplot(Elettrodomestici_vs_Alimentare)

write.xlsx(Elettrodomestici_vs_Alimentare, file = "Vendite_destag_outcome.xlsx",sheetName='Elettrodomestici', append = TRUE)


Mobili_vs_Alimentare<-TS_Mobili_dec$trend-TS_Alimentare_dec$trend

autoplot(Mobili_vs_Alimentare)

write.xlsx(Mobili_vs_Alimentare, file = "Vendite_destag_outcome.xlsx",sheetName='Mobili', append = TRUE)


Fotoottica_vs_Alimentare<-TS_Fotoottica_dec$trend-TS_Alimentare_dec$trend

autoplot(Fotoottica_vs_Alimentare)

write.xlsx(Fotoottica_vs_Alimentare, file = "Vendite_destag_outcome.xlsx",sheetName='Fotoottica', append = TRUE)


Casalinghi_vs_Alimentare<-TS_Casalinghi_dec$trend-TS_Alimentare_dec$trend

autoplot(Casalinghi_vs_Alimentare)

write.xlsx(Casalinghi_vs_Alimentare, file = "Vendite_destag_outcome.xlsx",sheetName='Casalinghi', append = TRUE)





Utilenseria_vs_Alimentare<-TS_Utilenseria_dec$trend-TS_Alimentare_dec$trend

autoplot(Utilenseria_vs_Alimentare)

write.xlsx(Utilenseria_vs_Alimentare, file = "Vendite_destag_outcome.xlsx",sheetName='Utilenseria', append = TRUE)




Cartoleria_vs_Alimentare<-TS_Cartoleria_dec$trend-TS_Alimentare_dec$trend

autoplot(Cartoleria_vs_Alimentare)

write.xlsx(Cartoleria_vs_Alimentare, file = "Vendite_destag_outcome.xlsx",sheetName='Cartoleria', append = TRUE)




Giocattoli_vs_Alimentare<-TS_Giocattoli_dec$trend-TS_Alimentare_dec$trend

autoplot(Giocattoli_vs_Alimentare)

write.xlsx(Giocattoli_vs_Alimentare, file = "Vendite_destag_outcome.xlsx",sheetName='Giocattoli', append = TRUE)









plot(as.ts(decompose_beer$seasonal))
plot(as.ts(decompose_beer$trend))
plot(as.ts(decompose_beer$random))
plot(decompose_beer)




















ggplot(data=Produzione,aes(x=Data,y=Alimentari))+geom_line(size=2,color = "red")+geom_line(size=2,aes(x=Data,y=Legno_carta),color = "blue")+geom_line(size=2,aes(x=Data,y=Gomma),color = "green")+geom_line(size=2,aes(x=Data,y=Computer),color = "orange")+geom_line(size=2,aes(x=Data,y=ElettronicaM),color = "purple")+geom_line(size=2,aes(x=Data,y=Calzature),color = "darkgreen")+geom_line(size=2,aes(x=Data,y=Farmaceutici),color = "black")+ylim(0, 150)




test_Legno_carta<-lm(Farmaceutici[1:24]-Legno_carta[1:24] ~ Time[1:24],data=Produzione)
summary(test_Legno_carta)

test_Computer<-lm(Farmaceutici[1:24]-Computer[1:24] ~ Time[1:24],data=Produzione)
summary(test_Computer)


test_ElettronicaM<-lm(Farmaceutici[1:24]-ElettronicaM[1:24] ~ Time[13:24],data=Produzione)
summary(test_ElettronicaM)


test_NCA<-lm(Farmaceutici[1:24]-NCA[1:24] ~ Time[1:24],data=Produzione)
summary(test_NCA)


test_calzature<-lm(Farmaceutici[1:24]-Calzature[1:24] ~ Time[1:24],data=Produzione)
summary(test_calzature)


test_Legno_carta<-lm(Alimentari-Legno_carta ~ Time,data=Produzione)
summary(test_Legno_carta)

test_Computer<-lm(Alimentari[13:24]-Computer[13:24] ~ Time[13:24],data=Produzione)
summary(test_Computer)


test_ElettronicaM<-lm(Alimentari[13:24]-ElettronicaM[13:24] ~ Time[13:24],data=Produzione)
summary(test_ElettronicaM)


test_NCA<-lm(Alimentari[13:24]-NCA[13:24] ~ Time[13:24],data=Produzione)
summary(test_NCA)



test_calzature<-lm(Alimentari[13:24]-Calzature[13:24] ~ Time[13:24],data=Produzione)
summary(test_calzature)









