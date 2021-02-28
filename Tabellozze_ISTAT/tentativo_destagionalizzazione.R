library(readxl)
library(fpp)
library(ggplot2)

Destagionalizzazione <- read_excel("GitHub/Advanced_Microeconomics_Project/Progetto Micro/Destagionalizzazione.xlsx")


ts_beer = ts(Destagionalizzazione$alimentare,frequency = 12)


dec<-decompose(ts_beer,type="additive")

plot(dec)


ts_beer_2 = ts(Destagionalizzazione$`abbigliamento e pellicce`,frequency = 12)

dec_2<-decompose(ts_beer_2,type="additive")

plot(dec_2)


ts_beer_3 = ts(Destagionalizzazione$Calzature,frequency = 12)

dec_3<-decompose(ts_beer_2,type="additive")

plot(dec_3)


ts_beer_4 = ts(Destagionalizzazione$Elettrodomestici,frequency = 12)

dec_4<-decompose(ts_beer_4,type="additive")

plot(dec_4)


ts_beer_5 = ts(Destagionalizzazione$Mobili,frequency = 12)

dec_5<-decompose(ts_beer_5,type="additive")

plot(dec_5)

ts_beer_6 = ts(Destagionalizzazione$Informatica,frequency = 12)

dec_6<-decompose(ts_beer_6,type="additive")

plot(dec_6)

ts_beer_7 = ts(Destagionalizzazione$Fotoottica,frequency = 12)

dec_7<-decompose(ts_beer_7,type="additive")

plot(dec_7)

ts_beer_8 = ts(Destagionalizzazione$Casalinghi,frequency = 12)

dec_8<-decompose(ts_beer_8,type="additive")

plot(dec_8)

ts_beer_9 = ts(Destagionalizzazione$Utilenseria,frequency = 12)

dec_9<-decompose(ts_beer_9,type="additive")

plot(dec_9)

ts_beer_10 = ts(Destagionalizzazione$Profumeria,frequency = 12)

dec_10<-decompose(ts_beer_10,type="additive")

plot(dec_10)


ts_beer_11 = ts(Destagionalizzazione$Cartoleria,frequency = 12)

dec_11<-decompose(ts_beer_11,type="additive")

plot(dec_11)


ts_beer_12 = ts(Destagionalizzazione$Gioc,frequency = 12)

dec_12<-decompose(ts_beer_12,type="additive")

plot(dec_12)


dec_2$random[38:48]=0

peppo<-(dec_2$x-dec_2$seasonal-dec_2$random) - (dec$x-dec$seasonal-dec$random)

plot(peppo)


dec_3$random[38:48]=0

peppo2<-dec_3$trend-dec$trend

plot(as.ts(peppo2))


dec_4$random[38:48]=0

peppo3<-dec_4$trend-dec$trend

plot(as.ts(peppo3))


dec_5$random[38:48]=0

peppo4<-dec_5$trend-dec$trend

plot(as.ts(peppo4))


dec_6$random[38:48]=0

peppo5<-dec_6$trend-dec$trend

plot(as.ts(peppo5))


dec_7$random[38:48]=0

peppo6<-dec_7$trend-dec$trend

plot(as.ts(peppo6))


dec_8$random[38:48]=0

peppo7<-dec_8$trend-dec$trend

plot(as.ts(peppo7))


dec_9$random[38:48]=0

peppo8<-dec_9$trend-dec$trend

plot(as.ts(peppo8))


dec_10$random[38:48]=0

peppo9<-dec_10$trend-dec$trend

plot(as.ts(peppo9))



dec_11$random[38:48]=0

peppo10<-dec_11$trend-dec$trend

plot(as.ts(peppo10))


dec_12$random[38:48]=0

peppo11<-dec_12$trend-dec$trend

plot(as.ts(peppo11))






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









