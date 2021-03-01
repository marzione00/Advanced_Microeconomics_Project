library(readxl)
Data <- read_excel("GitHub/Advanced_Microeconomics_Project/Progetto Micro/Testtt.xlsx")

Controllo_abbigliamento<-lm(Data$Abbigliamento[20:32]~Data$Time[20:32])
summary(Controllo_abbigliamento)

Controllo_calzature<-lm(Data$Calzature[20:32]~Data$Time[20:32])
summary(Controllo_calzature)

Controllo_Elettrodomestici<-lm(Data$Elettrodomestici[20:32]~Data$Time[20:32])
summary(Controllo_Elettrodomestici)

Controllo_Mobili<-lm(Data$Mobili[20:32]~Data$Time[20:32])
summary(Controllo_Mobili)

Controllo_Fotootica<-lm(Data$Fotoottica[20:32]~Data$Time[20:32])
summary(Controllo_Fotootica)

Controllo_Casalinghi<-lm(Data$Casalinghi[20:32]~Data$Time[20:32])
summary(Controllo_Casalinghi)

Controllo_Utilenseria<-lm(Data$Utilenseria[20:32]~Data$Time[20:32])
summary(Controllo_Utilenseria)

Controllo_Cartoleria<-lm(Data$Cartoleria[20:32]~Data$Time[20:32])
summary(Controllo_Cartoleria)

Controllo_Giocattoli<-lm(Data$Giocattoli[20:32]~Data$Time[20:32])
summary(Controllo_Giocattoli)



reg2<-lm(Data$Abbigliamento[20:36]~Data$T1[20:36]+Data$T2[20:36]+Data$T3[20:36])
summary(reg2)

reg3<-lm(Data$Calzature[20:36]~Data$T1[20:36]+Data$T2[20:36]+Data$T3[20:36])
summary(reg3)

reg4<-lm(Data$Elettrodomestici[20:36]~Data$T1[20:36]+Data$T2[20:36]+Data$T3[20:36])
summary(reg4)

reg5<-lm(Data$Mobili[20:36]~Data$T1[20:36]+Data$T2[20:36]+Data$T3[20:36])
summary(reg5)

reg6<-lm(Data$Casalinghi[20:36]~Data$T1[20:36]+Data$T2[20:36]+Data$T3[20:36])
summary(reg6)

reg7<-lm(Data$Fotoottica[20:36]~Data$T1[20:36]+Data$T2[20:36]+Data$T3[20:36])
summary(reg7)

reg8<-lm(Data$Utilenseria[20:36]~Data$T1[20:36]+Data$T2[20:36]+Data$T3[20:36])
summary(reg8)

reg9<-lm(Data$Cartoleria[20:36]~Data$T1[20:36]+Data$T2[20:36]+Data$T3[20:36])
summary(reg9)

reg10<-lm(Data$Giocattoli[20:36]~Data$T1[20:36]+Data$T2[20:36]+Data$T3[20:36])
summary(reg10)


