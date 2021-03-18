library(readxl)
library(ggplot2)
Data <- read_excel("Dati_destagionalizzati.xlsx")
Data2 <- read_excel("Dati_destagionalizzati_FULL_2016.xlsx")

Controllo_abbigliamento<-lm(Data$Abbigliamento[1:19]-Data$Alimentari[1:19]~Data$Time[1:19])
summary(Controllo_abbigliamento)

Controllo_calzature<-lm(Data$Calzature[1:19]-Data$Alimentari[1:19]~Data$Time[1:19])
summary(Controllo_calzature)

Controllo_Elettrodomestici<-lm(Data$Elettrodomestici[1:19]-Data$Alimentari[1:19]~Data$Time[1:19])
summary(Controllo_Elettrodomestici)

Controllo_Mobili<-lm(Data$Mobili[1:19]-Data$Alimentari[1:19]~Data$Time[1:19])
summary(Controllo_Mobili)

Controllo_Fotootica<-lm(Data$Fotoottica[1:19]-Data$Alimentari[1:19]~Data$Time[1:19])
summary(Controllo_Fotootica)

Controllo_Casalinghi<-lm(Data$Casalinghi[1:19]-Data$Alimentari[1:19]~Data$Time[1:19])
summary(Controllo_Casalinghi)

Controllo_Utilenseria<-lm(Data$Utilenseria[1:19]-Data$Alimentari[1:19]~Data$Time[1:19])
summary(Controllo_Utilenseria)

Controllo_Cartoleria<-lm(Data$Cartoleria[1:19]-Data$Alimentari[1:19]~Data$Time[1:19])
summary(Controllo_Cartoleria)

Controllo_Giocattoli<-lm(Data$Giocattoli[1:19]-Data$Alimentari[1:19]~Data$Time[1:19])
summary(Controllo_Giocattoli)


reg2<-lm(Data$Abbigliamento-Data$Alimentari~Data$Time+Data$T1+Data$T2+Data$T3)
summary(reg2)

reg3<-lm(Data$Calzature-Data$Alimentari~Data$Time+Data$T1+Data$T2+Data$T3)
summary(reg3)

reg4<-lm(Data$Elettrodomestici-Data$Alimentari~Data$Time+Data$T1+Data$T2+Data$T3)
summary(reg4)

reg5<-lm(Data$Mobili-Data$Alimentari~Data$Time+Data$T1+Data$T2+Data$T3)
summary(reg5)

reg6<-lm(Data$Fotoottica-Data$Alimentari~Data$Time+Data$T1+Data$T2+Data$T3)
summary(reg6)

reg7<-lm(Data$Casalinghi-Data$Alimentari~Data$T1+Data$T2+Data$T3)
summary(reg7)

reg8<-lm(Data$Utilenseria-Data$Alimentari~Data$T1+Data$T2+Data$T3)
summary(reg8)

reg9<-lm(Data$Cartoleria-Data$Alimentari~Data$T1+Data$T2+Data$T3)
summary(reg9)

reg10<-lm(Data$Giocattoli-Data$Alimentari~Data$T1+Data$T2+Data$T3)
summary(reg10)



###########################PLACEBO_1#####################################


reg2<-lm(Data$Abbigliamento-Data$Alimentari~Data$Time+Data$T4+Data$T5+Data$T6)
summary(reg2)

reg3<-lm(Data$Calzature-Data$Alimentari~Data$Time+Data$T4+Data$T5+Data$T6)
summary(reg3)

reg4<-lm(Data$Elettrodomestici-Data$Alimentari~Data$Time+Data$T4+Data$T5+Data$T6)
summary(reg4)

reg5<-lm(Data$Mobili-Data$Alimentari~Data$Time+Data$T4+Data$T5+Data$T6)
summary(reg5)

reg6<-lm(Data$Fotoottica-Data$Alimentari~Data$Time+Data$T4+Data$T5+Data$T6)
summary(reg6)

reg7<-lm(Data$Casalinghi-Data$Alimentari~Data$Time+Data$T4+Data$T5+Data$T6)
summary(reg7)

reg8<-lm(Data$Utilenseria-Data$Alimentari~Data$Time+Data$T4+Data$T5+Data$T6)
summary(reg8)

reg9<-lm(Data$Cartoleria-Data$Alimentari~Data$Time+Data$T4+Data$T5+Data$T6)
summary(reg9)

reg10<-lm(Data$Giocattoli-Data$Alimentari~Data$Time+Data$T4+Data$T5+Data$T6)
summary(reg10)



###########################PLACEBO_2#####################################


reg2<-lm(Data$Abbigliamento-Data$Alimentari~Data$Time+Data$T7+Data$T8+Data$T9)
summary(reg2)

reg3<-lm(Data$Calzature-Data$Alimentari~Data$Time+Data$T7+Data$T8+Data$T9)
summary(reg3)

reg4<-lm(Data$Elettrodomestici-Data$Alimentari~Data$Time+Data$T7+Data$T8+Data$T9)
summary(reg4)

reg5<-lm(Data$Mobili-Data$Alimentari~Data$Time+Data$T7+Data$T8+Data$T9)
summary(reg5)

reg6<-lm(Data$Fotoottica-Data$Alimentari~Data$Time+Data$T7+Data$T8+Data$T9)
summary(reg6)

reg7<-lm(Data$Casalinghi-Data$Alimentari~Data$Time+Data$T7+Data$T8+Data$T9)
summary(reg7)

reg8<-lm(Data$Utilenseria-Data$Alimentari~Data$Time+Data$T7+Data$T8+Data$T9)
summary(reg8)

reg9<-lm(Data$Cartoleria-Data$Alimentari~Data$Time+Data$T7+Data$T8+Data$T9)
summary(reg9)

reg10<-lm(Data$Giocattoli-Data$Alimentari~Data$Time+Data$T7+Data$T8+Data$T9)
summary(reg10)











Data2

ggplot(data=Data,aes(x=Time,y=Alimentari))+geom_line(size=2,color = "red")+geom_line(size=2,aes(x=Time,y=Abbigliamento),color = "blue")+ 
  labs(title="Abbigliamento")

ggplot(data=Data2,aes(x=Time,y=Alimentari))+geom_line(size=2,color = "red")+geom_line(size=2,aes(x=Time,y=Abbigliamento),color = "blue")+ 
  labs(title="Abbigliamento")

ggplot(data=Data,aes(x=Time,y=Alimentari))+geom_line(size=2,color = "red")+geom_line(size=2,aes(x=Time,y=Calzature),color = "darkgreen")+ 
  labs(title="Calzature")

ggplot(data=Data2,aes(x=Time,y=Alimentari))+geom_line(size=2,color = "red")+geom_line(size=2,aes(x=Time,y=Calzature),color = "darkgreen")+ 
  labs(title="Calzature")

ggplot(data=Data,aes(x=Time,y=Alimentari))+geom_line(size=2,color = "red")+geom_line(size=2,aes(x=Time,y=Elettrodomestici),color = "orange")+ 
  labs(title="Elettrodomestici")

ggplot(data=Data2,aes(x=Time,y=Alimentari))+geom_line(size=2,color = "red")+geom_line(size=2,aes(x=Time,y=Elettrodomestici),color = "orange")+ 
  labs(title="Elettrodomestici")

ggplot(data=Data,aes(x=Time,y=Alimentari))+geom_line(size=2,color = "red")+geom_line(size=2,aes(x=Time,y=Mobili),color = "purple")+ 
  labs(title="Mobili")

ggplot(data=Data,aes(x=Time,y=Alimentari))+geom_line(size=2,color = "red")+geom_line(size=2,aes(x=Time,y=Fotoottica),color = "darkblue")+ 
  labs(title="Fotoottica")

ggplot(data=Data,aes(x=Time,y=Alimentari))+geom_line(size=2,color = "red")+geom_line(size=2,aes(x=Time,y=Casalinghi),color = "cyan")+ 
  labs(title="Casalinghi")

ggplot(data=Data2,aes(x=Time,y=Alimentari))+geom_line(size=2,color = "red")+geom_line(size=2,aes(x=Time,y=Casalinghi),color = "cyan")+ 
  labs(title="Casalinghi")

ggplot(data=Data,aes(x=Time,y=Alimentari))+geom_line(size=2,color = "red")+geom_line(size=2,aes(x=Time,y=Utilenseria),color = "darkcyan")+ 
  labs(title="Utilenseria")

ggplot(data=Data2,aes(x=Time,y=Alimentari))+geom_line(size=2,color = "red")+geom_line(size=2,aes(x=Time,y=Utilenseria),color = "darkcyan")+ 
  labs(title="Utilenseria")

ggplot(data=Data,aes(x=Time,y=Alimentari))+geom_line(size=2,color = "red")+geom_line(size=2,aes(x=Time,y=Cartoleria),color = "black")+ 
  labs(title="Cartoleria")

ggplot(data=Data,aes(x=Time,y=Alimentari))+geom_line(size=2,color = "red")+geom_line(size=2,aes(x=Time,y=Giocattoli),color = "yellow")+ 
  labs(title="Giocattoli")

ggplot(data=Data2,aes(x=Time,y=Alimentari))+geom_line(size=2,color = "red")+geom_line(size=2,aes(x=Time,y=Giocattoli),color = "yellow")+ 
  labs(title="Giocattoli")






