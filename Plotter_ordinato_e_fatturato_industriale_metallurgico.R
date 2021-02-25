library(readxl)
library(ggplot2)

Fatturato <- read_excel("Tabellozze_ISTAT/Fatturato_industria.xlsx")

ggplot(data=Fatturato,aes(x=Time,y=Alimentare_CA))+geom_line(size=2,color = "red")+geom_line(size=2,aes(x=Time,y=Legno_e_carta_CC),color = "blue")+
  geom_line(size=2,aes(x=Time,y=Abbigliamento_CB),color = "green")+geom_line(size=2,aes(x=Time,y=Metallurgico_CH),color = "purple")+geom_line(size=2,aes(x=Time,y=Farmaceutico_CF),color = "orange")

ggplot(data=Fatturato,aes(x=Time,y=Chimica_CE))+geom_line(size=2,color = "red")+
  geom_line(size=2,aes(x=Time,y=Alimentare_CA),color = "green")




#F_Alimentari_vs_abbigliamento <- read_excel("Tabellozze_ISTAT/Fatturato_industria/Fatturato_industria_Alimentari_vs_abbigliamento.xlsx")

#F_Alimentari_vs_abbigliamento$delta<-F_Alimentari_vs_abbigliamento$Alimentari-F_Alimentari_vs_abbigliamento$Abbigliamento_CB 

#did_abbigliamento<-lm( delta ~ Time1 + Time2+Time3+Time0,data = F_Alimentari_vs_abbigliamento)
#summary(did_abbigliamento)


Fatturato_test <- read_excel("Tabellozze_ISTAT/Fatturato_industria/Fatturato_industria_Alimentari_vs_abbigliamento_COMMON_TREND.xlsx")

slope<-lm(Abbigliamento_CB-Alimentari ~ Time +Time0+Time00  , data = Fatturato_test )
summary(slope)



Fatturato_test_placebo <- read_excel("Tabellozze_ISTAT/Fatturato_industria/Fatturato_industria_Alimentari_vs_abbigliamento_placebo.xlsx")

Fatturato_test_placebo$delta<-Fatturato_test_placebo$Alimentari-Fatturato_test_placebo$Abbigliamento_CB 


did_abbigliamento_placebo<-lm( delta ~ Time1 + Time2+Time3+Time0,data = Fatturato_test_placebo)
summary(did_abbigliamento_placebo)



Fatturato_DID <- read_excel("Tabellozze_ISTAT/Fatturato_industria/Fatturato_industria_Alimentari_vs_metallurgico.xlsx")


did1 <-lm(Fatturato ~  C1+C2 +TimeA+Time1 + Time2+Time3 + C1*Time1 +C1*Time2+C1*Time3+C2*TimeA, data = Fatturato_DID)
summary(did1)