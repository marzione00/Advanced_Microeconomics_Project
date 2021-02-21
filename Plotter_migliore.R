library(readxl)
library(ggplot2)

#Vendite_READY <- read_excel("GitHub/Advanced_Microeconomics_Project/Tabellozze_ISTAT/Vendite_READY.xlsx")
Vendite_READY <- read_excel("Tabellozze_ISTAT/Vendite_READY.xlsx")
Vendite_READY_test <- read_excel("Tabellozze_ISTAT/Vendite_READY_test.xlsx")


#Vendite_READY$delta<-Vendite_READY$Alimentari-Vendite_READY$NAlimentare

ggplot(data=Vendite_READY,aes(x=Data,y=Alimentari))+geom_line(size=2,color = "red")+geom_line(size=2,aes(x=Data,y=NAlimentare),color = "blue")+
geom_line(size=2,color = "red")

#did<-lm( delta ~ Time + Time2+Time3,data = Vendite_READY)

did2 <-lm(Alimentari ~  C + Time + Time2+Time3 + C*Time +C*Time2+C*Time3, data = Vendite_READY_test)
summary(did2)
#ummary(did)
#plot(did)

Vendite_READY_test_placebo <- read_excel("Tabellozze_ISTAT/Vendite_READY_test_placebo.xlsx")

did2 <-lm(Alimentari ~  C + Time + Time2+Time3 + C*Time +C*Time2+C*Time3, data = Vendite_READY_test_placebo )
summary(did2)


Vendite_READY_common_trend_test <- read_excel("Tabellozze_ISTAT/Vendite_READY_common_trend_test.xlsx")

slope_alimentari<-lm(Alimentari- NAlimentare ~ Month, data = Vendite_READY_common_trend_test )
summary(slope_alimentari)




