library(readxl)
library(ggplot2)

#Vendite_READY <- read_excel("GitHub/Advanced_Microeconomics_Project/Tabellozze_ISTAT/Vendite_READY.xlsx")
Vendite_READY <- read_excel("Tabellozze_ISTAT/Vendite_READY.xlsx")


Vendite_READY$delta<-Vendite_READY$Alimentari-Vendite_READY$NAlimentare

ggplot(data=Vendite_READY,aes(x=Data,y=Alimentari))+geom_line(size=2,color = "red")+geom_line(size=2,aes(x=Data,y=NAlimentare),color = "blue")+
geom_line(size=2,color = "red")+geom_line(size=2,aes(x=Data,y=delta),color = "green")



did <-lm(delta ~  Time+ Time2+Time3, data = Vendite_READY)
summary(did)
plot(did)

