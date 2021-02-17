library(readxl)
library(ggplot2)

Vendite_READY <- read_excel("GitHub/Advanced_Microeconomics_Project/Tabellozze_ISTAT/Vendite_READY.xlsx")


ggplot(data=Vendite_READY,aes(x=Data,y=Alimentari))+geom_line(size=2,color = "red")+geom_line(size=2,aes(x=Data,y=NAlimentare),color = "blue")

didreg = lm(y ~ treated + time + did, data = mydata)
