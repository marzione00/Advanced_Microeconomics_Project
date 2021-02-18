library(readxl)
library(ggplot2)
library(rdrobust)

Prezzi_gasolio <- read_excel("GitHub/Advanced_Microeconomics_Project/Altre_tabellozze/Prezzi_gasolio.xlsx")

#ciccio<-rdrobust(Azioni_amazon$Value,Azioni_amazon$WeekIndex)
#summary(ciccio)
#rdplot(Azioni_amazon$Value,Azioni_amazon$WeekIndex,kernel= 'uniform')



RDD<-lm(formula = Price ~ Time + D + I(Time*D), data = Prezzi_gasolio)
summary(RDD)

Prezzi_gasolio $pred<-predict(RDD)

ggplot(data=Prezzi_gasolio ,aes(x=Time,y=Price))+geom_line(size=1,color = "red")+geom_line(size=1,color = "blue",aes(x=Time,y=pred),linetype=2)