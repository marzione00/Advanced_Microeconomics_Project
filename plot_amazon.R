library(readxl)
library(ggplot2)
library(rdrobust)

Azioni_amazon <- read_excel("GitHub/Advanced_Microeconomics_Project/Altre_tabellozze/Azioni_amazon.xlsx")

ggplot(data=Azioni_amazon,aes(x=WeekIndex,y=Value))+geom_line(size=2,color = "red")


#ciccio<-rdrobust(Azioni_amazon$Value,Azioni_amazon$WeekIndex)
#summary(ciccio)
#rdplot(Azioni_amazon$Value,Azioni_amazon$WeekIndex,kernel= 'uniform')



RDD<-lm(formula = Value ~ WeekIndex + D + I(WeekIndex*D), data = Azioni_amazon)
summary(RDD)

Azioni_amazon$pred<-predict(RDD)

ggplot(data=Azioni_amazon,aes(x=WeekIndex,y=Value))+geom_line(size=1,color = "red")+geom_line(size=1,color = "blue",aes(x=WeekIndex,y=pred),linetype=2)