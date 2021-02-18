library(readxl)
library(ggplot2)
library(rdrobust)

Consumi_benzina  <- read_excel("GitHub/Advanced_Microeconomics_Project/Altre_tabellozze/Consumi_benzina_F.xlsx")

#ciccio<-rdrobust(Azioni_amazon$Value,Azioni_amazon$WeekIndex)
#summary(ciccio)
#rdplot(Azioni_amazon$Value,Azioni_amazon$WeekIndex,kernel= 'uniform')



regression<-lm(formula = Consumption ~ Time + D, data = Consumi_benzina )
summary(regression)

Consumi_benzina$pred<-predict(regression)

ggplot(data=Consumi_benzina,aes(x=Time,y=Consumption))+geom_line(size=1,color = "red")+geom_line(size=1,color = "blue",aes(x=Time,y=pred),linetype=2)