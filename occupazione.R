library(readxl)
library(ggplot2)
Occupazione <- read_excel("GitHub/Advanced_Microeconomics_Project/Produzione/Occupazione.xlsx")

ggplot(data=Occupazione,aes(x=Time,y=(Ind-Det)/Ind))+geom_line(size=2,color = "red")+geom_line(size=2,aes(x=Time,y=log(Det)),color = "blue") 

test_Legno_carta<-lm(Ind[1:8]-Det[1:8] ~ Time[1:8],data=Occupazione)
summary(test_Legno_carta)
