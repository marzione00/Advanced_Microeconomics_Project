library(readxl)
library(ggplot2)

Vendite_READY <- read_excel("Vendite_READY.xlsx")
#Vendite_READY_test <- read_excel("/Users/MacBook/Documents/University of Milan/Progetto Micro/Vendite_READY_test.xlsx")
#install.packages("ggplot2")


Vendite_READY$delta<-Vendite_READY$Alimentari-Vendite_READY$NAlimentare
ggplot(data=Vendite_READY,aes(x=Data,y=Alimentari))+geom_line(size=2,color = "red")+geom_line(size=2,aes(x=Data,y=NAlimentare),color = "blue")+
  geom_line(size=2,color = "red")+geom_line(size=2,aes(x=Data,y=delta),color = "green")

did <-lm(Alimentari ~  C + Time + Time2+Time3 + C*Time +C*Time2+C*Time3, data = Vendite_READY_test)
summary(did)
#plot(did)


#Abbigliamento
Abbigliamento_READY <- read_excel("Abbigliamento_READY.xlsx")

Abbigliamento_READY$delta1<-Abbigliamento_READY$Alimentari-Abbigliamento_READY$Abbigliamento
ggplot(data=Abbigliamento_READY,aes(x=Data,y=Alimentari))+geom_line(size=2,color = "red")+geom_line(size=2,aes(x=Data,y=Abbigliamento),color = "blue")+
  geom_line(size=2,color = "red")+geom_line(size=2,aes(x=Data,y=delta1),color = "green")

Abbigliamento <- read_excel("Abbigliamento.xlsx")

did1 <-lm(Alimentari ~  C + Time + Time2+Time3 + C*Time +C*Time2+C*Time3, data = Abbigliamento)
summary(did1)



Abbigliamento_PLACEBO <- read_excel("Abbigliamento_PLACEBO.xlsx")

did2 <-lm(Alimentari ~  C + Time + Time2+Time3 + C*Time +C*Time2+C*Time3, data = Abbigliamento_PLACEBO  )
summary(did2)


Abbigliamento_READY_COMMON_TREND <- read_excel("Abbigliamento_COMMON_TREND.xlsx")

slope<-lm(Alimentari- Abbigliamento ~ Month, data = Abbigliamento_READY_COMMON_TREND )
summary(slope)

#plot(did1)

#Calzature
Calzature_READY <- read_excel("Calzature_READY.xlsx")
Calzature_READY$delta2<-Calzature_READY$Alimentari-Calzature_READY$Calzature
ggplot(data=Calzature_READY,aes(x=Data,y=Alimentari))+geom_line(size=2,color = "red")+geom_line(size=2,aes(x=Data,y=Calzature),color = "blue")+
  geom_line(size=2,color = "red")+geom_line(size=2,aes(x=Data,y=delta2),color = "green")

Calzature <- read_excel("Calzature.xlsx")

did2 <-lm(Alimentari ~  C + Time + Time2+Time3 + C*Time +C*Time2+C*Time3, data = Calzature)
summary(did2)



Calzature_PLACEBO <- read_excel("Calzature_PLACEBO.xlsx")

did2 <-lm(Alimentari ~  C + Time + Time2+Time3 + C*Time +C*Time2+C*Time3, data = Calzature_PLACEBO  )
summary(did2)


Calzature_READY_COMMON_TREND <- read_excel("Calzature_READY_COMMON_TREND.xlsx")

slope<-lm(Alimentari- Calzature ~ Month, data = Calzature_READY_COMMON_TREND )
summary(slope)



#plot(did2)

#Fotoottica
Fotoottica_READY <- read_excel("Fotoottica_READY.xlsx")
Fotoottica_READY$delta3<-Fotoottica_READY$Alimentari-Fotoottica_READY$Fotoottica
ggplot(data=Fotoottica_READY,aes(x=Data,y=Alimentari))+geom_line(size=2,color = "red")+geom_line(size=2,aes(x=Data,y=Fotoottica),color = "blue")+
  geom_line(size=2,color = "red")+geom_line(size=2,aes(x=Data,y=delta3),color = "green")



Fotoottica <- read_excel("Fotoottica.xlsx")
did3 <-lm(Alimentari ~  C + Time + Time2+Time3 + C*Time +C*Time2+C*Time3, data = Fotoottica)
summary(did3)


Fotoottica_PLACEBO <- read_excel("Fotoottica_PLACEBO.xlsx")

did2 <-lm(Alimentari ~  C + Time + Time2+Time3 + C*Time +C*Time2+C*Time3, data = Fotoottica_PLACEBO  )
summary(did2)


Fotoottica_READY_COMMON_TREND <- read_excel("Fotoottica_READY_COMMON_TREND.xlsx")

slope<-lm(Alimentari- Fotoottica ~ Month, data = Fotoottica_READY_COMMON_TREND )
summary(slope)




#plot(did3)

#Elettrodomestici
Elettrodomestici_READY <- read_excel("Elettrodomestici_READY.xlsx")
Elettrodomestici_READY$delta4<-Elettrodomestici_READY$Alimentari-Elettrodomestici_READY$Elettrodomestici
ggplot(data=Elettrodomestici_READY,aes(x=Data,y=Alimentari))+geom_line(size=2,color = "red")+geom_line(size=2,aes(x=Data,y=Elettrodomestici),color = "blue")+
  geom_line(size=2,color = "red")+geom_line(size=2,aes(x=Data,y=delta4),color = "green")

Elettrodomestici <- read_excel("Elettrodomestici.xlsx")
did4 <-lm(Alimentari ~  C + Time + Time2+Time3 + C*Time +C*Time2+C*Time3, data = Elettrodomestici)
summary(did4)

Elettrodomestici_PLACEBO <- read_excel("Elettrodomestici_PLACEBO.xlsx")

did2 <-lm(Alimentari ~  C + Time + Time2+Time3 + C*Time +C*Time2+C*Time3, data = Elettrodomestici_PLACEBO  )
summary(did2)


Elettrodomestici_READY_COMMON_TREND <- read_excel("Elettrodomestici_READY_COMMON_TREND.xlsx")

slope<-lm(Alimentari- Elettrodomestici ~ Month, data = Elettrodomestici_READY_COMMON_TREND )
summary(slope)


#plot(did4)

#Mobili
Mobili_READY <- read_excel("Mobili_READY.xlsx")
Mobili_READY$delta5<-Mobili_READY$Alimentari-Mobili_READY$Mobili
ggplot(data=Mobili_READY,aes(x=Data,y=Alimentari))+geom_line(size=2,color = "red")+geom_line(size=2,aes(x=Data,y=Mobili),color = "blue")+
  geom_line(size=2,color = "red")+geom_line(size=2,aes(x=Data,y=delta5),color = "green")

Mobili<- read_excel("Mobili.xlsx")
did5 <-lm(Alimentari ~  C + Time + Time2+Time3 + C*Time +C*Time2+C*Time3, data = Mobili)
summary(did5)


Mobili_PLACEBO <- read_excel("Mobili_PLACEBO.xlsx")

did2 <-lm(Alimentari ~  C + Time + Time2+Time3 + C*Time +C*Time2+C*Time3, data = Mobili_PLACEBO  )
summary(did2)


Mobili_READY_COMMON_TREND <- read_excel("Mobili_READY_COMMON_TREND.xlsx")

slope<-lm(Alimentari- Mobili ~ Month, data = Mobili_READY_COMMON_TREND )
summary(slope)



#plot(did5)

#Giochi
Giochi_READY <- read_excel("Giochi_READY.xlsx")
Giochi_READY$delta6<-Giochi_READY$Alimentari-Giochi_READY$Giochi
ggplot(data=Giochi_READY,aes(x=Data,y=Alimentari))+geom_line(size=2,color = "red")+geom_line(size=2,aes(x=Data,y=Giochi),color = "blue")+
  geom_line(size=2,color = "red")+geom_line(size=2,aes(x=Data,y=delta6),color = "green")

Giochi <- read_excel("Giochi.xlsx")
did6 <-lm(Alimentari ~  C + Time + Time2+Time3 + C*Time +C*Time2+C*Time3, data = Giochi)
summary(did6)

Giochi_PLACEBO <- read_excel("Giochi_PLACEBO.xlsx")

did2 <-lm(Alimentari ~  C + Time + Time2+Time3 + C*Time +C*Time2+C*Time3, data = Giochi_PLACEBO  )
summary(did2)


Giochi_READY_COMMON_TREND <- read_excel("Giochi_READY_COMMON_TREND.xlsx")

slope<-lm(Alimentari- Giochi ~ Month, data = Giochi_READY_COMMON_TREND )
summary(slope)




#plot(did6)

#Cartoleria
Cartoleria_READY <- read_excel("Cartoleria_READY.xlsx")
Cartoleria_READY$delta7<-Cartoleria_READY$Alimentari-Cartoleria_READY$Cartoleria
ggplot(data=Cartoleria_READY,aes(x=Data,y=Alimentari))+geom_line(size=2,color = "red")+geom_line(size=2,aes(x=Data,y=Cartoleria),color = "blue")+
  geom_line(size=2,color = "red")+geom_line(size=2,aes(x=Data,y=delta7),color = "green")

Cartoleria <- read_excel("Cartoleria.xlsx")
did7 <-lm(Alimentari ~  C + Time + Time2+Time3 + C*Time +C*Time2+C*Time3, data =Cartoleria)
summary(did7)

did2 <-lm(Alimentari ~  C + Time + Time2+Time3 + C*Time +C*Time2+C*Time3, data = Cartoleria_PLACEBO  )
summary(did2)


Cartoleria_READY_COMMON_TREND <- read_excel("Cartoleria_READY_COMMON_TREND.xlsx")

slope<-lm(Alimentari- Cartoleria ~ Month, data = Cartoleria_READY_COMMON_TREND )
summary(slope)


#plot(did7)

#Altro
Altro_READY <- read_excel("/Users/MacBook/Documents/University of Milan/Progetto Micro/Altro_READY.xlsx")
Altro_READY$delta8<-Altro_READY$Alimentari-Altro_READY$Altro
ggplot(data=Altro_READY,aes(x=Data,y=Alimentari))+geom_line(size=2,color = "red")+geom_line(size=2,aes(x=Data,y=Altro),color = "blue")+
  geom_line(size=2,color = "red")+geom_line(size=2,aes(x=Data,y=delta8),color = "green")

Altro <- read_excel("/Users/MacBook/Documents/University of Milan/Progetto Micro/Altro.xlsx")
did8 <-lm(Alimentari ~  C + Time + Time2+Time3 + C*Time +C*Time2+C*Time3, data = Elettrodomestici)
summary(did8)
#plot(did8)

