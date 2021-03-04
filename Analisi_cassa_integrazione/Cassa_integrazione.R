library(readxl)
library(ggplot2)

Dati_cassa_integrazione <- read_excel("Dati_cassa_integrazione.xlsx")
Dati_cassa_integrazione_Manifattura <- read_excel("Cassa_integrazione_Ordered.xlsx",sheet = "Manifattura")
Dati_cassa_integrazione_Manifattura <- read_excel("Cassa_integrazione_Ordered.xlsx",sheet = "Manifattura")

ggplot(data=Dati_cassa_integrazione_Manifattura ,aes(x=Time,y=CIG_S))+geom_line(size=2,color = "red")+
geom_line(size=2,color = "blue",aes(x=Time,y=CIG_O))+
  geom_line(size=2,color = "purple",aes(x=Time,y=CIG_SL))+
  geom_vline(xintercept = 1, linetype="dashed", color = "orange", size=1)+ 
  geom_vline(xintercept = 2, linetype="dashed", color = "red", size=1)+
  geom_vline(xintercept = 3, linetype="dashed", color = "orange", size=1)+ 
  geom_vline(xintercept = 4, linetype="dashed", color = "darkgreen", size=1)+
ggtitle("Ore CIG COVID (ord) vs non COVID (str1) vs non COVID (str2) - Manifattura")+
theme_bw() 



Dati_cassa_integrazione_Commercio <- read_excel("Cassa_integrazione_Ordered.xlsx",sheet = "Commercio")


ggplot(data=Dati_cassa_integrazione_Commercio  ,aes(x=Time,y=CIG_S))+geom_line(size=2,color = "red")+
  geom_line(size=2,color = "blue",aes(x=Time,y=CIG_O))+
  geom_line(size=2,color = "purple",aes(x=Time,y=CIG_SL))+
  geom_vline(xintercept = 1, linetype="dashed", color = "orange", size=1)+ 
  geom_vline(xintercept = 2, linetype="dashed", color = "red", size=1)+
  geom_vline(xintercept = 3, linetype="dashed", color = "orange", size=1)+ 
  geom_vline(xintercept = 4, linetype="dashed", color = "darkgreen", size=1)+
  ggtitle("Ore CIG COVID (ord) vs non COVID (str1) vs non COVID (str2) - Commercio")+
  theme_bw() 


Dati_cassa_integrazione_Costruzioni<- read_excel("Cassa_integrazione_Ordered.xlsx", sheet = "Costruzioni")


ggplot(data=Dati_cassa_integrazione_Costruzioni   ,aes(x=Time,y=CIG_S))+geom_line(size=2,color = "red")+
  geom_line(size=2,color = "blue",aes(x=Time,y=CIG_O))+
  geom_line(size=2,color = "purple",aes(x=Time,y=CIG_SL))+
  geom_vline(xintercept = 1, linetype="dashed", color = "orange", size=1)+ 
  geom_vline(xintercept = 2, linetype="dashed", color = "red", size=1)+
  geom_vline(xintercept = 3, linetype="dashed", color = "orange", size=1)+ 
  geom_vline(xintercept = 4, linetype="dashed", color = "darkgreen", size=1)+
  ggtitle("Ore CIG COVID (ord) vs non COVID (str1) vs non COVID (str2) - Costruzioni ")+
  theme_bw() 


Dati_cassa_integrazione_Immobiliare<- read_excel("Cassa_integrazione_Ordered.xlsx", sheet = "Immobiliare")

ggplot(data=Dati_cassa_integrazione_Immobiliare   ,aes(x=Time,y=CIG_S))+geom_line(size=2,color = "red")+
  geom_line(size=2,color = "blue",aes(x=Time,y=CIG_O))+
  geom_line(size=2,color = "purple",aes(x=Time,y=CIG_SL))+
  geom_vline(xintercept = 1, linetype="dashed", color = "orange", size=1)+ 
  geom_vline(xintercept = 2, linetype="dashed", color = "red", size=1)+
  geom_vline(xintercept = 3, linetype="dashed", color = "orange", size=1)+ 
  geom_vline(xintercept = 4, linetype="dashed", color = "darkgreen", size=1)+
  ggtitle("Ore CIG COVID (ord) vs non COVID (str1) vs (str2)  - Immob., Nol.,Info. etc. ")+
  theme_bw() 


Dati_cassa_integrazione_Full<- read_excel("Cassa_integrazione_Ordered.xlsx", sheet = "Full")

ggplot(data=Dati_cassa_integrazione_Full  ,aes(x=Time,y=CIG_S))+geom_line(size=2,color = "red")+
  geom_line(size=2,color = "blue",aes(x=Time,y=CIG_O))+
  geom_line(size=2,color = "purple",aes(x=Time,y=CIG_SL))+
  geom_vline(xintercept = 1, linetype="dashed", color = "orange", size=1)+ 
  geom_vline(xintercept = 2, linetype="dashed", color = "red", size=1)+
  geom_vline(xintercept = 3, linetype="dashed", color = "orange", size=1)+ 
  geom_vline(xintercept = 4, linetype="dashed", color = "darkgreen", size=1)+
  ggtitle("Ore CIG COVID (ord) vs non COVID (str) - Full ")+
  theme_bw() 


reg<-lm(Ore *3.28 /1000000000 ~ Time+ T1+T2+T3,data=Dati_cassa_integrazione)
summary(reg)

plot(Dati_cassa_integrazione$Ore*3.28 /1000000000 )