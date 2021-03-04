library(readxl)
library(ggplot2)

Dati_cassa_integrazione <- read_excel("Dati_cassa_integrazione.xlsx")
Dati_cassa_integrazione_Manifattura <- read_excel("Cassa_integrazione_Ordered.xlsx",sheet = "Manifattura")


ggplot(data=Dati_cassa_integrazione_Manifattura ,aes(x=Time,y=CIG_S))+geom_line(size=2,color = "red")+
geom_line(size=2,color = "blue",aes(x=Time,y=CIG_O))+
  geom_line(size=2,color = "purple",aes(x=Time,y=CIG_SL))+
  geom_vline(xintercept = 2, linetype="dashed", color = "red", size=1)+
  geom_vline(xintercept = 3, linetype="dashed", color = "orange", size=1)+ 
  geom_vline(xintercept = 4, linetype="dashed", color = "darkgreen", size=1)+
ggtitle("Ore CIG COVID (ord) vs non COVID (str1) (str2) - Manifattura")+
theme_bw() 

Dati_cassa_integrazione_Manifattura_DID <- read_excel("Cassa_integrazione_Ordered.xlsx",sheet = "Manifattura_DID")

did_Manifattura <-lm(Hours ~  C + T1 + T2+T3 + C*T1 +C*T2+C*T3, data = Dati_cassa_integrazione_Manifattura_DID)
summary(did_Manifattura)


Dati_cassa_integrazione_Commercio <- read_excel("Cassa_integrazione_Ordered.xlsx",sheet = "Commercio")


ggplot(data=Dati_cassa_integrazione_Commercio  ,aes(x=Time,y=CIG_S))+geom_line(size=2,color = "red")+
  geom_line(size=2,color = "blue",aes(x=Time,y=CIG_O))+
  geom_line(size=2,color = "purple",aes(x=Time,y=CIG_SL))+
  geom_vline(xintercept = 2, linetype="dashed", color = "red", size=1)+
  geom_vline(xintercept = 3, linetype="dashed", color = "orange", size=1)+ 
  geom_vline(xintercept = 4, linetype="dashed", color = "darkgreen", size=1)+
  ggtitle("Ore CIG COVID (ord) vs non COVID (str1) (str2) - Commercio")+
  theme_bw() 

Dati_cassa_integrazione_Commercio_DID <- read_excel("Cassa_integrazione_Ordered.xlsx",sheet = "Commercio_DID")

did_Commercio <-lm(Hours ~  C + T1 + T2+T3 + C*T1 +C*T2+C*T3, data = Dati_cassa_integrazione_Commercio_DID )
summary(did_Commercio)


Dati_cassa_integrazione_Costruzioni<- read_excel("Cassa_integrazione_Ordered.xlsx", sheet = "Costruzioni")


ggplot(data=Dati_cassa_integrazione_Costruzioni   ,aes(x=Time,y=CIG_S))+geom_line(size=2,color = "red")+
  geom_line(size=2,color = "blue",aes(x=Time,y=CIG_O))+
  geom_line(size=2,color = "purple",aes(x=Time,y=CIG_SL))+
  geom_vline(xintercept = 2, linetype="dashed", color = "red", size=1)+
  geom_vline(xintercept = 3, linetype="dashed", color = "orange", size=1)+ 
  geom_vline(xintercept = 4, linetype="dashed", color = "darkgreen", size=1)+
  ggtitle("Ore CIG COVID (ord) vs non COVID (str1) (str2) - Costruzioni ")+
  theme_bw() 

Dati_cassa_integrazione_Costruzioni_DID <- read_excel("Cassa_integrazione_Ordered.xlsx",sheet = "Costruzioni_DID")

did_Costruzioni <-lm(Hours ~  C + T1 + T2+T3 + C*T1 +C*T2+C*T3, data = Dati_cassa_integrazione_Costruzioni_DID )
summary(did_Costruzioni)


Dati_cassa_integrazione_Immobiliare<- read_excel("Cassa_integrazione_Ordered.xlsx", sheet = "Immobiliare")

ggplot(data=Dati_cassa_integrazione_Immobiliare   ,aes(x=Time,y=CIG_S))+geom_line(size=2,color = "red")+
  geom_line(size=2,color = "blue",aes(x=Time,y=CIG_O))+
  geom_line(size=2,color = "purple",aes(x=Time,y=CIG_SL))+
  geom_vline(xintercept = 2, linetype="dashed", color = "red", size=1)+
  geom_vline(xintercept = 3, linetype="dashed", color = "orange", size=1)+ 
  geom_vline(xintercept = 4, linetype="dashed", color = "darkgreen", size=1)+
  ggtitle("Ore CIG COVID (ord) vs non COVID (str1) (str2)  - Immob., Nol.,Info. etc. ")+
  theme_bw() 

Dati_cassa_integrazione_Immobiliare_DID <- read_excel("Cassa_integrazione_Ordered.xlsx",sheet = "Immobiliare_DID")

did_Immobiliare <-lm(Hours ~  C + T1 + T2+T3 + C*T1 +C*T2+C*T3, data = Dati_cassa_integrazione_Immobiliare_DID )
summary(did_Immobiliare)


Dati_cassa_integrazione_Full<- read_excel("Cassa_integrazione_Ordered.xlsx", sheet = "Full")

ggplot(data=Dati_cassa_integrazione_Full  ,aes(x=Time,y=CIG_S))+geom_line(size=2,color = "red")+
  geom_line(size=2,color = "blue",aes(x=Time,y=CIG_O))+
  geom_line(size=2,color = "purple",aes(x=Time,y=CIG_SL))+
  geom_vline(xintercept = 2, linetype="dashed", color = "red", size=1)+
  geom_vline(xintercept = 3, linetype="dashed", color = "orange", size=1)+ 
  geom_vline(xintercept = 4, linetype="dashed", color = "darkgreen", size=1)+
  ggtitle("Ore CIG COVID (ord) vs non COVID (str) - Full ")+
  theme_bw() 


Dati_cassa_integrazione_Full_DID <- read_excel("Cassa_integrazione_Ordered.xlsx",sheet = "Full_DID")

did_Full <-lm(Hours ~  C + T1 + T2+T3 + C*T1 +C*T2+C*T3, data = Dati_cassa_integrazione_Full_DID )
summary(did_Full)



reg<-lm(Ore *3.28 /1000000000 ~ Time+ T1+T2+T3,data=Dati_cassa_integrazione)
summary(reg)

plot(Dati_cassa_integrazione$Ore*3.28 /1000000000 )