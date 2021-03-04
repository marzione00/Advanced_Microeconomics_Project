library(readxl)
library(ggplot2)
Data <- read_excel("Dati_destagionalizzati_FULL.xlsx")

ggplot(data=Data,aes(x=Time,y=Alimentari))+geom_line(size=2,color = "red")+geom_line(size=2,aes(x=Time,y=Abbigliamento),color = "blue")+ 
  labs(title="Abbigliamento")+  geom_vline(xintercept = 1, linetype="dashed", color = "orange", size=1)+ 
  geom_vline(xintercept = 2, linetype="dashed", color = "red", size=1)+
  geom_vline(xintercept = 3, linetype="dashed", color = "orange", size=1)+ 
  geom_vline(xintercept = 4, linetype="dashed", color = "darkgreen", size=1)+theme_bw()

ggplot(data=Data,aes(x=Time,y=Alimentari))+geom_line(size=2,color = "red")+geom_line(size=2,aes(x=Time,y=Calzature),color = "darkgreen")+ 
  labs(title="Calzature")+  geom_vline(xintercept = 1, linetype="dashed", color = "orange", size=1)+ 
  geom_vline(xintercept = 2, linetype="dashed", color = "red", size=1)+
  geom_vline(xintercept = 3, linetype="dashed", color = "orange", size=1)+ 
  geom_vline(xintercept = 4, linetype="dashed", color = "darkgreen", size=1)+theme_bw()

ggplot(data=Data,aes(x=Time,y=Alimentari))+geom_line(size=2,color = "red")+geom_line(size=2,aes(x=Time,y=Elettrodomestici),color = "orange")+ 
  labs(title="Elettrodomestici") +  geom_vline(xintercept = 1, linetype="dashed", color = "orange", size=1)+ 
  geom_vline(xintercept = 2, linetype="dashed", color = "red", size=1)+
  geom_vline(xintercept = 3, linetype="dashed", color = "orange", size=1)+ 
  geom_vline(xintercept = 4, linetype="dashed", color = "darkgreen", size=1)+theme_bw()


ggplot(data=Data,aes(x=Time,y=Alimentari))+geom_line(size=2,color = "red")+geom_line(size=2,aes(x=Time,y=Mobili),color = "purple")+ 
  labs(title="Mobili")+  geom_vline(xintercept = 1, linetype="dashed", color = "orange", size=1)+ 
  geom_vline(xintercept = 2, linetype="dashed", color = "red", size=1)+
  geom_vline(xintercept = 3, linetype="dashed", color = "orange", size=1)+ 
  geom_vline(xintercept = 4, linetype="dashed", color = "darkgreen", size=1)+theme_bw()

ggplot(data=Data,aes(x=Time,y=Alimentari))+geom_line(size=2,color = "red")+geom_line(size=2,aes(x=Time,y=Fotoottica),color = "darkblue")+ 
  labs(title="Fotoottica") +  geom_vline(xintercept = 1, linetype="dashed", color = "orange", size=1)+ 
  geom_vline(xintercept = 2, linetype="dashed", color = "red", size=1)+
  geom_vline(xintercept = 3, linetype="dashed", color = "orange", size=1)+ 
  geom_vline(xintercept = 4, linetype="dashed", color = "darkgreen", size=1)+theme_bw()

ggplot(data=Data,aes(x=Time,y=Alimentari))+geom_line(size=2,color = "red")+geom_line(size=2,aes(x=Time,y=Casalinghi),color = "cyan")+ 
  labs(title="Casalinghi") + geom_vline(xintercept=5, linetype="dashed", color = "black", size=1) + geom_vline(xintercept=0, linetype="dashed", color = "black", size=1)

ggplot(data=Data,aes(x=Time,y=Alimentari))+geom_line(size=2,color = "red")+geom_line(size=2,aes(x=Time,y=Utilenseria),color = "darkcyan")+ 
  labs(title="Utilenseria") + geom_vline(xintercept=5, linetype="dashed", color = "black", size=1) + geom_vline(xintercept=0, linetype="dashed", color = "black", size=1)

ggplot(data=Data,aes(x=Time,y=Alimentari))+geom_line(size=2,color = "red")+geom_line(size=2,aes(x=Time,y=Cartoleria),color = "black")+ 
  labs(title="Cartoleria") + geom_vline(xintercept=5, linetype="dashed", color = "black", size=1) + geom_vline(xintercept=0, linetype="dashed", color = "black", size=1)

ggplot(data=Data,aes(x=Time,y=Alimentari))+geom_line(size=2,color = "red")+geom_line(size=2,aes(x=Time,y=Giocattoli),color = "yellow")+ 
  labs(title="Giocattoli")+ geom_vline(xintercept=5, linetype="dashed", color = "black", size=1) + geom_vline(xintercept=0, linetype="dashed", color = "black", size=1)