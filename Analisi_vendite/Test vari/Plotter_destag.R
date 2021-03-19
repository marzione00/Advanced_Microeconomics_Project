library(readxl)
library(ggplot2)
Data <- read_excel("Dati_destagionalizzati_FULL.xlsx")

ggplot(data=Data,aes(x=Time,y=Alimentari))+geom_line(size=2,color = "red")+geom_line(size=2,aes(x=Time,y=Abbigliamento),color = "blue")+ 
  geom_vline(xintercept = -4, linetype="dashed", color = "orange", size=1)+ 
  geom_vline(xintercept = -3, linetype="dashed", color = "red", size=1)+
  geom_vline(xintercept = -2, linetype="dashed", color = "orange", size=1)+ 
  geom_vline(xintercept = -1, linetype="dashed", color = "darkgreen", size=1)+ labs(y = "Selling value")+ labs(x = "Time (months)")+theme_bw(base_size = 18)


ggplot(data=Data,aes(x=Time,y=Alimentari))+geom_line(size=2,color = "red")+geom_line(size=2,aes(x=Time,y=Abbigliamento),color = "blue")+ 
  geom_vline(xintercept = 1, linetype="dashed", color = "orange", size=1)+ 
  geom_vline(xintercept = 2, linetype="dashed", color = "red", size=1)+
  geom_vline(xintercept = 3, linetype="dashed", color = "orange", size=1)+ 
  geom_vline(xintercept = 4, linetype="dashed", color = "darkgreen", size=1)+ labs(y = "Selling value")+ labs(x = "Time (months)")+theme_bw(base_size = 18)




ggplot(data=Data,aes(x=Time,y=Alimentari))+geom_line(size=2,color = "red")+geom_line(size=2,aes(x=Time,y=Calzature),color = "darkgreen")+ 
  geom_vline(xintercept = 1, linetype="dashed", color = "orange", size=1)+ 
  geom_vline(xintercept = 2, linetype="dashed", color = "red", size=1)+
  geom_vline(xintercept = 3, linetype="dashed", color = "orange", size=1)+ 
  geom_vline(xintercept = 4, linetype="dashed", color = "darkgreen", size=1)+ labs(y = "Selling value")+ labs(x = "Time (months)")+theme_bw(base_size = 18)

ggplot(data=Data,aes(x=Time,y=Alimentari))+geom_line(size=2,color = "red")+geom_line(size=2,aes(x=Time,y=Elettrodomestici),color = "orange")+ 
  geom_vline(xintercept = 1, linetype="dashed", color = "orange", size=1)+ 
  geom_vline(xintercept = 2, linetype="dashed", color = "red", size=1)+
  geom_vline(xintercept = 3, linetype="dashed", color = "orange", size=1)+ 
  geom_vline(xintercept = 4, linetype="dashed", color = "darkgreen", size=1)+ labs(y = "Selling value")+ labs(x = "Time (months)")+theme_bw(base_size = 18)


ggplot(data=Data,aes(x=Time,y=Alimentari))+geom_line(size=2,color = "red")+geom_line(size=2,aes(x=Time,y=Mobili),color = "purple")+ 
  geom_vline(xintercept = 1, linetype="dashed", color = "orange", size=1)+ 
  geom_vline(xintercept = 2, linetype="dashed", color = "red", size=1)+
  geom_vline(xintercept = 3, linetype="dashed", color = "orange", size=1)+ 
  geom_vline(xintercept = 4, linetype="dashed", color = "darkgreen", size=1)+theme_bw()+ labs(y = "Selling value")+ labs(x = "Time (months)")+theme_bw(base_size = 18)

ggplot(data=Data,aes(x=Time,y=Alimentari))+geom_line(size=2,color = "red")+geom_line(size=2,aes(x=Time,y=Fotoottica),color = "darkblue")+ 
  geom_vline(xintercept = 1, linetype="dashed", color = "orange", size=1)+ 
  geom_vline(xintercept = 2, linetype="dashed", color = "red", size=1)+
  geom_vline(xintercept = 3, linetype="dashed", color = "orange", size=1)+ 
  geom_vline(xintercept = 4, linetype="dashed", color = "darkgreen", size=1)+theme_bw()+ labs(y = "Selling value")+ labs(x = "Time (months)")+theme_bw(base_size = 18)

ggplot(data=Data,aes(x=Time,y=Alimentari))+geom_line(size=2,color = "red")+geom_line(size=2,aes(x=Time,y=Casalinghi),color = "purple")+ 
  geom_vline(xintercept = 1, linetype="dashed", color = "orange", size=1)+ 
  geom_vline(xintercept = 2, linetype="dashed", color = "red", size=1)+
  geom_vline(xintercept = 3, linetype="dashed", color = "orange", size=1)+ 
  geom_vline(xintercept = 4, linetype="dashed", color = "darkgreen", size=1)+theme_bw()+ labs(y = "Selling value")+ labs(x = "Time (months)")+theme_bw(base_size = 18)

ggplot(data=Data,aes(x=Time,y=Alimentari))+geom_line(size=2,color = "red")+geom_line(size=2,aes(x=Time,y=Utilenseria),color = "darkcyan")+ 
  geom_vline(xintercept = 1, linetype="dashed", color = "orange", size=1)+ 
  geom_vline(xintercept = 2, linetype="dashed", color = "red", size=1)+
  geom_vline(xintercept = 3, linetype="dashed", color = "orange", size=1)+ 
  geom_vline(xintercept = 4, linetype="dashed", color = "darkgreen", size=1)+theme_bw()+ labs(y = "Selling value")+ labs(x = "Time (months)")+theme_bw(base_size = 18)

ggplot(data=Data,aes(x=Time,y=Alimentari))+geom_line(size=2,color = "red")+geom_line(size=2,aes(x=Time,y=Cartoleria),color = "black")+ 
  geom_vline(xintercept = 1, linetype="dashed", color = "orange", size=1)+ 
  geom_vline(xintercept = 2, linetype="dashed", color = "red", size=1)+
  geom_vline(xintercept = 3, linetype="dashed", color = "orange", size=1)+ 
  geom_vline(xintercept = 4, linetype="dashed", color = "darkgreen", size=1)+ labs(y = "Selling value")+ labs(x = "Time (months)")+theme_bw(base_size = 18)


ggplot(data=Data,aes(x=Time,y=Alimentari))+geom_line(size=2,color = "red")+geom_line(size=2,aes(x=Time,y=Giocattoli),color = "Plum")+ 
  geom_vline(xintercept = 1, linetype="dashed", color = "orange", size=1)+ 
  geom_vline(xintercept = 2, linetype="dashed", color = "red", size=1)+
  geom_vline(xintercept = 3, linetype="dashed", color = "orange", size=1)+ 
  geom_vline(xintercept = 4, linetype="dashed", color = "darkgreen", size=1)+ labs(y = "Selling value")+ labs(x = "Time (months)")+theme_bw(base_size = 18)

