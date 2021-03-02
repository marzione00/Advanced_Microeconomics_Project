library(readxl)

Dati_Abbigliamento <- read_excel("Dati_destagionalizzati_DID.xlsx", sheet = "Abbigliamento")


Abbigliamento <-lm(Outcome ~  C + T1 + T2+T3 + C*T1 +C*T2+C*T3, data = Dati_Abbigliamento )
summary(Abbigliamento)

Abbigliamento_placebo <-lm(Outcome ~  C + T4 + T5+T6 + C*T4 +C*T5+C*T6, data = Dati_Abbigliamento )
summary(Abbigliamento_placebo)


Dati_Calzature <- read_excel("Dati_destagionalizzati_DID.xlsx", sheet = "Calzature")


Calzature <-lm(Outcome ~  C + T1 + T2+T3 + C*T1 +C*T2+C*T3, data = Dati_Calzature)
summary(Calzature)

Calzature_placebo <-lm(Outcome ~  C + T4 + T5+T6 + C*T4 +C*T5+C*T6, data = Dati_Calzature )
summary(Calzature_placebo)


Dati_Elettrodomestici <- read_excel("Dati_destagionalizzati_DID.xlsx", sheet = "Elettrodomestici")


Elettrodomestici <-lm(Outcome ~  C + T1 + T2+T3 + C*T1 +C*T2+C*T3, data = Dati_Elettrodomestici)
summary(Elettrodomestici)

Elettrodomestici_placebo <-lm(Outcome ~  C + T4 + T5+T6 + C*T4 +C*T5+C*T6, data = Dati_Elettrodomestici )
summary(Elettrodomestici_placebo)


Dati_Fotoottica <- read_excel("Dati_destagionalizzati_DID.xlsx", sheet = "Fotoottica")


Fotoottica  <-lm(Outcome ~  C + T1 + T2+T3 + C*T1 +C*T2+C*T3, data = Dati_Fotoottica )
summary(Fotoottica)

Fotoottica_placebo <-lm(Outcome ~  C + T4 + T5+T6 + C*T4 +C*T5+C*T6, data = Dati_Fotoottica )
summary(Fotoottica_placebo)