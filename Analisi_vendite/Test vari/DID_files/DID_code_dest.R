library(readxl)

Dati_Abbigliamento <- read_excel("Dati_destagionalizzati_DID.xlsx", sheet = "Abbigliamento")
Dati_Abbigliamento_long <- read_excel("Dati_destagionalizzati_DID_full.xlsx", sheet = "Abbigliamento")
Dati_Abbigliamento_phantom <- read_excel("Dati_destagionalizzati_DID_phantom.xlsx", sheet = "Abbigliamento")


Abbigliamento <-lm(Outcome ~  C + T1 + T2+T3 + C*T1 +C*T2+C*T3, data = Dati_Abbigliamento )
summary(Abbigliamento)


Abbigliamento_long <-lm(Outcome ~  C + T1 + T2+T3 + C*T1 +C*T2+C*T3, data = Dati_Abbigliamento_long )
summary(Abbigliamento_long)

Abbigliamento_phantom <-lm(Outcome ~  C + T1 + T2+T3 + C*T1 +C*T2+C*T3, data = Dati_Abbigliamento_phantom )
summary(Abbigliamento_phantom)

Abbigliamento_placebo <-lm(Outcome ~  C + T4 + T5+T6 + C*T4 +C*T5+C*T6, data = Dati_Abbigliamento )
summary(Abbigliamento_placebo)


Dati_Calzature <- read_excel("Dati_destagionalizzati_DID.xlsx", sheet = "Calzature")
Dati_Calzature_long <- read_excel("Dati_destagionalizzati_DID_full.xlsx", sheet = "Calzature")
Dati_Calzature_phantom <- read_excel("Dati_destagionalizzati_DID_phantom.xlsx", sheet = "Calzature")

Calzature <-lm(Outcome ~  C + T1 + T2+T3 + C*T1 +C*T2+C*T3, data = Dati_Calzature)
summary(Calzature)


Calzature_long <-lm(Outcome ~  C + T1 + T2+T3 + C*T1 +C*T2+C*T3, data = Dati_Calzature_long)
summary(Calzature_long)

Calzature_phantom <-lm(Outcome ~  C + T1 + T2+T3 + C*T1 +C*T2+C*T3, data = Dati_Calzature_phantom)
summary(Calzature_phantom)

Calzature_placebo <-lm(Outcome ~  C + T4 + T5+T6 + C*T4 +C*T5+C*T6, data = Dati_Calzature )
summary(Calzature_placebo)


Dati_Elettrodomestici <- read_excel("Dati_destagionalizzati_DID.xlsx", sheet = "Elettrodomestici")
Dati_Elettrodomestici_long <- read_excel("Dati_destagionalizzati_DID_full.xlsx", sheet = "Elettrodomestici")
Dati_Elettrodomestici_phantom <- read_excel("Dati_destagionalizzati_DID_phantom.xlsx", sheet = "Elettrodomestici")


Elettrodomestici <-lm(Outcome ~  C + T1 + T2+T3 + C*T1 +C*T2+C*T3, data = Dati_Elettrodomestici)
summary(Elettrodomestici)

Elettrodomestici_long <-lm(Outcome ~  C + T1 + T2+T3 + C*T1 +C*T2+C*T3, data = Dati_Elettrodomestici_long)
summary(Elettrodomestici_long)

Elettrodomestici_phantom <-lm(Outcome ~  C + T1 + T2+T3 + C*T1 +C*T2+C*T3, data = Dati_Elettrodomestici_phantom)
summary(Elettrodomestici_phantom)

Elettrodomestici_placebo <-lm(Outcome ~  C + T4 + T5+T6 + C*T4 +C*T5+C*T6, data = Dati_Elettrodomestici )
summary(Elettrodomestici_placebo)


Dati_Fotoottica <- read_excel("Dati_destagionalizzati_DID.xlsx", sheet = "Fotoottica")


Dati_Fotoottica_long <- read_excel("Dati_destagionalizzati_DID_full.xlsx", sheet = "Fotoottica")

Dati_Fotoottica_phantom <- read_excel("Dati_destagionalizzati_DID_phantom.xlsx", sheet = "Fotoottica")


Fotoottica  <-lm(Outcome ~  C + T1 + T2+T3 + C*T1 +C*T2+C*T3, data = Dati_Fotoottica )
summary(Fotoottica)

Fotoottica_long  <-lm(Outcome ~  C + T1 + T2+T3 + C*T1 +C*T2+C*T3, data = Dati_Fotoottica_long )
summary(Fotoottica_long)

Fotoottica_phantom  <-lm(Outcome ~  C + T1 + T2+T3 + C*T1 +C*T2+C*T3, data = Dati_Fotoottica_phantom )
summary(Fotoottica_phantom)

Fotoottica_placebo <-lm(Outcome ~  C + T4 + T5+T6 + C*T4 +C*T5+C*T6, data = Dati_Fotoottica )
summary(Fotoottica_placebo)


Dati_Casalinghi <- read_excel("Dati_destagionalizzati_DID.xlsx", sheet = "Casalinghi")
Dati_Casalinghi_phantom <- read_excel("Dati_destagionalizzati_DID_phantom.xlsx", sheet = "Casalinghi")


Casalinghi <-lm(Outcome ~  C + T1 + T2+T3 + C*T1 +C*T2+C*T3, data = Dati_Casalinghi )
summary(Casalinghi)

Casalinghi_phantom  <-lm(Outcome ~  C + T1 + T2+T3 + C*T1 +C*T2+C*T3, data = Dati_Casalinghi_phantom)
summary(Casalinghi_phantom)

Casalinghi_placebo <-lm(Outcome ~  C + T4 + T5+T6 + C*T4 +C*T5+C*T6, data = Dati_Casalinghi )
summary(Casalinghi_placebo)


Dati_Utilenseria <- read_excel("Dati_destagionalizzati_DID.xlsx", sheet = "Utilenseria")
Dati_Utilenseria_phantom <- read_excel("Dati_destagionalizzati_DID_phantom.xlsx", sheet = "Utilenseria")



Utilenseria <-lm(Outcome ~  C + T1 + T2+T3 + C*T1 +C*T2+C*T3, data = Dati_Utilenseria )
summary(Utilenseria)

Utilenseria_phantom<-lm(Outcome ~  C + T1 + T2+T3 + C*T1 +C*T2+C*T3, data = Dati_Utilenseria_phantom )
summary(Utilenseria_phantom)

Utilenseria_placebo <-lm(Outcome ~  C + T4 + T5+T6 + C*T4 +C*T5+C*T6, data = Dati_Utilenseria )
summary(Utilenseria_placebo)


Dati_Giocattoli <- read_excel("Dati_destagionalizzati_DID.xlsx", sheet = "Giocattoli")
Dati_Giocattoli_phantom <- read_excel("Dati_destagionalizzati_DID_phantom.xlsx", sheet = "Giocattoli")


Giocattoli <-lm(Outcome ~  C + T1 + T2+T3 + C*T1 +C*T2+C*T3, data = Dati_Giocattoli )
summary(Giocattoli)

Giocattoli_phantom <-lm(Outcome ~  C + T1 + T2+T3 + C*T1 +C*T2+C*T3, data = Dati_Giocattoli_phantom )
summary(Giocattoli_phantom)

Giocattoli_placebo <-lm(Outcome ~  C + T4 + T5+T6 + C*T4 +C*T5+C*T6, data = Dati_Giocattoli )
summary(Giocattoli_placebo)


