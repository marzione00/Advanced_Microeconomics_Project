library(readxl)
library(ggplot2)
require(reshape2)

#Vendite_READY <- read_excel("GitHub/Advanced_Microeconomics_Project/Tabellozze_ISTAT/Vendite_READY.xlsx")
Mobility_READY <- read_excel("Tabellozze_ISTAT/Google_mobility_data_MEAN_MONTH.xlsx")
Mobility_READY_plt <- read_excel("Tabellozze_ISTAT/Google_mobility_data_MEAN_MONTH_plt.xlsx")


#Vendite_READY$delta<-Vendite_READY$Alimentari-Vendite_READY$NAlimentare

Mobility_READY_plt <- melt(Mobility_READY_plt ,  id.vars = 'time', variable.name = 'Mobility_Data') 

ggplot(Mobility_READY_plt, aes(time,value)) + geom_line(aes(colour = Mobility_Data)) +scale_x_continuous(breaks = seq(0, 9, 1))





did <-lm(residential ~Time1+ Time2+Time3, data = Mobility_READY)
summary(did)

