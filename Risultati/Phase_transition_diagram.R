library(readxl)
library(ggplot2)

Phase_transition <- read_excel("Risultati/Phase_transition.xlsx")
ggplot(Phase_transition,aes(x=PM_attitute, y=Deaths),size=2,color = "red") + geom_line(color="red") +   geom_point(color="red") + labs(y = "Death ")+ labs(x = "PM attitute")+ theme_bw(base_size = 18) 