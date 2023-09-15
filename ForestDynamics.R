library(tidyverse)
arten <- c ("Fagus","Pinus","Picea","Tilia","Quercus","Acer","Carpinus","Betula","Fraxinus")
anfang <- c (44,4,10,4,2,13,6,4,13)
anfangs.baumzahl <- sum (anfang)

uebergang <- rbind(
  c(0.463,0.409,0.300,0.63,0.01,0.48,0.16,0.00,0.250),
  c(0.000,0.000,0.000,0.00,0.00,0.00,0.01,0.00,0.000),
  c(0.000,0.000,0.000,0.00,0.00,0.00,0.00,0.00,0.001),
  c(0.020,0.000,0.050,0.05,0.00,0.03,0.08,0.00,0.004),
  c(0.000,0.000,0.000,0.00,0.00,0.00,0.00,0.00,0.000),
  c(0.031,0.108,0.305,0.28,0.16,0.04,0.13,0.00,0.090),
  c(0.000,0.082,0.005,0.01,0.34,0.08,0.13,0.00,0.003),
  c(0.000,0.000,0.000,0.00,0.00,0.00,0.00,0.00,0.000),
  c(0.486,0.401,0.340,0.06,0.49,0.37,0.49,0.00,0.650))

n = 20		                               # Anzahl Generationen (Zeitraum der Simulation)
k = length (anfang)	                       # Anzahl Baumarten
zusammensetzung <- matrix(nrow=n,ncol=k)   # Matrix mit Zustandsvektoren

zusammensetzung [1,] <- anfang            # 1.Zeile der Matrix wird mit Anfangsbestand der Kronenbäume gefüllt
for (i in 1:19) { 
  verjuengung <- uebergang %*% zusammensetzung[i,]  #in jedem Schritt wird die Anfangszahl der Kronenbäume mit den Uebergangswahrscheinlichkeiten multipliziert 
  verjuengungs.baumzahl <- sum (verjuengung)        #Anzahl der Kronenbäume in der neuen Generation werden zusammengezählt
  zusammensetzung [i+1,] <- verjuengung * (anfangs.baumzahl/verjuengungs.baumzahl) } #die Anzahl Kronenbäuume in der nächsten Generation wird so skaliert, dass die Summe der Kronnenbäume gleich bleibt. 

#die Matrix "zusammensetzung" enthält nun die Resultate nach jedem Verjuengungsschritt=Generationenschritt: in jede Zeile [i+1,] kommt die neuen Anzahl Kronenbäume pro Art


colnames(zusammensetzung) <-arten
zusammensetzung <- data.frame(zusammensetzung)
zusammensetzung <-bind_cols(zusammensetzung,Generation=c(1:20))
zusammensetzung <- zusammensetzung %>% gather(key=Arten,value=Anzahl,Fagus,Pinus,Picea, Tilia, Quercus, Acer,Carpinus,  Betula, Fraxinus ,-Generation) # wir machen wieder das long format!
zusammensetzung
zusammensetzung$Arten <- as.factor(zusammensetzung$Arten)

m1<- ggplot(zusammensetzung, aes(x=Generation, y=Anzahl,group=Arten,color=Arten)) + 
  geom_line()+
  geom_point() + 
  labs(title="Einfaches Modell ")
m1

