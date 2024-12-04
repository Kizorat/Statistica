#seguirà l'analisi dei paesi cinvolti nell'indagine co annessi grafici

data<-read.csv("drug_consumption.csv",sep=",",header = T)

#ordino in base all'affluenza maggiore i paesi per poi farne un grafico a torta
Paesi<-ordered(data$Country,levels=c("-0.09765","0.24923","-0.46841","-0.28519","0.21128","0.96082","-0.57009"))
table(Paesi)
View(table(Paesi))

tablePaesi<-table(Paesi)

# grafico a barre
png("Paesi_bar_chart.png", width = 800, height = 800, res = 150)

barplot(tablePaesi,col =c("orange","green","blue","violet","red","yellow","grey"))

# grafico a torta
png("Paesi_pie_chart.png", width = 800, height = 800, res = 150)

pie(tablePaesi, col = c("orange","green","blue","violet","red","yellow","grey"),
    main = "Distribuzione delle fasce d'età")
