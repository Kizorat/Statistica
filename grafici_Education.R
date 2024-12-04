#seguirà l'analisi dei paesi cinvolti nell'indagine co annessi grafici

data<-read.csv("drug_consumption.csv",sep=",",header = T)

#provo un altro metodo poichè il calcolo l'ho già fatto in età e paese
educazione<-c(rep("lasciata a <16",28),rep(" lasciata a 16 ",99),rep("lasciata a 17 ",30),rep("lasciata a 18 ",100),rep(),rep("non certificati/rinuncia",506),rep("diploma",270),rep("università",480),rep("master",283),rep("dottorato",89))
View(table(educazione))

#***********************************************************************************
#da aggiustare********************************************************
titolo<-ordered(educazione,levels=c("lasciata a meno di 16","lasciata a 16","lasciata a 17","lasciata a 18","nessun certificato","diploma","università","master","dottorato"))
View(table(titolo))
#***********************************************************************************

# grafico a barre
png("Scuola_bar_chart.png", width = 800, height = 800, res = 150)

plot(titolo,xlab="Livello di studio posseduto.",col =c("orange","pink","coral","green","blue","violet","red","yellow","grey"))

# grafico a torta
png("Scuola_pie_chart.png", width = 800, height = 800, res = 150)

pie(titolo, col = c("orange","pink","coral","green","blue","violet","red","yellow","grey"),
    main = "Distribuzione dell'educazione scolastica")
