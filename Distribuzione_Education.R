
  #seguirà l'analisi dei paesi cinvolti nell'indagine co annessi grafici
  
  data<-read.csv("drug_consumption.csv",sep=",",header = T)
  
  #provo un altro metodo poichè il calcolo l'ho già fatto in età e paese
  educazione<-c(rep("lasciata_<16",28),rep("lasciata_16",99),rep("lasciata_17",30),rep("lasciata_18 ",100),rep(),rep("senza_certificato",506),rep("diploma",270),rep("università",480),rep("master",283),rep("dottorato",89))
  View(table(educazione))
  #***********************************************************************************
  titolo<-ordered(educazione,levels=c("lasciata_<16","lasciata_16","lasciata_17","lasciata_18 ","senza_certificato","diploma","università","master","dottorato"))
  View(table(titolo))
  #***********************************************************************************
  tableTitolo<-table(titolo)
  
  
  # grafico a barre
  #png("Scuola_bar_chart.png", width = 800, height = 800, res = 150)
  
  plot(tableTitolo,xlab="Livello di studio posseduto.",col =c("orange","pink","coral","green","blue","violet","red","yellow","grey"),ylim =c(0,800))
  
  # grafico a torta
  #png("Scuola_pie_chart.png", width = 800, height = 800, res = 150)
  
  pie(tableTitolo, col = c("orange","pink","coral","green","blue","violet","red","yellow","grey"),
      main = "Distribuzione dell'educazione scolastica")
  
  
  # Calcolo delle percentuali
  percentages <- round(100 * tableTitolo / sum(tableTitolo), 1)
  
  # Creazione delle etichette
  my_labels <- paste0(names(tableTitolo), " (", percentages, "%)")
  
  # Creazione del grafico a torta
  pie(tableTitolo,
      col = c("orange", "green", "blue", "violet", "red", "yellow", "skyblue"),  # Colori per ogni etnia
      main = "Distribuzione del titolo di studio",  # Titolo del grafico
      labels = my_labels,  # Etichette personalizzate
      cex = 0.8  # Riduzione della dimensione del testo
  )