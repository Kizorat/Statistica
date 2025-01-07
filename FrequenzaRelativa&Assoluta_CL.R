create_drug_subsets <- function(data, drug_columns, cl_value) {
  # Inizializza una lista per salvare le subset
  subsets <- list()
  
  # Per ogni colonna di droga specificata
  for (drug in drug_columns) {
    # Crea una subset filtrando solo le righe con il valore specificato (es. "CL0") per quella colonna
    subset_data <- subset(data, data[[drug]] == cl_value, select = drug)
    
    # Controlla se la subset è vuota
    if (nrow(subset_data) > 0) {
      # Rinomina la colonna della droga per avere un nome coerente
      colnames(subset_data) <- "Value"
      # Aggiungi il nome della droga come colonna
      subset_data$Drug <- drug
      # Aggiungi la subset alla lista
      subsets[[drug]] <- subset_data
    }
  }
  
  # Controlla se ci sono subset valide nella lista
  if (length(subsets) > 0) {
    # Ricompatta tutte le subset in un unico dataframe
    final_dataframe <- do.call(rbind, subsets)
  } else {
    # Se non ci sono subset valide, restituisci un dataframe vuoto
    final_dataframe <- data.frame(Value = character(), Drug = character(), stringsAsFactors = FALSE)
  }
  
  # Restituisci il dataframe finale
  return(final_dataframe)
}

# Esempio di utilizzo
# Definisci le colonne delle droghe
drug_columns <- c(
  "Alcohol", "Amphet", "Amyl", "Benzos", "Caff", "Cannabis", 
  "Choc", "Coke", "Crack", "Ecstasy", "Heroin", "Ketamine", 
  "Legalh", "LSD", "Meth", "Mushrooms", "Nicotine", "Semer", "VSA"
)

# Applica la funzione al dataset
Droghe_CL0 <- create_drug_subsets(data_modifcato, drug_columns,"CL0")
Droghe_CL1 <- create_drug_subsets(data_modifcato, drug_columns,"CL1")
Droghe_CL2 <- create_drug_subsets(data_modifcato, drug_columns,"CL2")
Droghe_CL3 <- create_drug_subsets(data_modifcato, drug_columns,"CL3")
Droghe_CL4 <- create_drug_subsets(data_modifcato, drug_columns,"CL4")
Droghe_CL5 <- create_drug_subsets(data_modifcato, drug_columns,"CL5")
Droghe_CL6 <- create_drug_subsets(data_modifcato, drug_columns,"CL6")
#creo delle table per ogni singolo CL
table_CL0<-table(Droghe_CL0)
table_CL1<-table(Droghe_CL1)
table_CL2<-table(Droghe_CL2)
table_CL3<-table(Droghe_CL3)
table_CL4<-table(Droghe_CL4)
table_CL5<-table(Droghe_CL5)
table_CL6<-table(Droghe_CL6)
#creo un barplot per ogni singolo table_CL
barplot(table_CL0,main="Frequenza di CL0 tra le Droghe",ylab = "Frequenza",col = "skyblue",las = 2,border = "black")
barplot(table_CL1,main="Frequenza di CL1 tra le Droghe",ylab = "Frequenza",col = "skyblue",las = 2,border = "black")
barplot(table_CL2,main="Frequenza di CL2 tra le Droghe",ylab = "Frequenza",col = "skyblue",las = 2,border = "black")
barplot(table_CL3,main="Frequenza di CL3 tra le Droghe",ylab = "Frequenza",col = "skyblue",las = 2,border = "black")
barplot(table_CL4,main="Frequenza di CL4 tra le Droghe",ylab = "Frequenza",col = "skyblue",las = 2,border = "black")
barplot(table_CL5,main="Frequenza di CL5 tra le Droghe",ylab = "Frequenza",col = "skyblue",las = 2,border = "black")
barplot(table_CL6,main="Frequenza di CL6 tra le Droghe",ylab = "Frequenza",col = "skyblue",las = 2,border = "black")

#converto la lista dei CL in table
dataframe_CL0<-as.data.frame(table_CL0)
dataframe_CL1<-as.data.frame(table_CL1)
dataframe_CL2<-as.data.frame(table_CL2)
dataframe_CL3<-as.data.frame(table_CL3)
dataframe_CL4<-as.data.frame(table_CL4)
dataframe_CL5<-as.data.frame(table_CL5)
dataframe_CL6<-as.data.frame(table_CL6)
#rinomino le colonne per ciascun dataframe
names(dataframe_CL0)<-c("CL","Droga","Frequenza")
names(dataframe_CL1)<-c("CL","Droga","Frequenza")
names(dataframe_CL2)<-c("CL","Droga","Frequenza")
names(dataframe_CL3)<-c("CL","Droga","Frequenza")
names(dataframe_CL4)<-c("CL","Droga","Frequenza")
names(dataframe_CL5)<-c("CL","Droga","Frequenza")
names(dataframe_CL6)<-c("CL","Droga","Frequenza")
#calcolo della frequenza relativa

fr_table_CL0<-dataframe_CL0$Frequenza/sum(as.numeric(dataframe_CL0$Frequenza))
fr_table_CL1<-dataframe_CL1$Frequenza/sum(as.numeric(dataframe_CL1$Frequenza))
fr_table_CL2<-dataframe_CL2$Frequenza/sum(as.numeric(dataframe_CL2$Frequenza))
fr_table_CL3<-dataframe_CL3$Frequenza/sum(as.numeric(dataframe_CL3$Frequenza))
fr_table_CL4<-dataframe_CL4$Frequenza/sum(as.numeric(dataframe_CL4$Frequenza))
fr_table_CL5<-dataframe_CL5$Frequenza/sum(as.numeric(dataframe_CL5$Frequenza))
fr_table_CL6<-dataframe_CL6$Frequenza/sum(as.numeric(dataframe_CL6$Frequenza))


#trasformo dopo il calcolo della frequenza relativa il dataframe in vettore
fr_vectorCL0<-as.vector(fr_table_CL0)
fr_vectorCL1<-as.vector(fr_table_CL1)
fr_vectorCL2<-as.vector(fr_table_CL2)
fr_vectorCL3<-as.vector(fr_table_CL3)
fr_vectorCL4<-as.vector(fr_table_CL4)
fr_vectorCL5<-as.vector(fr_table_CL5)
fr_vectorCL6<-as.vector(fr_table_CL6)

#eseguo un plot per le frequenze relative

plot(fr_table_CL0,type = "h",main="Frequenza relativa del CL0 tra le Droghe",xlab = "",ylab = "Frequenza",col=rainbow(15))
# Aggiunta delle etichette sull'asse X (sotto il grafico)
axis(side = 1,at = 1:nrow(dataframe_CL0),labels = dataframe_CL0$Droga,las = 2,line = 1,tick = FALSE,cex.axis = 0.8)


plot(fr_table_CL1,type = "h",main="Frequenza relativa del CL1 tra le Droghe",xlab = "",ylab = "Frequenza",col=rainbow(15))
# Aggiunta delle etichette sull'asse X (sotto il grafico)
axis(side = 1,at = 1:nrow(dataframe_CL1),labels = dataframe_CL1$Droga,las = 2,line = 1,tick = FALSE,cex.axis = 0.8)


plot(fr_table_CL2,type = "h",main="Frequenza relativa del CL2 tra le Droghe",xlab = "",ylab = "Frequenza",col=rainbow(15))
# Aggiunta delle etichette sull'asse X (sotto il grafico)
axis(side = 1,at = 1:nrow(dataframe_CL2),labels = dataframe_CL2$Droga,las = 2,line = 1,tick = FALSE,cex.axis = 0.8)




plot(fr_table_CL3,type = "h",main="Frequenza relativa del CL3 tra le Droghe",xlab = "",ylab = "Frequenza",col=rainbow(15))
# Aggiunta delle etichette sull'asse X (sotto il grafico)
axis(side = 1,at = 1:nrow(dataframe_CL3),labels = dataframe_CL3$Droga,las = 2,line = 1,tick = FALSE,cex.axis = 0.8)



plot(fr_table_CL4,type = "h",main="Frequenza relativa del CL4 tra le Droghe",xlab = "",ylab = "Frequenza",col=rainbow(15))
# Aggiunta delle etichette sull'asse X (sotto il grafico)
axis(side = 1,at = 1:nrow(dataframe_CL4),labels = dataframe_CL4$Droga,las = 2,line = 1,tick = FALSE,cex.axis = 0.8)



plot(fr_table_CL5,type = "h",main="Frequenza relativa del CL5 tra le Droghe",xlab = "",ylab = "Frequenza",col=rainbow(15))
# Aggiunta delle etichette sull'asse X (sotto il grafico)
axis(side = 1,at = 1:nrow(dataframe_CL5),labels = dataframe_CL5$Droga,las = 2,line = 1,tick = FALSE,cex.axis = 0.8)



plot(fr_table_CL6,type = "h",main="Frequenza relativa del CL6 tra le Droghe",xlab = "",ylab = "Frequenza",col=rainbow(15))
# Aggiunta delle etichette sull'asse X (sotto il grafico)
axis(side = 1,at = 1:nrow(dataframe_CL6),labels = dataframe_CL6$Droga,las = 2,line = 1,tick = FALSE,cex.axis = 0.8)


