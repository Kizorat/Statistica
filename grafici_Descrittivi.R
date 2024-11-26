
  data<-read.csv("drug_consumption.csv",sep=",",header = T)
  

  
  # Definizione delle fasce di età
  data$Age<- cut(data$Age, breaks = c(-Inf, -0.95197,-0.07854,0.49788,1.09449,1.82213,2.59171, Inf), labels = c("18", "18-24", "25-34", "35-44", "45-54","55-64","65+"),right = FALSE)
  data$Gender<-cut(data$Gender,breaks = c(-Inf ,-0.48246, Inf),labels = c("Femmina","Maschio"),right = TRUE)
  #riscrivo il dataset
  write.csv(data, "drug_consumption_modificato.csv", row.names = FALSE)
  
  #stampo nuovo csv
  data_modifcato<-read.csv("drug_consumption_modificato.csv",sep=",",header = T)
  View(data_modifcato)
  #Acquisisco solo coloro che si trovano in un range di età superiore a 65
  genere_eta<-subset(data_modifcato,data_modifcato$Age>65,select = c(Age,Gender))
  View(genere_eta)
  #genero una tabella relativi al genere
  conteggio_generi<-table(genere_eta$Gender)
  View(conteggio_generi)
  #costruzione di un grafico a torta
  pie(conteggio_generi,labels = names(conteggio_generi),main="Grafico sulle categorie di genere di età over 65")
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ---------------------------------------------------------------------------
  # Assegnare i generi
  data$Gender_Label <- ifelse(data$Gender > 0, "Male", "Female")
  
  drug_columns <- colnames(data)[8:32]
  
  # Trasformare i livelli di consumo
  for (col in drug_columns) {
    data[[col]] <- as.numeric(gsub("CL", "", data[[col]]))
  }
  
  # Creare una tabella di frequenza del consumo per fasce di età e livelli
  age_drug_table <- aggregate(. ~ Age_Category, data = data[c("Age_Category", drug_columns)], FUN = sum)
  
  # Creare un istogramma dei consumi totali per fasce di età
  barplot(t(as.matrix(age_drug_table[, -1])), # Trasporre per corretta disposizione
          beside = TRUE, 
          col = rainbow(ncol(age_drug_table) - 1), 
          main = "Consumo per Fasce di Età", 
          xlab = "Fascia di Età", 
          ylab = "Frequenza")
  
  # Aggiungere la leggenda con dimensioni ridotte
  legend("topright", legend = colnames(age_drug_table)[-1], 
         fill = rainbow(ncol(age_drug_table) - 1), 
         cex = 0.7) # Riduce la dimensione della leggenda
  
  
  # Creare un file PNG con risoluzione aumentata
  png("gender_pie_chart.png", width = 800, height = 800, res = 150)
  
  
  
  # Creare il grafico a torta
  pie(gender_table, col = c("blue", "pink"), 
      main = "Distribuzione per Sesso")
  
  # Chiudere il dispositivo grafico
  dev.off()
  