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


fr_table_CL0<-prop.table(table_CL0)
fr_table_CL1<-prop.table(table_CL1)
fr_table_CL2<-prop.table(table_CL2)
fr_table_CL3<-prop.table(table_CL3)
fr_table_CL4<-prop.table(table_CL4)
fr_table_CL5<-prop.table(table_CL5)
fr_table_CL6<-prop.table(table_CL6)


fr_vector<-as.vector(fr_table_CL0)
View(fr_vector)

barplot(fr_table_CL0,breaks = 20,main="Frequenza relativa del CL0 tra le Droghe",ylab = "Frequenza")
