data<-read.csv("drug_consumption.csv",sep=",",header = T)


# Caricare il dataset
data <- read.csv("drug_consumption.csv")

# Definizione delle fasce di età
data$Age_Category <- cut(data$Age, 
                         breaks = c(-Inf, -1, 0, 1, 2, Inf), 
                         labels = c("<18", "18-24", "25-34", "35-44", "45+"))

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

