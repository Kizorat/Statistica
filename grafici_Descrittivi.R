data<-read.csv("drug_consumption.csv",sep=",",header = T)


# Caricare il dataset
data <- read.csv("drug_consumption.csv")

# Definizione delle fasce di età
tabellaAge <- table(data$Age)


#range di età
#range di età 18-24     1
r18_24<-subset(tabellaAge,Age== -0.95197)
View(r18_24)
#range di età 25-34     2
r25_34<-subset(tabellaAge,Age== -0.07854)
View(r25_34)
#range di età 34-44     3
r35_44<-subset(tabellaAge,Age== 0.49788)
View(r35_44)
#range di età 45-54     4
r45_54<-subset(tabellaAge,Age== 1.09449)
View(r45_54)
#range di età 55-65     5
r55_64<-subset(tabellaAge,Age== 1.82213)
View(r55_64)
#range di età 65+       6
r65<-subset(tabellaAge,Age== 2.59171)
View(r65)





#cambiamo i valori delle età da numerici a carattere
tabellaAge= as.character(subset(tabellaAge,tabellaAge==-0.95197))
View(tabellaAge)
  
  
  
  
tabellaAge <- cut(data$Age, 
                         breaks = c(1, 2, 3, 4, 5, 6), 
                         labels = c("18-24", "25-34", "35-44", "45,55","55,64","65+"))
View(tabellaAge)                       

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

