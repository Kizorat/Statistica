#seguirà l'analisi dei paesi cinvolti nell'indagine co annessi grafici

data<-read.csv("drug_consumption.csv",sep=",",header = T)
#ordino in base all'affluenza maggiore i paesi per poi farne un grafico a torta
#Paesi<-ordered(data$Country,levels=c("-0.09765","0.24923","-0.46841","-0.28519","0.21128","0.96082","-0.57009"))
#table(Paesi)

#cambio il nome dei dati che riferiscono il tipo di educazione(CITATA SOPRA):
data$Country <- ifelse(data$Country == "-0.09765", "Australia",
                         ifelse(data$Country == "0.24923", "Canada",
                                ifelse(data$Country == "-0.46841", "Nuova_Zelanda",
                                       ifelse(data$Country == "-0.28519", "Altri",
                                              ifelse(data$Country == "0.21128", "Irlanda",
                                                     ifelse(data$Country == "0.96082", "UK",
                                                            ifelse(data$Country == "-0.57009", "USA",
                                                                                 data$Country))))))) # Mantieni il valore originale per altri casi




Paesi<-ordered(data$Country,levels=c("Australia","Canada","Nuova_Zelanda","Altri","Irlanda","UK","USA"))
table(Paesi)

tablePaesi<-table(Paesi)
View(tablePaesi)



#MEDIA E MEDIANA
#Australia
# dati_australia <- subset(data, Country == "Australia")
# 
# media_australia <- mean(dati_australia$Alcohol, na.rm = TRUE)
# mediana_australia <- median(dati_australia$Alcohol, na.rm = TRUE)
# 
# cat("Australia - Media:", media_australia, "Mediana:", mediana_australia, "\n")
# 
# 
# # Estraggo i Paesi unici
# paesi_unici <- unique(data$Country)
# 
# # Creo una tabella per salvare i risultati
# risultati <- data.frame(Paese = character(), Media = numeric(), Mediana = numeric(), stringsAsFactors = FALSE)
# 
# # Itero attraverso ogni Paese
# for (paese in paesi_unici) {
#   # Filtra i dati per il Paese corrente
#   dati_paese <- subset(data, Country == paese)
#   
#   # Calcolo media e mediana
#   media <- mean(dati_paese$Alcohol, na.rm = TRUE)
#   mediana <- median(dati_paese$Alcohol, na.rm = TRUE)
#   
#   # Aggiungo i risultati alla tabella
#   risultati <- rbind(risultati, data.frame(Paese = paese, Media = media, Mediana = mediana))
# }
# 
# # Visualizzo i risultati
# print(risultati)
# 
# 
# 
# 


#FINE MEDIA E MEDIANA



# grafico a barre
#png("Paesi_bar_chart.png", width = 800, height = 800, res = 150)
barplot(tablePaesi,col =c("orange","green","blue","violet","red","yellow","grey"))


# Calcolo delle percentuali
percentages <- round(100 * tablePaesi / sum(tablePaesi), 1)

# Creazione delle etichette
my_labels <- paste0(names(tablePaesi), " (", percentages, "%)")

# Creazione del grafico a torta
pie(tablePaesi,
    col = c("orange", "green", "blue", "violet", "red", "yellow", "skyblue"),  # Colori per ogni etnia
    main = "Distribuzione dei Paesi",  # Titolo del grafico
    labels = my_labels,  # Etichette personalizzate
    cex = 0.6,  # Riduzione della dimensione del testo
    radius=1
)

