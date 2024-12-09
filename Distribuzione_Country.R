#seguirà l'analisi dei paesi cinvolti nell'indagine co annessi grafici

data<-read.csv("drug_consumption.csv",sep=",",header = T)
#ordino in base all'affluenza maggiore i paesi per poi farne un grafico a torta
#Paesi<-ordered(data$Country,levels=c("-0.09765","0.24923","-0.46841","-0.28519","0.21128","0.96082","-0.57009"))
#table(Paesi)

#cambio il nome dei dati che riferiscono il tipo di educazione(CITATA SOPRA):
data$Country <- ifelse(data$Country == "-0.09765", "Australia",
                         ifelse(data$Country == "0.24923", "Canada",
                                ifelse(data$Country == "-0.46841", "Nuova Zelanda",
                                       ifelse(data$Country == "-0.28519", "Altri",
                                              ifelse(data$Country == "0.21128", "Irlanda",
                                                     ifelse(data$Country == "0.96082", "UK",
                                                            ifelse(data$Country == "-0.57009", "USA",
                                                                                 data$Country))))))) # Mantieni il valore originale per altri casi

Paesi<-ordered(data$Country,levels=c("Australia","Canada","Nuova Zelanda","Altri","Irlanda","UK","USA"))
table(Paesi)
View(table(Paesi))

tablePaesi<-table(Paesi)






# grafico a barre
#png("Paesi_bar_chart.png", width = 800, height = 800, res = 150)
barplot(tablePaesi,col =c("orange","green","blue","violet","red","yellow","grey"))



# grafico a torta
#png("Paesi_pie_chart.png", width = 800, height = 800, res = 150)
pie(tablePaesi, col = c("orange","green","blue","violet","red","yellow","grey"),
    main = "Distribuzione delle fasce d'età")


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

