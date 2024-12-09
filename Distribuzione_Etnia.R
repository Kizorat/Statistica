#seguirà l'analisi dei paesi cinvolti nell'indagine co annessi grafici

data<-read.csv("drug_consumption.csv",sep=",",header = T)

#cambio il nome dei dati che riferiscono il tipo di etnia
data$Ethnicity <- ifelse(data$Ethnicity == "-0.50212", "Asiatici",
                       ifelse(data$Ethnicity == "-1.10702", "Neri",
                              ifelse(data$Ethnicity == "1.90725", "Neri/Asiatici",
                                     ifelse(data$Ethnicity == "0.126", "Bianchi/Asiatici",
                                            ifelse(data$Ethnicity == "-0.22166", "Bianchi/Neri",
                                                   ifelse(data$Ethnicity == "0.1144", "Altri",
                                                          ifelse(data$Ethnicity == "-0.31685", "Bianchi",
                                                                 data$Ethnicity))))))) # Mantieni il valore originale per altri casi
View(data)
Etnia<-ordered(data$Ethnicity,levels=c("Asiatici","Neri","Neri/Asiatici","Bianchi/Asiatici","Bianchi/Neri","Altri","Bianchi"))
table(Etnia)
View(table(Etnia))

tableEtnia<-table(Etnia)






# grafico a barre
#png("Paesi_bar_chart.png", width = 800, height = 800, res = 150)
barplot(tableEtnia ,col =c("orange","green","blue","violet","red","yellow","skyblue"))



##############################################################################################################################################

# Calcolo delle percentuali
percentages <- round(100 * tableEtnia / sum(tableEtnia), 1)

# Creazione delle etichette
my_labels <- paste0(names(tableEtnia), " (", percentages, "%)")

# Creazione del grafico a torta
pie(tableEtnia,
    col = c("orange", "green", "blue", "violet", "red", "yellow", "skyblue"),  # Colori per ogni etnia
    main = "Distribuzione dell'Etnia",  # Titolo del grafico
    labels = my_labels,  # Etichette personalizzate
    cex = 0.6,  # Riduzione della dimensione del testo
    radius=1.1
)


###########################################################################################################################################################

