data<-read.csv("drug_consumption.csv",sep=",",header = T)
#cambio il nome dei dati che riferiscono il tipo di etnia
data$Age <- ifelse(data$Age == "-0.95197", "18-24",
                   ifelse(data$Age == "-0.07854", "25-34",
                          ifelse(data$Age == "0.49788", "35-44",
                                 ifelse(data$Age == "1.09449", "45-54",
                                        ifelse(data$Age == "1.82213", "55-64",
                                               ifelse(data$Age == "2.59171", "65+",
                                                      data$Age)))))) # Mantieni il valore originale per altri casi

View(data)
eta<-ordered(data$Age,levels=c("18-24","25-34","35-44","45-54","55-64","65+"))
table(eta)
View(table(eta))

table_Eta<-table(eta)


#grafici
barplot(table_Eta,col =c("green","blue","violet","red","yellow","grey"))

# Calcolo delle percentuali
percentages <- round(100 * table_Eta / sum(table_Eta), 1)

# Creazione delle etichette
my_labels <- paste0(names(table_Eta), " (", percentages, "%)")

# Creazione del grafico a torta
pie(table_Eta,
    col = c("orange", "green", "blue", "violet", "red", "yellow"),  # Colori per ogni etnia
    main = "Distribuzione dell'Etnia",  # Titolo del grafico
    labels = my_labels,  # Etichette personalizzate
    cex = 0.6,  # Riduzione della dimensione del testo
    radius=1.1
)


