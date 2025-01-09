# ANALISI DELLE DROGHE PER ETNIA
data<-read.csv("drug_consumption.csv",sep=",",header = T)
#modifico tutti i CL in valori numerici
#creazione della funzione di modifica dei CL
converitore_cl<-function(x){
  if(is.character(x)){
    return(as.numeric(sub("CL","",x)))
  }
  return(x)
}
#ciclo for dove passo ogni colonna
for (col in names(data)) {
  data[[col]]<-converitore_cl(data[[col]])
}
#verifico che la funzione è andata a buon fine
head(data)


#prendo tutte le droghe
sub_data<-subset(data,select = c(Ethnicity,Alcohol,Amphet,Amyl,Benzos,Caff,Cannabis,Choc,Coke,Crack,Ecstasy,Heroin,Ketamine,Legalh,LSD,Meth,Mushrooms,Nicotine,Semer,VSA))
#trasformo il dataset in dataframe
as.data.frame(sub_data)


# Filtraggio per Asiatici
sub_Asiatici <- subset(sub_data, sub_data$Ethnicity == -0.50212)
View(sub_Asiatici)

# Filtraggio per Neri
sub_Neri <- subset(sub_data, sub_data$Ethnicity == -1.10702)
View(sub_Neri)

# Filtraggio per Neri/Asiatici
sub_Neri_Asiatici <- subset(sub_data, sub_data$Ethnicity == 1.90725)
View(sub_Neri_Asiatici)

# Filtraggio per Bianchi/Asiatici",
sub_Bianchi_Asiatici <- subset(sub_data, sub_data$Ethnicity == 0.126)
View(sub_Bianchi_Asiatici)

# Filtraggio per Bianchi/Neri
sub_Bianchi_Neri <- subset(sub_data, sub_data$Ethnicity == -0.22166)
View(sub_Bianchi_Neri)

# Filtraggio per Altri
sub_Altri <- subset(sub_data, sub_data$Ethnicity == 0.1144)
View(sub_Altri)

# Filtraggio per Bianchi
sub_Bianchi <- subset(sub_data, sub_data$Ethnicity == -0.31685)
View(sub_Bianchi)


#MODA,MEDIA,MEDIANA..................................................................................
# Calcolare media, mediana e moda per ogni paese
# Funzione per calcolare la moda
moda <- function(x) {
  uniq_x <- unique(x)  # Trova i valori unici
  uniq_x[which.max(tabulate(match(x, uniq_x)))]  # Restituisce il valore con la massima frequenza
}

# Funzione per calcolare media, mediana e moda
calcola_statistiche <- function(data) {
  # Inizializzare un dataframe vuoto per contenere i risultati
  risultati <- data.frame(
    Droga = c("Alcohol", "Amphet", "Amyl", "Benzos", "Caff", "Cannabis", 
              "Choc", "Coke", "Crack", "Ecstasy", "Heroin", "Ketamine", 
              "Legalh", "LSD", "Meth", "Mushrooms", "Nicotine", "Semer", "VSA"),
    Media = NA,
    Mediana = NA,
    Moda = NA
  )
  
  # Ciclo attraverso tutte le droghe
  for (i in 2:ncol(data)) {  # Partiamo dalla seconda colonna (droghe)
    droga <- data[[i]]
    
    # Calcolare media, mediana e moda
    risultati$Media[i-1] <- mean(droga, na.rm = TRUE)
    risultati$Mediana[i-1] <- median(droga, na.rm = TRUE)
    risultati$Moda[i-1] <- moda(droga)
  }
  
  return(risultati)
}

# Applicare la funzione a ciascuna etnia

statistiche_Bianchi <- calcola_statistiche(sub_Bianchi)

statistiche_Asistici <- calcola_statistiche(sub_Asiatici)

statistiche_Neri <- calcola_statistiche(sub_Neri)

statistiche_Neri_Asiatici <- calcola_statistiche(sub_Neri_Asiatici)

statistiche_Altri <- calcola_statistiche(sub_Altri)

statistiche_Bianchi_Asiatici <- calcola_statistiche(sub_Bianchi_Asiatici)

statistiche_Bianchi_Neri <- calcola_statistiche(sub_Bianchi_Neri)

#prova
# print(statistiche_Altri)



# Grafico delle medie per Asiatici
barplot(statistiche_Asiatici$Media, 
        names.arg = statistiche_Asiatici$Droga,
        col = rainbow(length(statistiche_Asiatici$Media)),  # Colori arcobaleno
        las = 2,  # Ruotare le etichette sull'asse x
        main = "Media consumo droghe degli asiatici", 
        ylab = "Media")

boxplot(statistiche_Asiatici[, 2:ncol(statistiche_Asiatici)], 
        main = "Distribuzione del consumo droghe degli asiatici", 
        ylab = "Consumo", xlab = "Droghe", 
        col = rainbow(ncol(statistiche_Asiatici) - 1))  # Colori arcobaleno per ciascun box

# Grafico delle medie per Bianchi
barplot(statistiche_Bianchi$Media, 
        names.arg = statistiche_Bianchi$Droga,
        col = rainbow(length(statistiche_Bianchi$Media)),  # Colori arcobaleno
        las = 2,  # Ruotare le etichette sull'asse x
        main = "Media consumo droghe per persone bianche", 
        ylab = "Media")

boxplot(sub_Bianchi[, 2:ncol(sub_Bianchi)], 
        main = "Distribuzione del consumo delle droghe per persone bianche", 
        ylab = "Consumo", xlab = "Droghe", 
        col = rainbow(ncol(sub_UK) - 1))  # Colori arcobaleno per ciascun box


# Grafico delle medie per Neri
barplot(statistiche_Neri$Media, 
        names.arg = statistiche_Neri$Droga,
        col = rainbow(length(statistiche_Neri$Media)),  # Colori arcobaleno
        las = 2,  # Ruotare le etichette sull'asse x
        main = "Media consumo droghe dei Neri", 
        ylab = "Media")

boxplot(statistiche_Neri[, 2:ncol(statistiche_Neri)], 
        main = "Distribuzione del consumo delle droghe per i Neri", 
        ylab = "Consumo", xlab = "Droghe", 
        col = rainbow(ncol(sub_Neri) - 1))  # Colori arcobaleno per ciascun box

# Grafico delle medie per Neri_Asiatici 
barplot(statistiche_Neri_Asiatici$Media, 
        names.arg = statistiche_Neri_Asiatici$Droga,
        col = rainbow(length(statistiche_Neri_Asiatici$Media)),   
        las = 2,  # Ruotare le etichette sull'asse x
        main = "Media consumo droghe in Irlanda", 
        ylab = "Media")

boxplot(statistiche_Neri_Asiatici[, 2:ncol(statistiche_Neri_Asiatici)], 
        main = "Distribuzione del consumo delle droghe per Neri Asiatici", 
        ylab = "Consumo", xlab = "Droghe", 
        col = rainbow(ncol(statistiche_Neri_Asiatici) - 1))  # Colori arcobaleno per ciascun box

# Grafico delle medie per Bianchi_Asiatici 
barplot(statistiche_Bianchi_Asiatici$Media, 
        names.arg = statistiche_Bianchi_Asiatici$Droga,
        col = rainbow(length(statistiche_Bianchi_Asiatici$Media)), 
        las = 2,  # Ruotare le etichette sull'asse x
        main = "Media consumo droghe in Bianchi Asiatici", 
        ylab = "Media")

boxplot(statistiche_Bianchi_Asiatici[, 2:ncol(statistiche_Bianchi_Asiatici)], 
        main = "Distribuzione del consumo delle droghe in Altri", 
        ylab = "Consumo", xlab = "Droghe", 
        col = rainbow(ncol(statistiche_Bianchi_Asiatici) - 1))  # Colori arcobaleno per ciascun box


# Grafico delle medie per Bianchi_Neri 
barplot(statistiche_Bianchi_Neri$Media, 
        names.arg = statistiche_Bianchi_Neri$Droga,
        col = rainbow(length(statistiche_Bianchi_Neri$Media)),  # Colori arcobaleno
        las = 2,  # Ruotare le etichette sull'asse x
        main = "Media consumo droghe per i Bianchi neri", 
        ylab = "Media")

boxplot(statistiche_Bianchi_Neri[, 2:ncol(statistiche_Bianchi_Neri)], 
        main = "Distribuzione del consumo delle droghe per bianchi_Neri", 
        ylab = "Consumo", xlab = "Droghe", 
        col = rainbow(ncol(statistiche_Bianchi_Neri) - 1))  # Colori arcobaleno per ciascun box

# Grafico delle medie per Altri 
barplot(statistiche_Altri$Media, 
        names.arg = statistiche_Altri$Droga,
        col = rainbow(length(statistiche_Altri$Media)),  # Colori arcobaleno
        las = 2,  # Ruotare le etichette sull'asse x
        main = "Media consumo droghe in altri paesi", 
        ylab = "Media")

boxplot(statistiche_Altri[, 2:ncol(statistiche_Altri)], 
        main = "Distribuzione del consumo delle droghe in altri paesi", 
        ylab = "Consumo", xlab = "Droghe", 
        col = rainbow(ncol(statistiche_Altri) - 1))  # Colori arcobaleno per ciascun box
