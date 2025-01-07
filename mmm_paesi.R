# ANALISI DELLE DROGHE PER PAESE
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
sub_data<-subset(data,select = c(Country,Alcohol,Amphet,Amyl,Benzos,Caff,Cannabis,Choc,Coke,Crack,Ecstasy,Heroin,Ketamine,Legalh,LSD,Meth,Mushrooms,Nicotine,Semer,VSA))
#trasformo il dataset in dataframe
as.data.frame(sub_data)


# Filtraggio per l'Australia (usando il codice numerico che rappresenta l'Australia)
sub_Australia <- subset(sub_data, sub_data$Country == -0.09765)
View(sub_Australia)

# Filtraggio per l'Irlanda (usando il codice numerico per l'Irlanda)
sub_Irlanda <- subset(sub_data, sub_data$Country == 0.21128)
View(sub_Irlanda)

# Filtraggio per gli USA (usando il codice numerico per gli USA)
sub_USA <- subset(sub_data, sub_data$Country == -0.57009)
View(sub_USA)

# Filtraggio per il Regno Unito (usando il codice numerico per il UK)
sub_UK <- subset(sub_data, sub_data$Country == 0.96082)
View(sub_UK)

# Filtraggio per altri paesi (usando il codice numerico per altri paesi)
sub_Altri <- subset(sub_data, sub_data$Country == -0.28519)
View(sub_Altri)

# Filtraggio per il Canada (usando il codice numerico per il Canada)
sub_Canada <- subset(sub_data, sub_data$Country == 0.24923)
View(sub_Canada)

# Filtraggio per la Nuova Zelanda (usando il codice numerico per la Nuova Zelanda)
sub_Nuova_Zelanda <- subset(sub_data, sub_data$Country == -0.46841)
View(sub_Nuova_Zelanda)


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

# Applicare la funzione a ciascun paese

# Australia
statistiche_Australia <- calcola_statistiche(sub_Australia)

# Irlanda
statistiche_Irlanda <- calcola_statistiche(sub_Irlanda)

# USA
statistiche_USA <- calcola_statistiche(sub_USA)

# UK
statistiche_UK <- calcola_statistiche(sub_UK)

# Altri paesi
statistiche_Altri <- calcola_statistiche(sub_Altri)

# Canada
statistiche_Canada <- calcola_statistiche(sub_Canada)

# Nuova Zelanda
statistiche_Nuova_Zelanda <- calcola_statistiche(sub_Nuova_Zelanda)


# print(statistiche_Altri)
# print(statistiche_Canada)
#print(statistiche_Nuova_Zelanda)
# print(statistiche_Irlanda)
# print(statistiche_USA)
# print(statistiche_UK)
#print(statistiche_Australia)


# Grafico delle medie per l'Australia 
barplot(statistiche_Australia$Media, 
        names.arg = statistiche_Australia$Droga,
        col = rainbow(length(statistiche_Australia$Media)),  # Colori arcobaleno
        las = 2,  # Ruotare le etichette sull'asse x
        main = "Media consumo droghe in Australia", 
        ylab = "Media")

boxplot(sub_Australia[, 2:ncol(sub_Australia)], 
        main = "Distribuzione del consumo delle droghe in Australia", 
        ylab = "Consumo", xlab = "Droghe", 
        col = rainbow(ncol(sub_Australia) - 1))  # Colori arcobaleno per ciascun box

# Grafico delle medie per UK 
barplot(statistiche_UK$Media, 
        names.arg = statistiche_UK$Droga,
        col = rainbow(length(statistiche_UK$Media)),  # Colori arcobaleno
        las = 2,  # Ruotare le etichette sull'asse x
        main = "Media consumo droghe in UK", 
        ylab = "Media")

boxplot(sub_UK[, 2:ncol(sub_UK)], 
        main = "Distribuzione del consumo delle droghe in UK", 
        ylab = "Consumo", xlab = "Droghe", 
        col = rainbow(ncol(sub_UK) - 1))  # Colori arcobaleno per ciascun box


# Grafico delle medie per USA 
barplot(statistiche_USA$Media, 
        names.arg = statistiche_USA$Droga,
        col = rainbow(length(statistiche_USA$Media)),  # Colori arcobaleno
        las = 2,  # Ruotare le etichette sull'asse x
        main = "Media consumo droghe in USA", 
        ylab = "Media")

boxplot(sub_Australia[, 2:ncol(sub_USA)], 
        main = "Distribuzione del consumo delle droghe in USA", 
        ylab = "Consumo", xlab = "Droghe", 
        col = rainbow(ncol(sub_USA) - 1))  # Colori arcobaleno per ciascun box

# Grafico delle medie per l'Irlanda 
barplot(statistiche_Irlanda$Media, 
        names.arg = statistiche_Irlanda$Droga,
        col = rainbow(length(statistiche_Irlanda$Media)),   
        las = 2,  # Ruotare le etichette sull'asse x
        main = "Media consumo droghe in Irlanda", 
        ylab = "Media")

boxplot(sub_Irlanda[, 2:ncol(sub_Irlanda)], 
        main = "Distribuzione del consumo delle droghe in Irlanda", 
        ylab = "Consumo", xlab = "Droghe", 
        col = rainbow(ncol(sub_Irlanda) - 1))  # Colori arcobaleno per ciascun box

# Grafico delle medie per Altri 
barplot(statistiche_Altri$Media, 
        names.arg = statistiche_Altri$Droga,
        col = rainbow(length(statistiche_Altri$Media)), 
        las = 2,  # Ruotare le etichette sull'asse x
        main = "Media consumo droghe in Altri", 
        ylab = "Media")

boxplot(sub_Altri[, 2:ncol(sub_Altri)], 
        main = "Distribuzione del consumo delle droghe in Altri", 
        ylab = "Consumo", xlab = "Droghe", 
        col = rainbow(ncol(sub_Altri) - 1))  # Colori arcobaleno per ciascun box


# Grafico delle medie per Canada 
barplot(statistiche_Canada$Media, 
        names.arg = statistiche_Canada$Droga,
        col = rainbow(length(statistiche_Canada$Media)),  # Colori arcobaleno
        las = 2,  # Ruotare le etichette sull'asse x
        main = "Media consumo droghe in Canada", 
        ylab = "Media")

boxplot(sub_Canada[, 2:ncol(sub_Canada)], 
        main = "Distribuzione del consumo delle droghe in Canada", 
        ylab = "Consumo", xlab = "Droghe", 
        col = rainbow(ncol(sub_Canada) - 1))  # Colori arcobaleno per ciascun box

# Grafico delle medie per Nuova_Zelanda 
barplot(statistiche_Nuova_Zelanda$Media, 
        names.arg = statistiche_Nuova_Zelanda$Droga,
        col = rainbow(length(statistiche_Nuova_Zelanda$Media)),  # Colori arcobaleno
        las = 2,  # Ruotare le etichette sull'asse x
        main = "Media consumo droghe in Nuova_Zelanda", 
        ylab = "Media")

boxplot(sub_Nuova_Zelanda[, 2:ncol(sub_Nuova_Zelanda)], 
        main = "Distribuzione del consumo delle droghe in Nuova_Zelanda", 
        ylab = "Consumo", xlab = "Droghe", 
        col = rainbow(ncol(sub_Nuova_Zelanda) - 1))  # Colori arcobaleno per ciascun box
