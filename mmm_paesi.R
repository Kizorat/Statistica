# ANALISI DELLE DROGHE PER PAESE
data <- read.csv("drug_consumption.csv", sep = ",", header = T)

# Livelli di consumo sostituiti
level <- c("CL0" = "mai", "CL1" = "raro", "CL2" = "occasionale", 
           "CL3" = "frequente", "CL4" = "molto frequente", 
           "CL5" = "quotidiano", "CL6" = "abituale")

# Esempio di droga per fare la sostituzione dei livelli con numeri
consumo_droghe <- c(
  "Alcohol", "Amphet", "Amyl", "Benzos", "Caff", "Cannabis", "Choc", "Coke",
  "Crack", "Ecstasy", "Heroin", "Ketamine", "Legalh", "LSD", "Meth",
  "Mushrooms", "Nicotine", "Semer", "VSA"
)

# Sostituzione dei livelli di consumo con i numeri
data[consumo_droghe] <- lapply(data[consumo_droghe], function(col) {
  as.numeric(level[as.character(col)])
})

# Filtraggio per l'Australia (usando il codice numerico che rappresenta l'Australia)
sub_Australia <- subset(data, data$Country == -0.09765)
View(sub_Australia)

# Filtraggio per l'Irlanda (usando il codice numerico per l'Irlanda)
sub_Irlanda <- subset(data, data$Country == 0.21128)
View(sub_Irlanda)

# Filtraggio per gli USA (usando il codice numerico per gli USA)
sub_USA <- subset(data, data$Country == -0.57009)
View(sub_USA)

# Filtraggio per il Regno Unito (usando il codice numerico per il UK)
sub_UK <- subset(data, data$Country == 0.96082)
View(sub_UK)

# Filtraggio per altri paesi (usando il codice numerico per altri paesi)
sub_Altri <- subset(data, data$Country == -0.28519)
View(sub_Altri)

# Filtraggio per il Canada (usando il codice numerico per il Canada)
sub_Canada <- subset(data, data$Country == 0.24923)
View(sub_Canada)

# Filtraggio per la Nuova Zelanda (usando il codice numerico per la Nuova Zelanda)
sub_Nuova_Zelanda <- subset(data, data$Country == -0.46841)
View(sub_Nuova_Zelanda)

statistics <- data.frame(
  Drug = character(),
  Mean = numeric(),
  Median = numeric(),
  Mode = numeric()
)
# 
# for (drug in consumo_droghe) {
#   values <- sub_Australia[[drug]]
#   values <- values[!is.na(values)]  # Rimuovere i valori NA
  
  # Calcoli delle statistiche
  mean_val <- mean(sub_Australia)
  median_val <- median(sub_Australia)
  mode_val <- as.numeric(names(sort(-table(sub_Australia)))[1])  # Moda
  
  # Aggiungi i risultati al data frame
  statistics <- rbind(statistics, data.frame(Drug = drug, Mean = mean_val, Median = median_val, Mode = mode_val))
#}

# Visualizzare il data frame con le statistiche
print(statistics)
tablestat<-table(statistic)
View(tablestat)

par(mfrow = c(4, 5))  # Imposta il layout per 4 righe e 5 colonne di barplot
  # Crea il barplot
  barplot(c(mean = mean_val, median = median_val, mode = mode_val),
          beside = TRUE,
          names.arg = c("Media", "Mediana", "Moda"),
          col = c("blue", "green", "red"),
          main = paste("Australia -", drug),
          ylim = c(0, max(c(mean_val, median_val, mode_val)) + 1))
