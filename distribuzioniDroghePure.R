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
#calcolo della media,mediana,moda di tutte le droghe



#prendo tutte le droghe che non sono surrogati
sub_data<-subset(data,select = c(Alcohol,Amphet,Amyl,Benzos,Caff,Cannabis,Choc,Coke,Ecstasy,Heroin,Ketamine,LSD,Meth,Mushrooms,Nicotine))
#trasformo il dataset in dataframe
as.data.frame(sub_data)

View(sub_data)


median(risultati$Alcohol)
mean(risultati$Alcohol)
quantile(risultati$Alcohol)
table(sub_data$Alcohol)


median(risultati$Amphet)
mean(risultati$Amphet)
quantile(risultati$Amphet)
table(sub_data$Amphet) 

median(risultati$Amyl)
mean(risultati$Amyl)
quantile(risultati$Amyl)
table(sub_data$Amyl)

median(risultati$Benzos)
mean(risultati$Benzos)
quantile(risultati$Benzos)
table(sub_data$Benzos)

median(risultati$Caff)
mean(risultati$Caff)
quantile(risultati$Caff)
table(sub_data$Caff)

median(risultati$Cannabis)
mean(risultati$Cannabis)
quantile(risultati$Cannabis)
table(sub_data$Cannabis)

median(risultati$Choc)
mean(risultati$Choc)
quantile(risultati$Choc)
table(sub_data$Choc)

median(risultati$Coke)
mean(risultati$Coke)
quantile(risultati$Coke)
table(sub_data$Coke)

median(risultati$Ecstasy)
mean(risultati$Ecstasy)
quantile(risultati$Ecstasy)
table(sub_data$Ecstasy)

median(risultati$Heroin)
mean(risultati$Heroin)
quantile(risultati$Heroin)
table(sub_data$Heroin)

median(risultati$Ketamine)
mean(risultati$Ketamine)
quantile(risultati$Ketamine)
table(sub_data$Ketamine)

median(risultati$LSD)
mean(risultati$LSD)
quantile(risultati$LSD)
table(sub_data$LSD)

median(risultati$Meth)
mean(risultati$Meth)
quantile(risultati$Meth)
table(sub_data$Meth)

median(risultati$Mushrooms)
mean(risultati$Mushrooms)
quantile(risultati$Mushrooms)
table(sub_data$Mushrooms)

median(risultati$Nicotine)
mean(risultati$Nicotine)
quantile(risultati$Nicotine)
table(sub_data$Nicotine)




#creazione delle tabelle per le singole consumazioni
alcohol_data <- table(sub_data$Alcohol)
amphet_data <- table(sub_data$Amphet)
amyl_data <- table(sub_data$Amyl)
benzos_data <- table(sub_data$Benzos)
caff_data <- table(sub_data$Caff)
cannabis_data <- table(sub_data$Cannabis)
choc_data <- table(sub_data$Choc)
coke_data <- table(sub_data$Coke)  
ecstasy_data <- table(sub_data$Ecstasy)
heroin_data <- table(sub_data$Heroine)
ketamine_data <- table(sub_data$Ketamine)
LSD_data <- table(sub_data$LSD)
meth_data <- table(sub_data$Meth)
nicotine_data <- table(sub_data$Nicorine) 




#calcolo delle mediane
mediana_ordinate<-apply(sub_data, 2, median)
sort(mediana_ordinate,decreasing = TRUE)
# Stampa le mediane ordinate
print(mediana_ordinate)

# Opzionalmente, crea un barplot per visualizzarle
barplot(mediana_ordinate, 
        main="Mediane delle Sostanze (dal valore più alto)",
        ylab="Valore Mediano",
        las=2,
        col=rainbow(length(mediana_ordinate)))

#creazione della table dai dati delle droghe non surrogate
risultati<-lapply(sub_data, table)
View(risultati)
#media,mediana e quantili delle sigole droghe nella variabile risultati

crea_barplot_con_picchi <- function(data, nome_sostanza) {
  # Calcolare media e mediana
  media <- mean(data)
  mediana <- median(data)
  quartili <- quantile(data)
  
  # Creare il barplot
  barplot(data, col = "lightblue", main = paste("Distribuzione:", nome_sostanza),
          ylab = "Conteggi", ylim = c(0, max(data) * 1.2)) # Espandi l'asse y
  
  # Aggiungere le linee per la media e la mediana
  abline(h = media, col = "red", lwd = 2, lty = 2)   # Linea rossa per la media
  abline(h = mediana, col = "green", lwd = 2, lty = 2)  # Linea verde per la mediana
  #abline(h = quartili, col = "blue",lwd = 2, lty = 2) #La linea blu indica i quartili
  
  # Aggiungere una legenda
  legend("topright", legend = c(paste("Media =", round(media, 2)), paste("Mediana =", round(mediana, 2))
                                #, paste("Quartili =", round(quartili, 2))
  ),
  col = c("red", "green","blue"), lty = 2, lwd = 2)
}

# Creare i barplot per ciascuna sostanza
crea_barplot_con_picchi(alcohol_data, "Alcohol")
crea_barplot_con_picchi(amphet_data, "Amphet")
crea_barplot_con_picchi(amyl_data, "Amyl")
crea_barplot_con_picchi(benzos_data, "Benzos")
crea_barplot_con_picchi(caff_data, "Caffeina")
crea_barplot_con_picchi(cannabis_data, "Cannabis")
crea_barplot_con_picchi(choc_data, "Choc")
crea_barplot_con_picchi(coke_data, "Coke")
crea_barplot_con_picchi(ecstasy_data, "Ecstacy")
crea_barplot_con_picchi(heroin_data, "Heroin")
crea_barplot_con_picchi(ketamine_data, "Ketamine")
crea_barplot_con_picchi(LSD_data, "LSD")
crea_barplot_con_picchi(meth_data, "Meth")
crea_barplot_con_picchi(mushrooms_data, "Mushrooms")
crea_barplot_con_picchi(nicotine_data, "Nicotine")  
##eroina,funghi,nicotina