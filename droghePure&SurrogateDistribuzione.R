data<-read.csv("drug_consumption.csv",sep=",",header =T)
#Qr: studio della quantità di droga utilizzata in base allo spettro caratteriale tra droghe surrogate e non

#modifica dei CL in valori numerici:
convertitore_CL<-function(x){
  if(is.character((x)))
    return(as.numeric(sub("CL","",x)))
  return(x)
}
#passaggio delle colonne
for(col in names(data)){
  data[[col]]<-convertitore_CL(data[[col]])
}
View(data)


#effettuare un boxplot


#divisione droghe SURROGATE:
tableSurrogato<-c("ID","Age","Gender","Crack","Legalh","Semer","VSA")
tableS<-data[,tableSurrogato]
View(tableS)
#tabella solo delle droghe surrogate
tableSoloSur<-c("Crack","Legalh","Semer","VSA")
tableDrugSurrogate<-data[,tableSoloSur]
View(tableDrugSurrogate)

#conteggio frequenza surrogate
applaySurrogate<-lapply(tableDrugSurrogate,table)
View(applaySurrogate)

boxplot(applaySurrogate,"sostanze surrogate",ylab="valore",las=2,col=c("red","green","blue","purple"))
legend("left",1,1885,legend=c("Crack","Legalh","Semer","VSA"),col=c("red","green","blue","purple"),lty=1:1,cex = 0.6, xpd = TRUE)

#media mediana Crack
median(tableDrugSurrogate$Crack)
mean(tableDrugSurrogate$Crack)
quantile(tableDrugSurrogate$Crack)
table(tableDrugSurrogate$Crack)

#media mediana Legalh
median(tableDrugSurrogate$Legalh)
mean(tableDrugSurrogate$Legalh)
quantile(tableDrugSurrogate$Legalh)
table(tableDrugSurrogate$Legalh)

#media mediana Semer
median(tableDrugSurrogate$Semer)
mean(tableDrugSurrogate$Semer)
quantile(tableDrugSurrogate$Semer)
table(tableDrugSurrogate$Semer)

#media mediana VSA
median(tableDrugSurrogate$VSA)
mean(tableDrugSurrogate$VSA)
quantile(tableDrugSurrogate$VSA)
table(tableDrugSurrogate$VSA)

# Dati per ogni sostanza
crack_data <- table(tableDrugSurrogate$Crack)
legalh_data <- table(tableDrugSurrogate$Legalh)
semer_data <- table(tableDrugSurrogate$Semer)
vsa_data <- table(tableDrugSurrogate$VSA)

# Funzione per creare un barplot con media e mediana
crea_barplot_con_picchi <- function(data, nome_sostanza) {
  # Calcolare media e mediana
  media <- mean(data)
  mediana <- median(data)
  #quartili <- quantile(data)
  
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
crea_barplot_con_picchi(crack_data, "Crack")
crea_barplot_con_picchi(legalh_data, "Legalh")
crea_barplot_con_picchi(semer_data, "Semer")
crea_barplot_con_picchi(vsa_data, "VSA")



