#RQ:Studio della quantità di droga utilizzata in base allo spettro caratteriale della droga tra droghe surrogate e non.
#acquisizione del dataset
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


#prendo tutte le droghe che non sono surrogati
sub_data<-subset(data,select = c(Alcohol,Amphet,Amyl,Benzos,Caff,Cannabis,Choc,Coke,Ecstasy,Heroin,Ketamine,LSD,Meth,Mushrooms,Nicotine))
#trasformo il dataset in dataframe
as.data.frame(sub_data)

#Alcolici media,mediana,e quartile moda
median(sub_data$Alcohol)
mean(sub_data$Alcohol)
table(sub_data$Alcohol)
quantile(sub_data$Alcohol)

#Amphet media,mediana,moda
median(sub_data$Amphet)
mean(sub_data$Amphet)
table(sub_data$Amphet)

#Amyl media,mediana,moda
median(sub_data$Amyl)
mean(sub_data$Amyl)
table(sub_data$Amyl)

#Benzos media,mediana,moda
median(sub_data$Benzos)
mean(sub_data$Benzos)
table(sub_data$Benzos)

#Caff media,mediana,moda
median(sub_data$Caff)
mean(sub_data$Caff)
table(sub_data$Caff)

#Cannabis media,mediana,moda
median(sub_data$Cannabis)
mean(sub_data$Cannabis)
table(sub_data$Cannabis)

#Choc media,mediana,moda
median(sub_data$Choc)
mean(sub_data$Choc)
table(sub_data$Choc)

#Coke media,mediana,moda
median(sub_data$Coke)
mean(sub_data$Coke)
table(sub_data$Coke)

#Ecstasy media,mediana,moda
median(sub_data$Ecstasy)
mean(sub_data$Ecstasy)
table(sub_data$Ecstasy)

#Heroin media,mediana,moda
median(sub_data$Heroin)
mean(sub_data$Heroin)
table(sub_data$Heroin)

#Ketamine media,mediana,moda
median(sub_data$Ketamine)
mean(sub_data$Ketamine)
table(sub_data$Ketamine)

#LSD media,mediana,moda
median(sub_data$LSD)
mean(sub_data$LSD)
table(sub_data$LSD)

#Meth media,mediana,moda
median(sub_data$Meth)
mean(sub_data$Meth)
table(sub_data$Meth)

#Mushrooms media,mediana,moda
median(sub_data$Mushrooms)
mean(sub_data$Mushrooms)
table(sub_data$Mushrooms)

#Nicotine media,mediana,moda
median(sub_data$Nicotine)
mean(sub_data$Nicotine)
table(sub_data$Nicotine)

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
median(risultati$Alcohol)
mean(risultati$Alcohol)
quantile(risultati$Alcohol)

median(risultati$Amphet)
mean(risultati$Amphet)
quantile(risultati$Amphet)

median(risultati$Amyl)
mean(risultati$Amyl)
quantile(risultati$Amyl)

median(risultati$Benzos)
mean(risultati$Benzos)
quantile(risultati$Benzos)

median(risultati$Caff)
mean(risultati$Caff)
quantile(risultati$Caff)

median(risultati$Cannabis)
mean(risultati$Cannabis)
quantile(risultati$Cannabis)

median(risultati$Choc)
mean(risultati$Choc)
quantile(risultati$Choc)

median(risultati$Coke)
mean(risultati$Coke)
quantile(risultati$Coke)

median(risultati$Ecstasy)
mean(risultati$Ecstasy)
quantile(risultati$Ecstasy)

median(risultati$Heroin)
mean(risultati$Heroin)
quantile(risultati$Heroin)

median(risultati$Ketamine)
mean(risultati$Ketamine)
quantile(risultati$Ketamine)

median(risultati$LSD)
mean(risultati$LSD)
quantile(risultati$LSD)

median(risultati$Meth)
mean(risultati$Meth)
quantile(risultati$Meth)

median(risultati$Mushrooms)
mean(risultati$Mushrooms)
quantile(risultati$Mushrooms)

median(risultati$Nicotine)
mean(risultati$Nicotine)
quantile(risultati$Nicotine)

summary(risultati)

#seleziona le colonne numeriche
#risultati_numerici<-risultati[sapply(risultati,is.numeric)]

#boxplot_stats <- lapply(risultati_numerici, boxplot.stats)

#View(boxplot_stats)
#creaziione del boxplot
boxplot(risultati,notch = FALSE,main="Sostanze non surrogate",ylab="Frequenza assoluta",las=2,col=rainbow(15))
legend("topright",legend = c("Alcohol","Amphet","Amyl","Benzos","Caff","Cannabis","Choc","Coke","Ecstasy","Heroin","Ketamine","LSD","Meth","Mushrooms","Nicotine"),fill = rainbow(15),inset = c(0,-0.3),xpd = TRUE,cex = 0.5,ncol = 8)
#creazione di un dataframe formato dai soli utilizzatori di droga


