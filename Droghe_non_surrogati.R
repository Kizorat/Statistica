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
#calcolo della media,mediana,moda di tutte le droghe
as.

#prendo tutte le droghe che non sono surrogati
sub_data<-subset(data,select = c(Alcohol,Amphet,Amyl,Benzos,Caff,Cannabis,Choc,Coke,Ecstasy,Heroin,Ketamine,LSD,Meth,Mushrooms,Nicotine))
#trasformo il dataset in dataframe
as.data.frame(sub_data)

#Alcolici media,mediana,moda
median(sub_data$Alcohol)
mean(sub_data$Alcohol)
table(sub_data$Alcohol)

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



#creazione della table dai dati delle droghe non surrogate
risultati<-lapply(sub_data, table)
View(risultati)


#creaziione del boxplot
boxplot(risultati,"Sostanze non surrogate",ylab="valore",las=2,col=rainbow(15),ylim=c(0,1885))