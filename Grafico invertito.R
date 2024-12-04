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

#tabella solo delle droghe surrogate
tableSoloSur<-c("Crack","Legalh","Semer","VSA")
tableDrugSurrogate<-data[,tableSoloSur]
View(tableDrugSurrogate)


#conteggio frequenza surrogate
applaySurrogate<-lapply(tableDrugSurrogate,table)
View(applaySurrogate)

boxplot(applaySurrogate,"valore",ylab="sostanze surrogate",las=2,col=c("red","green","blue","purple"))
legend("left",1,1885,legend=c("Crack","Legalh","Semer","VSA"),col=c("red","green","blue","purple"),lty=1:1,cex = 0.6, xpd = TRUE)



