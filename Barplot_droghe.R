#aquisizione del dataset
data<-read.csv("drug_consumption.csv",sep=",",header = T)
data[] <- lapply(data, function(x) if (is.factor(x)) as.character(x) else x)
View(data)
summary(data)
#acquisizione delle  colonne relative al paese di provenienza e le droghe presenti nel dataset
nome_colonne<-c("ID","Age","Gender","Alcohol","Amphet","Amyl","Benzos","Caff","Cannabis","Choc","Coke","Crack","Ecstasy","Heroin","Ketamine","Legalh","LSD","Meth","Mushrooms","Nicotine","Semer","VSA")


#Acquisisco il genere Y
genereY<-subset(tabella, Gender== -0.48246)
View(genereY)
#Acquisisco il genere X
genereX<-subset(tabella, Gender== 0.48246)
View(genereX)


#range di età 18-24 per Y
range1<-subset(genereY, Age==-0.95197)
View(range1)
#range di età 25-34 per Y
range2<-subset(genereY, Age==-0.07854)
View(range2)
#range di età 35-44 per Y
range3<-subset(genereY, Age==0.49788)
View(range3)
#range di età 45-54 per Y
range4<-subset(genereY, Age==1.09449)
View(range4)
#range di età 55-64 per Y
range5<-subset(genereY, Age==1.82213)
View(range5)
#range di età 65+ per Y
range6<-subset(genereY, Age==2.59171)
View(range6)



#funzione di sostituzione dei valori LC presenti nel dataframe in valori numeri
sostituisci_livelli <- function(data, colonne) {
  
  # Definisco la mappatura dei livelli
  mappa_livelli <- c("CL0" = 0, "CL1" = 1, "CL2" = 2,"CL3" = 3, "CL4" = 4, "CL5" = 5, "CL6" = 6)
  
  # Applica la mappatura alle colonne selezionate
  data[colonne] <- lapply(data[colonne], function(colonna) {
    as.numeric(mappa_livelli[as.character(colonna)])
  })
  
  return(data)
}
nome_droghe<-c("Alcohol","Amphet","Amyl","Benzos","Caff","Cannabis","Choc","Coke","Crack","Ecstasy","Heroin","Ketamine","Legalh","LSD","Meth","Mushrooms","Nicotine","Semer","VSA")

range1<-sostituisci_livelli(range1,nome_droghe)
droghe<-as.data.frame(range1)
droghe<-droghe[-(1:3)]
barplot(droghe,main="Assunzione di droghe per uomini da 18-24 anni",cex.names = 0.4, ylab="Quantità",xlab="Droghe",col = rainbow(7),beside = TRUE)
#conta le occorenze di ogni colonna nel dataframe, il paramentro 2 stabilisce le colonne(1 righe)
tabella_droghe<- apply(range1,2, table)
#Cancello dalla lista le prime 3 righe
tabella_droghe<-tabella_droghe[-(1:3)]


Alcohol<-table(range1$Alcohol)
Alcohol <- as.data.frame(Alcohol,name)
View(Alcohol)
barplot(Alcohol,main="Assunzione di alcohol",col=rainbow(6))
# tabella_droghe[,"Alcohol"]<-Alcohol
# View(tabella_droghe)
# Amphet<-table(range1$Amphet)
# Amyl<-table(range1$Amyl)
# Benzos<-table(range1$Benzos)
# Caff<-table(range1$Caff)
# Cannabis<-table(range1$Cannabis)
# Choc<-table(range1$Choc)
# Coke<-table(range1$Coke)
# Crack<-table(range1$Crack)
# Ecstasy<-table(range1$Ecstasy)
# Heroin<-table(range1$Heroin)
# Ketamine<-table(range1$Ketamine)
# Legalh<-table(range1$Legalh)
# LSD<-table(range1$LSD)
# Meth<-table(range1$Meth)
# Mushrooms<-table(range1$Mushrooms)
# Nicotine<-table(range1$Nicotine)
# Semer<-table(range1$Semer)
# VSA<-table(range1$VSA)




