# Caricare il dataset
data <- read.csv("drug_consumption.csv")

# Definizione delle fasce di età
tabellaAge <- table(data$Age)


#range di età
#range di età 18-24     1
r18_24<-subset(data,Age== -0.95197)
View(r18_24)
#range di età 25-34     2
r25_34<-subset(tabellaAge,Age== -0.07854)
View(r25_34)
#range di età 34-44     3
r35_44<-subset(tabellaAge,Age== 0.49788)
View(r35_44)
#range di età 45-54     4
r45_54<-subset(tabellaAge,Age== 1.09449)
View(r45_54)
#range di età 55-65     5
r55_64<-subset(tabellaAge,Age== 1.82213)
View(r55_64)
#range di età 65+       6
r65<-subset(tabellaAge,Age== 2.59171)
View(r65)

#visualizzo 
applayr65<-lapply(r65,table)
View(applayr65)

as.data.frame(applay65)





#cambiamo i valori delle età da numerici a carattere
tabellaAge= as.character(subset(tabellaAge,tabellaAge==-0.95197))
View(tabellaAge)
