#dataset
data<- read.csv("drug_consumption.csv",sep=",",header=T)
#

#sommario totale
summary(data)

#iniziale descrizione del consumo in base all'eta e il sesso delle droghe
tableGenderAge<-c("ID","Age","Gender","Alcohol","Amphet","Amyl","Benzos","Caff","Cannabis","Choc","Crack","Coke","Ecstasy","Heroin","Heroin","Ketamine","Legalh","LSD","Meth","Mushrooms","Nicotine","Semer","VSA")
tableG<-data[,tableGenderAge]
View(tableG)

#divisione per gender Y 447 Maschi
GenderY<-subset(tableG,Gender==-0.48246)
View(GenderY)
#divisione per gender X 942 Femmine
GenderX<-subset(tableG,Gender==0.48246)
View(GenderX)


#Femmine
#range di età 18-24     1
range1<-subset(GenderX,Age== -0.95197)
View(range1)
#range di età 25-34     2
range2<-subset(GenderX,Age== -0.07854)
View(range2)
#range di età 34-44     3
range3<-subset(GenderX,Age== 0.49788)
View(range3)
#range di età 45-54     4
range4<-subset(GenderX,Age== 1.09449)
View(range4)
#range di età 55-65     5
range5<-subset(GenderX,Age== 1.82213)
View(range5)
#range di età 65+       6
range6<-subset(GenderX,Age== 2.59171)
View(range6)



#suddivisione in base al cosumo:
#CL0:mai usato, CL1:non usato nell'utlima decade, 
#CL2:usato nell'ultima decade(10 anni), CL3: usato nell'ultimo anno, 
#CL4:usato nell'ultimo mese, CL5:usato nell'ultima settimana, CL6: usato nell'ultimo giorno

#range di età 18-24 ******************************************************
tableAlcool1<-table(range1$Alcohol)
View(tableAlcool1)

tableAmphet1<-table(range1$Amphet)
View(tableAmphet1)

tableAmyl1<-table(range1$Amyl)
View(tableAmyl1)

tableBenzos1<-table(range1$Benzos)
View(tableBenzos1)

tableCaff1<-table(range1$Caff)
View(tableCaff1)

tableCannabis1<-table(range1$Amyl)
View(tableCannabis1)

tableChoc1<-table(range1$Choc)
View(tableChoc1)

tableCoke1<-table(range1$Coke)
View(tableCoke1)

tableCrack1<-table(range1$Crack)
View(tableCrack1)

tableEcstasy1<-table(range1$Ecstasy)
View(tableEcstasy1)

tableHeroin1<-table(range1$Heroin)
View(tableHeroin1)

tableKetamin1<-table(range1$Ketamin)
View(tableKetamin1)

tableLegalh1<-table(range1$Legalh)
View(tableLegalh1)

tableLSD1<-table(range1$LSD)
View(tableLSD1)

tableMeth1<-table(range1$Meth)
View(tableMeth1)

tableMushrooms1<-table(range1$Mushrooms)
View(tableMushrooms1)

tableNicotine1<-table(range1$Nicotine)
View(tableNicotine1)

tableSemer1<-table(range1$Semer)
View(tableSemer1)

tableVSA1<-table(range1$VSA)
View(tableVSA1)
#range di età 25-34 ******************************************************
tableAlcool2 <- table(range2$Alcohol)
View(tableAlcool2)

tableAmphet2 <- table(range2$Amphet)
View(tableAmphet2)

tableAmyl2 <- table(range2$Amyl)
View(tableAmyl2)

tableBenzos2 <- table(range2$Benzos)
View(tableBenzos2)

tableCaff2 <- table(range2$Caff)
View(tableCaff2)

tableCannabis2 <- table(range2$Cannabis) 
View(tableCannabis2)

tableChoc2 <- table(range2$Choc)
View(tableChoc2)

tableCoke2 <- table(range2$Coke)
View(tableCoke2)

tableCrack2 <- table(range2$Crack)
View(tableCrack2)

tableEcstasy2 <- table(range2$Ecstasy)
View(tableEcstasy2)

tableHeroin2 <- table(range2$Heroin)
View(tableHeroin2)

tableKetamin2 <- table(range2$Ketamin)
View(tableKetamin2)

tableLegalh2 <- table(range2$Legalh)
View(tableLegalh2)

tableLSD2 <- table(range2$LSD)
View(tableLSD2)

tableMeth2 <- table(range2$Meth)
View(tableMeth2)

tableMushrooms2 <- table(range2$Mushrooms)
View(tableMushrooms2)

tableNicotine2 <- table(range2$Nicotine)
View(tableNicotine2)

tableSemer2 <- table(range2$Semer)
View(tableSemer2)

tableVSA2 <- table(range2$VSA)
View(tableVSA2)
#range di età 34-44 ******************************************************
tableAlcool3 <- table(range3$Alcohol)
View(tableAlcool3)

tableAmphet3 <- table(range3$Amphet)
View(tableAmphet3)

tableAmyl3 <- table(range3$Amyl)
View(tableAmyl3)

tableBenzos3 <- table(range3$Benzos)
View(tableBenzos3)

tableCaff3 <- table(range3$Caff)
View(tableCaff3)

tableAmyl3 <- table(range3$Amyl)
View(tableAmyl3)

tableChoc3 <- table(range3$Choc)
View(tableChoc3)

tableCoke3 <- table(range3$Coke)
View(tableCoke3)

tableCrack3 <- table(range3$Crack)
View(tableCrack3)

tableEcstasy3 <- table(range3$Ecstasy)
View(tableEcstasy3)

tableHeroin3 <- table(range3$Heroin)
View(tableHeroin3)

tableKetamin3 <- table(range3$Ketamin)
View(tableKetamin3)

tableLegalh3 <- table(range3$Legalh)
View(tableLegalh3)

tableLSD3 <- table(range3$LSD)
View(tableLSD3)

tableMeth3 <- table(range3$Meth)
View(tableMeth3)

tableMushrooms3 <- table(range3$Mushrooms)
View(tableMushrooms3)

tableNicotine3 <- table(range3$Nicotine)
View(tableNicotine3)

tableSemer3 <- table(range3$Semer)
View(tableSemer3)

tableVSA3 <- table(range3$VSA)
View(tableVSA3)
#range di età 45-54 ******************************************************
tableAlcool4 <- table(range4$Alcohol)
View(tableAlcool4)

tableAmphet4 <- table(range4$Amphet)
View(tableAmphet4)

tableAmyl4 <- table(range4$Amyl)
View(tableAmyl4)

tableBenzos4 <- table(range4$Benzos)
View(tableBenzos4)

tableCaff4 <- table(range4$Caff)
View(tableCaff4)

tableAmyl4 <- table(range4$Amyl)  
View(tableAmyl4)

tableChoc4 <- table(range4$Choc)
View(tableChoc4)

tableCoke4 <- table(range4$Coke)
View(tableCoke4)

tableCrack4 <- table(range4$Crack)
View(tableCrack4)

tableEcstasy4 <- table(range4$Ecstasy)
View(tableEcstasy4)

tableHeroin4 <- table(range4$Heroin)
View(tableHeroin4)

tableKetamin4 <- table(range4$Ketamin)
View(tableKetamin4)

tableLegalh4 <- table(range4$Legalh)
View(tableLegalh4)

tableLSD4 <- table(range4$LSD)
View(tableLSD4)

tableMeth4 <- table(range4$Meth)
View(tableMeth4)

tableMushrooms4 <- table(range4$Mushrooms)
View(tableMushrooms4)

tableNicotine4 <- table(range4$Nicotine)
View(tableNicotine4)

tableSemer4 <- table(range4$Semer)
View(tableSemer4)

tableVSA4 <- table(range4$VSA)
View(tableVSA4)
#range di età 55-65 ******************************************************
tableAlcool5 <- table(range5$Alcohol)
View(tableAlcool5)

tableAmphet5 <- table(range5$Amphet)
View(tableAmphet5)

tableAmyl5 <- table(range5$Amyl)
View(tableAmyl5)

tableBenzos5 <- table(range5$Benzos)
View(tableBenzos5)

tableCaff5 <- table(range5$Caff)
View(tableCaff5)

tableAmyl5 <- table(range5$Amyl)  
View(tableAmyl5)

tableChoc5 <- table(range5$Choc)
View(tableChoc5)

tableCoke5 <- table(range5$Coke)
View(tableCoke5)

tableCrack5 <- table(range5$Crack)
View(tableCrack5)

tableEcstasy5 <- table(range5$Ecstasy)
View(tableEcstasy5)

tableHeroin5 <- table(range5$Heroin)
View(tableHeroin5)

tableKetamin5 <- table(range5$Ketamin)
View(tableKetamin5)

tableLegalh5 <- table(range5$Legalh)
View(tableLegalh5)

tableLSD5 <- table(range5$LSD)
View(tableLSD5)

tableMeth5 <- table(range5$Meth)
View(tableMeth5)

tableMushrooms5 <- table(range5$Mushrooms)
View(tableMushrooms5)

tableNicotine5 <- table(range5$Nicotine)
View(tableNicotine5)

tableSemer5 <- table(range5$Semer)
View(tableSemer5)

tableVSA5 <- table(range5$VSA)
View(tableVSA5)
#range di età 65+ ******************************************************
tableAlcool6 <- table(range6$Alcohol)
View(tableAlcool6)

tableAmphet6 <- table(range6$Amphet)
View(tableAmphet6)

tableAmyl6 <- table(range6$Amyl)
View(tableAmyl6)

tableBenzos6 <- table(range6$Benzos)
View(tableBenzos6)

tableCaff6 <- table(range6$Caff)
View(tableCaff6)

tableAmyl6 <- table(range6$Amyl)  
View(tableAmyl6)

tableChoc6 <- table(range6$Choc)
View(tableChoc6)

tableCoke6 <- table(range6$Coke)
View(tableCoke6)

tableCrack6 <- table(range6$Crack)
View(tableCrack6)

tableEcstasy6 <- table(range6$Ecstasy)
View(tableEcstasy6)

tableHeroin6 <- table(range6$Heroin)
View(tableHeroin6)

tableKetamin6 <- table(range6$Ketamin)
View(tableKetamin6)

tableLegalh6 <- table(range6$Legalh)
View(tableLegalh6)

tableLSD6 <- table(range6$LSD)
View(tableLSD6)

tableMeth6 <- table(range6$Meth)
View(tableMeth6)

tableMushrooms6 <- table(range6$Mushrooms)
View(tableMushrooms6)

tableNicotine6 <- table(range6$Nicotine)
View(tableNicotine6)

tableSemer6 <- table(range6$Semer)
View(tableSemer6)

tableVSA6 <- table(range6$VSA)
View(tableVSA6)

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












