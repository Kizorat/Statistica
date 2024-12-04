data<-read.csv("drug_consumption.csv",sep=",",header = T)



# Definizione delle fasce di età
data$Age<- cut(data$Age, breaks = c(-Inf, -0.95197,-0.07854,0.49788,1.09449,1.82213,2.59171, Inf), labels = c("18", "18-24", "25-34", "35-44", "45-54","55-64","65+"),right = FALSE)
data$Gender<-cut(data$Gender,breaks = c(-Inf ,-0.48246, Inf),labels = c("Femmina","Maschio"),right = TRUE)
#riscrivo il dataset
write.csv(data, "drug_consumption_modificato.csv", row.names = FALSE)

#stampo nuovo csv
data_modifcato<-read.csv("drug_consumption_modificato.csv",sep=",",header = T)
View(data_modifcato)
#Acquisisco solo coloro che si trovano in un range di età superiore a 65
genere_eta<-subset(data_modifcato,data_modifcato$Age>65,select = c(Age,Gender))
genere_eta2<-subset(data_modifcato,data_modifcato$Age>54 & data_modifcato$Age<65,select = c(Age,Gender))
genere_eta3<-subset(data_modifcato,data_modifcato$Age>44 & data_modifcato$Age<55,select = c(Age,Gender))
genere_eta4<-subset(data_modifcato,data_modifcato$Age>34 & data_modifcato$Age<45,select = c(Age,Gender))
genere_eta5<-subset(data_modifcato,data_modifcato$Age>24 & data_modifcato$Age<35,select = c(Age,Gender))
genere_eta6<-subset(data_modifcato,data_modifcato$Age>18 & data_modifcato$Age<25,select = c(Age,Gender))
View(genere_eta)
View(genere_eta2)
View(genere_eta3)
View(genere_eta4)
View(genere_eta5)
View(genere_eta6)
#genero una tabella relativi al genere
conteggio_generi<-table(genere_eta$Gender)
conteggio_generi2<-table(genere_eta2$Gender)
conteggio_generi3<-table(genere_eta3$Gender)
conteggio_generi4<-table(genere_eta4$Gender)
conteggio_generi5<-table(genere_eta5$Gender)
conteggio_generi6<-table(genere_eta6$Gender)
View(conteggio_generi)
View(conteggio_generi2)

#costruzione di un grafico a torta
pie(conteggio_generi,labels = names(conteggio_generi),main="Grafico sulle categorie di genere di età over 65")
pie(conteggio_generi2,labels = names(conteggio_generi2),main="Grafico sulle categorie di genere di età compresa tra 55 e 64")
pie(conteggio_generi3,labels = names(conteggio_generi3),main="Grafico sulle categorie di genere di età compresa tra 45 e 54")
pie(conteggio_generi4,labels = names(conteggio_generi4),main="Grafico sulle categorie di genere di età compresa tra 34 e 44")
pie(conteggio_generi5,labels = names(conteggio_generi5),main="Grafico sulle categorie di genere di età compresa tra 24 e 34")
pie(conteggio_generi6,labels = names(conteggio_generi6),main="Grafico sulle categorie di genere di età compresa tra 18 e 24")
------------------------------------------------------
  
  #aquisisco il genere femminile  
  Alcohol_genereF<-subset(data_modifcato,data_modifcato$Gender=="Femmina",select = c(Gender,Alcohol))
#aquisisco il genere maschile
Alcohol_genereU<-subset(data_modifcato,data_modifcato$Gender=="Maschio",select = c(Gender,Alcohol))
#creo una table sia per genere maschile che femminile
conteggio_AlcoholF<-table(Alcohol_genereF)
conteggio_AlcoholU<-table(Alcohol_genereU)
#inserisco in una singola table i valori ripettivi alla frequenza delle due table
conteggi_consumi<-rbind(Femminile=conteggio_AlcoholF,Maschile=conteggio_AlcoholU)
View(conteggi_consumi)
#creo un barplot del conteggio del consumo degli alcolici
barplot(conteggi_consumi,main="Confronto dei consumi tra generi",xlab = "Tipo di consumo",ylab = "Frequenza",col = c("pink","blue"),legend.text = rownames(conteggi_consumi),args.legend = list(title = "Genere", x = "topright"),ylim = c(0,1200))

-----------------------------------------
  #aquisisco il genere femminile  
  Amphet_genereF<-subset(data_modifcato,data_modifcato$Gender=="Femmina",select = c(Gender,Amphet))
#aquisisco il genere maschile
Amphet_genereU<-subset(data_modifcato,data_modifcato$Gender=="Maschio",select = c(Gender,Amphet))
#creo una table sia per genere maschile che femminile
conteggio_AmphetF<-table(Amphet_genereF)
conteggio_AmphetU<-table(Amphet_genereU)
#inserisco in una singola table i valori ripettivi alla frequenza delle due table
conteggi_consumi2<-rbind(Femminile=conteggio_AmphetF,Maschile=conteggio_AmphetU)
View(conteggi_consumi2)
#creo un barplot del conteggio del consumo delle amphet
barplot(conteggi_consumi2,main="Confronto dei consumi tra generi",xlab = "Tipo di consumo",ylab = "Frequenza",col = c("pink","blue"),legend.text = rownames(conteggi_consumi2),args.legend = list(title = "Genere", x = "topright"),ylim = c(0,1200))
----------------------------------------------------
  
  #manca cl 6 per il genere maschile 6  
  
  #aquisisco il genere femminile  
  Amyl_genereF<-subset(data_modifcato,data_modifcato$Gender=="Femmina",select = c(Gender,Amyl))
#aquisisco il genere maschile
Amyl_genereU<-subset(data_modifcato,data_modifcato$Gender=="Maschio",select = c(Gender,Amyl))
#creo una table sia per genere maschile che femminile
conteggio_AmylF<-table(Amyl_genereF)
conteggio_AmylU<-table(Amyl_genereU)
#inserisco in una singola table i valori ripettivi alla frequenza delle due table
conteggi_consumi3<-rbind(Femminile=conteggio_AmylF,Maschile=conteggio_AmylU)
View(conteggi_consumi3)
#creo un barplot del conteggio del consumo delle amphet
boxplot(conteggi_consumi3,main="Confronto dei consumi tra generi",xlab = "Tipo di consumo",ylab = "Frequenza",col = c("pink","blue"),legend.text = rownames(conteggi_consumi3),args.legend = list(title = "Genere", x = "topright"),ylim = c(0,1200))
----------------------------------------------------
  #aquisisco il genere femminile  
  Benzos_genereF<-subset(data_modifcato,data_modifcato$Gender=="Femmina",select = c(Gender,Benzos))
#aquisisco il genere maschile
Benzos_genereU<-subset(data_modifcato,data_modifcato$Gender=="Maschio",select = c(Gender,Benzos))
#creo una table sia per genere maschile che femminile
conteggio_BenzosF<-table(Benzos_genereF)
conteggio_BenzosU<-table(Benzos_genereU)
#inserisco in una singola table i valori ripettivi alla frequenza delle due table
conteggi_consumi4<-rbind(Femminile=conteggio_BenzosF,Maschile=conteggio_BenzosU)
View(conteggi_consumi4)
#creo un barplot del conteggio del consumo delle amphet
barplot(conteggi_consumi4,main="Confronto dei consumi tra generi",xlab = "Tipo di consumo",ylab = "Frequenza",col = c("pink","blue"),legend.text = rownames(conteggi_consumi4),args.legend = list(title = "Genere", x = "topright"),ylim = c(0,1200))
----------------------------------------------
  #aquisisco il genere femminile  
  Caff_genereF<-subset(data_modifcato,data_modifcato$Gender=="Femmina",select = c(Gender,Caff))
#aquisisco il genere maschile
Caff_genereU<-subset(data_modifcato,data_modifcato$Gender=="Maschio",select = c(Gender,Caff))
#creo una table sia per genere maschile che femminile
conteggio_CaffF<-table(Caff_genereF)
conteggio_CaffU<-table(Caff_genereU)
#inserisco in una singola table i valori ripettivi alla frequenza delle due table
conteggi_consumi5<-rbind(Femminile=conteggio_CaffF,Maschile=conteggio_CaffU)
View(conteggi_consumi5)
#creo un barplot del conteggio del consumo delle amphet
barplot(conteggi_consumi5,main="Confronto dei consumi tra generi",xlab = "Tipo di consumo",ylab = "Frequenza",col = c("pink","blue"),legend.text = rownames(conteggi_consumi5),args.legend = list(title = "Genere", x = "topleft"),ylim = c(0,1800))
---------------------------------------------------------------------------
  #aquisisco il genere femminile  
  Cannabis_genereF<-subset(data_modifcato,data_modifcato$Gender=="Femmina",select = c(Gender,Cannabis))
#aquisisco il genere maschile
Cannabis_genereU<-subset(data_modifcato,data_modifcato$Gender=="Maschio",select = c(Gender,Cannabis))
#creo una table sia per genere maschile che femminile
conteggio_CannabisF<-table(Cannabis_genereF)
conteggio_CannabisU<-table(Cannabis_genereU)
#inserisco in una singola table i valori ripettivi alla frequenza delle due table
conteggi_consumi6<-rbind(Femminile=conteggio_CannabisF,Maschile=conteggio_CannabisU)
View(conteggi_consumi6)
#creo un barplot del conteggio del consumo delle amphet
barplot(conteggi_consumi6,main="Confronto dei consumi tra generi",xlab = "Tipo di consumo",ylab = "Frequenza",col = c("pink","blue"),legend.text = rownames(conteggi_consumi6),args.legend = list(title = "Genere", x = "topright"),ylim = c(0,800))
---------------------------------------------------------------------------
  
  #aggiungere colonna per la table dichocU  
  
  
  #aquisisco il genere femminile  
  Choc_genereF<-subset(data_modifcato,data_modifcato$Gender=="Femmina",select = c(Gender,Choc))
#aquisisco il genere maschile
Choc_genereU<-subset(data_modifcato,data_modifcato$Gender=="Maschio",select = c(Gender,Choc))
#creo una table sia per genere maschile che femminile
conteggio_ChocF<-table(Choc_genereF)
conteggio_ChocU<-table(Choc_genereU)
#inserisco in una singola table i valori ripettivi alla frequenza delle due table
conteggi_consumi7<-rbind(Femminile=conteggio_ChocF,Maschile=conteggio_ChocU)
View(conteggi_consumi7)
#creo un barplot del conteggio del consumo delle amphet
barplot(conteggi_consumi7,main="Confronto dei consumi tra generi",xlab = "Tipo di consumo",ylab = "Frequenza",col = c("pink","blue"),legend.text = rownames(conteggi_consumi7),args.legend = list(title = "Genere", x = "topright"),ylim = c(0,800))
---------------------------------------------------------------------------
  #aquisisco il genere femminile  
  Coke_genereF<-subset(data_modifcato,data_modifcato$Gender=="Femmina",select = c(Gender,Coke))
#aquisisco il genere maschile
Coke_genereU<-subset(data_modifcato,data_modifcato$Gender=="Maschio",select = c(Gender,Coke))
#creo una table sia per genere maschile che femminile
conteggio_CokeF<-table(Coke_genereF)
conteggio_CokeU<-table(Coke_genereU)
#inserisco in una singola table i valori ripettivi alla frequenza delle due table
conteggi_consumi7<-rbind(Femminile=conteggio_CokeF,Maschile=conteggio_CokeU)
View(conteggi_consumi7)
#creo un barplot del conteggio del consumo delle amphet
barplot(conteggi_consumi7,main="Confronto dei consumi tra generi",xlab = "Tipo di consumo",ylab = "Frequenza",col = c("pink","blue"),legend.text = rownames(conteggi_consumi7),args.legend = list(title = "Genere", x = "topright"),ylim = c(0,1800))
---------------------------------------------------------------------------    
  #inserire due colonne per la table u di crack
  
  #aquisisco il genere femminile  
  Crack_genereF<-subset(data_modifcato,data_modifcato$Gender=="Femmina",select = c(Gender,Crack))
#aquisisco il genere maschile
Crack_genereU<-subset(data_modifcato,data_modifcato$Gender=="Maschio",select = c(Gender,Crack))
#creo una table sia per genere maschile che femminile
conteggio_CrackF<-table(Crack_genereF)
conteggio_CrackU<-table(Crack_genereU)
#inserisco in una singola table i valori ripettivi alla frequenza delle due table
conteggi_consumi8<-rbind(Femminile=conteggio_CrackF,Maschile=conteggio_CrackU)
View(conteggi_consumi8)
#creo un barplot del conteggio del consumo delle amphet
barplot(conteggi_consumi8,main="Confronto dei consumi tra generi",xlab = "Tipo di consumo",ylab = "Frequenza",col = c("pink","blue"),legend.text = rownames(conteggi_consumi8),args.legend = list(title = "Genere", x = "topright"),ylim = c(0,1800))
---------------------------------------------------------------------------    
  
  #aquisisco il genere femminile  
  Ecstasy_genereF<-subset(data_modifcato,data_modifcato$Gender=="Femmina",select = c(Gender,Ecstasy))
#aquisisco il genere maschile
Ecstasy_genereU<-subset(data_modifcato,data_modifcato$Gender=="Maschio",select = c(Gender,Ecstasy))
#creo una table sia per genere maschile che femminile
conteggio_EcstasyF<-table(Ecstasy_genereF)
conteggio_EcstasyU<-table(Ecstasy_genereU)
#inserisco in una singola table i valori ripettivi alla frequenza delle due table
conteggi_consumi9<-rbind(Femminile=conteggio_EcstasyF,Maschile=conteggio_EcstasyU)
View(conteggi_consumi9)
#creo un barplot del conteggio del consumo delle amphet
barplot(conteggi_consumi9,main="Confronto dei consumi tra generi",xlab = "Tipo di consumo",ylab = "Frequenza",col = c("pink","blue"),legend.text = rownames(conteggi_consumi9),args.legend = list(title = "Genere", x = "topright"),ylim = c(0,1800))  
---------------------------------------------------------------------------    
  
  #aquisisco il genere femminile  
  Heroin_genereF<-subset(data_modifcato,data_modifcato$Gender=="Femmina",select = c(Gender,Heroin))
#aquisisco il genere maschile
Heroin_genereU<-subset(data_modifcato,data_modifcato$Gender=="Maschio",select = c(Gender,Heroin))
#creo una table sia per genere maschile che femminile
conteggio_HeroinF<-table(Heroin_genereF)
conteggio_HeroinU<-table(Heroin_genereU)
#inserisco in una singola table i valori ripettivi alla frequenza delle due table
conteggi_consumi10<-rbind(Femminile=conteggio_HeroinF,Maschile=conteggio_HeroinU)
View(conteggi_consumi10)
#creo un barplot del conteggio del consumo delle amphet
barplot(conteggi_consumi10,main="Confronto dei consumi tra generi",xlab = "Tipo di consumo",ylab = "Frequenza",col = c("pink","blue"),legend.text = rownames(conteggi_consumi10),args.legend = list(title = "Genere", x = "topright"),ylim = c(0,1800))  

---------------------------------------------------------------------------    
  
  #aquisisco il genere femminile  
  Ketamine_genereF<-subset(data_modifcato,data_modifcato$Gender=="Femmina",select = c(Gender,Ketamine))
#aquisisco il genere maschile
Ketamine_genereU<-subset(data_modifcato,data_modifcato$Gender=="Maschio",select = c(Gender,Ketamine))
#creo una table sia per genere maschile che femminile
conteggio_KetamineF<-table(Ketamine_genereF)
conteggio_KetamineU<-table(Ketamine_genereU)
#inserisco in una singola table i valori ripettivi alla frequenza delle due table
conteggi_consumi11<-rbind(Femminile=conteggio_KetamineF,Maschile=conteggio_KetamineU)
View(conteggi_consumi11)
#creo un barplot del conteggio del consumo delle amphet
barplot(conteggi_consumi11,main="Confronto dei consumi tra generi",xlab = "Tipo di consumo",ylab = "Frequenza",col = c("pink","blue"),legend.text = rownames(conteggi_consumi11),args.legend = list(title = "Genere", x = "topright"),ylim = c(0,2000))  

---------------------------------------------------------------------------    
  
  #aquisisco il genere femminile  
  Legalh_genereF<-subset(data_modifcato,data_modifcato$Gender=="Femmina",select = c(Gender,Legalh))
#aquisisco il genere maschile
Legalh_genereU<-subset(data_modifcato,data_modifcato$Gender=="Maschio",select = c(Gender,Legalh))
#creo una table sia per genere maschile che femminile
conteggio_LegalhF<-table(Legalh_genereF)
conteggio_LegalhU<-table(Legalh_genereU)
#inserisco in una singola table i valori ripettivi alla frequenza delle due table
conteggi_consumi12<-rbind(Femminile=conteggio_LegalhF,Maschile=conteggio_LegalhU)
View(conteggi_consumi12)
#creo un barplot del conteggio del consumo delle amphet
barplot(conteggi_consumi12,main="Confronto dei consumi tra generi",xlab = "Tipo di consumo",ylab = "Frequenza",col = c("pink","blue"),legend.text = rownames(conteggi_consumi12),args.legend = list(title = "Genere", x = "topright"),ylim = c(0,1800))  

--------------------------------------------------------------------------------
  #aquisisco il genere femminile  
  LSD_genereF<-subset(data_modifcato,data_modifcato$Gender=="Femmina",select = c(Gender,LSD))
#aquisisco il genere maschile
LSD_genereU<-subset(data_modifcato,data_modifcato$Gender=="Maschio",select = c(Gender,LSD))
#creo una table sia per genere maschile che femminile
conteggio_LSDF<-table(LSD_genereF)
conteggio_LSDU<-table(LSD_genereU)
#inserisco in una singola table i valori ripettivi alla frequenza delle due table
conteggi_consumi13<-rbind(Femminile=conteggio_LSDF,Maschile=conteggio_LSDU)
View(conteggi_consumi13)
#creo un barplot del conteggio del consumo delle amphet
barplot(conteggi_consumi13,main="Confronto dei consumi tra generi",xlab = "Tipo di consumo",ylab = "Frequenza",col = c("pink","blue"),legend.text = rownames(conteggi_consumi13),args.legend = list(title = "Genere", x = "topright"),ylim = c(0,1800))  

--------------------------------------------------------------------------------
  #aquisisco il genere femminile  
  Meth_genereF<-subset(data_modifcato,data_modifcato$Gender=="Femmina",select = c(Gender,Meth))
#aquisisco il genere maschile
Meth_genereU<-subset(data_modifcato,data_modifcato$Gender=="Maschio",select = c(Gender,Meth))
#creo una table sia per genere maschile che femminile
conteggio_MethF<-table(Meth_genereF)
conteggio_MethU<-table(Meth_genereU)
#inserisco in una singola table i valori ripettivi alla frequenza delle due table
conteggi_consumi14<-rbind(Femminile=conteggio_MethF,Maschile=conteggio_MethU)
View(conteggi_consumi14)
#creo un barplot del conteggio del consumo delle amphet
barplot(conteggi_consumi14,main="Confronto dei consumi tra generi",xlab = "Tipo di consumo",ylab = "Frequenza",col = c("pink","blue"),legend.text = rownames(conteggi_consumi14),args.legend = list(title = "Genere", x = "topright"),ylim = c(0,1800))  

--------------------------------------------------------------------------------
  #aquisisco il genere femminile  
  Mushrooms_genereF<-subset(data_modifcato,data_modifcato$Gender=="Femmina",select = c(Gender,Mushrooms))
#aquisisco il genere maschile
Mushrooms_genereU<-subset(data_modifcato,data_modifcato$Gender=="Maschio",select = c(Gender,Mushrooms))
#creo una table sia per genere maschile che femminile
conteggio_MushroomsF<-table(Mushrooms_genereF)
conteggio_MushroomsU<-table(Mushrooms_genereU)
#inserisco in una singola table i valori ripettivi alla frequenza delle due table
conteggi_consumi15<-rbind(Femminile=conteggio_MushroomsF,Maschile=conteggio_MushroomsU)
View(conteggi_consumi15)
#creo un barplot del conteggio del consumo delle amphet
barplot(conteggi_consumi15,main="Confronto dei consumi tra generi",xlab = "Tipo di consumo",ylab = "Frequenza",col = c("pink","blue"),legend.text = rownames(conteggi_consumi15),args.legend = list(title = "Genere", x = "topright"),ylim = c(0,1800))  

--------------------------------------------------------------------------------
  #aquisisco il genere femminile  
  Nicotine_genereF<-subset(data_modifcato,data_modifcato$Gender=="Femmina",select = c(Gender,Nicotine))
#aquisisco il genere maschile
Nicotine_genereU<-subset(data_modifcato,data_modifcato$Gender=="Maschio",select = c(Gender,Nicotine))
#creo una table sia per genere maschile che femminile
conteggio_NicotineF<-table(Nicotine_genereF)
conteggio_NicotineU<-table(Nicotine_genereU)
#inserisco in una singola table i valori ripettivi alla frequenza delle due table
conteggi_consumi16<-rbind(Femminile=conteggio_NicotineF,Maschile=conteggio_NicotineU)
View(conteggi_consumi16)
#creo un barplot del conteggio del consumo delle amphet
barplot(conteggi_consumi16,main="Confronto dei consumi tra generi",xlab = "Tipo di consumo",ylab = "Frequenza",col = c("pink","blue"),legend.text = rownames(conteggi_consumi16),args.legend = list(title = "Genere", x = "topright"),ylim = c(0,1800))  

--------------------------------------------------------------------------------
  ## Aggiungere per gli Uomini la colonna CL3,per le femmine CL2 e CL4  
  
  #aquisisco il genere femminile  
  Semer_genereF<-subset(data_modifcato,data_modifcato$Gender=="Femmina",select = c(Gender,Semer))
#aquisisco il genere maschile
Semer_genereU<-subset(data_modifcato,data_modifcato$Gender=="Maschio",select = c(Gender,Semer))
#creo una table sia per genere maschile che femminile
conteggio_SemerF<-table(Semer_genereF)
conteggio_SemerU<-table(Semer_genereU)
#inserisco in una singola table i valori ripettivi alla frequenza delle due table
conteggi_consumi17<-rbind(Femminile=conteggio_SemerF,Maschile=conteggio_SemerU)
View(conteggi_consumi17)
#creo un barplot del conteggio del consumo delle amphet
barplot(conteggi_consumi17,main="Confronto dei consumi tra generi",xlab = "Tipo di consumo",ylab = "Frequenza",col = c("pink","blue"),legend.text = rownames(conteggi_consumi17),args.legend = list(title = "Genere", x = "topright"),ylim = c(0,1800))  

--------------------------------------------------------------------------------
  #aquisisco il genere femminile  
  VSA_genereF<-subset(data_modifcato,data_modifcato$Gender=="Femmina",select = c(Gender,VSA))
#aquisisco il genere maschile
VSA_genereU<-subset(data_modifcato,data_modifcato$Gender=="Maschio",select = c(Gender,VSA))
#creo una table sia per genere maschile che femminile
conteggio_VSAF<-table(VSA_genereF)
conteggio_VSAU<-table(VSA_genereU)
#inserisco in una singola table i valori ripettivi alla frequenza delle due table
conteggi_consumi18<-rbind(Femminile=conteggio_VSAF,Maschile=conteggio_VSAU)
View(conteggi_consumi18)
#creo un barplot del conteggio del consumo delle amphet
barplot(conteggi_consumi18,main="Confronto dei consumi tra generi",xlab = "Tipo di consumo",ylab = "Frequenza",col = c("pink","blue"),legend.text = rownames(conteggi_consumi18),args.legend = list(title = "Genere", x = "topright"),ylim = c(0,1800))  


