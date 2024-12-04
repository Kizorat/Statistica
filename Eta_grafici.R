data<-read.csv("drug_consumption.csv",sep=",",header = T)


Age<-c(rep("18-24",-0.95197),rep("25-34",-0.07854),rep("35-44",0.49788),rep("45-54",1.09449),rep("55-64",1.82213),rep("65+",2.59171))

table (Age) #calcola le frequenze assolute
View(table (Age))


n_Eta<-ordered(data$Age,levels=c("-0.95197","-0.07854","0.49788","1.09449","1.82213","2.59171"))
table(n_Eta)
View(table(n_Eta))

table_Eta<-table(n_Eta)



png("Age_bar_chart.png", width = 800, height = 800, res = 150)

barplot(table_Eta,col =c("green","blue","violet","red","yellow","grey"))




# Creare un file PNG con risoluzione aumentata

png("Age_pie_chart.png", width = 800, height = 800, res = 150)
# Creare il grafico a torta
pie(table_Eta, col = c("green","blue","violet","red","yellow","grey"),
    main = "Distribuzione delle fasce d'età")

# Chiudere il dispositivo grafico

