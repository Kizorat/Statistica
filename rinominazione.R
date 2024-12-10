#rinomino i dati 

data<-read.csv("drug_consumption.csv",sep=",",header = T)


#Genere
#cambio il nome dei dati che riferiscono il tipo di genere
data$Gender <- ifelse(data$Gender == "0.48246", "Maschio",
                   ifelse(data$Gender == "-0.48246", "Femmina",
                                                      data$Gender)) 

#Educazione
#cambio il nome dei dati che riferiscono al titolo di studio
data$Education <- ifelse(data$Education == "-2.43591", "<16",
                   ifelse(data$Education == "-1.7379", "16",
                          ifelse(data$Education == "-1.43719", "17",
                                 ifelse(data$Education == "-1.22751", "18",
                                        ifelse(data$Education == "-0.61113", "non_Diplomato",
                                               ifelse(data$Education == "-0.05921", "Diplomato",
                                                      ifelse(data$Education == "0.45468", "Laureato",
                                                             ifelse(data$Education == "1.16365", "Master",
                                                                    ifelse(data$Education == "1.98437", "Dottorato",
                                                      data$Education)))))))))

#cambio il nome dei dati che riferiscono il tipo di etnia
data$Age <- ifelse(data$Age == "-0.95197", "18-24",
                   ifelse(data$Age == "-0.07854", "25-34",
                          ifelse(data$Age == "0.49788", "35-44",
                                 ifelse(data$Age == "1.09449", "45-54",
                                        ifelse(data$Age == "1.82213", "55-64",
                                               ifelse(data$Age == "2.59171", "65+",
                                                      data$Age))))))
#Educazione
#cambio il nome dei dati che riferiscono il tipo di etnia
data$Ethnicity <- ifelse(data$Ethnicity == "-0.50212", "Asiatici",
                         ifelse(data$Ethnicity == "-1.10702", "Neri",
                                ifelse(data$Ethnicity == "1.90725", "Neri/Asiatici",
                                       ifelse(data$Ethnicity == "0.126", "Bianchi/Asiatici",
                                              ifelse(data$Ethnicity == "-0.22166", "Bianchi/Neri",
                                                     ifelse(data$Ethnicity == "0.1144", "Altri",
                                                            ifelse(data$Ethnicity == "-0.31685", "Bianchi",
                                                                   data$Ethnicity))))))) 


#Etnia
#cambio il nome dei dati che riferiscono il tipo di etnia
data$Ethnicity <- ifelse(data$Ethnicity == "-0.50212", "Asiatici",
                         ifelse(data$Ethnicity == "-1.10702", "Neri",
                                ifelse(data$Ethnicity == "1.90725", "Neri/Asiatici",
                                       ifelse(data$Ethnicity == "0.12600", "Bianchi/Asiatici",
                                              ifelse(data$Ethnicity == "-0.22166", "Bianchi/Neri",
                                                     ifelse(data$Ethnicity == "0.11440", "Altri",
                                                            ifelse(data$Ethnicity == "-0.31685", "Bianchi",
                                                                   data$Ethnicity))))))) 
#Paese
#cambio il nome dei dati che riferiscono il tipo di educazione:
data$Country <- ifelse(data$Country == "-0.09765", "Australia",
                       ifelse(data$Country == "0.24923", "Canada",
                              ifelse(data$Country == "-0.46841", "Nuova Zelanda",
                                     ifelse(data$Country == "-0.28519", "Altri",
                                            ifelse(data$Country == "0.21128", "Irlanda",
                                                   ifelse(data$Country == "0.96082", "UK",
                                                          ifelse(data$Country == "-0.57009", "USA",
                                                                 data$Country))))))) 


