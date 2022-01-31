library(tidyverse)
#install.packages('RPostgreSQL')
library(RPostgreSQL)
library(lubridate)
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname="Proiect_BD4", user="postgres", 
                 host = 'localhost', password="postgres")
tables <- dbGetQuery(con, 
                     "select table_name from information_schema.tables where table_schema = 'public'")
tables

sediu <- dbReadTable(con, "sediu")
angajati <- dbReadTable(con, "angajati")
clienti <- dbReadTable(con, "clienti")
facturi <- dbReadTable(con, "facturi")
comenzi <- dbReadTable(con, "comenzi") 
detaliicomanda <- dbReadTable(con, "detaliicomanda")
produse <- dbReadTable(con, "produse")




##Roberto Mantale##
#1) Care sunt clientii care au cumparat telefoane si cosmetice?
temp <- dplyr::intersect(
    clienti %>%
    inner_join(facturi, by=c('codclient')) %>%
    inner_join(comenzi, by=c('idfactura')) %>%
    inner_join(detaliicomanda, by=c('nrcomanda')) %>%
    inner_join(produse, by=c('codprodus')) %>%
    filter(categorieprodus == 'Telefoane') %>%
    select(numeclient, prenumeclient),
    clienti %>%
    inner_join(facturi, by=c('codclient')) %>%
    inner_join(comenzi, by=c('idfactura')) %>%
    inner_join(detaliicomanda, by=c('nrcomanda')) %>%
    inner_join(produse, by=c('codprodus')) %>%
    filter(categorieprodus == 'Cosmetice') %>%
    select(numeclient, prenumeclient)
)
view(temp)

#2) Care este pretul mediu al electrocasnicelor cumparate de Lupu Vasile?
temp <- produse %>%
  filter(categorieprodus == 'Electrocasnice') %>%
  inner_join(detaliicomanda, by=c('codprodus')) %>%
  inner_join(comenzi, by=c('nrcomanda')) %>%
  inner_join(facturi, by=c('idfactura')) %>%
  inner_join(clienti, by=c('codclient')) %>%
  filter(numeclient == 'Lupu') %>%
  filter(prenumeclient =='Vasile') %>%
  summarise(pret_mediu = mean(pret))
View(temp) 

#3) Care sunt localitatile cu minim 2 clienti?

#4) Afisati top 3 categorii de produse cu cele mai multe vanzari
temp <- produse %>%
  select(categorieprodus) %>%
  inner_join(detaliicomanda, by=c('codprodus')) %>%
  mutate(valoare_vanzari = sum(cantitate, pretbucata))
  arrange(valoare_vanzari) %>%
  top(3)
View(temp)


##Japaila Ionela##
#1) Care sunt clientii cu o valoare totala a vanzarilor>1000
temp<-clienti%>%
  inner_join(facturi,by=c("codclient"))%>%
  inner_join(comenzi,by=c("idfactura"))%>%
  inner_join(detaliicomanda,by=c("nrcomanda"))%>%
  mutate(fullname=paste(numeclient,prenumeclient,sep=' '))%>%
  group_by(codclient,fullname)%>%
  summarise(valoareTotala=sum(cantitate*pretbucata))%>%
  filter(valoareTotala>1000)%>%
  arrange(desc(valoareTotala))
View(temp)

#2) Care sunt zilele in care s-au vandut produsele cu codul 1 si 2?
temp<-produse%>%
  inner_join(detaliicomanda,by=c('codprodus'))%>%
  inner_join(comenzi,by=c('nrcomanda'))%>%
  inner_join(facturi,by=c('idfactura'))%>%
  filter(codprodus==1)%>%
  select(datafacturare)%>%
  inner_join(
    produse%>%
      inner_join(detaliicomanda,by=c('codprodus'))%>%
      inner_join(comenzi,by=c('nrcomanda'))%>%
      inner_join(facturi,by=c('idfactura'))%>%
      filter(codprodus==2)%>%
      select(datafacturare)
  )
View(temp)

#3) Care sunt clientii care au cumparat macar aceleasi produse ca si clientul Lupu Vasile?
temp <- clienti %>%
  inner_join(facturi,by=c("codclient")) %>%
  inner_join(comenzi,by=c("idfactura")) %>%
  inner_join(detaliicomanda,by=c("nrcomanda")) %>%
  inner_join(produse,by=c("codprodus"))%>%
  select(numeclient,prenumeclient,numeprodus)%>%
  group_by(numeclient,prenumeclient)%>%
  filter(numeprodus %in% 
           ( clienti %>%
               inner_join(facturi,by=c("codclient")) %>%
               inner_join(comenzi,by=c("idfactura")) %>%
               inner_join(detaliicomanda,by=c("nrcomanda")) %>%
               inner_join(produse,by=c("codprodus"))%>%
               filter(numeclient=='Lupu' & prenumeclient=='Vasile')%>%
               select(numeprodus)
           ))

#4) Afisati nivelul ierarhic al fiecarui angajat începând cu 8 


  
  
  
  
  
  
  
  
  


