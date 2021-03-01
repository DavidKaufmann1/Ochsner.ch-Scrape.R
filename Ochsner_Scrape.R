setwd('/Users/davidkaufmann/Documents/Bewerbungen/Job21/Data Analyst Ochsner Sport/Project')

library('xml2')
library('rvest')
library('RSelenium')
library(stringr)
library(tidyr)
library(dplyr)
library(R.utils)
library(httr)
library(robotstxt)

Sys.setenv(LANGUAGE='en')

#Webseite um Erlaubnis Fragen --------------------------------------------------------------------------------------------------------------------------------------------------
get_robotstxt('https://www.ochsnersport.ch/de/shop/')
robo<-robotstxt(domain = 'https://www.ochsnersport.ch/de/shop/')
robo$domain
robo$permissions
paths_allowed('https://www.ochsnersport.ch/de/shop/')
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
#Produkte-URL's scrapen---------------------------------------------------------------------------------------------------------------------------------------------------------
rD<-rsDriver(browser = 'firefox', port = 988L)
remDr<-rD$client

#Sportarten
vec_sportarten<-vector()

for (j in 1:72) { #manuell eingeben, wie viele Seiten es sind, gibt sicher besseren Prozess
  remDr$navigate(paste0('https://www.ochsnersport.ch/de/shop/neutral-sportarten-00015640-c.html?page=',j))
  Sys.sleep(3)
  webElem <- remDr$findElement("css", "body")
  
  for (i in 1:200) {
    webElem$sendKeysToElement(list(key = "down_arrow"))
    Sys.sleep(0.1)
  }
  
  
  html_sportarten<-read_html(remDr$getPageSource()[[1]])
  
  URL_sportarten<-html_sportarten %>%
    html_nodes('section a div div div a')%>%
    html_attr('href')
  
  vec_sportarten <-c(vec_sportarten, URL_sportarten)
}

#Bekleidung
vec_bekleidung<-vector()

for (j in 1:112) {
  remDr$navigate(paste0('https://www.ochsnersport.ch/de/shop/neutral-bekleidung-00015641-c.html?page=',j))
  Sys.sleep(3)
  webElem <- remDr$findElement("css", "body")
  
  for (i in 1:200) {
    webElem$sendKeysToElement(list(key = "down_arrow"))
    Sys.sleep(0.1)
  }
  
  
  html_bekleidung<-read_html(remDr$getPageSource()[[1]])
  
  URL_bekleidung<-html_bekleidung %>%
    html_nodes('section a div div div a')%>%
    html_attr('href')
  
  
  vec_bekleidung <-c(vec_bekleidung, URL_bekleidung)
}

#Schuhe
vec_schuhe<-vector()

for (j in 1:45) {
  remDr$navigate(paste0('https://www.ochsnersport.ch/de/shop/neutral-schuhe-00015643-c.html?page=',j))
  Sys.sleep(3)
  webElem <- remDr$findElement("css", "body")
  
  for (i in 1:200) {
    webElem$sendKeysToElement(list(key = "down_arrow"))
    Sys.sleep(0.1)
  }
  
  
  html_schuhe<-read_html(remDr$getPageSource()[[1]])
  
  URL_schuhe<-html_schuhe %>%
    html_nodes('section a div div div a')%>%
    html_attr('href')
  
  vec_schuhe <-c(vec_schuhe, URL_schuhe)
}

#Ausrüstung
vec_ausrüstung<-vector()

for (j in 1:72) {
  remDr$navigate(paste0('https://www.ochsnersport.ch/de/shop/neutral-ausruestung-00015642-c.html?page=',j))
  Sys.sleep(3)
  webElem <- remDr$findElement("css", "body")
  
  for (i in 1:200) {
    webElem$sendKeysToElement(list(key = "down_arrow"))
    Sys.sleep(0.1)
  }
  
  
  html_ausrüstung<-read_html(remDr$getPageSource()[[1]])
  
  URL_ausrüstung<-html_ausrüstung %>%
    html_nodes('section a div div div a')%>%
    html_attr('href')
  
  vec_ausrüstung <-c(vec_ausrüstung, URL_ausrüstung)
}

vec_Produkte_URL<-c(vec_sportarten,vec_schuhe,vec_gonser,vec_bekleidung,vec_ausrüstung)
vec_Produkte_URL<-unique(vec_Produkte_URL)
#-----------------------------------------------------------------------------------------------------------------------------

#Produktinfos scrapen---------------------------------------------------------------------------------------------------------
rD<-rsDriver(browser = 'firefox', port = 426L)
remDr<-rD$client


Daten<-setNames(data.frame(matrix(ncol = 13, nrow = 0)), c('Name','Marke','Url','Sterne','Anzahl','Kategorie_1','Kategorie_2','Kategorie_3','Kategorie_4','Kategorie_5','Kategorie_6','Kategorie_7','Kategorie_8'))
Daten<-Daten%>%
  mutate_all(as.character)


for (i in vec_Produkte_URL[1:3912]) {
  remDr$navigate(paste0('https://www.ochsnersport.ch',i))
  
  Sys.sleep(6)
 
  Produktinfo_html<- withTimeout(read_html(remDr$getPageSource()[[1]]), timeout = 10) #hier noch automatisches error-handling einbauen!

  Produktinfo <- Produktinfo_html%>%
    html_nodes('main div div ul li a span.breadcrumb__label')%>%
    html_text('text')
  
  Produktinfo <- Produktinfo[Produktinfo!='Home']
  
  Name <- Produktinfo_html%>%
    html_nodes('div div h1')%>%
    html_text('text')
  
  Sterne<-Produktinfo_html%>%
    html_nodes('.rating-line__active')%>%
    html_attr('style')
  
  Sterne <- Sterne[1]
  
  Sterne<-str_replace(Sterne,'width: 100%;','5') #dies kann man noch vereinfachen!!
  Sterne<-str_replace(Sterne,'width: 90%;','4.5')
  Sterne<-str_replace(Sterne,'width: 80%;','4')
  Sterne<-str_replace(Sterne,'width: 70%;','3.5')
  Sterne<-str_replace(Sterne,'width: 60%;','3')
  Sterne<-str_replace(Sterne,'width: 50%;','2.5')
  Sterne<-str_replace(Sterne,'width: 40%;','2')
  Sterne<-str_replace(Sterne,'width: 30%;','1.5')
  Sterne<-str_replace(Sterne,'width: 20%;','1')
  
  Anzahl<-Produktinfo_html%>%
    html_nodes('.underline')%>%
    html_text()
  
  Marke<-Produktinfo_html%>%
    html_nodes('body div main div div div div div div a img')%>%
    html_attr('alt')
  
  Marke <- Marke[1]
  
  Daten<-Daten %>%
    add_row(Name=Name,
            Marke=Marke,
            Url=i,
            Sterne=Sterne,
            Anzahl=Anzahl,
            Kategorie_1=Produktinfo[1],
            Kategorie_2=Produktinfo[2],
            Kategorie_3=Produktinfo[3],
            Kategorie_4=Produktinfo[4],
            Kategorie_5=Produktinfo[5],
            Kategorie_6=Produktinfo[6],
            Kategorie_7=Produktinfo[7],
            Kategorie_8=Produktinfo[8])
}

Daten<-Daten%>%
  mutate(Kategorie_2=ifelse(is.na(Kategorie_2),Kategorie_1,Kategorie_2))%>%
  mutate(Kategorie_3=ifelse(is.na(Kategorie_3),Kategorie_2,Kategorie_3))%>%
  mutate(Kategorie_4=ifelse(is.na(Kategorie_4),Kategorie_3,Kategorie_4))%>%
  mutate(Kategorie_5=ifelse(is.na(Kategorie_5),Kategorie_4,Kategorie_5))%>%
  mutate(Kategorie_6=ifelse(is.na(Kategorie_6),Kategorie_5,Kategorie_6))%>%
  select(!Kategorie_7)%>%
  select(!Kategorie_8)


write.csv(Daten,'/Users/davidkaufmann/Documents/Bewerbungen/Job21/Data Analyst Ochsner Sport/Project/Daten.csv')

#One line for each comment
Daten_expanded <- Daten[rep(row.names(Daten), Daten$Anzahl), 1:11]

write.csv(Daten_expanded,'/Users/davidkaufmann/Documents/Bewerbungen/Job21/Data Analyst Ochsner Sport/Project/Daten_expanded.csv')

