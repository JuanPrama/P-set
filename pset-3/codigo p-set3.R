##==David Eduardo Bonilla 202015465==##
##==Juan David Prada Amaya 202112612==##
---
  Title: "Taller de R: Estadsitica y Programacion"
pagetitle: "Taller 3"
---
install.packages("data.table")


require(data.table)
require(pacman)
require(ggplot2)
p_load(tidyverse, ## manipular/limpiar conjuntos de datos.
       rio, ## para leer/escribir archivos desde diferentes formatos. 
       skimr, ## skim: describir un conjunto de datos
       janitor) ##  tabyl: frecuencias relativas

starts_with <- dplyr::starts_with
select <- dplyr::select
##==1.1 Lista de archivos input ==##

archivos <- list.files(path = "input", full.names = TRUE, recursive = TRUE)
archivos[is.na(archivos)] <- 0 
##==1.2 Importar archivos ==##

datos1<- import_list(file= archivos %>% str_subset("Fuerza de trabajo"))
datos2<- import_list(file= archivos %>% str_subset("No ocupados"))
datos3<- import_list(file= archivos %>% str_subset("Ocupados"))
datos<- c(datos1, datos2, datos3)

##==1.3 COmbinar conjunto de datos ==##
##debido a que se vuelve un accion demasiada pesada de correr si se exporta de la variable datos se va hacer nuevamente con la variable archivos
fuerza <- import_list(file= archivos %>% str_subset("Fuerza de trabajo"))%>% clean_names() 
fuerza1 <- rbindlist(fuerza)
ocu <- import_list(file= archivos %>% str_subset("Ocupados"))%>% clean_names()
ocu1 <- rbindlist(ocu)
nocu <- import_list(file= archivos %>% str_subset("No ocupados"))%>% clean_names()
nocu1 <- rbindlist(nocu)

##==2.1 Creacion de bases de datos ==##

fuerza1 <- replace(fuerza1, is.na(fuerza1),0)
fuerza1$FT <- as.numeric(fuerza1$FT)
suma_fuerza_laboral <- sum(subset(fuerza1, FT == 1))
suma_fuerza_laboral2 <- sum(subset(fuerza1, pet == 1))

ocu1 <- replace(fuerza1, is.na(ocu1),0)
ocu1$FT <- as.numeric(ocu1$FT)
suma_oc <- sum(subset(ocu1, FT == 1))

nocu1 <- replace(nocu1, is.na(nocu1),0)
nocu1$FT <- as.numeric(nocu1$FT)
suma_nocu <- sum(subset(nocu1, DSI == 1))


##==2.2 Colapsar datos a nivel mensual ==##

output <- left_join(fuerza1, by=c("MES","FT","pet"))%>%
          left_join(ocu1, by=c("MES","FT"))%>%
          left_join(nocu1, by=c("MES","DSI"))


##==2.3 Tasas de desempleo y ocupacion ==##

tasades <- sum(output$DSI)/sum(output$FT)
tasaocu <- sum(output$FT)/sum(output$pet)

##== GGPlot ==##

ggplot(data=output, aes(x= mes, y= tasades))+ geom_line()+labs(title = "Tasas de desempleo por mes", y="Tasa")
ggplot(data=output, aes(x= mes, y= tasaocu))+ geom_line()+labs(title = "Tasas de ocupacion por mes", y="Tasa")