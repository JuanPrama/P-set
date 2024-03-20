##David Eduardo Bonilla 202015465
##Juan David Prada Amaya 202112612
---
  title: "Taller de R: Estadística y Programación"
date: "`r Sys.Date()`"
pagetitle: "Taller 2"
output: 
  pdf_document: default
---
library(pacman)
library(ggplot2)
p_load(rio,skimr,janitor, dplyr)
#Los siguientes puntos se realizarán utilizando la [Encuesta de Micronegocios 2022](https://microdatos.dane.gov.co/index.php/catalog/796/get-microdata), que se centra en empresas con un máximo de 9 empleados. Puedes encontrar el diccionario de datos en el portal del [DANE](https://microdatos.dane.gov.co/index.php/catalog/796/data-dictionary/F12?file_name=M%C3%B3dulo%20de%20identificaci%C3%B3n) o descargarlo en formato [PDF](https://microdatos.dane.gov.co/index.php/catalog/796/download/22665).

# 1. Importar/exportar bases de datos

#+ 1.1 Importar

#Importe las bases de datos \textbf{Módulo de sitio o ubicación} en un objeto llamado `location` y Módulo de identificación en un objeto llamado `identification`.

location <- import(file="input/Módulo de sitio o ubicación.dta")
identification <- import(file="input/Módulo de identificación.dta")

#+ 1.2 Exportar

#Exporte a la carpeta output los objetos cargados en el punto anterior, guárdelos como **location.rds** y **identification.rds**.

export(location, file="output/location.rds")
export(identification, file="output/identification.rds")

#<!------------------>
# 2. Generar variables

#+ 2.1 Usando la variable **grupos 4**, se debe generar una nueva variable llamada `bussiness_type`, que tomará los siguientes valores:


#- **Agricultura** cuando `grupos 4` sea igual a 1.

bussiness_type = ifelse(test = identification$GRUPOS4==1, yes="Agricultura", no=0)
#- **Industria manufacturera** cuando `grupos 4` sea igual a 2.
#- **Comercio** cuando `grupos 4` sea igual a 3.
#- **Servicios** cuando `grupos 4` sea igual a 4.
##opcion #1

identification$bussiness_type = ifelse(test = identification$GRUPOS4=="01", yes="Agricultura",
                                       no=ifelse(test = identification$GRUPOS4=="02", yes="Industria manufacturera",
                                                 no=ifelse(test = identification$GRUPOS4=="03", yes="Comercio",
                                                           no=ifelse(test = identification$GRUPOS4=="04", yes="Servicios", no=NA))))
##opcion #2

identification2 <- identification
identification2$GRUPOS4 <- as.numeric(identification2$GRUPOS4)
identification2$bussiness_type = ifelse(test = identification2$GRUPOS4==01, yes="Agricultura",
                                        no=ifelse(test = identification2$GRUPOS4==02, yes="Industria manufacturera",
                                                  no=ifelse(test = identification2$GRUPOS4==03, yes="Comercio",
                                                            no=ifelse(test = identification2$GRUPOS4==04, yes="Servicios", no=NA))))

#+ 2.2 Se debe crear una variable llamada `grupo_etario` que divida a los propietarios de micronegocios en cuatro grupos etarios. Los rangos de edades seleccionados deben ser justificados.

identification$grupo_etario = ifelse(test = identification$P241<=30, yes="Etario Joven",
                                     no=ifelse(test = identification$P241<=45, yes="Etario Joven-Adultez",
                                               no=ifelse(test = identification$P241<=65, yes="Etario Adulto",
                                                         no="Etario Adulto Mayor")))
##Etario Joven- Este grupo comprende a los emprendedores que estan iniciando sus micronegocios, este grupo tiene una alta energia motivacion y capacidad riesgos gracias a que estos poseen menos experiencia
##Etario Joven-Adultez-Este grupo comprende a los emprendedores que se encuentran en sus fases inciales o media, tienen una combiancion de experiencia laboral y energia que les permite enfretarse a desafios y adaptarse a situaciones mucho mas rapido que el grupo anterior
##Etario Adulto-Este grupo comprende a los emprendedores que se encuentran en una fase media o avanzada de sus negocios, tienen una amplia experiencia y conocimientos en sus campos  
##Etario Adulto Mayor-Este grupo comprende a los emprendedores que en su gran mayoria se encuentran en la fase avanzada de sus negocios, continuan siendo activas en el mundo empresarial a pesar de su edad

#+ 2.3 Sobre el objeto `location`, genere una variable llamada `ambulante`, que sera igual a 1 si la variable `P3053` es igual a 3, 4 o 5.

location$Ambulante = ifelse(test = location$P3053==3|location$P3053==4|location$P3053==5, yes =1,no=0)

#<!------------------>
# 3. Eliminar filas/columnas de un conjunto de datos

#+ 3.1 Almacene en un objeto llamado `identification_sub`  las variables `DIRECTORIO`, `SECUENCIA_P`, `SECUENCIA_ENCUESTA`, `grupo_etario`,  `ambulante`, `COD_DEPTO` y `F_EXP`.

identification_sub <- select(.data = identification , DIRECTORIO , SECUENCIA_P, SECUENCIA_ENCUESTA, grupo_etario, COD_DEPTO, F_EXP)
#La variable ambulante no se puede extraer debido a que no corresponde a estos datos 

#+ 3.2 Del objeto `location` seleccione solo las variables `DIRECTORIO`, `SECUENCIA_P`, `SECUENCIA_ENCUESTA`, `ambulante`
#`P3054`, `P469`, `COD_DEPTO`, `F_EXP` y guárdelo en nuevo objeto llamado `location_sub`.

location_sub <- select(.data = location, DIRECTORIO, SECUENCIA_P, SECUENCIA_ENCUESTA, Ambulante, P3054, P469, COD_DEPTO, F_EXP )

#<!------------------>
# 4. Combinar bases de datos

#+ 4.1 Use las variables `DIRECTORIO`, `SECUENCIA_P` y `SECUENCIA_ENCUESTA` para unir en una única base de datos, los objetos `location_sub` y `identification_sub`.

base_nueva <- left_join(x=identification, y=location, by=c("DIRECTORIO","SECUENCIA_ENCUESTA","SECUENCIA_P"))

#<!------------------>
# 5. Descriptivas

#+ 5.1 Utilizando funciones como `skim` o `summary`, cree breves estadísticas descriptivas de la base de datos creada previamente. `(HINT: Observaciones en NA, conteo de variables únicas)`

summary(base_nueva)

#+ 5.2. Use las funciones `group_by` y `summarise` para extraer variables descriptivas, como la cantidad de asociados por departamento, grupo etario, entre otros. Además, cree un pequeño párrafo con los hallazgos que encuentre.

datos_1 <- base_nueva %>% group_by(grupo_etario)%>%summarise(promedio_edad =mean(P241, na.rm = T))
##El analisis al que se puede llegar viendo los resultados en este caso es de que en Etario adulto el promedio de la edad es de 54.7 años, Etario adulto mayor de 71.3, Etario Joven-Aultez de 38.2 y Etario Joven de 25.4.

datos_2 <- base_nueva %>% group_by(COD_DEPTO.x)%>%summarise(promedio_edad =mean(P241, na.rm = T))
##Por medio de este resultado se puede encontrar el promedio de la edad por departamento segun el codigo que le corresponde a cada uno 