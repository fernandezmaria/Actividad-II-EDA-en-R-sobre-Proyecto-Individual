# 21838529 FERNANDEZ MORIN MARIA ########
# Title: Actividad II: EDA ####
# File: Navigating through R UI
# Project: EDA ON MY INDIVIDUAL PROJECT IN R

# LOAD PACKAGES AND CSV ---------------------------------------------------
pacman::p_load(tm, SnowballC, tidyverse, wordcloud, lubridate, scales, dplyr)

#Utilizo la funcion read_csv2 porque el csv esta separado por ;
dataset1 <- read_csv2("https://docs.google.com/spreadsheets/d/e/2PACX-1vSthKCZay3KMPpmlwiJ4Ny8IymOtSF3XOj3MyNAgCLV3oQQSPGipKoARW20IQVzeOhFnbuZ9Dbgj0o3/pubhtml")
head(dataset1)

# GRAPHS ------------------------------------------------------------------

# Frecuencia de locales en los diferentes distritos
freq <- barplot(prop.table(table(dataset1[2])), col = heat.colors(25), main = 'Frecuencia de comercios por distrito')

# Graficamos la situación de los locales en cada zona
g <-  ggplot(dataset1, aes(id_distrito_local, fill=desc_situacion_local) ) +
  labs(title = "Situación de los locales según el distrito")+ylab("") +
  theme(plot.title = element_text(size = rel(1), colour = "black"))

Situacion <- g+geom_bar(position="dodge") + scale_fill_manual(values = alpha(c("green", "yellow","yellow","yellow","red","blue","grey"), 1)) +
  theme(axis.title.x = element_text(face="bold", size=10)) 
# De esta manera, el resultado final queda guardado en 'Situacion'

# LOAD SECOND CSV ---------------------------------------------------------
# TITLE: Dataset2.Locales-Epigrafes202010
#dataset2 <- read.csv2(file.choose(), header = TRUE, sep = ';')#no sirve
dataset2 <- read_csv2("C:/Users/Maria/Documents/LPE/Proy.Individual/Dataset2.Locales-Epigrafes202010.csv")
head(dataset2)

# GRAPHS 2.0 --------------------------------------------------------------
g1 <- ggplot(dataset2, aes(id_seccion, id_distrito_local)) + geom_point(aes(colour = id_distrito_local)) 
g1

#ggplot(dataset2, aes(id_seccion)) +
  #geom_point(mapping = aes(x = id_seccion, y = id_distrito_local, color = id_distrito_local))

#Los valores unicos que recoge R sobre este atributo no se corresponden con los que deberian ser
unique(dataset2$id_seccion)

# LOAD THIRD DATASET ------------------------------------------------------
# TITLE: estructura_cnae2009
dataset3mal <- read.csv(file.choose(), header = TRUE, sep = ',')
#dataset3mal <- read_csv("C:/Users/Maria/Documents/LPE/Proy.Individual/estructura_cnae2009.csv")
head(dataset3mal)

#Vemos que está lleno de nulos porque hay columnas vacias
Nulos <- sum(is.na(dataset3mal))
Nulos

#En este dataset vienen columnas que estan vacias, por tanto cojo solo las que quiero
dataset3 <- select(dataset3mal, COD_CNAE2009, CODINTEGR, TITULO_CNAE2009)
head(dataset3)

#Escojo solo las primeras 10 lineas para graficar a modo de ejemplo
dataset3 <- dataset3[1:10,]

#De esta manera se puede ver a que corresponde cada codigo
ggplot(dataset3) + 
  geom_point(mapping = aes(x = COD_CNAE2009, y =TITULO_CNAE2009))
