rm(list=ls())

#install.packages('readxl')  
#install.packages("VIM")
library(readxl)
library(dplyr)
library(VIM)

df<-read_excel('C:\\Users\\Kimmy\\Desktop\\DA Challenge\\Social Media Content Performance - June 2025.xlsx')
df2<-df

glimpse(df)
colSums(is.na(df))

###################
#Nulos en columnas
###################

na_index<-which(colSums(is.na(df))!=0)
nperdidos<- colSums(is.na(df))[na_index]

#% de los nulos
ndatos<-nrow(df)
pperdidos <- round(100*nperdidos/ndatos,2)

#Evaluar variables numéricas y factores
var_car<-df[sapply(df,is.character)]
var_car<-var_car[,!names(var_car) %in% c("Post_ID", "Main_Hashtag")]

for (i in names(var_car)){
  var_car[[i]]<-as.factor(var_car[[i]])
}

var_car$Clicks<-df$Clicks

var_num<-df[sapply(df,is.numeric)]
var_num2<-var_num[,!names(var_num) %in% c("Longitude", "Latitude")]

##################
#Evaluar nulos
##################

x11()
matrixplot(var_num)
#No se observó ninguna relación con variables numéricas

x11()
matrixplot(var_car)
#Sospecha de relación con plataforma

##################
#Nulos por plataforma
##################

total_na<-var_car[c("Platform", "Clicks")][is.na(var_car$Clicks),]%>%group_by(Platform) %>%
  group_by(Platform) %>%
  summarise(Total_na=n())

total_nona<-var_car[c("Platform", "Clicks")][!is.na(var_car$Clicks),]%>%group_by(Platform) %>%
  group_by(Platform) %>%
  summarise(Total_na=n())

nulos_plat<-full_join(total_na, total_nona, by = "Platform")
names(nulos_plat)<-c("Platform", "Total Nulos", "Total No Nulos")
nulos_plat[is.na(nulos_plat)]<-0

nulos_plat["% Nulos"]<-100*nulos_plat$`Total Nulos`/(nulos_plat$`Total Nulos`+nulos_plat$`Total No Nulos`)

nulos_plat

#No se tiene información de Clicks ni Click_Through_Rate para las publicaciones provenientes de 
#Instagram, X ni Youtube. Facebook y TikTok sí cuentan con esta información completa.
#LinkedIn es la única plataforma que tiene tanto nulos como datos no nulos, un 42% de las publicaciones
#de LinkedIn no tiene datos en las variables Clicks ni Click_Through_Rate.

lk<-df[df$Platform=="LinkedIn",]
lk_nulos<-lk[is.na(lk$Clicks),]
lk_nonulos<-lk[!is.na(lk$Clicks),]

revision <- lk[,sapply(lk,is.character)&(names(lk)!="Post_ID")]
revision_num <- lk[, sapply(lk, is.numeric)]

for(i in names(revision)){
  revision[[i]]<-as.factor(revision[[i]])
}

revision<-merge(revision, revision_num)
revision_num<-revision[sapply(revision, is.numeric)]

x11()
matrixplot(revision_num)
names(revision_num)

windows()
VIM::pbox(revision_num[c(10,12)], pos=1)

t.test(Engagement ~ is.na(Clicks), data=revision_num)
#Se observa una diferencia entre la media de engagement de los registros que tienen nulos
#en Clicks, y en los que no.
#Esto significa que imputar por la media generaría un sesgo en el conjunto de datos observados.

t.test(Shares ~ is.na(Clicks), data=revision_num)
#Sí existe una diferencia significativa en las medias de ambos grupos.

t.test(Likes ~ is.na(Clicks), data=revision_num)
#Sí existe una diferencia significativa en las medias de ambos grupos

revision_cat<-revision[sapply(revision, is.factor)]
revision_cat["Clicks"]<-revision["Clicks"]

#Ver cant
x11()
matrixplot(revision_cat)
df %>% 
  group_by(Post_Type) %>%
  summarize(Total_NA_Clicks = sum(is.na(Clicks)), 
            Total = n(),
            Pct_Nulos = 100*Total_NA_Clicks/n())

#Debido a que un 42% de los datos de publicaciones en LinkedIn no tienen datos de los clicks, y este es
#un alto porcentaje, no se van a imputar estos valores nulos. 
#Únicamente no se van a considerar las variables Clicks ni CTR en el análisis descriptivo de esta plataforma, 
#al igual que para el resto de plataformas que no cuentan con esta información.
