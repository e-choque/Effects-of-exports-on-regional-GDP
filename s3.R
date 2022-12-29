# Ejercicio 2
# Description script 
# Import Base ####
library(readxl)
datos <- read_xlsx("base_tesis2.xlsx",sheet = "base_R")
View(datos)

attach(datos)
names(datos)

# Analisis estadistico
install.packages("psych")
library(psych)

# Para graficos elegantes
install.packages("ggplot2")
library(ggplot2)

#An?lisis econom?trico
install.packages("lmtest")  #<-Pruebas de especificacion para modelos de regresi?n lineal
install.packages("tseries") #<-Para series de tiempo
install.packages("PerformanceAnalytics")# Econometric tools for performance and risk analysis
install.packages("urca") #An?lisis de ra?ces unitarias
install.packages("car") #Acompa?ante de regresi?n aplicada
install.packages("strucchange") # Estabilidad de par?metros en el tiempo
install.packages("FinTS") # Para an?lisis de series de tiempo financieras


library(lmtest)
library(tseries)
library(PerformanceAnalytics)
library(urca)
library(car)
library(strucchange)
library(FinTS)

######################## 1. Analisis estadistico ####

describe(pib)
describe(x_tm)
jarque.bera.test(pib)
jarque.bera.test(x_tm)

quantile(pib,c(0.10,0.25,0.50,0.75,0.90))
quantile(pib,1:9/10)

quantile(x_tm,c(0.10,0.25,0.50,0.75,0.90))
quantile(x_tm,1:9/10)

# 1.1. Graficas ####

#### Graficando 

a<-ggplot(data = datos, aes(x = year, y = x1))+
  geom_line(color = "#00AFBB", size = 2) +  
  stat_smooth( color = "#FC4E07", fill = "#FC4E07",method = "loess", show.legend = TRUE)+
  ggtitle("Var. % de los exportaciones en dolares, periodo 1993-2019")+
  xlab("tiempo") + ylab("En porcentaje, %")
a

b<-ggplot(data = datos, aes(x = year, y = pib))+
  geom_line(color = "#00AFBB", size = 2) +  
  stat_smooth( color = "#FC4E07", fill = "#FC4E07",method = "loess", show.legend = TRUE)+
  ggtitle("Crec. del PIB  de Santa Cruz, periodo 1993-2019")+
  xlab("tiempo") + ylab("En porcentaje, %")
b


# Diagrama de dispersion 

scatterplot(pib~x1, regLine=TRUE, smooth=FALSE, boxplots='xy',
            ellipse=list(levels=c(.5, .9)), xlab="Crec. valor de exportaciones (En %)", 
            ylab="Crec. PIB  de Santa Cruz (en %)",
            main="Relacion ente el Crec. valor de exportaciones y el Crec. PIB  de Santa Cruz")

qplot( x1, pib, geom = c("point", "smooth") ,
       xlab = "Var. % del valor de exportaciones",
       ylab = "Var. % del PIB ",
       main = "Relacion exportaciones y Crec. del PIB ")

cor(datos)
chart.Correlation(datos[3:9]) 


