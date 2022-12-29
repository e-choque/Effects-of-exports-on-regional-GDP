# Ejercicio 1 
# Modelo de series de tiempo: Determinar el efecto de las exportaciones sobre el PIB pc depto07 
# controlado por: 
# be: base empresarial; inv_pub: inversion publica
# Este ejercicio considera dos variables: pibpc y xvalor
# Import Base ####
library(readxl)
datos <- read_xlsx("base_tesis.xlsx",sheet = "base02_R")
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

######################## 1. Analisis estadistico####

describe(pibpc)
describe(xvalor)
jarque.bera.test(pibpc)
jarque.bera.test(xvalor)

quantile(pibpc,c(0.10,0.25,0.50,0.75,0.90))
quantile(pibpc,1:9/10)

quantile(xvalor,c(0.10,0.25,0.50,0.75,0.90))
quantile(xvalor,1:9/10)

# 1.1. Graficas ####

#### Graficando 

a<-ggplot(data = datos, aes(x = year, y = xvalor))+
  geom_line(color = "#00AFBB", size = 2) +  
  stat_smooth( color = "#FC4E07", fill = "#FC4E07",method = "loess", show.legend = TRUE)+
  ggtitle("Var. % de los exportaciones en dolares, periodo 1993-2019")+
  xlab("tiempo") + ylab("En porcentaje, %")
a

b<-ggplot(data = datos, aes(x = year, y = pibpc))+
  geom_line(color = "#00AFBB", size = 2) +  
  stat_smooth( color = "#FC4E07", fill = "#FC4E07",method = "loess", show.legend = TRUE)+
  ggtitle("Crec. del PIB pc de Santa Cruz, periodo 1989-2019")+
  xlab("tiempo") + ylab("En porcentaje, %")
b


# Diagrama de dispersion 

scatterplot(pibpc~xvalor, regLine=TRUE, smooth=FALSE, boxplots='xy',
            ellipse=list(levels=c(.5, .9)), xlab="Crec. valor de exportaciones (En %)", 
            ylab="Crec. PIB pc de Santa Cruz (en %)",
            main="Relacion ente el Crec. valor de exportaciones y el Crec. PIB pc de Santa Cruz")

qplot( xvalor, pibpc, geom = c("point", "smooth") ,
       xlab = "Var. % del valor de exportaciones",
       ylab = "Var. % del PIB pc",
       main = "Relacion exportaciones y Crec. del PIB pc")

cor(datos)
chart.Correlation(datos[2:4]) 

#2. Generar variables de series de tiempo  ####
# Nota: A partir de aqui ambas series deben tener  el mismo numero de observaciones
pibpc <- ts(datos$pibpc, start=c(1993, 1), freq=1)
xvalor <- ts(datos$xvalor, start=c(1993, 1), freq=1)

#Graficamos las variables
plot(pibpc, main="Crec. del PIB pc")
plot(xvalor, main = "Var. % de xvalor")


ggplot(datos, aes(year)) +
  geom_line(aes(y = pibpc, colour = "Crec. del PIB pc")) +
  geom_line(aes(y = xvalor, colour = "Var. % de xvalor")) +
  scale_colour_hue("variable")+
  ggtitle("Exportaciones y Crec. del PIB pc")+
  xlab("tiempo") + ylab("En porcentaje, %")


#3. Analisis de raiz unitaria o pruebas de estacionariedad ####

help("ur.df")
help("ur.pp")
help("ur.kpss")


ur.df.pibpc<-ur.df(pibpc, type = "none", selectlags = "AIC")
summary(ur.df.pibpc) # H0: Y es no estacionaria o es I(1)

ur.pp.pibpc<-ur.pp(pibpc, type="Z-tau", model="constant",
                      lags="long")
summary(ur.pp.pibpc) # H0: Y es no estacionaria o es I(1)

ur.kpss.pibpc<-ur.kpss(pibpc, type="tau", lags="long")
summary(ur.kpss.pibpc) # H0: Y es estacionaria o es I(0) ; t_t<valor_critico para rechazar H0

ur.df.xvalor<-ur.df(xvalor, type = "none", selectlags = "AIC")
summary(ur.df.xvalor)

ur.pp.xvalor<-ur.pp(xvalor, type="Z-tau", model="constant",
                       lags="long")
summary(ur.pp.xvalor)

ur.kpss.xvalor<-ur.kpss(xvalor, type="tau", lags="long")
summary(ur.kpss.xvalor)

# 4. Aplicando un termino autoregresivo####
pibpclag1=lag(pibpc,-1)

# 5. Regresion ####
muestra <-  ts.intersect(pibpc,xvalor,pibpclag1)
muestra
mco<-lm(pibpc~xvalor+pibpclag1, muestra)
summary(mco)

# estadisticos de regresion ####
Resid <- mco$residuals
Resid <- ts(Resid, start=c(1993, 1), freq=1)
sd(Resid)*3
hist(Resid)
plot(Resid)
Resid

# Prueba de Jarque-Bera ; All Ho: resid is distributed normal
jarque.bera.test(Resid) #HO: normalidad de los residuos 
shapiro.test(Resid) #HO: normalidad de los residuos
Resid


# Identificando Outlier
myboxplot1 <- boxplot(Resid)
names(myboxplot1)
myboxplot1$out


#Variable de impulso en series de tiempo
D1998<- ts(datos$d1998, start=c(1993, 1), freq=1)
plot(D1998)

# Aplicando una muestra comun 2 ####
muestra2<-  ts.intersect(pibpc,xvalor,pibpclag1,D1998)
mco2<-lm(pibpc~xvalor+pibpclag1+D1998, muestra2)
summary(mco2)

# mis estad?sticos de regresi?n con R
Resid2 <- mco2$residuals
Resid2 <- ts(Resid2, start=c(1993, 1), freq=1)
sd(Resid2)*3
hist(Resid2)
plot(Resid2)


# 6. Puebas de especificaci?n econom?trica

# 6.1.  Normalidad en los residuos
# Ho: Existe distribuci?n normal en los residuos
# Ho: No existe distribuci?n normal en los residuos

plot(Resid2)

jarque.bera.test(Resid2)
shapiro.test(Resid2)

# 6.2. No autocorrelacion ####

# Ho: No existe autocorrelacion en los residuos en un rezago
dwtest(mco2)


#  Breusch - Godfrey
## Perform Breusch-Godfrey test for first-order serial correlation: bgtest(y1 ~ x)
# Ho: No existe autocorrelaci?n en los residuos en un rezago
bgtest(mco2, order = 1)
# Ho: No existe autocorrelaci?n en los residuos en dos rezago
bgtest(mco2, order = 2)

# 6.3. No Heterocedasticidad ####
#Ho: Los residuos son homosced?sticos.
# Heterskocedasticity Goldfed-Quandt Test
gqtest(mco2)
## perform Harrison-McCabe test
hmctest(mco2)

## No (heterocedasticidad autoregresiva) ARCH 1 y 2 rezagos

## 6.4. Prueba Arch ; H0 : no ARCH effect 
ArchTest (Resid2, lags=1, demean = FALSE)
ArchTest (Resid2, lags=2, demean = FALSE)


# 6.5. Prueba de White
white.test<-bptest(mco2,~fitted(mco2)+I(fitted(mco2)^2))
white.test

# 6.6. No multicolinealidad ####
vif(mco2)

#6.7. Correcta especificacion matematica del modelo y de las variables ####
#  Rainbow analysis
# Ho: El modelo es lineal (en las variables)
# H1: El modelo no es lineal (en las variables)
rain <- raintest(mco2)
rain

#6.8. No existe especificaci?n cu?dratica o c?bica en el modelo
resettest(mco2 , power=2, type="fitted")
resettest(mco2 , power=3, type="fitted")


# 6.9. Analisis de estabilidad de los parametros en el tiempo ####
# Deteccion de cambio estructural

ocus <- efp(mco2, data=muestra2, type="OLS-CUSUM")
bound.ocus <- boundary(ocus, alpha=0.05)
plot(ocus)

ocus2 <- efp(mco2, data=muestra2, type="Rec-CUSUM")
bound.ocus <- boundary(ocus2, alpha=0.05)
plot(ocus2)

fs <- Fstats(mco2, data=muestra2)
plot(fs)
plot(fs, pval=TRUE)
plot(fs, aveF=TRUE)
sctest(fs, type="expF")


# Presentacion de resultados ####
install.packages("stargazer")
library(stargazer)

stargazer(mco2, type="text")

# Simulaciones Bootstrap ####

install.packages("RcmdrMisc")
library(RcmdrMisc)

.bs.samples <- Boot(mco2, R=999, method="case")
plotBoot(.bs.samples)
confint(.bs.samples, level=0.95, type="basic")
remove(.bs.samples)

#HACIENDO EL PRON?STICO ####
x0<-data.frame(xvalor=10, pibpclag1=5.00, D1998=0)
predict(mco2, x0)

#PRONOSTICO CON INTERVALO DE CONFIANZA
predict(mco2, x0, interval="confidence")
