# Ejercicio 2
# Import Base ####
library(readxl)
datos <- read_xlsx("base_tesis2.xlsx",sheet = "base_R")
View(datos)

attach(datos)
names(datos)

#2. Generar variables de series de tiempo  ####
# Nota: A partir de aqui ambas series deben tener  el mismo numero de observaciones
pib <- ts(datos$pib, start=c(1993, 1), freq=1)
x_tm <- ts(datos$x_tm, start=c(1993, 1), freq=1)
x1 <- ts(datos$x1, start=c(1993, 1), freq=1)
x2 <- ts(datos$x2, start=c(1993, 1), freq=1)
x3 <- ts(datos$x3, start=c(1993, 1), freq=1)
x4 <- ts(datos$x4, start=c(1993, 1), freq=1)
inv <- ts(datos$inv, start=c(1993, 1), freq=1)

#Graficamos las variables
plot(pib, main="Crec. del PIB ")
plot(x_tm, main = "Var. % de x_tm")
plot(x1, main = "Var. % de x1")

ggplot(datos, aes(year)) +
  geom_line(aes(y = pib, colour = "Crec. del PIB ")) +
  geom_line(aes(y = x1, colour = "Var. % de x1")) +
  scale_colour_hue("variable")+
  ggtitle("Exportaciones TM y Crec. del PIB ")+
  xlab("tiempo") + ylab("En porcentaje, %")


#3. Analisis de raiz unitaria o pruebas de estacionariedad ####

help("ur.df")
help("ur.pp")
help("ur.kpss")

# pib
ur.df.pib<-ur.df(pib, type = "none", selectlags = "AIC")
summary(ur.df.pib) # H0: Y es no estacionaria o es I(1)

ur.pp.pib<-ur.pp(pib, type="Z-tau", model="constant",
                      lags="long")
summary(ur.pp.pib) # H0: Y es no estacionaria o es I(1)

ur.kpss.pib<-ur.kpss(pib, type="tau", lags="long")
summary(ur.kpss.pib) # H0: Y es estacionaria o es I(0) ; t_t<valor_critico para rechazar H0

#x_tm
ur.df.x_tm<-ur.df(x_tm, type = "none", selectlags = "AIC")
summary(ur.df.x_tm)

ur.pp.x_tm<-ur.pp(x_tm, type="Z-tau", model="constant",
                       lags="long")
summary(ur.pp.x_tm)

ur.kpss.x_tm<-ur.kpss(x_tm, type="tau", lags="long")
summary(ur.kpss.x_tm)

#x1
ur.df.x_tm<-ur.df(x1, type = "none", selectlags = "AIC")
summary(ur.df.x1)

ur.pp.x1<-ur.pp(x1, type="Z-tau", model="constant",
                  lags="long")
summary(ur.pp.x1)

ur.kpss.x1<-ur.kpss(x1, type="tau", lags="long")
summary(ur.kpss.x1)

#x2
ur.df.x_tm<-ur.df(x2, type = "none", selectlags = "AIC")
summary(ur.df.x2)

ur.pp.x2<-ur.pp(x2, type="Z-tau", model="constant",
                lags="long")
summary(ur.pp.x2)

ur.kpss.x2<-ur.kpss(x2, type="tau", lags="long")
summary(ur.kpss.x2)

#x3
ur.df.x3<-ur.df(x3, type = "none", selectlags = "AIC")
summary(ur.df.x3)

ur.pp.x3<-ur.pp(x3, type="Z-tau", model="constant",
                lags="long")
summary(ur.pp.x3)

ur.kpss.x3<-ur.kpss(x3, type="tau", lags="long")
summary(ur.kpss.x3)

#x4

ur.df.x_tm<-ur.df(x4, type = "none", selectlags = "AIC")
summary(ur.df.x4)

ur.pp.x4<-ur.pp(x4, type="Z-tau", model="constant",
                lags="long")
summary(ur.pp.x4)

ur.kpss.x4<-ur.kpss(x4, type="tau", lags="long")
summary(ur.kpss.x4)

#inv
ur.df.x_tm<-ur.df(inv, type = "none", selectlags = "AIC")
summary(ur.df.inv)

ur.pp.inv<-ur.pp(inv, type="Z-tau", model="constant",
                 lags="long")
summary(ur.pp.inv)

ur.kpss.inv<-ur.kpss(inv, type="tau", lags="long")
summary(ur.kpss.inv)


# 4. Aplicando un termino autoregresivo####
piblag1=lag(pib,-1)

# 5. Regresion ####
muestra <-  ts.intersect(pib,x_tm,x1,x2,x3,x4,inv,piblag1)
muestra
mco<-lm(pib~x_tm+piblag1+x1+x2+x3+ x4 +inv, muestra)
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

# 6.2. No autocorrelacion ####

# Ho: No existe autocorrelacion en los residuos en un rezago
dwtest(mco)


#  Breusch - Godfrey
## Perform Breusch-Godfrey test for first-order serial correlation: bgtest(y1 ~ x)
# Ho: No existe autocorrelaci?n en los residuos en un rezago
bgtest(mco, order = 1)
# Ho: No existe autocorrelaci?n en los residuos en dos rezago
bgtest(mco, order = 2)

# 6.3. No Heterocedasticidad ####
#Ho: Los residuos son homosced?sticos.
# Heterskocedasticity Goldfed-Quandt Test
gqtest(mco)
## perform Harrison-McCabe test
hmctest(mco)

## No (heterocedasticidad autoregresiva) ARCH 1 y 2 rezagos

## 6.4. Prueba Arch ; H0 : no ARCH effect 
ArchTest (Resid2, lags=1, demean = FALSE)
ArchTest (Resid2, lags=2, demean = FALSE)


# 6.5. Prueba de White
white.test<-bptest(mco,~fitted(mco)+I(fitted(mco)^2))
white.test

# 6.6. No multicolinealidad ####
vif(mco)

#6.7. Correcta especificacion matematica del modelo y de las variables ####
#  Rainbow analysis
# Ho: El modelo es lineal (en las variables)
# H1: El modelo no es lineal (en las variables)
rain <- raintest(mco)
rain

#6.8. No existe especificaci?n cu?dratica o c?bica en el modelo
resettest(mco , power=2, type="fitted")
resettest(mco , power=3, type="fitted")


# 6.9. Analisis de estabilidad de los parametros en el tiempo ####
# Deteccion de cambio estructural

ocus <- efp(mco, data=muestra, type="OLS-CUSUM")
bound.ocus <- boundary(ocus, alpha=0.05)
plot(ocus)

ocus2 <- efp(mco, data=muestra, type="Rec-CUSUM")
bound.ocus <- boundary(ocus2, alpha=0.05)
plot(ocus2)


# Presentacion de resultados 1 ####
install.packages("stargazer")
library(stargazer)

stargazer(mco, type="text", title = "Resultados de regresiones")

# Presentacion de resultados 2####

#HACIENDO EL PRON?STICO ####
x0<-data.frame(x_tm=10, x1=10,x2=10,x3=10,x4=10,inv=10, piblag1=5.00)
predict(mco, x0)

#PRONOSTICO CON INTERVALO DE CONFIANZA
predict(mco, x0, interval="confidence")
