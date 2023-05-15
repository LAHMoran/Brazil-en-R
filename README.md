# Brazil-en-R
Modelo de demanda de energía, caso Brazil. 1980 2019

attach(DB_Brazil)
#convertir nuestras series a series de tiempo
Yt <- ts(y,frequency=1,start=c(1980))
ELE=ts(ele,frequency=1,start=c(1980))
PRELt = ts(Prel,frequency=1,start=c(1980))
TMt = ts(tm,frequency=1,start=c(1980))           
#paso 1 listo se conviritio en series de tiempo

#para ver que tendencia los datos los grafico, por es0 el comando plot

plot(Yt)
plot(ELE)
plot(PRELt)
plot(TMt)

plot(DB_Brazil)

summary(Yt)
summary(ELE)
summary(PRELt)
summary(TMt)

#Gafico de dsipersion para saber relacion de dos variables

plot(Yt,ELE)
plot(ELE,Yt)

#VAMOS A NOMBRAR LAS VARIABLES REQUERIDAS PARA DICHOS MODELOS

#GNERAR LAS TENDENCIAS REQUERIDAS

x=length👍
t<-seq(1:(x))

#cuadratica

t2<-t^2

#logaritmo lineal

y<-log👍

#modelos
lin<-lm(Yt~t); summary(lin)

cuad<-lm(Yt~t+t^2); summary(cuad)

cub<-lm(y~t); summary (cub) 

logline<-lm(y~t);summary(logline)

#Primera parte

linelog<-lm(ele~tm+tm^2);summary(linelog)


# Carga de datos
attach(DB_Brazil)
Yt <- ts(ele, frequency = 1, start = c(1980))  # Consumo de energía residencial
TMt <- ts(tm, frequency = 1, start = c(1980))  # Temperatura promedio
TMt2 <- TMt^2  # Temperatura al cuadrado

# Gráfico de dispersión
plot(TM2t, Yt, xlab = "Temperatura al cuadrado", ylab = "Consumo de energía residencial")

# Modelo lineal logaritmo
logmodel <- lm(log(Yt) ~ TMt + TMt2)
summary(logmodel)
____________________________________________________________________________________DEsorden
attach(DB_Brazil)

·declarar las variables

Yt=ts(y,frequency=1,start=c(1980))
ELE=ts(ele,frequency=1,start=c(1980))
PRELt=ts(Prel,frequency=1,start=c(1980))
TMt=ts(tm,frequency=1,start=c(1980))

#convertir nuestras series a series de tiempo

Yt <- ts(y,frequency=1,start=c(1980))
ELE=ts(ele,frequency=1,start=c(1980))
PRELt = ts(Prel,frequency=1,start=c(1980))
TMt = ts(tm,frequency= 1,start=c(1980))

·Generar Diferencias

dly<-diff(Yt)
dlele<-diff(ELE)
dlpr<-diff(PRELt)
dltm<-diff(TMt)
dltm2<-diff((TMt)^2)
························
install.packages("dyn")

library(dyn)
····primer modelo que se declara

ltm2<-I(log(TMt)^2)
::::::::::::::::::::::::::::::::::
·······Creacion de objetos de diferencia de variables
modlp<-lm(log(ELE)~log(Yt)+log(PRELt)+log(TMt)+ltm2); summary(modlp)
modlps<-lm(log(ELE)~log(Yt)+log(PRELt)+log(TMt)+ltm2+0); summary(modlps)

·Citerio , el que cuumpla con los signos···
mcei<-ts(modlp$residuals, start=1980, frequency = 1)
mcen<-ts(modlps$residuals, start=1980, frequency = 1)

install.packages("dyn")

#modelo 9
modcp1<-dyn$lm(dlele~dly+dlpr+dltm+lag(mcei,-1)+0);summary(modcp1)
modcp2<-dyn$lm(dlele~dly+dlpr+dltm+lag(mcen,-1)+0);summary(modcp2)

···························································


attach(DB_Brazil)
#convertir nuestras series a series de tiempo
Yt <- ts(y,frequency=1,start=c(1980))
ELE=ts(ele,frequency=1,start=c(1980))
PRELt = ts(Prel,frequency=1,start=c(1980))
TMt = ts(tm,frequency=1,start=c(1980))           
#paso 1 listo se conviritio en series de tiempo

#para ver que tendencia los datos los grafico, por es0 el comando plot

plot(Yt)
plot(ELE)
plot(PRELt)
plot(TMt)

plot(DB_Brazil)

summary(Yt)
summary(ELE)
summary(PRELt)
summary(TMt)

#Gafico de dsipersion para saber relacion de dos variables

plot(Yt,ELE)
plot(ELE,Yt)

#VAMOS A NOMBRAR LAS VARIABLES REQUERIDAS PARA DICHOS MODELOS

#GNERAR LAS TENDENCIAS REQUERIDAS

x=length
t<-seq(1:(x))

#cuadratica

t2<-t^2

#logaritmo lineal

y<-log

#modelos
lin<-lm(Yt~t); summary(lin)

cuad<-lm(Yt~t+t^2); summary(cuad)

cub<-lm(y~t); summary (cub) 

logline<-lm(y~t);summary(logline)

#Modelo lineal log

linelog<-lm(ele~tm+tm^2);summary(linelog)


# Carga de datos
attach(DB_Brazil)
Yt <- ts(ele, frequency = 1, start = c(1980))  # Consumo de energía residencial
TMt <- ts(tm, frequency = 1, start = c(1980))  # Temperatura promedio
TMt2 <- TMt^2  # Temperatura al cuadrado

# Gráfico de dispersión
plot(TM2t, Yt, xlab = "Temperatura al cuadrado", ylab = "Consumo de energía residencial")

# Modelo lineal logaritmo
logmodel <- lm(log(Yt) ~ TMt + TMt2)
summary(logmodel)
#hasta aqui empieza la tarea 2
attach(DB_Brasil_)
#convertir a series de tiempo
Yt<-ts(y,frequency=1,start=c(1980))
ELEt=ts(ele,frequency=1,start=c(1980))
View(DB_Brasil_)
#modelar la tendencia#
summary(lm(ele~t))
#tendencia lineal
##Cuadratica
t2<-t^2
##cubica
t3=t^3
#packages
library("stats")
#Stats
logline<-lm(ele~t);summary(logline)
tcuad<-lm(ele~t);summary(tcuad)
tcub<-lm(ele~t);summary(tcub)
#modelo exponencial
#guardar los parametros del log lineal
b.0<-logline$cofficient[1]
b.1<-logline$cofficient[2]
##guardar como date frammer
ds<-data.frame(ELEt,t)
#grafica de tendencias
t.logline<-exp(logline$fitted.values)
t.cuad<-cuad$fitted.values
t.cub<-cub$fitted.values
#Instalar
install.packages("mFilter")
library(mFilter)
ELEt.hpl<-hpfilter(ELEt,freq=6.25, type=c("lambda"))
ELEt.hpl<-hpfilter(ELEt,freq=29, type=c("frequency"))
#Creacion extraccion.
T.Yt.hpl<-ELEt.hpl$trend
T.Yt.hpf<-ELEt.hpl$trend
#grafica
plot(ELEt,HPlambda=6.25,HPfreq=29)

#Clase despues del paro

#tenemos que cargar  es paquete para los calculos
install.packages("aTSA")

library(aTSA)

#Pureba ejecutada de dick fuller aplificada
adf.test(ELE)
#esta mamada se usa para las pruebas de raices unitarias pero debemos hacerlo para cada variable
#OJO los valores criticos son datos datos para comparar ya estan dados los datos de valores criticos
#ahora hacemos la prueba phillips perroni
pp.test(ELE)
#calulamos diferencias para electricidad
dELE<-diff(ELE)

######################
#Vamos a hacer modelo de largo plazo
#Relaciones a Largo Plazo
#Electricidad positivo 
#Precio  negativo
#Ingreso o pib  es positivo 
#Temperatura positivo o negativo depende del clima dle pais

#haciendo el modleo a alrgo plazo
attach(DB_Brazil)
ltm2<-I(log(TMt)^2)
modlp<-lm(log(ELE)~log(Yt)+log(PRELt)+log(TMt)+ltm^2);summary(modlp)
####
mod9<-lm(log(ELE)~log(Yt)+log(PRELt)+log(TMt)+log(TMt^2));summary(mod9)

#falta lo del la tm

Call:
lm(formula = log(ELE) ~ log (Yt) + log(TMt) + ltm2 + 0)

(Yt)
(ELE)
(PRELt)
(TMt)

#extraer residuales de ese modelo
#modelo largo plazo
mcel<-modlps$residuals

#modelo corto plazo
modcp<-lm(diff(log(ELE)~))

###########################
#instalar
install.packages("dyn")
#menciona o sepa pero tambien
library(dyn)
#hacer series de tiempo

#generar logaritmo de variables
ly=log(y)
lele=log(ele)
lpr=log(Prel)
ltm=log(tm)
ltm2=I(log(tm)^2)


#convertir nuestras series a series de tiempo
ly=ts(y,frequency=1,start=c(1980))
lele=ts(ele,frequency=1,start=c(1980))
lpr=ts(Prel,frequency=1,start=c(1980))
ltm=ts(tm,frequency=1,start=c(1980)) 
ltm2=ts(ltm2,frequency=1,start=c(1980)) 
#modelo 9
mod8<-dyn$lm(lele~ly+lag(ly,-2)+lpr+ltm+ltm2+0);summary(mod8)
(sum((mod8$fitted.values-mean(lele))^2))/(sum((lele-mean(lele))^2))

# sepa no me acuerdo
library(dyn)
library(elebrz)
library(dyn)
library(car) 
library(stats)
library(moments)

#modelos de practica


······btm+
  ····243214mean(log(TMt))*-erjfen
  ···241234mean(log(TMt))*-erjfen
  ···24122mean(dltm2))*-erjfen
··········Metodologia de Hendy de lo general a lo particular
····esta parte es importante porque es la referencia de la temperatira porque con respecto a este nivel medio veremos el modelo que tanto compara el cambio porcentual
x

modcp<-dyn$lm(dlele~+dly+lag(dly,-1)+lag(dly,-2)+lag(dly,-3)+lag(dly,-4)
            +dlpr+lag(dlpr,-1)+lag(dlpr,-2)+lag(dlpr,-3)+lag(dlpr,-4)
            +dltm+lag(dltm,-1)+lag(dltm,-2)+lag(dltm,-3)+lag(dltm,-4)
            +dltm2+lag(dltm2,-1)+lag(dltm2,-2)+lag(dltm2,-3)+lag(dltm2,-4)
            +lag(mcei,-1)+0);summary(modcp)
·····en este punto es donde aparece una filote de numeros······
·····eliminamos al PValue mas alto hasta dejar al menos un rezagp·······
·····de los rezagos restantes los sumasmos y vamos caluclando el efecto total y como va a afectaar en tal periodo de años dependiendo el rezado
