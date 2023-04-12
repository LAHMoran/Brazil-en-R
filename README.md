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
