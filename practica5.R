##################################################Practica 5##########################################################
##################################################Ejercicio 1#########################################################
temp<-c(170,172,173,174,174,175,176,177,180,180,180,180,180,181,181,182,182,182,182,184,184,185,186,188)
rela<-c(.84,1.31,1.42,1.03,1.07,1.08,1.04,1.8,1.45,1.6,1.61,2.13,2.15,.84,1.43,.9,1.81,1.94,2.68,1.49,2.52,3,1.87,3.08)
#a)
#grafica de tallos
boxplot(temp)
boxplot(rela)
#comentarios:
#los datos  aparentemete no tienen outliers, sin embargo en la relacion se encuentran mas dispersos los datos.
#b)
pendiente<-cov(temp,rela)/var(temp)
ordeorigen<-mean(rela)-(pendiente*mean(temp))
recta<-(pendiente*temp)+ordeorigen
difer<-rela-recta
sse<-sum(difer^2)
sst<-sum((rela-mean(rela))^2)
rcuad<-1-(sse/sst)
#comentarios
#aplicando el estadistico de r^2 nos da que el modelo pede explicar la variable de relacion en un 45.14%, por lo 
#tanto no es suficiente
#c)
datos1<-data.frame(temp,rela)
require(ggplot2)
ggplot(datos1,aes(temp,rela))+geom_point(col="red",pch=18)+geom_smooth(method = "lm",col="darkblue")
#comentarios
#la regresion no es suficiente para pronosticar los datos en este caso porque los datos se encuentran muy dispersos
##################################################Ejercicio 2#######################################################
edad<-c(0,0,2,11,7,16,9,0,12,4)
linbase<-c(1.72,4.48,4.06,1.26,5.31,.57,3.37,3.44,.74,1.24)
reformu<-c(1.88,5.93,5.54,2.67,6.53,.74,4.94,4.89,.69,1.42)
datos2<-data.frame(edad,linbase,reformu)
ggplot(datos2,aes(edad,linbase))+geom_point()+geom_smooth(method = "lm")
ggplot(datos2,aes(edad,reformu))+geom_point()+geom_smooth(method = "lm")
#comentarios: la relacion en ambas graficas, lo cual me parece un tanto ilogico, debido a que conforme aumenta la 
#edad del motor debe aumentar las emisiones de gas.
##################################################Ejercicio 3######################################################
x<-c(47,62,65,70,70,78,95,100,114,118,124,127,140,140,140,150,152,164,198,221)
y<-c(38,62,53,67,84,79,93,106,117,116,127,114,134,139,142,170,149,154,200,215)
datos3<-data.frame(x,y)
ggplot(datos3,aes(x,y))+geom_point()+geom_smooth(method = "lm")
cor(x,y)
#la regresion es buena para pronosticar los valores de y, y considerando que la correlacion entre las variables es
#cercana a uno, estan fuertemente relacionadas.
##################################################Ejercicio 4######################################################
a<-c(5.8,8.8,11,13.6,18.5,21,23.7,26,28.3,31.9,36.5,38.2,40.4)
b<-c(7.8,8.2,6.9,5.3,4.7,4.9,4.3,2.7,2.8,1.8,1.9,1.1,.4)
datos4<-data.frame(a,b)
ggplot(datos4,aes(a,b))+geom_point()+geom_smooth(method = "lm")
cor(a,b)
#la recta de regresion es un buen parametro para pronosticar los posibles valores  de la concentracion de amonio
#################################################Ejercicio 5#######################################################
tem<-c(59,63,68,72,74,78,83,0,20,40,60,80,100,0,0,0,0,0)
alarg<-c(118,182,247,208,197,135,132,0,0,0,0,0,0,50,100,150,200,250)
datos5<-data.frame(tem,alarg)
ggplot(datos5,aes(tem,alarg))+geom_point()+ geom_line(aes(0,0))
ggplot(datos5,aes(tem,alarg))+geom_point()+ geom_line(aes(55,100))
#por los valores de los puntos es mas estetica la segunda grafica, sin embargo dada la dispercion no hay relacion 
#alguna
#################################################Ejercicio 6#######################################################
#la grafica tiene 2 outliers y  se puede apreiar una relacion media-fuerte y negativa.

#################################################Ejercicio 7#######################################################
#x=resistencia curada
#y=resistencia despues de 28 dias
rd28<-1800+(1.3*2500)
rd28_2<-1800+(1.3*2400)
#cambia en 1.3 unidades
#cambia en 130 unidades
#cambia en -130 unidades
#################################################Ejercicio 8#######################################################
#a) el valor de y=4400 y la de=350, entonces 4400+2de=5100, por lo tanto p=95%
#b) ya excedio el valor de los 5000 con x=2500 P=100%
##
