f = function(x) {4/(pi*(1+x^2))}

x1 = seq(0,1,0.001)
f1 = f(x)


#Su valor esperado ( μ ) es 0.4412712 y su varianza ( σ 2 ) ) es 0.07851927. Representa gráficamente dicha función de densidad.
mu = 0.4412712
sd = 0.07851927
plot(x1, f1, col="red", lwd=3)

#Simula 50 muestras de tamaño n igual a 4 (utiliza la semilla 321).
set.seed(321)
simu = sample(x1, size = 50*4, replace=TRUE, prob = f1)

#Calcula la suma de las observaciones de cada muestra
muestras_simu = as.data.frame(matrix(simu, ncol=4))
suma_muestras_simu =  apply(samples.X,1,sum)

#¿Cuál es el valor de la media de la suma muestral?
mean(muestras_simu)

#¿Qué relación tiene (aproximada) con la media de la población?
mean(simu)*4
# media muestral = n * media poblacion

#¿La distribución de la suma muestral es exactamente a una N(nμ, nσ2) ?
#yoquese hermano

#Calcula la media de cada muestra
medias_muestra = apply(muestras_simu,1,mean)
medias_muestra

#¿Cuál es el valor de la media de la media muestral?
mean(medias_muestra) 

#¿Qué relación tiene (aproximada) con la media de la población?
mean(simu)
#son iguales

#¿Cuál es el valor de la varianza de la media muestral?
var(medias_muestra)

#¿Qué relación tiene con la varianza de la población?
var(simu) / 4
#es n veces menor


