#Los pesos de los hombres adultos de una determinada población se distribuyen normalmente, con una media de 80 kg y una desviación estándar de 15 kg.
mean = 80
sd = 15
var = sd^2

#Calcula la probabilidad de que: 
#La suma del peso de 9 hombres esté entre 700 y 800kg.
n = 9
pnorm(800, mean = n*mu, sd = sqrt(n*var), lower.tail = TRUE) - pnorm(700, mean = n*mu, sd = sqrt(n*var), lower.tail = TRUE)

#La media del peso de 9 hombres esté entre 78 y 80kg
n = 9
pnorm(80, mean = mean, sd = sqrt(var/n)) - pnorm(78, mean = mean, sd = sqrt(var/n))

#La varianza del peso de 9 hombres esté entre 200 y 250
n = 9
pchisq((n-1)*250/var, df = n-1, lower.tail = TRUE) - pchisq((n-1)*200/var, df = n-1, lower.tail = TRUE)

#Simula 100 muestras de tamaño n igual a 9 (utiliza la semila 321)
n = 9
set.seed(321)
simu = rnorm(100*n, mean = mean, sd = sd)
muestras_simu = as.data.frame(matrix(simu, ncol=9))

#Haz el histograma de la suma muestral
suma_muestras_simu = apply(muestras_simu, 1, sum)
hist(suma_muestras_simu, freq=FALSE)
curve(dnorm(x, mean=n*mean, sd=sqrt(var*n)), col="red", lwd=3, add=TRUE)

#¿La distribución de la suma muestral se puede aproximar a una N ( n μ , n σ 2 ) ? Compare el histograma y la distribución aproximada.
media_muestral_simu = apply(muestras_simu, 1, mean)
hist(media_muestral_simu, freq=FALSE)
curve(dnorm(x, mean=mean, sd=sqrt(var/n)), col="red", lwd=3, add=TRUE)

#¿La distribución de la media muestral se puede aproximar a una N ( μ , σ 2 / n ) ? Compare el histograma y la distribución aproximada.
varianza_muestral_simu = apply(muestras_simu, 1, var)
hist(varianza_muestral_simu, freq=FALSE)
curve( dchisq(x, df=n-1), col="red", lwd=3, add=TRUE)

#¿La distribución de la varianza muestral se puede aproximar a una χ n − 1 2 ? Compare el histograma y la distribución aproximada.
hist((n-1)*varianza_muestral_simu/var)

hist((n-1)*varianza_muestral_simu/var, freq=FALSE)
curve( dchisq(x, df=n-1), col="red", lwd=3, add=TRUE)