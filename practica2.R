#Formas de invocar una librería
#formas 1
library(dplyr)
library(tidyr)
#forma 2
require(dplyr)
#instalar paquete en caso de que no esté instalado
if (! require (dplyr) ) {
  install.packages ("dplyr", dependencies = TRUE )
  require ( dplyr )
}

#uso rbind(data.frame, nuevo.data.frame) para agregar una columna al final del data.frame


#invocar datos tipo fecha
fecha_nacimiento <- as.Date(c("2008-1-25", "2006-10-4", "2008-3-27") )

#Lectura de archivo
data <- read.csv2(choose.files(),encoding = "UTF-8",row.names = 1)

#renombramiento de variables categoricas
data$Cilindrada <- factor(data$Cilindrada,
                          levels=c("4 cilindros","6 cilindros","8 cilindros"),
                          labels=c("4 cil","6 cil","8 cil"))

#obtencion de cilindradas
c <- data.frame(data$Cilindrada)
c <- c %>% rename(cilindros = data.Cilindrada)

#tabla de contingencia
contingencia <- xtabs(~ cilindros, data = c)
contingencia <- addmargins(contingencia, 1)

contingencia2 <- xtabs(~ Cilindrada + Cambios, data = data) 
contingencia2 <- prop.table(contingencia2)
contingencia2 <- addmargins(contingencia2)

contingencia3 <- xtabs(~ Cilindrada + Cambios + Motor, data = data)
contingencia3 <- prop.table(contingencia3)
contingencia3 <- addmargins(contingencia3)

data$Cambios <- factor(data$Cambios)
contingencia3v <- ftable(data[["Cilindrada"]],data[["Cambios"]],data[["Motor"]])

#MTC potencia
resumen_potencia <- data %>% summarise(media = mean(Potencia),
                                       mediana = median(Potencia),
                                       varianza = var(Potencia),
                                       desviacion_estandar = sd(Potencia),
                                       rango_intercuartil = IQR(Potencia)) 
library(modeest)
moda_potencia <- mean(mlv(data$Potencia))

resumen_potencia <- cbind(resumen_potencia, moda_potencia)

#MTC potencia por grupo
resumen_potencia_grupo <- group_by(data,Cilindrada) %>% summarise(media = mean(Potencia),
                                                                  mediana = median(Potencia),
                                                                  varianza = var(Potencia),
                                                                  desviacion_estandar = sd(Potencia),
                                                                  rango_intercuartil = IQR(Potencia)) 


moda_cil4 <- mlv((data %>% filter(Cilindrada == "4 cil"))$Potencia)
moda_cil6 <- mlv((data %>% filter(Cilindrada == "6 cil"))$Potencia)
moda_cil8 <- mlv((data %>% filter(Cilindrada == "8 cil"))$Potencia)

modas_cil <- c(moda_cil4,moda_cil6,moda_cil8)
resumen_potencia_grupo <- cbind(resumen_potencia_grupo,modas_cil)

#---------------------------------------------------------------------------------
library(discreteRV)
#se generan 2 variables discretas
#eje
eje <- data[["Eje"]]
eje <- eje/sum(eje)

n <- length(eje)
num <- 1:n

X <- RV(outcomes = num, probs = eje)
esperado <- E(X)
varianza <- V(X)
desv_est <- SD(X)

#Peso
peso <- data[["Peso"]]
peso <- peso/sum(peso)

n <- length(peso)
num <- 1:n

Y <- RV(outcomes = num, probs = peso)
esperado <- E(Y)
varianza <- V(Y)
desv_est <- SD(Y)

#Combinacion lineal
Z <- X + 2*Y
esperado <- E(Z)
varianza <- V(Z)
desv_est <- SD(Z)

#data.frame resumen
variables <- c("X","Y","Z")
esperado <- c(E(X),E(Y),E(Z))
varianza <- c(V(X),V(Y),V(Z))
desv_est <- c(SD(X),SD(Y),SD(Z))
resumen <- data.frame(variables,esperado,varianza,desv_est)

#distribucion normal peso
x <- seq(-20,60,0.5)
y <- dnorm(x, mean=E(Y) , sd=SD(Y))
d_n <- data.frame(x,y)

library ( ggpubr )
g <- ggplot ( d_n , aes (x , y ) ) + geom_line ( color = " blue ")
g <- g + theme_pubr ()
g_qq <- ggqqplot(d_n[c(1:30),],
                x = "y",
                color = "blue")

#-------------------------------------------------------------------------------
library ( ggpubr )
#Se obtiene una media movil
set.seed(22)
poblacion <- rnorm(2000, mean = 3.21, sd = 4.11)
media_poblacion <- mean(poblacion)

tam_muestra <- 1000
muestra <- sample(poblacion, tam_muestra)

n <- 1:tam_muestra
media_movil <- cumsum(muestra)/n

data <- data.frame(n,media_movil)
g <- ggline ( data = data ,
              x = "n",
              y = "media_movil",
              plot_type = "l",
              color = "blue",
              main = "Media móvil",
              xlab = "Tamaño de la muestra",
              ylab = "Media muestral")
g <- g + geom_hline ( aes ( yintercept = media_poblacion ) ,
                      color = "red", linetype = 2)

#Distribucion de estimadores puntuales con diferentes muestras de igual tamaño
#para una misma poblacion
set.seed(123)
poblacion <- rnorm(1500, mean = 2.98, sd=0.12)
media_poblacion <- mean(poblacion)

tamaño_muestra <- 800
repeticiones <- 100
muestra <- replicate(repeticiones, sample(poblacion, tamaño_muestra))

medias <- colMeans(muestra)
medias <- as.data.frame(medias)

g <- gghistogram ( data = medias ,
                   x = "medias",
                   bins = 20 ,
                   title = "Distribución de la media muestral",
                   xlab = "Media",
                   ylab = "Frecuencia",
                   color = "blue",
                   fill = "blue",
                   alpha = 0.2)

g <- g + geom_vline ( aes ( xintercept = media_poblacion ) ,
                      color = "red", linetype = 1)

#Prueba de hipotesis que se cumpla la hipotesis nula
set.seed(234)
media_poblacion_antiguo <- 530
media_muestra_nuevo <- 527.9
desv_est <- 48
n <- 1600
error_est <- desv_est/sqrt(n)
x <- seq(media_poblacion_antiguo - 5.2*error_est, 
         media_poblacion_antiguo + 5.2*error_est,
         0.01)
y <- dnorm(x, mean = media_poblacion_antiguo, sd = error_est)
datos <- data.frame(x,y)

#grafica para hipotesis unilateral
g <- ggplot ( data = datos , aes ( x ) )
g <- g + stat_function ( fun = dnorm ,
                         args = list ( mean = media_poblacion_antiguo ,
                                       sd = error_est ) ,
                         colour = " steelblue ", size = 1)
g <- g + ylab ("")
g <- g + scale_y_continuous ( breaks = NULL )
g <- g + scale_x_continuous ( name = "Tiempo de procesamiento [ms]")
g <- g + theme_pubr ()

g <- g + geom_area ( data = subset ( datos ,
                                     x < media_muestra_nuevo ) ,
                     aes ( y = y ) ,
                     colour = "steelblue",
                     fill = "steelblue",
                     alpha = 0.5)

g <- g + geom_vline ( aes ( xintercept = media_poblacion_antiguo ) ,
                      color = "red", linetype = 1)

#valor z y p de prueba de hipotesis unilateral
z <- (media_muestra_nuevo - media_poblacion_antiguo)/error_est
p <- pnorm(z, lower.tail = TRUE)

#Gráfica para hipotesis bilateral
g <- ggplot ( data = datos , aes ( x ) )

g <- g + stat_function ( fun = dnorm ,
                         args = list ( mean = media_poblacion_antiguo ,
                                       sd = error_est ) ,
                         colour = " steelblue ", size = 1)

g <- g + ylab ("")
g <- g + scale_y_continuous ( breaks = NULL )
g <- g + scale_x_continuous ( name = " Tiempo de procesamiento [ms]")
g <- g + theme_pubr ()

g <- g + geom_area ( data = subset ( datos ,
                                     x < media_muestra_nuevo ) ,
                     aes ( y = y ) ,
                     colour = "steelblue",
                     fill = "steelblue",
                     alpha = 0.5)

area_inferior <- pnorm ( media_muestra_nuevo ,
                         mean = media_poblacion_antiguo ,
                         sd = desv_est )

corte_x <- qnorm (1 - area_inferior ,
                  mean = media_poblacion_antiguo ,
                  sd = desv_est )

g <- g + geom_area ( data = subset ( datos ,
                                     x > corte_x ) ,
                     aes ( y = y ) ,
                     colour = "steelblue",
                     fill = "steelblue",
                     
                     alpha = 0.5)

g <- g + geom_vline ( aes ( xintercept = media_poblacion_antiguo ) ,
                      color = "red", linetype = 1)

#valor z y p de prueba de hipotesis bilateral
z <- (media_muestra_nuevo - media_poblacion_antiguo)/error_est
p <- 2 * pnorm(z, lower.tail = TRUE)

#------------------------------------------------------------------------------
library ( TeachingDemos ) # shapiro.test
library ( ggpubr ) # para los gráficos

#PRUEBA Z
#datos
muestra <- c (19.33 , 29.37 , 29.14 , 32.10 , 25.04 , 22.22 , 31.26 , 26.92 ,
              31.40 , 17.66 , 22.55 , 20.69 , 24.68 , 28.74 , 26.85 , 29.68 ,
              29.27 , 26.72 , 27.08 , 20.62)
desv_est <- 2.32
valor_nulo <- 20
n <- length(muestra)
datos <- data.frame(muestra)

#grafico qq
g <- ggqqplot(datos, x = "muestra", color = "SteelBlue")

#prueba shapiro test
normalidad <- shapiro.test(muestra)

#valor p
alfa <- 0.01
media <- mean(muestra)
z <- (media - valor_nulo)/desv_est
p <- 2 * pnorm(z, lower.tail = FALSE)

#prueba z
prueba <- z.test(media,
                 mu = valor_nulo,
                 alternative = "two.sided",
                 stdev = desv_est,
                 conf.level = 1 - alfa)

#PRUEBA T DE STUDENT
#datos
tiempo <- c (411.5538 , 393.2753 , 445.8905 , 411.4022 , 498.8969 ,
             388.6731 , 430.0382 , 469.4734 , 409.5844 , 442.0800 ,
             418.1169 , 408.4110 , 463.3733 ,407.0908 , 516.5222)
n <- length(tiempo)
grados_libertad <- n - 1
valor_nulo <- 500

#grafico qq
g <- ggqqplot(data = data.frame(tiempo),
              x = "tiempo",
              color = "steelblue",
              xlab = "Teórico",
              ylab = "Muestra",
              title = "Gráfico Q-Q muestra v/s distr.normal")

#Prueba shapiro test
normalidad <- shapiro.test(tiempo)

#valor p
alfa <- 0.025
media <- mean(tiempo)
desv_est <- sd(tiempo)
error <- desv_est / sqrt(n)
t <- (media - valor_nulo)/error
p <- pt(t, df = grados_libertad, lower.tail = TRUE)

#Intervalo de confianza
t_critico <- qt(alfa, df = grados_libertad, lower.tail = FALSE)
superior <- media + t_critico*error
cat("intervalo de confianza: [ INF , ",superior," ]")

#prueba t
prueba <- t.test(tiempo,
                 alternative = "less",
                 mu = valor_nulo,
                 conf.level = 1 - alfa)

#PRUEBA T DE STUDENTS PARA PRUEBAS PAREADAS
instancia <- seq (1 , 35 , 1)
#datos
t_A <- c (436.5736 , 470.7937 , 445.8354 , 470.9810 , 485.9394 ,
          464.6145 , 466.2139 , 468.9065 , 473.8778 , 413.0639 ,
          496.8705 , 450.6578 , 502.9759 , 465.6358 , 437.6397 ,
          458.8806 , 503.1435 , 430.0524 , 438.5959 , 439.7409 ,
          464.5916 , 467.9926 , 415.3252 , 495.4094 , 493.7082 ,
          433.1082 , 445.7433 , 515.2049 , 441.9420 , 472.1396 ,
          451.2234 , 476.5149 , 440.7918 , 460.1070 , 450.1008)

t_B <- c (408.5142 , 450.1075 , 490.2311 , 513.6910 , 467.6467 ,
          484.1897 , 465.9334 , 502.6670 , 444.9693 , 456.3341 ,
          501.1443 , 471.7833 , 441.1206 , 544.1575 , 447.8844 ,
          432.4108 , 477.1712 , 482.4828 , 458.2536 , 474.9863 ,
          496.0153 , 485.8112 , 457.4253 , 483.3700 , 510.7131 ,
          467.5739 , 482.5621 , 453.5986 , 385.9391 , 548.7884 ,
          467.2533 , 494.7049 , 451.9716 , 522.3699 , 444.1270)

diferencia <- t_A - t_B
valor_nulo <-  12.08591
alfa <- 0.05

#prueba shapiro test
normalidad <- shapiro.test(diferencia)

#prueba t student correspondiente
prueba <- t.test(x = t_A,
                 y = t_B,
                 paired = TRUE,
                 alternative = "two.sided",
                 mu = valor_nulo,
                 conf.level = 1 - alfa)



#PRUEBA T DE STUDENTS PARA PRUEBAS INDEPENDIENTES
#datos
vacuna_A <- c(6.04 , 19.84 , 8.62 , 13.02 , 12.20 , 14.78 , 4.53 , 26.67 ,
              3.14 , 19.14 , 10.86 , 13.13 , 6.34 , 11.16 , 7.62)

vacuna_B <- c(5.32 , 3.31 , 5.68 , 5.73 , 4.86 , 5.68 , 2.93 , 5.48 , 6.10 ,
              2.56 , 7.52 , 7.41 , 4.02)

#pruebas shapiro tests
normalidadA <- shapiro.test(vacuna_A)
normalidadB <- shapiro.test(vacuna_B)

#prueba correspondiente
alfa <- 0.01
prueba <- t.test(x = vacuna_A,
                 y = vacuna_B,
                 paired = FALSE,
                 alternative = "greater",
                 mu = 0,
                 conf.level = 1 - alfa)
#------------------------------------------------------------------------------
library(ggpubr)

#Calculo del poder estático a medida que aumenta el tamaño de la muestra
n <- seq(5,8000,5)
desv_est <- 6
alfa <- 0.05
tam_efecto <- 0.5
poder <- power.t.test(n = n,
                      delta = tam_efecto,
                      sd = desv_est,
                      sig.level = alfa,
                      type = "two.sample",
                      alternative = "two.sided")$power
datos <- data.frame(n, poder)
g <- ggplot(datos, aes(n,poder))
g <- g + geom_line(colour = "red")
g <- g + xlab("Tamaño de la muestra")
g <- g + ylab("Poder estadístico")
g <- g + theme_pubr()
g <- g + ggtitle("Relación entre el poder y el tamaño de la muestra")

#Calculo teórico del poder
library(ggpubr)
library(pwr)

sigma <- 12
alfa <- 0.05
n <- 36

SE <- sigma / sqrt(n)
# Gráficar la distribución muestral de la media de las diferencias si
# la hipótesis nula fuera verdadera .
media_nula <- 0
x <- seq ( -6 * SE , 4 * SE , 0.01)
y <- dnorm (x , mean = media_nula , sd = SE )
g <- ggplot ( data = data.frame (x , y ) , aes ( x ) )

g <- g + stat_function (
  fun = dnorm ,
  args = list ( mean = media_nula , sd = SE ) ,
  colour = "red ", size = 1)

g <- g + ylab ("")
g <- g + scale_y_continuous ( breaks = NULL )
g <- g + scale_x_continuous ( name = " Diferencia en tiempos de ejecución [ms]",
                              breaks = seq ( -6 , 4 , 2) )

g <- g + theme_pubr ()
# Colorear la región de rechazo de la hipótesis nula .
media_nula <- 0
Z_critico <- qnorm ( alfa /2 , mean = media_nula , sd = SE , lower.tail = FALSE )
q_critico_inferior <- media_nula - Z_critico
q_critico_superior <- media_nula + Z_critico

g <- g + geom_area ( data = subset (data.frame(x,y) , x < q_critico_inferior ) ,
                     aes ( y = y ) ,
                     colour = "red",
                     fill = "red",
                     alpha = 0.5)

g <- g + geom_area ( data = subset (data.frame(x,y) , x > q_critico_superior ) ,
                     aes ( y = y ) ,
                     colour = "red",
                     fill = "red",
                     alpha = 0.5)
# Superponer la distribución muestral de la media de las diferencias
# si la la diferencia de medias fuera -4.
media_efecto <- -4
g <- g + stat_function (
  fun = dnorm ,
  args = list ( mean = media_efecto , sd = SE ) ,
  colour = " blue ", size = 1)
# Colorear la región de la nueva curva situada en la región de
# rechazo de la curva original .
x1 <- seq ( -6 * SE , 4 * SE , 0.01)
y1 <- dnorm (x , mean = media_efecto , sd = SE )
g <- g + geom_area ( data = subset ( data.frame ( x1 , y1 ) ,
                                     x < q_critico_inferior ) ,
                     aes ( x = x1 , y = y1 ) ,
                     colour = "blue",
                     fill = "blue",
                     alpha = 0.5)

g <- g + geom_area ( data = subset ( data.frame ( x1 , y1 ) ,
                                     x > q_critico_superior ) ,
                     aes ( x = x1 , y = y1 ) ,
                     colour = "blue",
                     fill = "blue",
                     alpha = 0.5)
# Calcular el poder de acuerdo al análisis teórico .
poder <- pnorm (q_critico_inferior ,
                mean = media_efecto ,
                sd = SE ,
                lower.tail = TRUE )
+ pnorm (q_critico_superior ,
         mean = media_efecto ,
         sd = SE ,
         lower.tail = FALSE )

cat (" Poder = ", poder , "\n")
# Calcular la probabilidad de cometer un error tipo II.
beta <- 1 - poder
cat ("Beta = ", beta , "\n")



# CALCULO DEL PODER, TAMAÑO DE LA MUESTRA Y TAMAÑO DEL EFECTO 
n <- 36
diferencia <- 4
desv_est <- 12
alfa <- 0.05
poder <- 0.9
#calculo del poder con power.t.test
resultado <- power.t.test ( n = n ,
                            delta = diferencia ,
                            sd = desv_est ,
                            sig.level = alfa ,
                            power = NULL ,
                            type = "paired",
                            alternative = "two.sided")
# Cálculo del tamaño de la muestra con power.t.test
resultado <- power.t.test ( n = NULL ,
                            delta = diferencia ,
                            sd = desv_est ,
                            sig.level = alfa ,
                            power = poder ,
                            type = "paired",
                            alternative = "two.sided")

n <- ceiling ( resultado [["n"]])
# Calcular el tamaño del efecto (d de Cohen ).
d <- (4 / desv_est ) * (( n - 2) / ( n - 1.25) )
# Calcular el poder usando la función pwr.t.test ().
resultado <- pwr.t.test ( n = n ,
                          d = d ,
                          sig.level = alfa ,
                          power = NULL ,
                          type = "paired",
                          alternative = "two.sided")
# Cálculo del tamaño de la muestra usando la función pwr.t.test().
resultado <- pwr.t.test ( n = NULL ,
                          d = d ,
                          sig.level = alfa ,
                          power = poder ,
                          type = "paired",
                          alternative = "two.sided")

n <- ceiling ( resultado [["n"]])

#-------------------------------------------------------------------------------
#METODO DE WALD PARA UNA PROPORCION

#valores conocidos
n <- 150
p_exito <- 0.64
alfa <- 0.05
valor_nulo <- 0.7
#construccion del intervalo de confianza
error_est <- sqrt((p_exito * (1 - p_exito))/n)
z_critico <- qnorm(alfa/2, lower.tail = FALSE)
inferior <- p_exito - z_critico * error_est
superior <- p_exito + z_critico * error_est
cat("Intervalo de confianza = [ ",inferior," , ",superior," ]")
#Prueba de hipotesis
error_est_hip <- sqrt((valor_nulo * (1 - valor_nulo))/n)
z <- (p_exito - valor_nulo)/error_est_hip
p <- pnorm(z, lower.tail = FALSE)


#METODO DE WALD PARA DOS PROPORCIONES (valor nulo = 0)

#datos
n_hombres <- 48
n_mujeres <- 42
exitos_hombres <- 26
exitos_mujeres <- 20
alfa <- 0.05
valor_nulo <- 0
#probabilidad de exito
p_hombres <- exitos_hombres / n_hombres
p_mujeres <- exitos_mujeres / n_mujeres
#estimar la diferencia
diferencia <- p_hombres - p_mujeres
# Construcción del intervalo de confianza .
error_hombres <- ( p_hombres * (1 - p_hombres ) ) / n_hombres
error_mujeres <- ( p_mujeres * (1 - p_mujeres ) ) / n_mujeres
error_est <- sqrt ( error_hombres + error_mujeres )
Z_critico <- qnorm ( alfa / 2 , lower.tail = FALSE )
inferior <- diferencia - Z_critico * error_est
superior <- diferencia + Z_critico * error_est
cat (" Intervalo de confianza = [", inferior , ", ", superior , "]\n", sep = "")
# Prueba de hipótesis .
p_agrupada <- ( exitos_hombres + exitos_mujeres ) / ( n_hombres + n_mujeres )
error_hombres <- ( p_agrupada * (1 - p_agrupada ) ) / n_hombres
error_mujeres <- ( p_agrupada * (1 - p_agrupada ) ) / n_mujeres
error_est_hip <- sqrt ( error_hombres + error_mujeres )
Z <- ( diferencia - valor_nulo ) / error_est_hip
p <- 2 * pnorm (Z , lower.tail = FALSE )


#METODO DE WALD PARA DOS PROPORCIONES (valor nulo distinto a 0)

#datos
n_hombres <- 89
n_mujeres <- 61
exitos_hombres <- 45
exitos_mujeres <- 21
alfa <- 0.05
valor_nulo <- 0.1
#probabilidad de exito
p_hombres <- exitos_hombres / n_hombres
p_mujeres <- exitos_mujeres / n_mujeres
#estimar la diferencia
diferencia <- p_hombres - p_mujeres
# Construcción del intervalo de confianza .
error_hombres <- ( p_hombres * (1 - p_hombres ) ) / n_hombres
error_mujeres <- ( p_mujeres * (1 - p_mujeres ) ) / n_mujeres
error_est <- sqrt ( error_hombres + error_mujeres )
Z_critico <- qnorm ( alfa / 2 , lower.tail = FALSE )
inferior <- diferencia - Z_critico * error_est
superior <- diferencia + Z_critico * error_est
cat (" Intervalo de confianza = [", inferior , ", ", superior , "]\n", sep = "")
# Prueba de hipótesis .
p_agrupada <- ( exitos_hombres + exitos_mujeres ) / ( n_hombres + n_mujeres )
error_hombres <- ( p_hombres * (1 - p_hombres ) ) / n_hombres
error_mujeres <- ( p_mujeres * (1 - p_mujeres ) ) / n_mujeres
error_est <- sqrt ( error_hombres + error_mujeres )
Z <- ( diferencia - valor_nulo ) / error_est
p <- pnorm (Z , lower.tail = FALSE )

#METODO DE WILSON PARA UNA PROPORCION

#datos
n <- 150
p_exito <- 0.64
alfa <- 0.05
valor_nulo <- 0.7
# Calcular cantidad de éxitos .
exitos <- p_exito * n
# Prueba de Wilson en R.
prueba <- prop.test ( exitos , n = n , p = valor_nulo ,
                      alternative = "greater", conf.level = 1 - alfa )


# METODO DE WILSON PARA UNA DIFERENCIA ENTRE DOS PROPORCIONES
n <- c(c(48 , 42) )
exitos <- c(26 , 20)
alfa <- 0.05
valor_nulo <- 0.0
# Prueba de Wilson en R.
prueba <- prop.test ( exitos , n = n , alternative = "two.sided",
                      conf.level = 1 - alfa )

#PODER 
#-------------------------------------------------------------------------------
# PRUEBA CHI-CUADRADO DE HOMOGENEIDAD
#tabla de contingencia
programadores <- c(42 , 56 , 51 , 27 , 24)
programadoras <- c(25 , 24 , 27 , 15 , 9)
tabla <- as.table(rbind(programadores,programadoras))
dimnames ( tabla ) <- list ( sexo = c("programadores", "programadoras") ,
                             lenguajes = c("C", "Java", "Python", "Ruby", "Otro") )
#prueba chi-cuadrado de homogeneidad
prueba <- chisq.test ( tabla )
print ( prueba )


#PRUEBA CHI-CUADRADO DE BONDAD DE AJUSTE
#crear tabla de contingencia
nomina <- c(236 , 78 , 204 , 76 , 66)
muestra <- c(17 , 9 , 14 , 10 , 5)
tabla <- as.table(rbind(nomina,muestra))
dimnames ( tabla ) <- list ( sexo = c("Nómina", "Muestra") ,
                             lenguajes = c("C", "Java", "Python", "Ruby", "Otro") )
#Verifica si se esperan mas de 5 observaciones por cada grupo
n_nomina <- sum( nomina )
n_muestra <- 55
proporciones <- round ( nomina / n_nomina , 3)
esperados <- round ( proporciones * n_muestra , 3)
print ( esperados )
# Hacer prueba chi - cuadrado de bondad de ajuste.
prueba <- chisq.test ( tabla , correct = FALSE )
print ( prueba )


#PRUEBA CHI-CUADRADO DE INDEPENDENCIA
#Se crea la tabla de contingencia
comestible <- c(404 , 1948 , 32 , 228 , 1596)
venenoso <- c(48 , 1708 , 0 , 600 , 1556)
tabla <- as.table(rbind(comestible,venenoso))
dimnames ( tabla ) <- list ( sexo = c("Comestible", "Venenoso") ,
                             lenguajes = c("Campana", "Convexo", "Hundido", "Nudoso", "Plano") )
# Hacer prueba chi - cuadrado de independencia .
prueba <- chisq.test ( tabla )
cat ("\ nLa prueba internamente calcula los valores esperados :\n")
esperados <- round ( prueba [["expected"]] , 3)
print ( esperados )
cat ("\ nResultado de la prueba :\n")
print ( prueba )


# PRUEBA EXACTA DE FISHER
#tabla de contingencia
vacuna <- c(rep (" Argh ", 6) , rep (" Grrr ", 11) )
resultado <- c(rep(" Humano ", 12) , rep(" Vampiro ", 5) )
datos <- data.frame ( resultado , vacuna )
tabla <- xtabs (~. , datos )
# Aplicar prueba exacta de Fisher .
alfa <- 0.05
prueba <- fisher.test ( tabla , 1 - alfa )


#PRUEBA DE MCNEMAR
#tabla de contingencia
alumno <- seq (1:25)
modelo_1 <- c(rep("Correcto", 16) , rep("Incorrecto", 9) )
modelo_2 <- c(rep("Correcto", 9) , rep("Incorrecto", 11) , rep("Correcto", 5) )
datos <- data.frame ( alumno , modelo_2 , modelo_1)
tabla <- table ( modelo_2 , modelo_1)
# Aplicar prueba de McNemar .
prueba <- mcnemar.test ( tabla )


#PRUEBA Q DE COCHRAN CON LAS CORRECCIONES
library ( tidyverse )
library ( RVAideMemoire )
if (!require(rcompanion) ) {
  install.packages("rcompanion", dependencies = TRUE )
  require(rcompanion)
}

# Crear matriz de datos .
instancia <- 1:15
annealing <- c(0 , 1 , 0 , 0 , 0 , 0 , 0 , 1 , 0 , 0 , 0 , 0 , 1 , 0 , 0)
hormigas <- c(0 , 0 , 1 , 0, 0 , 1 , 0 , 0, 0 , 1 , 0 , 0 , 0 , 0 , 1)
genetico <- c(1 , 0 , 1 , 1, 1 , 1 , 0 , 1, 0 , 1 , 1 , 0 , 0 , 1 , 1)
datos <- data.frame ( instancia , annealing , hormigas , genetico )

# Llevar matriz de datos a formato largo .
datos <- datos %>% pivot_longer (c("annealing", "hormigas", "genetico") ,
                                 names_to = "metaheuristica",
                                 values_to = "resultado")

datos [["instancia"]] <- factor ( datos [["instancia"]])
datos [["metaheuristica"]] <- factor ( datos [["metaheuristica"]])

# Hacer prueba Q de Cochran .
prueba <- cochran.qtest ( resultado ~ metaheuristica | instancia ,
                          data = datos , alpha = 0.05)
print ( prueba )

# Procedimiento post -hoc con correcci ón de Bonferroni .
post_hoc_1 <- pairwiseMcnemar ( resultado ~ metaheuristica | instancia ,
                                data = datos , method = "bonferroni")
cat ("\ nProcedimiento post - hoc con corrección de Bonferroni \n")
print ( post_hoc_1)

# Procedimiento post -hoc con corrección de Holm .
post_hoc_2 <- pairwiseMcnemar ( resultado ~ metaheuristica | instancia ,
                                data = datos , method = "holm")
cat ("\ nProcedimiento post - hoc con corrección de Holm \n")
print ( post_hoc_2)

#conclusion; despues de realizar la prueba q de cochran, y los procedimientos
#post hoc,  se concluye que la evidencia no es lo suficientemente fuerte para 
#poder afirmar que existen diferencias entre las metaheurísticas, pero que
#podría ser adecuado hacer un estudio con una muestra mayor puesto que los 
#resultados de la prueba Q de Cochran y de los procedimientos post-hoc son 
#contradictorios.

#-------------------------------------------------------------------------------
#recordatorio

#distribuciones continuas: 
#normal:
#dnorm (x , mean = media , sd = desv _ est )
#pnorm(q, mean, sd, lower.tail)
#qnorm(p, mean, sd, lower.tail)
#rnorm(n, mean, sd)

#z estandar (listo)

#chi-cuadrado: x^2 = sum(Z(1:k)^2)
#             media = v
#             desv_est = 2v
#dchisq(x, df).
#pchisq(q, df, lower.tail).
#qchisq(p, df, lower.tail).
#rchisq(n, df).

#t-student: t = Z*sqrt((v^2)/(X^2))
#           media = 0 (para v>1)
#           desv_est = sqrt(v/(v-2)) (para v>2)
#dt(x, df).
#pt(q, df, lower.tail).
#qt(p, df, lower.tail).
#rt(n, df).

#F: F = ((X^2)1/v1)/((X^2)2/(v2))
#   media = v2/(v2-2) (v2>2)
#   desv_est = sqrt((2*(v2^2)*(v1 + v2 - 2)/(v1*((v2-2)^2)*(v2-4)))
#df(x, df1, df2).
#pf(q, df1, df2, lower.tail).
#qf(p, df1, df2, lower.tail).
#rf(n, df1, df2).



#distribuciones discretas: 
  #bernoulli: media = p, 
  #           desv_est = sqrt(p*(1-p)), 
  #dbern(x, prob).
  #pbern(q, prob, lower.tail).
  #qbern(p, pro, lower.tail).
  #rbern(n, prob).

  #geometrica: 1exito_en_enesimo_intento = ((1-p)^(n-1))*p, 
  #                               media = 1/p , 
  #                               desv_est = sqrt((1-p)/(p^2))
  #dgeom(x, prob).
  #pgeom(q, prob, lower.tail).
  #qgeom(p, prob, lower.tail).
  #rbern(n, prob).

  #binomial: (n k)= factorial(n)/(factorial(k)*factorial(n-k))
  #         (p^k)*((1-p)^(n-k))
  #         Pr = (n k)*((p^k)*((1-p)^(n-k)))
  #         media = np
  #         desv_est = sqrt(np*(1-p))
  #dbinom(x, size, prob).
  #pbinom(x, size, prob).
  #qbinom(p, size, prob).
  #rbinom(n, size, prob).

  #binomial negativa: (n-1 k-1)*(p^k)*((1-p)^(1-k))
  #                   media = (k*(1-p))/p
  #                   desv_est = sqrt((k*(1-p))/(p^2))
  #dnbinom(x, size, prob).
  #pnbinom(q, size, prob, lower.tail).
  #qnbinom(p, size, prob, lower.tail).
  #rnbinom(n, size, prob).

  #poisson: Pr(observar k-eventos) = (lambda*(e^(-lambda)))/factorial(k)
  #dpois(x, lambda).
  #ppois(q, lambda, lower.tail).
  #qqpois(p, lambda, lower.tail).
  #rpois(n, lambda).
