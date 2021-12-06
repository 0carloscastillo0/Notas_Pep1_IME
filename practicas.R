#-------------------------------------------------------------------------------
#                 PRACTICAS CAPITULO 1
#-------------------------------------------------------------------------------
#Librerias a utilizar
library(dplyr) #filter, select, mutate, arrange, summarise, group_by
library(tidyr) #pivot_longer, pivot_wider
#library (ggpubr)

# Cargar archivo.csv (ingles) como un data frame
#data <- read.csv(choose.files(), encoding="UTF-8")

# Cargar archivo.csv2 (español) como un data frame
data <- read.csv2(choose.files(), encoding="UTF-8",row.names = 1)

#peso mayores que 2
peso_ideal <- data %>% filter(Peso > 2 & Cambios == "3 cambios")
peso_ideal <- peso_ideal %>% select(Peso,Desplazamiento)
peso_ideal <- peso_ideal %>% mutate(Peso,Desplazamiento,Yardas=(Peso*Desplazamiento*1.5)/3)
ranking_Peso <- (peso_ideal %>% arrange(desc(Peso))) %>% select(Peso)
ranking_Yardas <- (peso_ideal %>% arrange(desc(Yardas))) %>% select(Yardas)

#formato largo archivo
num <- c(1:32)
datitos <- data %>% select(Eje, Peso, Carburadores)
datitos <- cbind(datitos,num)
datitos <- datitos %>% select(num, Eje, Peso, Carburadores)
formato_largo <- datitos %>% pivot_longer(c("Eje", "Peso", "Carburadores"),
                                       names_to = "Características",
                                       values_to = "Dates")

#Formato corto
formato_corto <- formato_largo %>% pivot_wider(names_from = "Características",
                                               values_from = "Dates")

#Renombrar variables
nuevos_nombres <- data %>% rename(R = Rendimiento, 
                                  C = Cilindrada, 
                                  D = Desplazamiento,
                                  P = Potencia,
                                  E = Eje,
                                  Pe = Peso,
                                  Cu = Cuarto_milla,
                                  M = Motor,
                                  T = Transmision,
                                  Ca = Cambios,
                                  Car = Carburadores)

nuevos_nombres$T <- factor(nuevos_nombres$T,
                           levels=c("Manual","Automático"),
                           labels = c("Manual", "Automático"))

#Escribir las nuevas variables y su informacion en un archivo
write.csv2 ( nuevos_nombres, "Mtcars_nuevo.csv")

#-------------------------------------------------------------------------------
#                 PRACTICAS CAPITULO 2
#-------------------------------------------------------------------------------
#Obtencion de datos
data <- read.csv2(choose.files(), encoding="UTF-8",row.names = 1)

#formas de obtener la media
media1 <- mean(data$Rendimiento)
media2 <- mean(data[["Rendimiento"]])

#formas de obtener medias de diversas columnas de un data.frame
medias1 <- sapply(data[c(3,4,5)],mean)
medias2 <- sapply(data[c(3:5)],mean)

#PERCENTILES
cuartiles <- quantile(data$Rendimiento)
quintiles <- quantile(data$Rendimiento, seq(0,1,0.2))
deciles <- quantile(data[["Rendimiento"]], seq(0,1,0.1))
percentiles <- quantile(data[["Rendimiento"]], seq(0,1,0.01))

#Operacion en variables continuas
library(dplyr)
medidas_rendimiento <- data %>% summarise(Media = mean(Rendimiento),
                                          Mediana = median(Rendimiento),
                                          Varianza = var(Rendimiento),
                                          Desv_est = sd(Rendimiento),
                                          Rango_int = IQR(Rendimiento))

#Tablas de contingencia de las dos formas
tabla1 <- table(data$Cilindrada)
tabla2 <- xtabs(~ Cilindrada , data = data)

#total por fila y agregarlo a la tabla
total <- marginSums(tabla2)
tabla_total <- addmargins(tabla1, 1)

#tabla de proporciones
prop <- prop.table(tabla1)
prop <- addmargins(prop,1)

#tabla de contingencia de 2 variables y de proporciones
tabla <- xtabs(~ Cilindrada + Cambios, data = data)
tabla_table <- table(data[["Cilindrada"]], data[["Cambios"]])

sum_fila <- prop.table(tabla, margin = 1)
sum_fila <- addmargins(sum_fila, margin = 2)

sum_columna <- prop.table(tabla, margin = 2)
sum_columna <- addmargins(sum_columna , margin = 1)

prop2 <- prop.table(tabla)
prop2 <- addmargins(prop2)

#tabla de contingencia de 3 variables
forma1_3v <- xtabs(~ Cilindrada + Cambios + Motor, data = data)

data$Cambios <- factor(data$Cambios) #¿es necesario convertire una variable
                                    #categórica a categórica?
forma2_3v <- ftable(data$Cilindrada, data$Cambios, data$Motor)

#resumen usando group by
resumen <- group_by(data, Cilindrada) %>% summarise(cant = n(),
                                                    media = mean(Peso),
                                                    mediana = median(Peso),
                                                    varianza = var(Peso),
                                                    desv_est = sd(Peso),
                                                    rango_int = IQR(Peso))

#   MODA
library(modeest) # mlv
library(multimode) # locmodes
#moda discreta y continua
moda_discreta <- mlv(data$Cambios)
moda_continua <- locmodes(data$Rendimiento, mod02)

#Gráficos de 1 variable numérica
library ( ggpubr ) #gghistogram

histograma <- gghistogram(data,
                          x = "Desplazamiento",
                          binds = 10,
                          add = "mean",
                          xlab = "Metros",
                          ylab = "Frecuencia",
                          color = "blue",
                          fill = "blue")

caja <- ggboxplot(data$Rendimiento,
                  color = "red",
                  fill = "pink",
                  ylab = "Rendimiento")

#Gráficos de 1 variable categórica
contingencia <- as.data.frame( xtabs(~ Cambios, data = data) )

barras <- ggbarplot (contingencia ,
                     x = "Cambios",
                     y = "Freq",
                     fill = c("brown", "purple", "orange") ,
                     title = "Cantidad de cambios de los automóviles",
                     xlab = "Cantidad de cambios",
                     ylab = "Frecuencia")

torta <- ggpie(contingencia,
               x = "Freq",
               label = "Cambios",
               fill = c("red", "yellow", "green") ,
               title = "Cantidad de cambios de los automóviles",
               lab.pos = "in")

#Gráfico de 2 variables numéricas
dispersion <- ggscatter(data,
                       x = "Rendimiento",
                       y = "Potencia",
                       color = "blue",
                       title = "Rendimiento v/s Potencia",
                       xlab = "Rendimiento [millas / galón]",
                       ylab = "Potencia [phz]")

#Gráficos de dos variables categóricas
contingencias <- as.data.frame(xtabs(~ Motor + Cilindrada, data = data))

barras_segmentadas <- ggplot(contingencias , aes(fill = Motor , y = Freq , x = Cilindrada))
barras_segmentadas <- barras_segmentadas + geom_bar ( position = "stack", stat = "identity")
barras_segmentadas <- barras_segmentadas + labs(y = "Frecuencias") + ggtitle("Barras apiladas")
barras_segmentadas <- barras_segmentadas + theme_pubr()

barras_agrupadas <- ggplot(contingencias , aes(fill = Motor , y = Freq , x = Cilindrada))
barras_agrupadas <- barras_agrupadas + geom_bar ( position = "dodge", stat = "identity")
barras_agrupadas <- barras_agrupadas + labs(y = "Frecuencias") + ggtitle("Barras agrupadas")
barras_agrupadas <- barras_agrupadas + theme_pubr()

barras_estandarizadas <- ggplot(contingencias , aes(fill = Motor , y = Freq , x = Cilindrada))
barras_estandarizadas <- barras_estandarizadas + geom_bar ( position = "fill", stat = "identity")
barras_estandarizadas <- barras_estandarizadas + labs(y = "Frecuencias") + ggtitle("Barras esrandarizadas")
barras_estandarizadas <- barras_estandarizadas + theme_pubr()

gráfico <- ggarrange(barras_segmentadas, barras_agrupadas, barras_estandarizadas, nrow = 1, common.legend = TRUE)
titulo <- text_grob ("Tipo de motor por cantidad de Cilindros",
                     face = "bold", size = 24)
gráfico_tipos_barras <- annotate_figure(gráfico , top = titulo)

library ( ggmosaic ) #geom_mosaic
mosaico <- ggplot(data = contingencias)
mosaico <- mosaico + geom_mosaic(aes(weight = Freq , x = product ( Cilindrada ) , fill = Motor))
mosaico <- mosaico + labs(y = "Motor", x = "Cilindrada", title = "Tipo de motor por cantidad de cilindros")
mosaico <- mosaico + scale_fill_manual ( values =c("orange", "purple") )

#Gráficos de 1 variable numérica y 1 variable categórica
caja_cat <- ggboxplot ( data , x = "Cilindrada",
                 y = "Rendimiento",
                 palette = c("light blue", "pink", "yellow") ,
                 fill = "Cilindrada",
                 title = "Rendimiento por cantidad de cilindros",
                 xlab = "Cilindros",
                 ylab = "Rendimiento [ millas /gal ón]")

tiras <- ggstripchart ( data , x = "Cilindrada",
                    y = "Rendimiento",
                    palette = c("blue", "red", "dark green") ,
                    color = "Cilindrada",
                    title = "Rendimiento por cantidad de cilindros",
                    xlab = "Cilindros",
                    ylab = "Rendimiento [ millas /gal ón]")
#-------------------------------------------------------------------------------
#                 PRACTICAS CAPITULO 3
#-------------------------------------------------------------------------------
library ( discreteRV )# RV, E, V, SD, SofIID
library ( ggpubr )

#Generar variable discreta
resultados <- 1:6
probabilidades = c(0.25 , 0.125 , 0.125 , 0.125 , 0.125 , 0.25)
X <- RV ( outcomes = resultados , probs = probabilidades )
esperado <- E(X)
varianza <- V(X)
dv <- SD(X)

#lanzamiento de dados y gráfico de resultados
lanzar_5 <- SofIID(X, n=5)
lanzar_10 <- SofIID(X, n=10)
lanzar_20 <- SofIID(X, n=20)

par(mfrow =c(1 , 3))
plot(lanzar_5 ,
     main =" Lanzamiento de 5 dados ",
     xlab =" Suma de resultados ",
     ylab =" Probabilidad ")
plot(lanzar_10 ,
     main =" Lanzamiento de 10 dados ",
     xlab =" Suma de resultados ",
     ylab =" Probabilidad ")
plot(lanzar_20 ,
     main =" Lanzamiento de 20 dados ",
     xlab =" Suma de resultados ",
     ylab =" Probabilidad ")

#combinacion lineal
resultados <- 1:6
probabilidades = c(0.25 , 0.125 , 0.125 , 0.125 , 0.125 , 0.25)

X <- RV ( outcomes = resultados , probs = probabilidades )
Y <- RV ( outcomes = resultados , probs = 1/6)
Z <- 0.5 * X + 0.5 * Y
z_esperado <- E(Z)
z_varianza <- V(Z)
z_desv_est <- SD(Z)

#distribucion normal para diferentes medias y desv_est
media1 <- 0
dv1 <- 1
x <- seq(-15,15,0.1)
y <- dnorm(x , mean = media1 , sd = dv1)
normal1 <- data.frame(x,y)

media2 <- 10
dv2 <- 6
x <- seq(-15,15,0.1)
y <- dnorm(x , mean = media2 , sd = dv2)
normal2 <- data.frame(x,y)

g <- ggplot ( normal1 , aes (x , y ) ) + geom_line ( color = " blue ")
g <- g + geom_line ( data = normal2 , color = "red")
g <- g + theme_pubr ()

#gráfico Q-Q
datos <- read.csv2(choose.files(), encoding="UTF-8",row.names = 1)
g <- ggqqplot(datos,
              x = "Rendimiento",
              color = "blue")
#-------------------------------------------------------------------------------
#                 PRACTICAS CAPITULO 4
#-------------------------------------------------------------------------------
library ( ggpubr )

#Media movil 
set.seed(13)
poblacion <- rnorm(n = 1500, mean = 4.25, sd = 0.99)
media_poblacion <- mean(poblacion)

muestra <- sample(poblacion, 1000)

n <- seq(along = muestra)
media <- cumsum(muestra)/n

datos <- data.frame(n,media)
g <- ggline ( data = datos ,
              x = "n",
              y = "media",
              plot_type = "l",
              color = "blue",
              main = "Media móvil",
              xlab = "Tamaño de la muestra",
              ylab = "Media muestral")
g <- g + geom_hline ( aes ( yintercept = media_poblacion ) ,
                      color = "red", linetype = 2)

#Medias muestrales
set.seed(60)
poblacion <- rnorm(n = 1500, mean = 4.25, sd = 0.99)
media_poblacion <- mean(poblacion)

tamaño_muestra <- 1200
repeticiones <- 100
muestras <- replicate(repeticiones, sample(poblacion,tamaño_muestra))

medias <- colMeans(muestras)
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

#Prueba de hipotesis unilateral
set.seed (872)

media_poblacion_antiguo <- 530
media_muestra_nuevo <- 527.9
desv_est <- 48
n <- 1600
error_est <- desv_est / sqrt ( n )

x <- seq( media_poblacion_antiguo - 5.2 * error_est ,
          media_poblacion_antiguo + 5.2 * error_est ,
          0.01)
y <- dnorm (x , mean = media_poblacion_antiguo , sd = error_est )
datos <- data.frame (x , y )


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

print ( g )

Z <- ( media_muestra_nuevo - media_poblacion_antiguo ) / error_est

# Calcular el valor p.
p_1 <- pnorm (Z , lower.tail = TRUE )
p_2 <- pnorm ( media_muestra_nuevo , mean = media_poblacion_antiguo ,
               sd = error_est )

#prueba de hipotesis bilateral
set.seed (208)

media_poblacion_antiguo <- 530
media_muestra_nuevo <- 527.9
desv_est <- 48
n <- 1600
error_est <- desv_est / sqrt ( n )

x <- seq( media_poblacion_antiguo - 5.2 * error_est ,
          media_poblacion_antiguo + 5.2 * error_est ,
          0.01)

y <- dnorm (x ,
            mean = media_poblacion_antiguo ,
            sd = error_est )

dataframe <- data.frame (x , y )

g <- ggplot ( data = dataframe , aes ( x ) )

g <- g + stat_function ( fun = dnorm ,
                         args = list ( mean = media_poblacion_antiguo ,
                                       sd = error_est ) ,
                         colour = " steelblue ", size = 1)

g <- g + ylab ("")
g <- g + scale_y_continuous ( breaks = NULL )
g <- g + scale_x_continuous ( name = " Tiempo de procesamiento [ms]")
g <- g + theme_pubr ()

g <- g + geom_area ( data = subset ( dataframe ,
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

g <- g + geom_area ( data = subset ( dataframe ,
                                     x > corte_x ) ,
                     aes ( y = y ) ,
                     colour = "steelblue",
                     fill = "steelblue",
                     
                     alpha = 0.5)

g <- g + geom_vline ( aes ( xintercept = media_poblacion_antiguo ) ,
                      color = "red", linetype = 1)

print ( g )

# Calcular el valor Z para la muestra .
Z <- ( media_muestra_nuevo - media_poblacion_antiguo ) / error_est

# Calcular el valor p ( recordando ahora que la hip ó tesis es bilateral ).
p <- 2 * pnorm (Z , lower.tail = TRUE )

#-------------------------------------------------------------------------------
#                 PRACTICAS CAPITULO 5
#-------------------------------------------------------------------------------
library ( TeachingDemos ) # shapiro.test
library ( ggpubr ) # para los gráficos

#PRUEBA Z

muestra <- c (19.33 , 29.37 , 29.14 , 32.10 , 25.04 , 22.22 , 31.26 , 26.92 ,
              31.40 , 17.66 , 22.55 , 20.69 , 24.68 , 28.74 , 26.85 , 29.68 ,
              29.27 , 26.72 , 27.08 , 20.62)
desv_est <- 2.32
n <- length ( muestra )
valor_nulo <- 20

datos <- data.frame ( muestra )
g <- ggqqplot ( datos , x = "muestra", color = "SteelBlue")

normalidad <- shapiro.test ( muestra )

alfa <- 0.01
media <- mean ( muestra )
Z <- ( media - valor_nulo ) / desv_est

p <- 2 * pnorm (Z , lower.tail = FALSE )

prueba <- z.test ( media , 
                   mu = valor_nulo , 
                   alternative = "two.sided",
                   stdev = desv_est , 
                   conf.level = 1 - alfa )

# PRUEBA T DE STUDENT
tiempo <- c (411.5538 , 393.2753 , 445.8905 , 411.4022 , 498.8969 ,
             388.6731 , 430.0382 , 469.4734 , 409.5844 , 442.0800 ,
             418.1169 , 408.4110 , 463.3733 ,407.0908 , 516.5222)
n <- length(tiempo)
grados_libertad <- n - 1
valor_nulo <- 500

g <- ggqqplot(data = data.frame ( tiempo ) ,
              x = "tiempo",
              color = "steelblue",
              xlab = "Teórico",
              ylab = "Muestra",
              title = "Gráfico Q-Q muestra v/s distr.normal")

alfa <- 0.025
media <- mean(tiempo)
desv_est <- sd(tiempo)
error <- desv_est/sqrt(n)
t <- (media - valor_nulo) / error

p <- pt(t, df = grados_libertad, lower.tail = TRUE)

t_critico <- qt(alfa, df = grados_libertad, lower.tail = FALSE)
superior <- media + t_critico * error
cat("Intervalo de confianza = ( - INF , ", superior, ")")

prueba <- t.test(tiempo,
                 alternative = "less",
                 mu = valor_nulo,
                 conf.level = 1-alfa)

# PRUEBA T-STUDENT, MUESTRAS PAREADAS
instancia <- seq (1 , 35 , 1)

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

normalidad <- shapiro.test ( diferencia )

alfa <- 0.05

#se aplica la prueba t-students para dos muestras pareadas de dos formas distintas
prueba_1 <- t.test ( diferencia ,
                     alternative = "two.sided",
                     mu = valor_nulo ,
                     conf.level = 1 - alfa )
prueba_2 <- t.test ( x = t_A ,
                     y = t_B ,
                     paired = TRUE ,
                     alternative = "two.sided",
                     mu = valor_nulo ,
                     conf.level = 1 - alfa )

# PRUEBA T DE STUDENTS PARA DOS MUESTRAS INDEPENDIENTES

vacuna_A <- c(6.04 , 19.84 , 8.62 , 13.02 , 12.20 , 14.78 , 4.53 , 26.67 ,
              3.14 , 19.14 , 10.86 , 13.13 , 6.34 , 11.16 , 7.62)

vacuna_B <- c(5.32 , 3.31 , 5.68 , 5.73 , 4.86 , 5.68 , 2.93 , 5.48 , 6.10 ,
              2.56 , 7.52 , 7.41 , 4.02)

normalidad_A <- shapiro.test ( vacuna_A )
normalidad_B <- shapiro.test ( vacuna_B )

alfa <- 0.01
prueba <- t.test ( x = vacuna_A ,
                   y = vacuna_B ,
                   paired = FALSE ,
                   alternative = "greater",
                   mu = 0 ,
                   conf.level = 1 - alfa )

media_A <- mean ( vacuna_A )
media_B <- mean ( vacuna_B )
diferencia <- media_A - media_B

#-------------------------------------------------------------------------------
#                 PRACTICAS CAPITULO 6
#-------------------------------------------------------------------------------
library ( ggpubr )
library ( tidyverse ) # power.t.test

#CURVAS DE PODER PARA PRUEBA BILATERAL t

# Generar un vector con un rango de valores para la efecto
# de medias .
efecto <- seq ( -2.5 , 2.5 , 0.01)

# Calcular el poder para una prueba t bilareral , para cada tama ño
# del efecto , asumiendo una muestra con desviaci ón est á ndar igual a 1.
# Se consideran 4 escenarios para calcular el poder :
# 1. Una muestra de tama ño 6 y nivel de significaci ón 0.05.
# 2. Una muestra de tama ño 6 y nivel de significaci ón 0.01.
# 3. Una muestra de tama ño 10 y nivel de significaci ón 0.05.
# 4. Una muestra de tama ño 10 y nivel de significaci ón 0.01.
n_6_alfa_05 <- power.t.test ( n = 6 ,
                              delta = efecto ,
                              sd = 1 ,
                              sig.level = 0.05 ,
                              type = "one.sample",
                              alternative = "two.sided") $ power

n_6_alfa_01 <- power.t.test ( n = 6 ,
                              delta = efecto ,
                              sd = 1 ,
                              sig.level = 0.01 ,
                              type = "one.sample",
                              alternative = "two.sided") $ power

n_10_alfa_05 <- power.t.test ( n = 10 ,
                               delta = efecto ,
                               sd = 1 ,
                               sig.level = 0.05 ,
                               type = "one.sample",
                               alternative = "two.sided") $ power

n_10_alfa_01 <- power.t.test ( n = 10 ,
                               delta = efecto ,
                               sd = 1 ,
                               sig.level = 0.01 ,
                               type = "one.sample",
                               alternative = "two.sided") $ power

datos <- data.frame ( efecto , n_6_alfa_05 , n_6_alfa_01 ,
                      n_10_alfa_05 , n_10_alfa_01)

datos <- datos %>% pivot_longer (!"efecto",
                                 names_to = "fuente",
                                 values_to = "poder")

niveles <- c("n_6_alfa_05", "n_6_alfa_01", "n_10_alfa_05",
             "n_10_alfa_01")

etiquetas <- c("n=6 , alfa =0 ,05", "n=6 , alfa =0,01", "n=10 , alfa =0,05",
               "n=10 , alfa =0,01")

datos [["fuente"]] <- factor ( datos [["fuente"]] , levels = niveles ,
                               labels = etiquetas )

g <- ggplot ( datos , aes ( efecto , poder , colour = factor ( fuente ) ) )
g <- g + geom_line ()
g <- g + labs ( colour = "")
g <- g + ylab (" Poder estadístico ")
g <- g + xlab (" Tamaño del efecto ")

g <- g + scale_color_manual ( values =c("red", "blue", "chartreuse4",
                                        "orange") )
g <- g + theme_pubr ()
g <- g + ggtitle ("Curvas de poder para prueba t bilateral")
g <- g + geom_vline ( xintercept = 0 , linetype = "dashed")

# CALCULO DEL PODER POR CADA TAMAÑO DE LA MUESTRA ASUMIENTO PRUEBA BILATERAL PARA
# UNA MUESTRA
n <- seq (5 , 8000 , 5)

desv_est <- 6
alfa <- 0.05
tam_efecto <- 0.5

poder <- power.t.test ( n = n ,
                        delta = tam_efecto ,
                        sd = desv_est ,
                        sig.level = alfa ,
                        type = "two.sample",
                        alternative = "two.sided") $ power

datos <- data.frame (n , poder )
g <- ggplot ( datos , aes (n , poder ) )
g <- g + geom_line ( colour = "red")
g <- g + ylab ("Poder estadístico")
g <- g + xlab ("Tamaño de la muestra")
g <- g + theme_pubr ()
g <- g + ggtitle ("Relación entre el poder y el tamaño de la muestra")


#OBTENCION DE PODER SI LA HIPOTESIS NULA FUERA VERDADERA
library ( ggpubr )
library ( pwr )

# Fijar valores conocidos .
sigma <- 12
alfa <- 0.05
n <- 36

# Calcular el error estándar .
SE <- sigma / sqrt ( n )

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
#                 PRACTICAS CAPITULO 7
#-------------------------------------------------------------------------------
# MÉTODO DE WALD PARA UNA PROPORCION

n <- 150
p_exito <- 0.64
alfa <- 0.05
valor_nulo <- 0.7

# Construcción del intervalo de confianza .
error_est <- sqrt (( p_exito * (1 - p_exito ) ) / n )
Z_critico <- qnorm ( alfa / 2 , lower.tail = FALSE )
inferior <- p_exito - Z_critico * error_est
superior <- p_exito + Z_critico * error_est
cat (" Intervalo de confianza = [", inferior , ", ", superior , "]\n", sep = "")

# Prueba de hipótesis .
error_est_hip <- sqrt (( valor_nulo * (1 - valor_nulo ) ) / n)
Z <- ( p_exito - valor_nulo ) / error_est_hip
p <- pnorm (Z , lower.tail = FALSE )

# METODO DE WALD PARA LA DIFERENCIA ENTRE DOS PROPORCIONES

n_hombres <- 48
n_mujeres <- 42
exitos_hombres <- 26
exitos_mujeres <- 20
alfa <- 0.05
valor_nulo <- 0

# Calcular probabilidades de é xito .
p_hombres <- exitos_hombres / n_hombres
p_mujeres <- exitos_mujeres / n_mujeres

# Estimar la diferencia .
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

# METODO DE WILSON PARA 1 PROPORCION

n <- 150
p_exito <- 0.64
alfa <- 0.05
valor_nulo <- 0.7

# Calcular cantidad de é xitos .
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

#-------------------------------------------------------------------------------
#                 PRACTICAS CAPITULO 8
#-------------------------------------------------------------------------------
# PRUEBA CHI-CUADRADO DE HOMOGENEIDAD

programadores <- c(42 , 56 , 51 , 27 , 24)
programadoras <- c(25 , 24 , 27 , 15 , 9)

tabla <- as.table ( rbind ( programadores , programadoras ) )
dimnames ( tabla ) <- list ( sexo = c("programadores", "programadoras") ,
                             lenguajes = c("C", "Java", "Python", "Ruby", "Otro") )

prueba <- chisq.test ( tabla )
print ( prueba )

# PRUEBA CHI-CUADRADO DE BONDAD DE AJUSTE

nomina <- c(236 , 78 , 204 , 76 , 66)
muestra <- c(17 , 9 , 14 , 10 , 5)

tabla <- as.table ( rbind ( nomina , muestra ) )
dimnames ( tabla ) <- list ( grupo = c("Nómina", "Muestra") ,
                             lenguajes = c("C", "Java", "Python", "Ruby", "Otro") )


# Verificar si se esperan más de 5 observaciones por cada grupo .
n_nomina <- sum( nomina )
n_muestra <- 55
proporciones <- round ( nomina / n_nomina , 3)
esperados <- round ( proporciones * n_muestra , 3)
print ( esperados )

# Hacer prueba chi - cuadrado de bondad de ajuste.
prueba <- chisq.test ( tabla , correct = FALSE )
print ( prueba )


# PRUEBA CHI-CUADRADO DE INDEPENDENCIA 

comestible <- c(404 , 1948 , 32 , 228 , 1596)
venenoso <- c(48 , 1708 , 0 , 600 , 1556)

tabla <- as.table ( rbind ( comestible , venenoso ) )
dimnames ( tabla ) <- list ( tipo = c("comestible", "venenoso") ,
                             sombrero = c("campana", "convexo", "hundido",
                                          "nudoso", "plano") )

# Hacer prueba chi - cuadrado de independencia .
prueba <- chisq.test ( tabla )
cat ("\ nLa prueba internamente calcula los valores esperados :\n")
esperados <- round ( prueba [["expected"]] , 3)
print ( esperados )

cat ("\ nResultado de la prueba :\n")
print ( prueba )


#PRUEBA EXACTA DE FISHER

vacuna <- c(rep (" Argh ", 6) , rep (" Grrr ", 11) )
resultado <- c(rep(" Humano ", 12) , rep(" Vampiro ", 5) )
datos <- data.frame ( resultado , vacuna )
tabla <- xtabs (~. , datos )

# Aplicar prueba exacta de Fisher .
alfa <- 0.05
prueba <- fisher.test ( tabla , 1 - alfa )


#PRUEBA DE MCNEMAR
alumno <- seq (1:25)
modelo_1 <- c(rep("Correcto", 16) , rep("Incorrecto", 9) )
modelo_2 <- c(rep("Correcto", 9) , rep("Incorrecto", 11) , rep("Correcto", 5) )
datos <- data.frame ( alumno , modelo_2 , modelo_1)
tabla <- table ( modelo_2 , modelo_1)

# Aplicar prueba de McNemar .
prueba <- mcnemar.test ( tabla )

#PRUEBA Q-COCHRAN CON LAS CORRECIONES

library ( tidyverse )
library ( RVAideMemoire )
library ( rcompanion )

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
