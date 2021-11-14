#Fernando Mireles

vector_Deportes = c(6726, 1653, 31844, 22553, 1311, 60838, 1292, 7410 ,31977, 21014, 16777, 59660, 5681, 41743, 3325, 3021, 20995, 8246, 11685, 2831, 22192, 5282, 11001, 1330, 6783, 4788, 30438, 44099, 7650, 7448, 7904, 8113, 33022, 9994, 2470, 30476, 4731, 10583, 12065, 10013)

vector_Deportes_o = sort(vector_Deportes)

# Media
media_departamemto_deportes = mean(vector_Deportes_o)
media_departamemto_deportes

#Mediana
median(vector_Deportes_o)

#Moda
library(modeest)
mlv(vector_Deportes_o, method = "mfv")

#TABLA DE DISPERSION

# Despues de calcular las medidas de tendencia central, calculamos
# lo que es el primer dato de nuestra tabla
# que seria (vector_Deportes - media_departamemto_deportes_o) de la tabla de dispersion

media_departamemto_deportes_Dispersion1 = c(vector_Deportes_o - media_departamemto_deportes)

# Despues elevamos el resultado, de la x menos la media, y nos daria como resultado lo siguiente

media_departamemto_deportes_Dispersion2 = c(media_departamemto_deportes_Dispersion1 * media_departamemto_deportes_Dispersion1)

# Despues de eso sacamos la frecuencia, con la que se repiten los datos

media_departamemto_deportes_Dispersion3 = table(media_departamemto_deportes_Dispersion2)

# Luego procedemos a sacar la varianza de la tabla

media_departamemto_deportes_Dispersion4 = sum(media_departamemto_deportes_Dispersion2)/(length(media_departamemto_deportes_Dispersion2)-1)

# Despues la desviacion estandar

media_departamemto_deportes_Dispersion5 = sqrt(media_departamemto_deportes_Dispersion4)
media_departamemto_deportes_Dispersion5
# Terminamos la tabla de Dispersion, nos movemos a la tabla de frecuencias
# ---------------------------------------------------------------------------------------#

# Miguel del Castillo

# Obtenemos el valor de la k

k <- 0
while (2^k < length(vector_Deportes_o)) {
  k <- k + 1
}
k

# Una Vez identificada la K, procedemos a establecer el valor del Intervalo

intervalo <- (vector_Deportes_o[length(vector_Deportes_o)] - vector_Deportes_o[1])/k
intervalo

# Una vez definido el intervalo podemos crear en plural, los intervalos y
# con ello, definir los rangos de cada clase

intervalos <- cut(vector_Deportes_o, breaks = k)
intervalos

# Ya definidos los rangos de cada clase, podemos empezar a crear la tabla de 
# frecuencia

#Y como primer paso, identificaremos las frecuencias de cada clase

tablaF <- as.data.frame(table(Clases = factor(cut(vector_Deportes_o, breaks = k))))
tablaF

# Una vez identficada las frecuencias, con la siguientes lineas de codigo,
# obtenemos la frecuencia relativa y acumulada
transform(tablaF,
          Fac = cumsum(Freq),
          Rel = round(prop.table(Freq), 4),
          RelAc = round(cumsum(prop.table(Freq)), 4)
)

# Ahora con lo anterior hecho, podemos obtener el histograma, y con ayuda
# de los intervalos, creamos la tabla
FA <- table(intervalos)
FA

# Utilizamos la funcion barplot, para visualizar la frecuencia absoluta de
# las clases



barplot(FA, main = "HISTORGRAMA DEPARTAMENTO DEPORTES")

# HISTORGAMA

FR <- table(intervalos) / length(vector_Deportes_o)
FR

# Al igual, podemos visualizar los datos, con barplot, y ver mas graficamente
# el resultado

barplot(FR)

porcentaje <- FR * 100
porcentaje

# Por ultimo, estableceremos el punto medio, el cual, primero tenemos que identificar
# las clases de cada rango

menor_o_datos <- min(vector_Deportes_o)
contador <- 0
clasesP <- 0
while (contador <= k + 1) {
  clasesP[contador] <- c(vector_Deportes_o + (intervalo * (contador - 1)))
  contador <- contador + 1
}
clasesP

# Ya obtenidos los rangos de cada clase comenzamos a definir los Puntos Medios de cada
# clase, con el objetivo de poder graficarlos

contador2 <- 1
puntoM <- 0
while (contador2 <= length(clasesP)) {
  if (contador2 == 7){
    break
  }
  puntoM[contador2] <- c((clasesP[contador2] + clasesP[contador2 + 1]) / 2)
  contador2 <- contador2 + 1
}
puntoM

# Como datos adicionales, tenemos que identificar una clase anterior de la primer y
# ultima clase el punto medio, para poder dar inicio a la recta

minimoM <- ( (min(clasesP) - intervalo) + min(clasesP) ) / 2
minimoM

maximoM <- ( (max(clasesP) + intervalo) + max(clasesP) ) / 2
maximoM

# Luego ordenaremos los nuevos puntos medios en el vector

puntoMM <- sort(c(puntoM, minimoM, maximoM))
puntoMM

# En el vector de Frecuencia Absoluta, vamos a agregar los puntos inciales de los
# puntos Medios de minimoM, y Maximo M

FA <- c(0, FA, 0)
FA

# Una vez hecho esto, creamos un plot en el cual asignamos como eje X, los puntos
# Medios, y del Eje Y las frecuencias

#----------------------------------------------------------------------------#

# Fernando Chavez


# POLIGONO DE FRECUENCIAS

plot(x = puntoMM, y = FA, col = 2, type = "b", main = "Poligono de frecuencias Departamento Deportes")

# DIAGRAMA PASTEL

etiqueta <- paste(porcentaje, "%", sep = " ")
etiqueta

# Despues de eso, creamos el vecto de colores, el cual, le ponemos la cantidad
# de colores que representan cada intervalo
colores <- c("Blue", "Green", "Red", "Purple","Yellow","Black")
colores

# Una vez hecho todo esto, podemos dar pie a crear nuestro diagrama pastel,
# utilizando la funcion pie

pie(porcentaje,
    labels = etiqueta,
    clockwise = TRUE,
    col = colores,
    main = "Diagrama Pastel Departamento Deportes")

#-------------------------------------------------------------------------------#

# Juan de Dios

# VECTOR MINORISTA

vector_minorististas = c(21204,12939,24586,2280,14326,22914,27512,16530,17936,23845,23047, 24529, 13661, 17746, 24947,20577,17081,16150,16834,29564,16834,24985,35302,23978,25422,20254,15333,23636,14402,17442)

vector_minorista_o = sort(vector_minorististas)

# Media
media_departamemto_minorista = mean(vector_minorista_o)

#Mediana
median(vector_minorista_o)

#Moda
library(modeest)
mlv(vector_minorista_o, method = "mfv")

#TABLA DE DISPERSION

# Despues de calcular las medidas de tendencia central, calculamos
# lo que es el primer dato de nuestra tabla
# que seria (vector_minorista - media_departamemto_minorista_o) de la tabla de dispersion

media_departamemto_minorista_Dispersion1 = c(vector_minorista_o - media_departamemto_minorista)

# Despues elevamos el resultado, de la x menos la media, y nos daria como resultado lo siguiente

media_departamemto_minorista_Dispersion2 = c(media_departamemto_minorista_Dispersion1 * media_departamemto_minorista_Dispersion1)

# Despues de eso sacamos la frecuencia, con la que se repiten los datos

media_departamemto_minorista_Dispersion3 = table(media_departamemto_minorista_Dispersion2)

# Luego procedemos a sacar la varianza de la tabla

media_departamemto_minorista_Dispersion4 = sum(media_departamemto_minorista_Dispersion2)/(length(media_departamemto_minorista_Dispersion2)-1)

# Despues la desviacion estandar se saca raiz

media_departamemto_minorista_Dispersion5 = sqrt(media_departamemto_minorista_Dispersion4)
media_departamemto_minorista_Dispersion5

# Terminamos la tabla de Dispersion, nos movemos a la tabla de frecuencias

# Obtenemos el valor de la k

k <- 0
while (2^k < length(vector_minorista_o)) {
  k <- k + 1
}
k

# Una Vez identificada la K, procedemos a establecer el valor del Intervalo

intervalo <- (vector_minorista_o[length(vector_minorista_o)] - vector_minorista_o[1])/k
intervalo

# Una vez definido el intervalo podemos crear en plural, los intervalos y
# con ello, definir los rangos de cada clase

intervalos <- cut(vector_minorista_o, breaks = k)
intervalos

# Ya definidos los rangos de cada clase, podemos empezar a crear la tabla de 
# frecuencia

#Y como primer paso, identificaremos las frecuencias de cada clase

tablaF <- as.data.frame(table(Clases = factor(cut(vector_minorista_o, breaks = k))))
tablaF

# Una vez identficada las frecuencias, con la siguientes lineas de codigo,
# obtenemos la frecuencia relativa y acumulada
transform(tablaF,
          Fac = cumsum(Freq),
          Rel = round(prop.table(Freq), 4),
          RelAc = round(cumsum(prop.table(Freq)), 4)
)

# Ahora con lo anterior hecho, podemos obtener el histograma, y con ayuda
# de los intervalos, creamos la tabla
FA <- table(intervalos)
FA

# Utilizamos la funcion barplot, para visualizar la frecuencia absoluta de
# las clases

barplot(FA, main = "Histograma Departamento Deportes")

# HISTORGAMA

FR <- table(intervalos) / length(vector_minorista_o)
FR

#-----------------------------------------------------------------------------#

# Kevin Ricardo

# Al igual, podemos visualizar los datos, con barplot, y ver mas graficamente
# el resultado

barplot(FR)

porcentaje <- FR * 100
porcentaje

# Por ultimo, estableceremos el punto medio, el cual, primero tenemos que identificar
# las clases de cada rango

menor_o_datos <- min(vector_minorista_o)
contador <- 0
clasesP <- 0
while (contador <= k + 1) {
  clasesP[contador] <- c(vector_minorista_o + (intervalo * (contador - 1)))
  contador <- contador + 1
}
clasesP

# Ya obtenidos los rangos de cada clase comenzamos a definir los Puntos Medios de cada
# clase, con el objetivo de poder graficarlos

contador2 <- 1
puntoM <- 0
while (contador2 <= length(clasesP)) {
  if (contador2 == 7){
    break
  }
  puntoM[contador2] <- c((clasesP[contador2] + clasesP[contador2 + 1]) / 2)
  contador2 <- contador2 + 1
}
puntoM

# Como datos adicionales, tenemos que identificar una clase anterior de la primer y
# ultima clase el punto medio, para poder dar inicio a la recta

minimoM <- ( (min(clasesP) - intervalo) + min(clasesP) ) / 2
minimoM

maximoM <- ( (max(clasesP) + intervalo) + max(clasesP) ) / 2
maximoM

# Luego ordenaremos los nuevos puntos medios en el vector

puntoMM <- sort(c(puntoM, minimoM, maximoM))
puntoMM

# En el vector de Frecuencia Absoluta, vamos a agregar los puntos inciales de los
# puntos Medios de minimoM, y Maximo M

FA <- c(0, FA, 0)
FA

# Una vez hecho esto, creamos un plot en el cual asignamos como eje X, los puntos
# Medios, y del Eje Y las frecuencias

# POLIGONO DE FRECUENCIAS

plot(x = puntoMM, y = FA, col = 2, type = "b", main = "Poligono de frecuencias Departamento Minoristas")

# DIAGRAMA PASTEL

etiqueta <- paste(porcentaje, "%", sep = " ")
etiqueta

# Despues de eso, creamos el vecto de colores, el cual, le ponemos la cantidad
# de colores que representan cada intervalo
colores <- c("Blue", "Green", "Red", "Purple","Yellow","Black")
colores

# Una vez hecho todo esto, podemos dar pie a crear nuestro diagrama pastel,
# utilizando la funcion pie

pie(porcentaje,
    labels = etiqueta,
    clockwise = TRUE,
    col = colores,
    main = "Diagrama Pastel Departamento Minoristas")


