#Practica 5##
#Ejercicio 1
numArtefactos<- c(17, 54, 10, 34, 90, 33, 49, 82, 12, 23, 56, 78, 44, 102, 10, 53, 4, 28, 37, 95)
numArtefactos
typeof(numArtefactos)
#Emplea datos doble

#¿Cómo almacena los valores numéricos: integer o double? Transforma el tipo de dato a número entero llamando al objeto ‘numArtefactos_int’. 
numArtefactos_int<-as.integer(numArtefactos)
is.integer(numArtefactos_int)
#Se transforma a integer y se pregunta si lo es, da resultado TRUE

#2. Calcula la media del objeto ‘numArtefactos_int’.
mean(numArtefactos_int)# La media es 45.55

#3. 3. Calcula la mediana del objeto ‘numArtefactos_int’. Define brevemente lamediana: concepto y cálculo
median(numArtefactos_int)#El resultado es 40.55
#La mediana es el valor que ocupa el lugar central de todos los datos cuando éstos están ordenados de menor a mayor.

#4 Calcula la moda del objeto ‘numArtefactos_int’. Explica detalladamente el procedimiento para su cálculo: empleo de funciones, operadores etc.
moda <- function(numArtefactos_int) {
  u<-unique(numArtefactos_int)
  tab<-tabulate(match(numArtefactos_int,u))
  u[tab == max(tab)]
}
moda(numArtefactos_int)#El resultado es 10
#La moda se calcula como el valor más repetido dentro de la variable. Para calcularla en R, se crea una función llamada moda, dentro se nombra un objeto con la función unico, que selecciona los valores unicos, bajo una selección los valores máximos de tab

#5. Calcula el número de veces que se repite el valor correspondiente con la moda.
table(numArtefactos_int)
#El valor de la moda que es 10, sale 2 veces

#6 Calcula los cuartiles del objeto ‘numArtefactos_int’.
quantile(numArtefactos_int)#El resultado es: 0%: 4.0; 25%:21.5; 50%: 40.5;  75%:61.5; 100%: 102.0 

#7. Calcula el rango intercuartílico del objeto ‘numArtefactos_int’. Interpreta elresultado.
IQR(numArtefactos_int)#El resultado es 40. Esto es así porque es la diferencia entre el tercer y el primer cuartil.

#8Calcula el rango del objeto ‘numArtefactos_int’. Almacena el rango en un vector denominado ‘rango_artefactos’.
rango_artefactos<- range(numArtefactos_int)
rango_artefactos

#9Calcula la varianza del objeto ‘numArtefactos_int’. Emplea 2 funciones para su cálculo. 
var(numArtefactos_int)#El resultado es 927.1026

#10Calcula la desviación estándar del objeto ‘numArtefactos_int’. Emplea 2funciones para su cálculo
sd(numArtefactos_int)
sqrt(var(numArtefactos_int))#El resultado de ambas funciones es 30.44836

#11
#La varianza y la desviación estándar indican si los valores se encuentran más o menos próximos a las medidas de posición. La desviación estándar es simplemente la raíz cuadrada positiva de la varianza.

#12Visualiza gráficamente de manera horizontal la dispersión del objeto ‘numArtefactos_int’.
plot(numArtefactos_int)

#13.Crea un vector llamado ‘vector3’ a partir de la siguiente secuencia de valores’21, 45, 33, 98, 34, 90, 67, 87, 45, 11, 73, 38, 28, 15, 50, 57, 12, 87, 29, 1’
vector3 <- c(21, 45, 33, 98, 34, 90, 67, 87, 45, 11, 73, 38, 28, 15, 50, 57, 12, 87, 29, 1)

#14Calcula el coeficiente de variación de los objetos: 1)‘numArtefactos_int’ y 2) ‘vector3’. Emplea 2 funciones para su cálculo. Compara e interpreta los resultados.
install.packages("FinCal")
library(FinCal)

coefficient.variation(sd=sd(numArtefactos_int), avg = mean(numArtefactos_int))*100#66.8%
sd(numArtefactos_int)/mean(numArtefactos_int)*100#66.8%
coefficient.variation(sd=sd(vector3), avg = mean(vector3))*100#63.6%
sd(vector3)/mean(vector3)*100#63.6%

#15Genera una tabla-resumen de los estadísticos descriptivos expuestos: media, mediana, desviación estándar etc.
summary(numArtefactos_int)# Los resultados son:   Min.:4.00; 1st Qu.:21.50;  Median:40.50;  Mean:45.55; 3rd Qu.:61.50;  Max.:102.00 
library(e1071)

#16. Calcula el coeficiente de asimetría del objeto ‘vector3’. Interpreta su resultado.Exponga ejemplos de distribuciones de variables con asimetría positiva y negativa y simétricas. Explique cada uno de estos escenarios. 
library(e1071)#Instalamos este paquete
asimetria <- skewness(vector3, na.rm = TRUE, type = 3)
asimetria#El resultado es 0.3138528. Esto significa que la distribución es asimétrica hacia la derecha

#17.7.Calcula la curtosis del objeto ‘vector3’. ¿Qué tipo de curtosis se encuentra asociada al anterior objeto? Justifica tu respuesta. 
curtosis <- kurtosis(vector3,na.rm= TRUE,type=3)
curtosis#El resultado es -1.237981. Al ser esta, significa que es playkurtic , lo que significa que tiende a producir menos valores atípicos y menos extremos que la distribución normal.



