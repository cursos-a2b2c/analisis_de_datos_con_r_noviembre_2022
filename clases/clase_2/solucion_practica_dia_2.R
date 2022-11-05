#################################################################
#              Curso de análisis de datos con R
#               Práctica Limpieza de Datos
#################################################################
#1) Importar el dataset “antropometria” de alturas, pesos, edades y sexos de una muestra poblacional. 
antropometria <- read.csv("c://Users/Andy/Downloads/antropometria.csv") 

#¿Qué tipo de datos contiene el dataframe?
head(antropometria)
tail(antropometria)
View(antropometria)
str(antropometria)

#¿Existen elementos con NA?
any(is.na(antropometria))

#De yapa, usemos la función table para contar las categorias de sex
table(antropometria$sex)

#Vemos que hay algunos elementos numéricos en la columna sex que son datos mal cargados, habría que decidir qué hacer con eso...
#De recontra yapa porque es bastante avanzado, podríamos poner todos NA en lugar de números
#Para eso podemos usar la función %in% que nos permite preguntar si cada elemento de la columna sex pertenece a algún elemento de un vector
#por ejemplo, del vector c("F", "M"). Si no está ahí, lo ponemos en NA
antropometria$sex[!antropometria$sex %in% c("F","M")] <- NA #Si entienden esta linea, están entendiendo perfecto R!
table(antropometria$sex, useNA = "ifany") #Vemos que ahí ya se pusieron en NA los números. 

#Generar una nueva tabla removiendo todas las filas con algún NA y exportarla a un nuevo csv llamado antropometria_filtrado.csv
antropometria_filtrado <- antropometria[complete.cases(antropometria), ]
any(is.na(antropometria_filtrado))
write.csv(antropometria_filtrado, file = "c://Users/Andy/Downloads/antropometria_filtrado.csv", row.names = F, quote = T)


#2) Durante la pandemia se realizó un estudio sobre los posibles beneficios que podrían tener las técnicas de mindfulness para disminuir los niveles de depresión y ansiedad en estudiantes de la universidad de Oxford. Con este objetivo se dieron los niveles de depresión y ansiedad en tres tiempos: antes del comienzo del comienzo del curso de mindfulness (T1), inmediatamente luego de terminarlo (T2) y un mes más tarde (T3)
#Importe la tabla oxford_mindfulness.xlsx.

#Para leer excel, instalamos el paquete readxl si no lo tenemos, R por default no tiene la capacidad de leer excel, solo csv o txt
#install.packages("readxl") #Descomentar esta linea para instalar
library(readxl)
oxford_mindfulness <- read_excel("c://Users/Andy/Downloads/oxford_mindfulness.xlsx")

#La función read_excel devuelve un tipo de data.frame más moderno que se llama tibble. Lo transformamos a data.frame que ya lo conocemos
oxford_mindfulness <- data.frame(oxford_mindfulness)

#Haga una limpieza de los datos: identifique datos faltantes, filas duplicadas, datos anómalos. Elimine las filas que presenten estos problemas.
str(oxford_mindfulness)
View(oxford_mindfulness) #Revisamos un poco a mano

#Podemos usar la función duplicated para ver si hay filas duplicadas
any(duplicated(oxford_mindfulness))

#Hacemos un resumen de cada columna, ahí mismo vamos a poder ver si hay columnas con datos faltantes
summary(oxford_mindfulness)

#Para ver si hay datos raros en las columnas categóricas, podemos hacer table en cada una de esas columnas
table(oxford_mindfulness$Gender)
table(oxford_mindfulness$Citizen)
table(oxford_mindfulness$Degree)
#Y así con todas...


#Hay algunas columnas con NA's, como Depression_T2, Depression_T3, Anxious_T2 y Anxious_T3
#Sacamos las columnas con datos faltantes
oxford_mindfulness <- oxford_mindfulness[complete.cases(oxford_mindfulness), ]
summary(oxford_mindfulness)


#Agregue a la tabla la información del cambio en los niveles de depresión y ansiedad entre el inicio y el final del estudio. 
#Ejemplo: Depression_T3T1 = Depression_T3 - Depression_T1

#Crear una columna nueva es tan fácil como llamarla y asignarle valores
oxford_mindfulness$Depression_T3T1 <- oxford_mindfulness$Depression_T3-oxford_mindfulness$Depression_T1

#Exporte el dataset en formato xlsx.
#install.packages("writexl")
library(writexl) #Descomentar esta linea para instalar
write_xlsx(oxford_mindfulness, path = "c://Users/Andy/Downloads/oxford_mindfulness_modificado.xlsx")

#Clase Bonus: Estructuras de control

#Imprimir en pantalla los números del 1 al 100 utilizando un for.
for(i in 1:100){ 
  print(i)  
}

#Imprimir en pantalla los números del 1 al 100 sin utilizar un for. Ayuda: : o ?seq
numeros <- seq(from = 1, to = 100, by = 1)
print(numeros)
#Otra forma
numeros <- 1:100
numeros

#Elegir un número al azar entre 0 y 1 con la función runif(). Utilizando if else imprimir “mayor que 0.5” o “menor que 0.5” dependiendo el caso.
?runif #runif genera n numeros al azar entre 0 y 1
numero_al_azar <- runif(n = 1)
if(numero_al_azar > 0.5){
  print("Mayor a 0.5")
}else{
  print("Menor o igual a 0.5")
}

#Si quisieramos probar tres condiciones, por ejemplo, menor, mayor o igual
if(numero_al_azar > 0.5){
  print("Mayor a 0.5")
}else if(numero_al_azar < 0.5){
  print("Menor a 0.5")
}else{
  print("Igual a 0.5")
}

#Para valientes: Generar una matriz de 5x10. Colocar dentro de cada posición la suma de la fila y de la columna de esa posición. Utilizá un for dentro de otro for.
#Definimos una matriz de ceros
m <- matrix(0, ncol = 10, nrow = 5)
for(fila in 1:5){ #Nos movemos primero en la fila y dentro de cada fila, una por una cada columna
  for(columna in 1:10){
    m[fila, columna] <- fila + columna #Subseteamos la matriz en la fila y columna dada y le asignamos la suma
  }
}
print(m) #Veamos si funcionò
