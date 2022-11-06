#Consigna 1: df_antropometria.
df_antropo = read.table('antropometria.csv', sep=',', header=T)#Leo table con separador "," y con nombres de columna (headers).
print(str(df_antropo))#La estructura me muestra columnas numeric (height, weight, age) y character (sex).

#Chequeos NA.
ubicaciones_na = which(is.na(df_antropo), arr.ind=T)#Cuando uso "arr.ind", which() me devuelve dos columnas: en la primera, la fila donde se hallo el NA; en la segunda, la columna donde se hallo el NA.
print(df_antropo[ubicaciones_na[,1],])#Busco las filas donde encontre NAs, y muestro esas entradas en el dataframe.
table(ubicaciones_na[,2])#Si me fijo las columnas donde existieron NAs, veo que solo fueron la primera y segunda. O sea, height y weight.

df_antropo = df_antropo[-ubicaciones_na[,1],]#Elimino todas las filas con NA.
write.table(df_antropo, file='antropometria_filtrado.csv', sep=',', col.names=T, quote=F)#Exporto.

#Consigna 2: Mindfulness.
#a) Importacion.
#library(openxlsx)#Si usan excel usen esto.
#df_oxmind = read.xlsx('oxford_mindfulness.xlsx')
df_oxmind = read.table('oxford_mindfulness.csv', sep=',', header=T)

#b) Limpieza y reconocimiento.
#Chequeo NAs.
ubicaciones_na = which(is.na(df_oxmind), arr.ind=T)#Existen NAs en columnas 20 a 24.
df_oxmind = df_oxmind[-ubicaciones_na[,1],]#Elimino

#Chequeo duplicadas.
print(which(duplicated(df_oxmind)))#No encuentro filas duplicadas.

#Chequeo anomalias.
head(df_oxmind[,1:7])#La columna 1-7 muestra datos base del sujeto. La edad es un numeric, asi como el numero de referencia, y el resto character.
hist(df_oxmind$Age, xlab='Edades', ylab='Frecuencia', main=NA)#Histograma de edad. No veo nada raro.
print(min(df_oxmind$Age))#La edad mas chica es 18, tiene sentido.
print(table(df_oxmind$Gender))#Las respuestas a genero son validas.
print(table(df_oxmind$Citizen))#Idem ciudadania.
print(table(df_oxmind$Ethnicity))#Idem etnicidad.
print(table(df_oxmind$English))#Idem idioma.
print(table(df_oxmind$Degree))#Idem nivel educativo.
#En resumen tenemos un dataset con mas genero femenino que masculino, y poca representatividad de demas generos. Las ciudadanias estan balanceadas, pero la etnicidad es predominantemente blanca. La mayoria habla ingles. La mayoria es de carrera de grado.

#Otras var categoricas.
print(table(df_oxmind$Brexit_Identity))#Les preguntaron tambien por brexit, solo se ven las dos respuestas no anomalas, sesgado hacia "remain".
print(table(df_oxmind$Meditation))#Respuestas "si" o "no" a si practican meditacion. Ambas categorias representadas, la mayoria en "si".
print(table(df_oxmind$Mindfulness))#Idem respuestas validas en mindfulness, pero casi nadie lo practica.
print(table(df_oxmind$Treatment))#Se les hizo el tratamiento o no? Variable binaria.

#Var numericas.
head(df_oxmind[,8:12])#Las cinco variables del test "big five", representan la personalidad de la persona.
print(summary(df_oxmind[,8:12]))#Muestran rangos parecidos, incluidos sus max/min. Nada anomalo.

hist(df_oxmind$Identity_Strength, breaks=20)#Identity strength tambien tiene un rango razonable.
hist(df_oxmind$Motivation)#Motivacion alta, al parecer. Dichosos.

summary(df_oxmind[,19:24])#Variables de depresion o ansiedad. Tambien rangos razonables. Sin anomalias detectadas.

#c) Cambios en depresion/ansiedad.
df_oxmind$Depression_T1T3 = df_oxmind$Depression_T3 - df_oxmind$Depression_T1#Una resta simple.
hist(df_oxmind$Depression_T1T3, xlab='Cambio en depresion', ylab='Frecuencia', main=NA, breaks=20)#Chusmeo la distribucion.

#d) Exportacion.
#write.xlsx(df_oxmind, file='oxford_mindfulness_procesado.xlsx')#Si usan excel usen esto.
write.table(df_oxmind, file='oxford_mindfulness_procesado.csv', sep=',', col.names=T, quote=F)
