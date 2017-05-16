#Random Forests: Deserción estudiantil
#Grupo: David Ñañez, Gustavo Sanchez, Zuleny Gomez


#-------------------------------------Abrir y reestructurar la base de datos ------------------------------------#


#Abrimos la base de datos

desercion <- read.csv("DEFINITIVA.csv", stringsAsFactors=FALSE, sep=";")

#Para insepccionar la base de datos:

str(desercion)

#La base de datos esta compuesta por 1904 observaciones y 15 variables, como observaciones se encuentran 1904 estudiantes de la Universidad del Rosario
#que ingresaron ya se en el periodo I del año 2012 o el periodo I para cursar alguna de las carreras que ofertaba la univerisdad.
#como variables se tiene genero(variable categorica), v10 que me dice la facultad a la que pertenecen, un indice de creditos aprobados/creditos vistos, 
#tmabien nos dice si la persona ha realizaod dos carreras al mismo tiempo, el semestre en el que se encuentran (si aun no han desertado) y tambien se cuenta con el promedio obtendio por semestre
#en el transcurso de la carrera

#Parte de la reestructuracion de la base de datos es el poder dejar el outcome de interes como un factor, pues cuando importé la base de datos estableci la condicion de que no cambiara las variables string a factor , entonces como mi variable de interes es
#desercion necesito convertirla a factor ya que de lo contrario no se podria implementar el algoritmo. 

desercion$deserto <- factor(desercion$deserto, labels= c("No", "Si"))

#Asi mismo se decide convertir como variable factor si la persona hizo doble programa, el genero dado que son variables categoricas

desercion$doble2 <- factor(desercion$doble2, labels = c("No", "Si"))
desercion$genero <- factor(desercion$genero, labels = c("Femenino", "Masuculino"))



#-------------------------------Examinar la base de datos ---------------------------------------------------#

#Puedo examinar mi base de datos, comenzando por ejemplo por nuestro outcome de interes

prop.table(table(desercion$deserto))

#con este comando realizo una tabla de frecuencias que me muestra la proporcion de estudiantes dentro de esa muestra que desertaron, en este caso
#el 54.25% de los estudiantes no desertaron mientras que el 45.74% si lo hicieron. Podemos notar que la tasa de deserción de los estudiantes es bastante alta


#Asi mismo tambien podemos saber cual es la proporcion de estudiantes que se animan a realizar doble programa

table(desercion$doble2)

#De los 1904 estudiantes, solo 113 deciden hacer doble programa, mientras que 1791 optan por no hacerlo. 


#Para tener informacion mas detallada de la base de datos puedo hacer estadistica descriptiva de la variable promedio semestre

summary(desercion$promedio_semestre1)

#como es una variable numerica, puedo obtener estadistica descriptiva, ésta me dice que en el semsestre 1, el promedio minimo fue de 0.68
#y el valor maximo fue de 5.0, el promedio de calificaciones de este grupo de estudiantes que cursan diferentes carreras fue de 3.82





#---------------------------------------Gráficas-----------------------------------------------#

#Podemos obtener un histograma que me permita visualizar los promedios alcanzados por los estudiantes por cada semestre 

#histograma
ggplot(desercion, aes(x = promedio_semestre1 ))+geom_histogram()
ggplot(desercion, aes(x = promedio_semestre2 ))+geom_histogram()       
ggplot(desercion, aes(x = promedio_semestre3 ))+geom_histogram()       
ggplot(desercion, aes(x = promedio_semestre4 ))+geom_histogram()
ggplot(desercion, aes(x = promedio_semestre5 ))+geom_histogram()
ggplot(desercion, aes(x = promedio_semestre6 ))+geom_histogram()
ggplot(desercion, aes(x = promedio_semestre7 ))+geom_histogram()
ggplot(desercion, aes(x = promedio_semestre8 ))+geom_histogram()

#Asi mismo, podemos obtener un histograma del indice de creditos aprobados/creditos vistos

ggplot(desercion, aes(x = indice_aprobados ))+geom_histogram()

#serie de tiempo
ggplot(subset(desercion, State %in% c("", "")), aes(x=, y=, color=State))+geom_point()

#barras
ggplot(desercion, aes(factor(deserto)))+geom_bar()


#---------------------------------------Implementación Algoritmo Randome Forests-----------------------------#


#Genero semilla
set.seed(123)
#
