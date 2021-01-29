#POSTWORK SESION 1

#1.-Importa los datos de soccer de la temporada 2019/2020 de la primera división de la liga española a R, los datos los puedes encontrar en el siguiente enlace: https://www.football-data.co.uk/spainm.php

soccer <- read.csv("https://www.football-data.co.uk/mmz4281/1920/SP1.csv")

#2.-Del data frame que resulta de importar los datos a R, extrae las columnas que contienen los números de goles anotados por los equipos que jugaron en casa (FTHG) y los goles anotados por los equipos que jugaron como visitante (FTAG)

Goles_casa<- soccer$FTHG
Goles_visitante<- soccer$FTAG

#3.- Consulta cómo funciona la función table en R al ejecutar en la consola ?table 

#Primero realicé las tablas de los goles de casa y coles de visitante
Tabla_1<- table(Goles_casa) 
Tabla_1
Tabla_2<- table(Goles_visitante)
Tabla_2
#Usando las tablas anteriores elaboré las tablas de frecuencia relativas en base a los goles
Tabla_3<- prop.table(x=Tabla_1)
Tabla_3
Tabla_4<- prop.table(x=Tabla_2)
Tabla_4



#Por ultimo, elaboré una tabla con ambos vectores, goles en casa y goles en visitantes; y posteriormente realicé una tabla de frecuencia relativa conjunta
Tabla_5<- table(Goles_casa,Goles_visitante)
Tabla_6<- prop.table(x=Tabla_5)
Tabla_6
#POSTWORK SESION 2

#Desarrollo

#Ahora vamos a generar un cúmulo de datos mayor al que se tenía, esta es una situación habitual que 
#se puede presentar para complementar un análisis, siempre es importante estar revisando las 
#características o tipos de datos que tenemos, por si es necesario realizar alguna transformación en 
#las variables y poder hacer operaciones aritméticas si es el caso, además de sólo tener presente 
#algunas de las variables, no siempre se requiere el uso de todas para ciertos procesamientos.

#Importa los datos de soccer de las temporadas 2017/2018, 2018/2019 y 2019/2020 de la primera división
#de la liga española a R, los datos los puedes encontrar en el siguiente enlace: 
# https://www.football-data.co.uk/spainm.php

SP1<- read.csv("https://www.football-data.co.uk/mmz4281/1718/SP1.csv")
SP2<- read.csv("https://www.football-data.co.uk/mmz4281/1819/SP1.csv")
SP3<- read.csv("https://www.football-data.co.uk/mmz4281/1920/SP1.csv")

#Obten una mejor idea de las características de los data frames al usar las funciones: str, head, 
#View y summary

str(SP1)
head(SP1)
View(SP1)
summary(SP1)

str(SP2)
head(SP2)
View(SP2)
summary(SP2)

str(SP3)
head(SP3)
View(SP3)
summary(SP3)

#Con la función select del paquete dplyr selecciona únicamente las columnas Date, HomeTeam, AwayTeam,
#FTHG, FTAG y FTR; esto para cada uno de los data frames. (Hint: también puedes usar lapply).
library(dplyr)

SP1<- mutate(SP1, Date = as.Date(Date, "%d/%m/%y"))
SP2<- mutate(SP2, Date = as.Date(Date, "%d/%m/%Y"))
SP3<- mutate(SP3, Date = as.Date(Date, "%d/%m/%Y"))

SP4<-list(SP1,SP2,SP3)
SPf<- lapply(SP4, select,Date, HomeTeam, AwayTeam,FTHG, FTAG, FTR)
str(SPf)

#Asegúrate de que los elementos de las columnas correspondientes de los nuevos data frames sean del 
#mismo tipo (Hint 1: usa as.Date y mutate para arreglar las fechas). Con ayuda de la función rbind 
#forma un único data frame que contenga las seis columnas mencionadas en el punto 3 
#(Hint 2: la función do.call podría ser utilizada).

LigaEspañola<- do.call(rbind,SPf)
head(data_frame)
tail(data_frame)
str(data_frame)

write.csv(LigaEspañola, "LigaEspañola.csv", row.names = FALSE)

#POSTWORK SESION 3

#Desarrollo
#Ahora graficaremos probabilidades (estimadas) marginales y conjuntas para el número de goles que 
#anotan en un partido el equipo de casa o el equipo visitante.

#Con el último data frame obtenido en el postwork de la sesión 2, elabora tablas de frecuencias 
#relativas para estimar las siguientes probabilidades

LigaEspañola<- read.csv("LigaEspañola.csv")
str(LigaEspañola)
head(LigaEspañola)
View(LigaEspañola)
#La probabilidad (marginal) de que el equipo que juega en casa anote x goles (x=0,1,2,)

FTHG<- table(LigaEspañola$FTHG)
(FTHG1<- prop.table(FTHG))
View(FTHG1)

dfH<-as.data.frame(FTHG1) #Creé los data frames para poder usar ggplot, ya que no puedo usarlo con una tabla

#La probabilidad (marginal) de que el equipo que juega como visitante anote y goles (y=0,1,2,)

FTAG<- table(LigaEspañola$FTAG)
FTAG1<- prop.table(FTAG)

dfA<- as.data.frame(FTAG1)

#La probabilidad (conjunta) de que el equipo que juega en casa anote x goles y el equipo que juega como 
#visitante anote y goles (x=0,1,2,, y=0,1,2,)

FTTG<-table(LigaEspañola$FTHG,LigaEspañola$FTAG)
FTTG1<- prop.table(FTTG)

dfT<- as.data.frame(FTTG1)

#Realiza lo siguiente:

#Un gráfico de barras para las probabilidades marginales estimadas del número de goles que anota el 
#equipo de casa.

barplot(FTHG1, main = "Probabilidad marginal FTHG", 
        xlab = "Numero de goles", 
        ylab = "Frecuencia relativa", 
        col = "Yellow")

ggplot(dfH, aes(x = Var1, y = Freq)) + 
  geom_bar(stat = "identity", col = "black", fill = "Yellow" ) +
  ggtitle("Probabilidad marginal FTHG") +
  xlab("Numero de goles") +
  ylab("Frecuencia relativa") +
  theme_grey()

#Un gráfico de barras para las probabilidades marginales estimadas del número de goles que anota el 
#equipo visitante.

barplot(FTAG1, main = "Probabilidad marginal FTAG",
        xlab = "Numero de goles", 
        ylab = "Frecuencia relativa",
        col = "Orange")

ggplot(dfA, aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity", col = "Black", fill = "Orange") + 
  ggtitle("Probabilidad marginal FTAG") +
  labs(x = "Numero de goles",
       y = "Frecuencia relativa") +
  theme_grey()

#Un HeatMap para las probabilidades conjuntas estimadas de los números de goles que anotan el equipo de
#casa y el equipo visitante en un partido.

library(scales)

ggplot(dfT, aes(x = Var1, y = Var2, fill = Freq)) +
  ggtitle("Probabilidad conjunta FTTG") +
  labs(x = "Goles casa",
       y = "Goles visitante") +
  geom_tile() +
  scale_fill_gradient2(low = "Black", high = "Blue", mid = "Yellow") +
  theme_dark()

#POSTWORK SESION 4 

#Desarrollo
#Ahora investigarás la dependencia o independencia del número de goles anotados por el equipo de casa
#y el número de goles anotados por el equipo visitante mediante un procedimiento denominado bootstrap,
#revisa bibliografía en internet para que tengas nociones de este desarrollo.

#Ya hemos estimado las probabilidades conjuntas de que el equipo de casa anote X=x goles 
#(x=0,1,... ,8), y el equipo visitante anote Y=y goles (y=0,1,... ,6), en un partido. Obtén una tabla 
#de cocientes al dividir estas probabilidades conjuntas por el producto de las probabilidades 
#marginales correspondientes.

#Llamamos y renombramos a nuestras 3 variables.
visitante <- FTAG1
local <- FTHG1
conjunta <- FTTG1

cocientes <- apply(conjunta, 2, function(col) col/local)
cocientes <- apply(cocientes, 1, function(fila) fila/visitante)
cocientes <- t(cocientes)

#Mediante un procedimiento de boostrap, obtén más cocientes similares a los obtenidos en la tabla del
#punto anterior. Esto para tener una idea de las distribuciones de la cual vienen los cocientes en la
#tabla anterior. Menciona en cuáles casos le parece razonable suponer que los cocientes de la tabla en
#el punto 1, son iguales a 1 (en tal caso tendríamos independencia de las variables aleatorias X y Y).

medias <- c()
for(i in 1:10000){
  set.seed(2*i)
  medias[i] = mean(sample(cocientes, replace = TRUE))
  
}

head(medias)

library(ggplot2)

ggplot() + 
  geom_histogram(aes(medias), bins = 50) + 
  geom_vline(aes(xintercept = mean(medias))) +
  ggtitle("Histograma de distribución de medias muestrales")

#Segun el Teorema Central del Limite, la media deberia ser igual a 1 para suponer que las variables
#son independientes, y visualmente se oberva en el histograma que la media es inferior a 1. Por lo 
#cual las variables X y Y no son independientes.

#POSTWORK SESION 5

#A partir del conjunto de datos de soccer de la liga española de las temporadas 2017/2018, 2018/2019 
#y 2019/2020, crea el data frame SmallData, que contenga las columnas date, home.team, home.score, 
#away.team y away.score; esto lo puedes hacer con ayuda de la función select del paquete dplyr. Luego
#crea un directorio de trabajo y con ayuda de la función write.csv guarde el data frame como un archivo
#csv con nombre soccer.csv. Puedes colocar como argumento row.names = FALSE en write.csv.

library(dplyr)

SP1<- read.csv("https://www.football-data.co.uk/mmz4281/1718/SP1.csv")
SP2<- read.csv("https://www.football-data.co.uk/mmz4281/1819/SP1.csv")
SP3<- read.csv("https://www.football-data.co.uk/mmz4281/1920/SP1.csv")

SP1<- mutate(SP1, Date = as.Date(Date, "%d/%m/%y"))
SP2<- mutate(SP2, Date = as.Date(Date, "%d/%m/%Y"))
SP3<- mutate(SP3, Date = as.Date(Date, "%d/%m/%Y"))

lista<-list(SP1,SP2,SP3)
lista<- lapply(lista, select,Date, HomeTeam,FTHG,AwayTeam,FTAG,)
str(lista)

Data<- do.call(rbind,lista)
SmallData <- select(Data, date = Date, home.team = HomeTeam, 
               home.score = FTHG, away.team = AwayTeam, 
               away.score = FTAG)


head(SmallData)
tail(SmallData)
class(SmallData)

write.csv(SmallData, file= "soccer.csv", row.names = FALSE)

#Con la función create.fbRanks.dataframes del paquete fbRanks importa el archivo soccer.csv a R y al 
#mismo tiempo asignarlo a una variable llamada listasoccer. Se creará una lista con los elementos scores
#y teams que son data frames listos para la función rank.teams. Asigna estos data frames a variables 
#llamadas anotaciones y equipos.

install.packages("fbRanks")
library(fbRanks)

listasoccer <- create.fbRanks.dataframes(scores.file = "soccer.csv")
anotaciones <- listasoccer$scores
equipos <- listasoccer$teams


#Con ayuda de la función unique crea un vector de fechas (fecha) que no se repitan y que correspondan a
#las fechas en las que se jugaron partidos. Crea una variable llamada n que contenga el número de fechas
#diferentes. Posteriormente, con la función rank.teams y usando como argumentos los data frames 
#anotaciones y equipos, crea un ranking de equipos usando únicamente datos desde la fecha inicial y 
#hasta la penúltima fecha en la que se jugaron partidos, estas fechas las deberá especificar en max.date
#y min.date. Guarda los resultados con el nombre ranking.

fecha <- unique(anotaciones$date)
n <- length(fecha)
ranking <- rank.teams(scores = anotaciones, teams = equipos,
                      max.date = fecha[n-1],
                      min.date = fecha[1])

#Finalmente estima las probabilidades de los eventos, el equipo de casa gana, el equipo visitante gana
#o el resultado es un empate para los partidos que se jugaron en la última fecha del vector de fechas
#fecha. Esto lo puedes hacer con ayuda de la función predict y usando como argumentos ranking y fecha[n]
#que deberá especificar en date.

pred <- predict(ranking, date = fecha[n])

#POSTWORK SESION 6

#Desarrollo
#Importe el conjunto de datos match.data.csv a R y realice lo siguiente:

library(dplyr)
data <- read.csv("match.data.csv")
str(data)

#Agrega una nueva columna sumagoles que contenga la suma de goles por partido.

data1 <- data %>% 
  mutate(date = as.Date(date, "%Y-%m-%d"),
         sumagoles = home.score + away.score) 

#Obtén el promedio por mes de la suma de goles.

data1<- data1%>%
  mutate(Mes = format(date, "%Y-%m")) %>%
  group_by(Mes) %>%
  summarise(promgoles = mean(sumagoles))

class(data1)

data1 <- as.data.frame(data1)


#Crea la serie de tiempo del promedio por mes de la suma de goles hasta diciembre de 2019.

data2 <- data1[1:96,]

GolesPromedio <- ts(data2$promgoles, start = 1,
                 frequency = 10)

#Grafica la serie de tiempo.

ts.plot(GolesPromedio)

#POSTWORK SESION 7

#Desarrollo
#Utilizando el manejador de BDD Mongodb Compass (previamente instalado), deberás de realizar las 
#siguientes acciones:

install.packages("mongolite")
library(mongolite)

install.packages("data.table")
library(data.table)
  
#Alojar el fichero data.csv en una base de datos llamada match_games, nombrando al collection como 
#match

match=data.table::fread("https://raw.githubusercontent.com/beduExpert/Programacion-con-R-Santander/master/Sesion-07/Postwork/data.csv")
names(match)

my_collection = mongo(collection = "match", db = "match_games") 
my_collection$insert(match)  

#Una vez hecho esto, realizar un count para conocer el número de registros que se tiene en la base

#Realiza una consulta utilizando la sintaxis de Mongodb, en la base de datos para conocer el número de 
#goles que metió el Real Madrid el 20 de diciembre de 2015 y contra qué equipo jugó, ¿perdió ó fue 
#goleada?

#Agrega el dataset de mtcars a la misma BDD

#Por último, no olvides cerrar la conexión con la BDD.


