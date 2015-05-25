##-------------------------------------------------##
##-----------      Modelos Linelaes     -----------##
##-----------        Trabajo 01         -----------##
##-- Nombre:Jhonatan M Castañeda Tinoco


# 2.1 Leer el archivo de datos data.txt, y analizar de que estructura de datos se trata.
# Utilice la función read.table()

list.files()
data<-read.table("data.txt", header = TRUE, dec=",", sep="\t")

#str(data)
#'data.frame':  19059 obs. of  18 variables:#

typeof(data)
#list



# 2.2 Calcular el mínimo, la media, el máximo de la variable Edad.
# Utilice las funciones min(), mean(), max(), de ser necesario utilice 
# el parámetro na.rm = TRUE

#summary(data)

min(data[[1]],na.rm = TRUE)
# 14

mean(data[[1]],na.rm = TRUE)
#41,73808

max(data[[1]],na.rm = TRUE)
#112

#se debe de poner na.rm = TRUE ya que sin esto R cometeria un error por tomar casillas vasias


# 2.3 Para la variable Genero, contar cuantos sujetos son de Genero: Femenino.
# Utilice la función table()

table(data[[3]])


#gen<-factor(data[[3]])
#summary(gen)


# 2.4 Encontrar la Edad mínima, media, máxima de los sujetos que Si son dependientes.
z<-c()
y<-factor(x=data[[4]])
y<-as.integer(y)
for (i in 1:length(y)){
  if (y[i]>1) {
    if (y[i]<3){
  z<-c(z,data[[1]][i])
}
}
}

min(z,na.rm = TRUE)
# 22

mean(z,na.rm = TRUE)
#41,28607

max(z,na.rm = TRUE)
#66




# 2.5 Identificar el tipo de elementos que contiene cada variable.
# Utilice la función typeof()
z<-c()
for (i in 1: length(data)){
  z<-c(z,typeof(data[i]))
  
}
  print (z)     

# [1] "list" "list" "list" "list" "list" "list" "list"
#[8] "list" "list" "list" "list" "list" "list" "list"
#[15] "list" "list" "list" "list"



# 2.6 Identificar la clase de cada variable (columna).
# Utilice la función class()

z<-c()
for (i in 1: length(data)){
  z<-c(z,class(data[i]))
  
}
print (z)   


# [1] "data.frame" "data.frame" "data.frame" "data.frame"
#[5] "data.frame" "data.frame" "data.frame" "data.frame"
#[9] "data.frame" "data.frame" "data.frame" "data.frame"
#[13] "data.frame" "data.frame" "data.frame" "data.frame"
#[17] "data.frame" "data.frame"


# 2.7 Calcular la media de todas las variables numéricas (double, integer).
# Recordar que para un factor no es posible obtener la media debido a que 
# éstos representan variables

for (i in 1:length(data)){
  z<-c()
  if (is.integer(data[[i]])){
    z<-c(z,min(data[[i]],na.rm = TRUE))
    
    
    z<-c(z,mean(data[[i]],na.rm = TRUE))
    
    
    
    z<-c(z,max(data[[i]],na.rm = TRUE))
    
    
    print(i)
    print(z)
  }
  
  if (is.double(data[[i]])){
    z<-c(z,min(data[[i]],na.rm = TRUE))
    
    
    z<-c(z,mean(data[[i]],na.rm = TRUE))
    
    
    
    z<-c(z,max(data[[i]],na.rm = TRUE))
    
    
    print(i)
    print(z)
  }
  
    
}





# 2.8 Calcular el porcentaje de valores perdidos que contiene cada variable.
# Utilice la función is.na()


for (i in 1:length(data)){
  z<-c()
  for (j in 1:length(data[[i]])){
    z<-c(z,is.na(data[[i]][j]))
         as.numeric(z)
         w<-mean(z)
           
}
print (i)
print(w)
}





# 3. Selecionando sujetos mediante un determinado criterio:


# 3.1 Seleccione los sujetos con una Edad mayor a 40 años.
# Utilice la función subset()

y <- subset(data,subset = data[[1]]>40)


# 3.2 Seleccione los sujetos que tienen Vivienda Propia.

y <- subset(data,subset = data[[2]]=="Propia")


# 3.3 Seleccione los sujetos que tienen más ($>$) de dos cargas familiatres.

y <- subset(data,subset = data[[6]]>2)

# 3.4 Seleccione los sujetos con una Deuda superior o igual a 500 dólares
# y más ($>$) de 8 Dias_Atraso.


y <- subset(data,subset = data[[9]]>500)

w <- subset(y,subset = y[[10]]>8)




# 3.5 Seleccione los sujetos con un Score mayor o igual a 900 puntos, una Edad menor
# o igual a 35 años y con más ($>$) de 3 tarjetas de crédito (Numero_TC).

y <- subset(data,subset = data[[13]]>=900)

w <- subset(y,subset = y[[1]]<=35)

z <- subset(w,subset = w[[14]]>3)


# 4. Gráficos:
# 4.1 Realice un histograma de la variable Edad, utilice como color de relleno: red

library(ggplot2)
edad <- data[,1]
hist(edad,col = "red")


# 4.2 Realice un diagrama de cajas de la variable Edad, utilice como color de relleno: green
# Utilice la función boxplot()

library(ggplot2)
boxplot(edad,colour="green")
g <- ggplot(data = data, aes(x=data[[]], y=Ingreso, fill=Region))
g + geom_boxplot() 