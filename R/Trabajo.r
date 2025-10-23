#ÁngelaHernandez_Trabajo2.R
# Trabajo final Bioinformática - Curso 25/26
# Análisis de parámetros biomédicos por tratamiento

# 1. Cargar librerías (si necesarias) y datos del archivo "datos_biomed.csv". (0.5 pts)
datos_biomed <- read.csv("datos_biomed.csv", header = TRUE) #Utilizo header=TRUE para informar de que la primera línea son los nombres de las columnas
# 2. Exploración inicial con las funciones head(), summary(), dim() y str(). ¿Cuántas variables hay? ¿Cuántos tratamientos? (0.5 pts)
head(datos_biomed) #primeras línas archivo
summary(datos_biomed)
dim(datos_biomed) #numero de filas y de columnas
str(datos_biomed)
#El numero de variables corresponde al numero de columnas del documento por eso utilizo dim(), aunque no lo pida de esta forma también conocemos el número de pacientes
dim(datos_biomed) #5 variables en este caso
length(unique(datos_biomed$Tratamiento)) #nos muestra el número de tratamientos, en este caso 3 
# 3. Una gráfica que incluya todos los boxplots por tratamiento. (1 pt)
par(mfrow = c(3, 1)) #Sirve para poder juntar los 3 boxplot de las distintas variables analizadas por tratamiento.
boxplot(Glucosa ~ Tratamiento,
        data = datos_biomed,
        col = c("blue", "green","black"),
        main = "Glucosa por tratamiento",
        ylab = "Nivel de Glucosa",
        xlab = "")
boxplot(Presion ~ Tratamiento,
        data = datos_biomed,
        col = c("yellow", "orange","white"),
        main = "Presión por tratamiento",
        ylab = "Presión",
        xlab = "")
boxplot(Colesterol ~ Tratamiento,
        data = datos_biomed,
        col = c("pink", "red","grey"),
        main = "Colesterol por tratamiento",
        ylab = "Colesterol",
        xlab = "Tratamiento")
# 4. Realiza un violin plot (investiga qué es). (1 pt)
#Un violin plot muestra la distribución de datos de una variable numérica para diferentes grupos. Es parecido a un boxplot, pero  incluye una estimación de la densidad.
install.packages("ggplot2") #instalo y cargo librerias de R
library(ggplot2)
ggplot(datos_biomed, aes(x = Tratamiento, y = Glucosa, fill = Tratamiento)) + 
  geom_violin(trim = FALSE) + #forma del violín (trim=FALSE muestra todo el rango)
  geom_boxplot(width = 0.1, fill = "white") + #añade un boxplot dentro para ver la mediana y cuartiles
  labs(title = "Distribución de Glucosa por Tratamiento",
       x = "Tratamiento",
       y = "Nivel de Glucosa")
# 5. Realiza un gráfico de dispersión "Glucosa vs Presión". Emplea legend() para incluir una leyenda en la parte inferior derecha. (1 pt)
datos_biomed$Tratamiento <- as.factor(datos_biomed$Tratamiento) #Convierto tratamiento en un columna numerica porque es la única forma de ejecutar el gráfico
plot(datos_biomed$Glucosa, datos_biomed$Presion,
     col = colores <- c("Placebo" = "blue", "FarmacoA" = "orange", "FarmacoB" = "green"), pch = 19, #pch nos muestra los puntos sólidos en el gráfico
     main = "Relación entre Glucosa y Presión", xlab = "Glucosa", ylab = "Presión")
# Leyenda
legend("bottomright", legend = levels(datos_biomed$Tratamiento),
       col = c("blue", "orange"),
       pch = 19)
# 6. Realiza un facet Grid (investiga qué es): Colesterol vs Presión por tratamiento. (1 pt)
#El face grid permite dividir el gráfico en paneles según el tratamiento. En este caso, muestra cómo se relacionan los valores de colesterol y presión para cada grupo de tratamiento, permitiendo hacer una comparación más visual.
datos_biomed$Tratamiento <- as.factor(datos_biomed$Tratamiento) #Convierto tratamiento en un columna numerica porque es la única forma de ejecutar el gráfico
library(ggplot2)
ggplot(datos_biomed, aes(x = Presion, y = Colesterol, color = Tratamiento)) + #Gráfico básico como el del ejercicio anterior pero con los datos que aplicamos en este gráfico
  geom_point(size = 2) +  #Añadimos los puntos de dispersión
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +  # divide el gráfico por tratamiento, Añadimos una línea de tendencia lineal,se=FALSE elimina el sombreado de error
  facet_grid(~ Tratamiento) + #divide el gráfico en varios paneles según el tratamiento
    labs(title = "Relación entre Colesterol y Presión por Tratamiento",
       x = "Presión",
       y = "Colesterol")
# 7. Realiza un histogramas para cada variable. (0.5 pts)
hist(datos_biomed$Glucosa, col = "pink", main = "Distribución de Glucosa", xlab = "Glucosa")
hist(datos_biomed$Presion, col = "orange", main = "Distribución de Presión", xlab = "Presión")
hist(datos_biomed$Colesterol, col = "green", main = "Distribución de Colesterol", xlab = "Colesterol")
# 8. Crea un factor a partir del tratamiento. Investifa factor(). (1 pt)
#La función factor() convierte una variable de texto en un factor, es decir, en una variable categórica que R puede reconocer como grupos o niveles.
# Creamos una nueva columna llamada 'Tratamiento_factor' que sea un factor del tratamiento original
datos_biomed$Tratamiento_factor <- factor(datos_biomed$Tratamiento)
# 9. Obtén la media y desviación estándar de los niveles de glucosa por tratamiento. Emplea aggregate() o apply(). (0.5 pts)
media_glucosa <- aggregate(Glucosa ~ Tratamiento, data = datos_biomed, FUN = mean) #media
desviación_glucosa <- aggregate(Glucosa ~ Tratamiento, data = datos_biomed, FUN = sd) #desviación estandar
media_glucosa
desviación_glucosa
# 10. Extrae los datos para cada tratamiento y almacenalos en una variable. Ejemplo todos los datos de Placebo en una variable llamada placebo. (1 pt)
placebo <- subset(datos_biomed, Tratamiento == "Placebo") #subset sirve para filtar los datos y utilizar solo los marcados, en este caso placebo
farmacoA <- subset(datos_biomed, Tratamiento == "FarmacoA")
farmacoB <- subset(datos_biomed, Tratamiento == "FarmacoB")
# 11. Evalúa si los datos siguen una distribución normal y realiza una comparativa de medias acorde. (1 pt)
#Evaluamos la normalidad con el test de Shapiro-Wilk
# Si p > 0.05 -> los datos son normales.
# Si p < 0.05 -> los datos NO son normales.
# Normalidad para Glucosa
shapiro.test(placebo$Glucosa)
shapiro.test(farmacoA$Glucosa)
shapiro.test(farmacoB$Glucosa)
# Normalidad para Presión
shapiro.test(placebo$Presion)
shapiro.test(farmacoA$Presion)
shapiro.test(farmacoB$Presion)
# Normalidad para Colesterol
shapiro.test(placebo$Colesterol)
shapiro.test(farmacoA$Colesterol)
shapiro.test(farmacoB$Colesterol)
#Todos siguen una distribución normal y tiene más de dos muestras por lo que utilizamos el test de ANOVA
# ANOVA para Glucosa
anova_glucosa <- aov(Glucosa ~ Tratamiento, data = datos_biomed)
summary(anova_glucosa)
# ANOVA para Presión
anova_presion <- aov(Presion ~ Tratamiento, data = datos_biomed)
summary(anova_presion)
# ANOVA para Colesterol
anova_colesterol <- aov(Colesterol ~ Tratamiento, data = datos_biomed)
summary(anova_colesterol)
# 12. Realiza un ANOVA sobre la glucosa para cada tratamiento. (1 pt)
anova_glucosa <- aov(Glucosa ~ Tratamiento, data = datos_biomed)
summary(anova_glucosa)
