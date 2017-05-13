library(readr)
library(ggplot2)
library(gmodels)
library(randomForest)

desercion <- read_delim("DEFINITIVA.csv", ";", escape_double = FALSE, trim_ws = TRUE)
desercion$deserto <- factor(desercion$deserto,labels = c("no","si"))
View(desercion)

##Descripción  Base de Datos----
str(desercion)
prop.table(table(desercion$deserto))

#histograma
ggplot(desercion, aes(x = promedio_semestre1 ))+geom_histogram()
ggplot(desercion, aes(x = promedio_semestre2 ))+geom_histogram()       
ggplot(desercion, aes(x = promedio_semestre3 ))+geom_histogram()       
ggplot(desercion, aes(x = promedio_semestre4 ))+geom_histogram()
ggplot(desercion, aes(x = promedio_semestre5 ))+geom_histogram()
ggplot(desercion, aes(x = promedio_semestre6 ))+geom_histogram()
ggplot(desercion, aes(x = promedio_semestre7 ))+geom_histogram()
ggplot(desercion, aes(x = promedio_semestre8 ))+geom_histogram()

ggplot(desercion, aes(x = indice_aprobados ))+geom_histogram()

#serie de tiempo
ggplot(subset(desercion, State %in% c("", "")), aes(x=, y=, color=State))+geom_point()

#barras
ggplot(desercion, aes(factor(deserto)))+geom_bar()

##Random Forests----
set.seed(123)
train_sample <- sample(1904, 1714)
desercion_train <- desercion[train_sample,]
desercion_test <- desercion[-train_sample,]

prop.table(table(desercion_train$deserto))
prop.table(table(desercion_test$deserto)) 

desercion_rf <- randomForest(desercion_train[-6], desercion_train$deserto, importance=TRUE)
desercion_rf
 
plot(desercion_rf, main="Errors Rate")
arImpPlot(desercion_rf, sort = TRUE, main="Variables Importance",type=2, n.var=10)
 
desercion_pred_rf <- predict(desecion_rf, desercion_test)
CrossTable(desercion_test$deserto, desercion_pred_rf, prop.chisq=FALSE, prop.c=FALSE, prop.r=FALSE, dnn=c("real", 'pronostico'))
