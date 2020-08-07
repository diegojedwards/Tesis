library(nnet)
library(neuralnet)
require(dplyr)
require(randomForest)

#Norma de un vector

LF_IW[[1]]$norma_xyz <- sqrt(LF_IW[[1]]$accX ^ 2 + LF_IW[[1]]$accY ^ 2 + LF_IW[[1]]$accZ ^ 2)
LF_IW[[1]]$norma_xy <- sqrt(LF_IW[[1]]$accX ^ 2 + LF_IW[[1]]$accY ^ 2)

LF_IW[[1]]$angulo_xy <- atan2(LF_IW[[1]]$accY,LF_IW[[1]]$accX) * 360 / (2*pi) 
LF_IW[[1]][LF_IW[[1]]$angulo_xy < 0, "angulo_xy"] <- LF_IW[[1]][LF_IW[[1]]$angulo_xy < 0, "angulo_xy"] + 360

LF_IW[[1]]$angulo_z <- atan2(LF_IW[[1]]$norma_xy,LF_IW[[1]]$accZ) * 360 / (2*pi)
# LF_IW[[1]]$angulo_z <- atan2(LF_IW[[1]]$norma,LF_IW[[1]]$angulo_z) * 360 / (2*pi)

LF_IW[[1]]$contact_numeric <- as.numeric(LF_IW[[1]]$contact)

#Elimino valores repetidos

LF_IW_1 <- LF_IW[[1]] %>% distinct()


#Separo la base en entrenamiento y testeo

train <- sample(1:length(LF_IW_1[,1]), length(LF_IW_1[,1])*0.7)

b.train <- LF_IW_1[train, ]
b.test <- LF_IW_1[-train,]

b.train %>% group_by(contact) %>% summarise(mean(promedio), sd(promedio)) #Summary para cada valor de contacto

#Modelo multinomial 

#Con las 3 varibles
modelo1 <- multinom(contact ~ accX + accY + accZ, data = b.train)

result.modelo1 <- summary(modelo1)
apply(result.modelo1$fitted.values, 1, sum)
predicciones <- predict(modelo1, b.test[, 1 : 3])

#Con la variable promedio
modelo2 <- multinom(contact ~ promedio, data = b.train)

result.modelo2 <- summary(modelo2)
apply(result.modelo2$fitted.values, 1, sum)
predicciones <- predict(modelo2, b.test)

Pruebo una red neuronal

NN <- neuralnet(contact_numeric ~ accX + accY + accZ, b.train, hidden = 5 , linear.output = FALSE)
plot(NN)

predict_testNN = predict(NN, b.test[, c(1, 2, 3, 10)])
b.test$predicciones <- predict_testNN

tapply(b.test$predicciones, b.test$contact_numeric, summary)

pred <- ifelse(predict_testNN > 0.9, 1, 0)

#Random forest

modelRF <- randomForest(contact ~ angulo_xy, b.train)



