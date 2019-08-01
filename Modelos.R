library(nnet)
library(neuralnet)
require(dplyr)

#Separo la base en entrenamiento y testeo

train <- sample(1:length(LF_IW[[1]][,1]), length(LF_IW[[1]][,1])*0.7)

b.train <- LF_IW[[1]][train, ]
b.test <- LF_IW[[1]][-train,]

b.train %>% group_by(contact) %>% summarise(mean(promedio)) #Summary para cada valor de contacto

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


#Pruebo una red neuronal

NN <- neuralnet(contact ~ accX + accY + accZ, b.train, hidden = 3 , linear.output = FALSE)
plot(NN)

predict_testNN = compute(NN, b.test[,1:3])
predict_testNN = (predict_testNN$net.result * (max(data$rating) - min(data$rating))) + min(data$rating)
