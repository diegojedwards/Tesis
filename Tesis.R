require(ggplot2)
require(plotly)
require(tidyr)
library(latex2exp)
require(plot3D)

list_ind <- c(paste0(0, seq(1,9)), 10, 11)

#Importo las referencias de HS y To para cada pie, e indicaciones de tiempos de mediciones en IW.

ind <- list()
ind.ac <- list()

for (i in list_ind) {
  
  ind[[i]] <- read.csv(paste0("Timing Sujetos 1-11 Notacion Cientifica OK/Sub", i, "_Timing_IW"), sep = ",")
  ind.ac[[i]] <- read.csv(paste0("Timing Sujetos 1-11 Notacion Cientifica OK/Sub", i, "_Timing_IWact"), sep = ",")
}

#Analizo el pie izquierdo

#Importo los datos de las mediciones en los ejes x, y, z, para individuo

Sub_LF <- list()

for (i in 1 : 11) {
  
  Sub_LF[[i]] <- read.csv(paste0("Subject Data_txt format/Sub", i, "_LF.txt"))
}

#Me quedo solamente con las mediciones de caminata en superficies planas

LF_IW <- list()

for (i in 1 : 11){
  
  LF_IW[[i]] <- Sub_LF[[i]][ind.ac[[i]]$Init : ind.ac[[i]]$Fin, ]
  
}

#Agrego a la base de las mediciones la variable respuesta con los instantes donde 
#se producen el contacto de talón y el toe-off.
#Agrego tambien el promedio de las 3 mediciones.

for (i in 1 : 11){
  
  var_contac <- rep(0, dim(LF_IW[[i]])[1])
  var_contac[ind[[i]]$LF_TO] <- 1
  var_contac[ind[[i]]$LF_HS] <- 2
  
  var_apoyo <- rep(0, dim(LF_IW[[i]])[1])
  
  for(j in 1:length(ind[[i]]$LF_TO)) {
    
    # browser()
    var_apoyo[ind[[i]]$LF_TO[j] : ind[[i]]$LF_HS[j]] <- 1
  }
  
  LF_IW[[i]]$contact <- var_contac
  LF_IW[[i]]$apoyo <- var_apoyo
  LF_IW[[i]]$promedio <- apply(LF_IW[[i]], 1, mean)
}


#------------Individuo 1-----------------------------

#Verifico donde está el primer 1 o 2 del individuo 1

i <- 1
while(LF_IW[[1]]$contact[i] < 1) {i <- i + 1}
to <- which(LF_IW[[1]]$contact == 1)

j <- i
while(LF_IW[[1]]$contact[j] < 2) {j <- j + 1}
hs <- which(LF_IW[[1]]$contact == 2)

#Grafico la señal con los pasos

base1 <- gather(LF_IW[[1]][1 : to[4], c("accX", "accY", "accZ")], ejes, valor)
base1$seg <- 1 : to[4]
base1$ejes <- as.factor(base1$ejes)

ggplot(base1[], aes(x = seg, y = valor, colour = ejes))+  
  geom_line()+
  geom_vline(xintercept= to[1], colour = "black") +
  geom_vline(xintercept= hs[1], colour = "blue") +
  geom_vline(xintercept= to[2], colour = "black") +
  geom_vline(xintercept= hs[2], colour = "blue") + theme_bw() + 
  xlab("tiempo (en muestras)") + ylab(TeX("$aceleración (m/s^2)$")) + 
  theme(plot.title = element_text(hjust = 0.5), title = element_text(size=15), text = element_text(size=20)) +
  ggtitle("Señales de ejes X, Y, Z") 

# ggplot() + geom_line(aes(x = 1 : to[3], y = valor), LF_IW[[1]][1 : to[3],], colour="red")

#Verifico donde está el primer 1 o 2 del individuo 2

i <- 1
while(LF_IW[[2]]$contact[i] < 1) {i <- i + 1}
to <- which(LF_IW[[2]]$contact == 1)

j <- i
while(LF_IW[[2]]$contact[j] < 2) {j <- j + 1}
hs <- which(LF_IW[[2]]$contact == 2)

base1 <- gather(LF_IW[[1]][1 : to[4], c("accX", "accY", "accZ")], ejes, valor)
base1$seg <- 1 : to[4]
base1$ejes <- as.factor(base1$ejes)

ggplot(base1[], aes(x = seg, y = valor, colour = ejes))+  
  geom_line()+
  geom_vline(xintercept= to[1], colour = "black") +
  geom_vline(xintercept= hs[1], colour = "blue") +
  geom_vline(xintercept= to[2], colour = "black") +
  geom_vline(xintercept= hs[2], colour = "blue") + theme_bw() + 
  xlab("tiempo (en muestras)") + ylab(TeX("$aceleración (m/s^2)$")) + 
  theme(plot.title = element_text(hjust = 0.5), title = element_text(size=15), text = element_text(size=20)) +
  ggtitle("Señales de ejes X, Y, Z") 

# ggplot() + geom_line(aes(x = 1 : to[3], y = valor), LF_IW[[1]][1 : to[3],], colour="red")


#Verifico donde está el primer 1 o 2 del individuo 2


##Pruebas

i <- 1
while(LF_IW[[2]]$contact[i] < 1) {i <- i + 1}
to <- which(LF_IW[[2]]$contact == 1)

j <- i
while(LF_IW[[2]]$contact[j] < 2) {j <- j + 1}
hs <- which(LF_IW[[2]]$contact == 2)


base1 <- gather(LF_IW[[1]][1 : to[4], c("accX", "accY", "accZ")], ejes, valor)
base1$seg <- 1 : to[4]
base1$ejes <- as.factor(base1$ejes)

ggplot(LF_IW[[1]][1:1000,], aes(x = 1 : 1000, y = promedio)) + geom_line() + 
  geom_vline(xintercept= to[1], colour = "yellow") +
  geom_vline(xintercept= hs[1], colour = "green") +
  geom_vline(xintercept= to[2], colour = "yellow") +
  geom_vline(xintercept= hs[2], colour = "green") +
  geom_vline(xintercept= to[3], colour = "yellow") +
  geom_vline(xintercept= hs[3], colour = "green") +
  geom_vline(xintercept= to[4], colour = "yellow") +
  geom_vline(xintercept= hs[4], colour = "green") 
  


ggplot(base1[], aes(x = seg, y = valor, colour = ejes))+  
  geom_line()+
  geom_vline(xintercept= to[1], colour = "black") +
  geom_vline(xintercept= hs[1], colour = "blue") +
  geom_vline(xintercept= to[2], colour = "black") +
  geom_vline(xintercept= hs[2], colour = "blue") + theme_bw() + 
  xlab("tiempo (en muestras)") + ylab(TeX("$aceleración (m/s^2)$")) + 
  theme(plot.title = element_text(hjust = 0.5), title = element_text(size=15), text = element_text(size=20)) +
  ggtitle("Señales de ejes X, Y, Z") 

ggplot() + geom_line(aes(x = 1 : to[3], y = valor), LF_IW[[1]][1 : to[3],], colour="red")


##Pruebas

LF_IW[[1]]$contact <- as.factor(LF_IW[[1]]$contact)

ggplot(LF_IW[[1]], aes(accX, accY, colour = contact)) + geom_point()
scatter3D(LF_IW[[1]]$accX, LF_IW[[1]]$accY, LF_IW[[1]]$accZ, colvar = as.numeric(LF_IW[[1]]$contact), add = FALSE)

p <- plot_ly(LF_IW[[1]], x = ~accX, y = ~accY, z = ~accZ,
             marker = list(color = ~contact, colorscale = c('#FFE1A1', '#683531'), showscale = TRUE))


#Analizo las correlaciones entre las var. predictoras

cor(LF_IW[[1]][, c(1, 2, 3)])

plot(LF_IW[[1]]$accX, LF_IW[[1]]$accZ)



