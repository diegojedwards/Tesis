library("depmixS4") 
library("HMM")
library("mhsmm")

#------------Cargo las bases que voy a utilzar-----------------

Ind1_LF_ac <- read.csv("Ind1_LF_ac.csv")



# Voy a tratar de armaar el modelo HHM con mis datos

prob.ini <- c(dim(subset(LF_IW[[1]], apoyo== 1))[1] / length(LF_IW[[1]]$apoyo),
dim(subset(LF_IW[[1]], apoyo== 2))[1] / length(LF_IW[[1]]$apoyo))

df <- as.data.frame(table(observaciones, LF_IW[[1]]$apoyo))

df %>%  group_by(observaciones, Var2) %>% mutate(freq = Freq / sum(Freq))

hmm <- initHMM(c("0","1"), as.factor(LF_IW[[1]]$accX), startProbs = prob.ini)
print(hmm)
simhmm <- simHMM(hmm, 100)
observaciones <- as.factor(LF_IW[[1]]$accX)
estados_predic <- viterbi(hmm, observaciones) #función para predecir nuevos estados.


#------ Utilizo mhsmm ----------

LF_IW[[1]] <- LF_IW[[1]] %>%  mutate(apoyo = replace(apoyo, apoyo == 1, 2)) %>%
                mutate(apoyo = replace(apoyo, apoyo == 0, 1))

mu0 <- mean(LF_IW[[2]][1 : to[4],1][LF_IW[[2]][1 : to[4],4] == 1])
mu1 <- mean(LF_IW[[2]][1 : to[4],1][LF_IW[[2]][1 : to[4],4] == 2])
des0 <- sd(LF_IW[[2]][1 : to[4],1][LF_IW[[2]][1 : to[4],4] == 1])
des1 <- sd(LF_IW[[2]][1 : to[4],1][LF_IW[[2]][1 : to[4],4] == 2])

prob.ini <- c(dim(subset(LF_IW[[1]], apoyo== 1))[1] / length(LF_IW[[1]]$apoyo),
              dim(subset(LF_IW[[1]], apoyo== 2))[1] / length(LF_IW[[1]]$apoyo))
P <- matrix(c(0.5, 0.5, 0.5, 0.5), nrow = 2)
b <- list(mu = c(mu0, mu1), sigma = c(des0^2, des1^2))
startval <- hmmspec(init = prob.ini, trans = P, parms.emis = b, dens.emis = dnorm.hsmm)
train <- list(s = LF_IW[[1]][1 : 300, 5], x = LF_IW[[1]][1 : 300, 1], N = 300)
class(train) <- "hsmm.data"
h1 = hmmfit(train, startval, mstep = mstep.norm)
plot(train, xlim = c(0, 300))

prediccion <- predict(h1, train)

plot(prediccion, xlim = c(0, 300))

table(train[[1]], prediccion$s)



init0 <- rep(1/J, J)
P0 <- matrix(1/J, nrow = J, ncol = J)
b0 <- list(mu = c(-3, 1, 3), sigma = c(1, 1, 1))
startval <- hmmspec(init = init0, trans = P0, parms.emis = b0, dens.emis = dnorm.hsmm)
h1 = hmmfit(train, startval, mstep = mstep.norm)
plot(h1$loglik, type = "b", ylab = "Log-likelihood", xlab = "Iteration")

summary(h1)
train2 <- simulate(model, nsim = 50, seed = 1234, rand.emis = rnorm.hsmm)
yhat <- predict(h1, train2)
mean(yhat$s != train2$s)
predict()


