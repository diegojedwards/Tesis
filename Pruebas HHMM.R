library("depmixS4") 
library("HMM")
library("mhsmm")
library(dplyr)
library(zoo)

#----------Cargo las bases que voy a utilzar-----------------

Ind1_LF_ac <- read.csv("Ind1_LF_ac.csv")


#------------Aplico medias móviles para suavizar la serie-----------

accX_mm <- rollmean(Ind1_LF_ac$accX, 10)

plot(x = 1:1000, y = Ind1_LF_ac[1:1000, "accX"], "l")
lines(accX_mm[1:1000], col="red", lwd=2)

df <- Ind1_LF_ac[-c(seq(1, 4), seq(22697,22701)),]
df$accX <- accX_mm

#---- Voy a tratar de estimar los parámetros-------------

mod <- depmix(list(accX ~ 1) , data = Ind1_LF_ac, nstates = 2, family = list(gaussian())) # use gaussian() for normally distributed data
fit.mod <- fit(mod)
a <- posterior(fit.mod)

table(a$state, Ind1_LF_ac$apoyo)

a <- a %>% mutate(state = replace(state, state == 1, 0)) %>%
           mutate(state = replace(state, state == 2, 1))

tabla1 <- table(a$state, Ind1_LF_ac$apoyo)

(tabla1[1,1] + tabla1[2,2]) / sum(tabla1)

# Realizo el modelo suavizando la serie mediante medias móviles.

mod1 <- depmix(list(accX ~ 1) , data = df, nstates = 2, family = list(gaussian())) # use gaussian() for normally distributed data
fit.mod1 <- fit(mod1)
a1 <- posterior(fit.mod1)

a1 <- a1 %>% mutate(state = replace(state, state == 1, 0)) %>%
             mutate(state = replace(state, state == 2, 1))

tabla2 <- table(a1$state, df$apoyo)

(tabla2[1,1] + tabla2[2,2]) / sum(tabla2)

g0 <- (ggplot(df, aes(x = roll, y = accX)) + geom_line() +
         theme(axis.ticks = element_blank(), axis.title.y = element_blank())) %>% ggplotGrob

df$roll <- seq(1, length(df[,1]))
df$apoyo <- as.factor(df$apoyo)


g1 <- (ggplot(df[1:100,], aes(x = roll, y = apoyo, fill = apoyo, col = apoyo)) + 
         geom_bar(stat = "identity", alpha = I(0.7)) + 
         scale_fill_manual(values = mycols, name = "State:\nPerson that\nrolled the\ndice", labels = c("Alice", "Bob")) +
         scale_color_manual(values = mycols, name = "State:\nPerson that\nrolled the\ndice", labels = c("Alice", "Bob")) +
         theme(axis.ticks = element_blank(), axis.text.y = element_blank()) +
         labs(y = "Actual State")) %>% ggplotGrob


# plot output
plot.hmm.output <- function(model.output){
  g0 <- (ggplot(df, aes(x = roll, y = accX)) + geom_line() +
                  theme(axis.ticks = element_blank(), axis.title.y = element_blank())) %>% ggplotGrob
  g1 <- (ggplot(model.output$draws, aes(x = roll, y = state, fill = state, col = state)) + 
           geom_bar(stat = "identity", alpha = I(0.7)) + 
           scale_fill_manual(values = mycols, name = "State:\nPerson that\nrolled the\ndice", labels = c("Alice", "Bob")) +
           scale_color_manual(values = mycols, name = "State:\nPerson that\nrolled the\ndice", labels = c("Alice", "Bob")) +
           theme(axis.ticks = element_blank(), axis.text.y = element_blank()) +
           labs(y = "Actual State")) %>% ggplotGrob
  g2 <- (ggplot(model.output$draws, aes(x = roll, y = est.state.labels, fill = est.state.labels, col = est.state.labels)) + 
           geom_bar(stat = "identity", alpha = I(0.7)) +
           scale_fill_manual(values = mycols, name = "State:\nPerson that\nrolled the\ndice", labels = c("Alice", "Bob")) +
           scale_color_manual(values = mycols, name = "State:\nPerson that\nrolled the\ndice", labels = c("Alice", "Bob")) +
           theme(axis.ticks = element_blank(), axis.text.y = element_blank()) + 
           labs(y = "Estimated State")) %>% ggplotGrob
  g3 <- (ggplot(model.output$hmm.post.df, aes(x = roll, y = value, col = variable)) + geom_line() +
           scale_color_manual(values = mycols, name = "State:\nPerson that\nrolled the\ndice", labels = c("Alice", "Bob")) +
           theme(axis.ticks = element_blank(), axis.text.y = element_blank()) + 
           labs(y = "Posterior Prob.")) %>%
    ggplotGrob()
  g0$widths <- g1$widths
  return(grid.arrange(g0, g1, g2, g3, widths = 1, nrow = 4))
}
plot.hmm.output(hmm1)











# Voy a tratar de armar el modelo HHM con mis datos

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


