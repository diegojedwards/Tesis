library("mhsmm")

# Ejemplo paper "Hidden Semi Markov Models for Multiple Observation Sequences: 
#                The mhsmm Package for R"

J <- 3
initial <- rep(1/J, J)
P <- matrix(c(0.8, 0.5, 0.1, 0.05, 0.2, 0.5, 0.15, 0.3, 0.4), nrow = J)
b <- list(mu = c(-3, 0, 2), sigma = c(2, 1, 0.5))
model <- hmmspec(init = initial, trans = P, parms.emis = b, dens.emis = dnorm.hsmm)
train1 <- simulate(model, nsim = 300, seed = 1234, rand.emis = rnorm.hsmm)
str(train)
plot(train, xlim = c(0, 100))