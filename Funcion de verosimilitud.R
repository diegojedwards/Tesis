require(graphics)


### Voy a tratar de armar las funciones de verosimilitud

### Función outer -----------------

x <- 1:9
names(x) <- x
# Multiplication & Power Tables
x %o% x
y <- 2:8
names(y) <- paste(y,":", sep = "")
outer(y, x, "^")

##--- Funciones de verosimilitud ---------------

x <- rpois(40, 2)
y <- rpois(40, 1)

lambda <- c(1, 2)
delta <- c(0.2, 0.8)

sum(log(outer(x,lambda,dpois)%*%delta))

x <- rnorm(40, 6, 2)
y <- rnorm(40, 2, 1)

media <- c(8, 2)
desvio <- c(2, 1)
delta <- c(0.2, 0.8)

sum(log(outer(x, media, dnorm, desvio)%*%delta))

## Examples from Venables & Ripley
acf(lh)
acf(lh, type = "covariance")
pacf(lh)

acf(ldeaths)
acf(ldeaths, ci.type = "ma")
acf(ts.union(mdeaths, fdeaths))
ccf(mdeaths, fdeaths, ylab = "cross-correlation")
# (just the cross-correlations)

presidents # contains missing values
acf(presidents, na.action = na.pass)
pacf(presidents, na.action = na.pass)
# }

acf(LF_IW[[1]]$accX)
