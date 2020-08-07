# libraries 
library(depmixS4)
library(ggplot2)
library(gridExtra)
library(reshape2)
library(dplyr)

# the setup 
# functions
simulate <- function(N, dice.val = 6, jbns, switch.val = 4){
  
  # simulate variables
  # could just use one dice sample but having both alice and bob makes it simple to try 
  # different mechanics e.g. bob only throws 1 die, or whatever other probability distribution
  # you want to set.
  bob.dice <- sample(1:dice.val, N, replace = T) + sample(1:dice.val, N, replace = T)
  alice.dice <- sample(1:dice.val, N, replace = T) + sample(1:dice.val, N, replace = T)
  bob.jbns <- rpois(N, jbns[1])
  alice.jbns <- rpois(N, jbns[2])
  
  draws <- data.frame(state = rep(NA, N), obs = rep(NA, N), dice = rep(NA, N))
  draws$state[1] <- "alice"
  draws$obs <- alice.jbns[1]
  draws$dice <- alice.dice[1]
  for(k in 2:N){
    if(draws$state[k-1] == "alice"){
      if(draws$dice[k-1] < switch.val+1){
        draws$state[k] <- "bob"
        draws$obs[k] <- bob.jbns[k]
        draws$dice[k] <- bob.dice[k]
      }else{
        draws$state[k] <- "alice"
        draws$obs[k] <- alice.jbns[k]
        draws$dice[k] <- alice.dice[k]
      }
    }else if(draws$state[k-1] == "bob"){
      if(draws$dice[k-1] < switch.val+1){
        draws$state[k] <- "alice"
        draws$obs[k] <- alice.jbns[k]
        draws$dice[k] <- alice.dice[k]
      }else{
        draws$state[k] <- "bob"
        draws$obs[k] <- bob.jbns[k]
        draws$dice[k] <- bob.dice[k]
      }
    }
  }
  
  # return
  return(cbind(roll = 1:N, draws))
}

set.seed(20181031)
N <- 100
draws <- simulate(N, jbns = c(12, 4), switch.val = 4)  

mycols <- c("darkmagenta", "turquoise")
cols <- ifelse(draws$state == "alice", mycols[1], mycols[2])
ggplot(draws, aes(x = roll, y = obs)) + geom_line()


fit.hmm <- function(draws){
  
  # HMM with depmix
  mod <- depmix(obs ~ 1, data = draws, nstates = 2, family = poisson()) # use gaussian() for normally distributed data
  fit.mod <- fit(mod)
  
  # predict the states by estimating the posterior
  est.states <- posterior(fit.mod)
  head(est.states)
  
  # results
  tbl <- table(est.states$state, draws$state)
  draws$est.state.labels <- c(colnames(tbl)[which.max(tbl[1,])], colnames(tbl)[which.max(tbl[2,])])[est.states$state]
  est.states$roll <- 1:100
  colnames(est.states)[2:3] <- c(colnames(tbl)[which.max(tbl[1,])], colnames(tbl)[which.max(tbl[2,])])
  hmm.post.df <- melt(est.states, measure.vars = c("alice", "bob"))
  
  # print the table
  print(table(draws[,c("state", "est.state.labels")]))
  
  # return it
  return(list(draws = draws, hmm.post.df = hmm.post.df))
}

hmm1 <- fit.hmm(draws)

# plot output
plot.hmm.output <- function(model.output){
  g0 <- (ggplot(model.output$draws, aes(x = roll, y = obs)) + geom_line() +
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

