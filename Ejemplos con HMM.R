library("HMM")

#------Ejemplo 1------------------------

hmm  <-  initHMM(c("A","B"), c("L","R"), transProbs=matrix(c(.8,.2,.2,.8),2),
                 emissionProbs=matrix(c(.6,.4,.4,.6),2))

observations <-  c("L","L","R","R")
logBackwardProbabilities <-  backward(hmm,observations)

hmm <- initHMM(c("0", "1", "2"))

#Ejemplo utilizando esta función

states <- c("Target","Outlier")
targetProb <- c(0.4, 0.6)
outlierProb <- c(0.6, 0.4)

transProb <- matrix(c(targetProb, outlierProb), 2)
elements <- c("short","normal","long")
targetStateProb <- c(0.1, 0.3, 0.6)
outlierStateProb <- c(0.6, 0.3, 0.1)

emissProb <- matrix(c(targetStateProb,outlierStateProb), 2, byrow = T)
hmm <- initHMM(States = states, Symbols = elements, transProbs=transProb,
               emissionProbs = emissProb)

simhmm <- simHMM(hmm, 10)
simulated <- data.frame(state=simhmm$states, element=simhmm$observation)

testElements <- c("long","normal","normal","short",
                  "normal","normal","short","long")
stateViterbi <- viterbi(hmm, testElements)
predState <- data.frame(Element=testElements, State=stateViterbi)