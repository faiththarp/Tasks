setwd('~/Desktop/Evolution/Tasks/Task_09')
install.packages("diversitree")
library("diversitree")
transition_0t01 <- 0.1
transition_1to0 <- 0.1
speciation_0 <- 0.2
extinction_0 <- 0.1 
speciation_1 <- 0.2
extinction_1 <- 0.1
maxN <- 1e3
maxT <- 50 
Pars <- c(speciation_0, speciation_1, extinction_0, extinction_1, transition_0t01, transition_1to0)
simTree <-tree.bisse(Pars, max.taxa = maxN, max.t = maxT)
str(simTree)
stateTable <-table(simTree$tip.state)
stateTable / sum(stateTable)
library(ape)