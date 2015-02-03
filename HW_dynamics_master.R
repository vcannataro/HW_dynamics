################
#H-W population dynamics simulation
#Author: Vincent Cannataro
#WORK IN PROGRESS
################


##Set this as the working directory on your computer, and within the folder 
#include HW_dynamics_sim.R (also in the github repository)
setwd("~/GitHub/HW_dynamics")

#Number of each genotype in the population initially:
AA <- 1000
Aa <- 0
aa <- 1000

#Generations to run the simulation:
Generations <- 50


#Chance of allele mutations per generation:
A.to.a <- 0
a.to.A <- 0

#Some measure of relative fitness. 
AA.fit <- 1
Aa.fit <- 1
aa.fit <- 1


source("HW_dynamics_sim.R")


