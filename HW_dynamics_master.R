################
#H-W population dynamics simulation
#Author: Vincent Cannataro
#WORK IN PROGRESS
################

setwd("~/GitHub/HW_dynamics")

#Number of each genotype in the population initially:
AA <- 10000
Aa <- 0
aa <- 10000

#Generations to run the simulation:
Generations <- 50


#Chance of allele mutations per generation:
A.to.a <- 0
a.to.A <- 0

source("HW_dynamics_sim.R")


