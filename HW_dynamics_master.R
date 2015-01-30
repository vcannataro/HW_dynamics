################
#H-W population dynamics simulation
#Author: Vincent Cannataro
#WORK IN PROGRESS
################

setwd("~/GitHub/HW_dynamics")

#Number of each genotype in the population initially 
AA <- 1000
Aa <- 1000
aa <- 1000

Generations <- 200

A.to.a <- 1e-3
a.to.A <- 1e-5

source("HW_dynamics_sim.R")

