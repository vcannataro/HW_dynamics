##################
#Code that controls evolution
#Author: Vincent Cannataro
#WORK IN PROGRESS
##################

#The total population size of the individuals
Total.pop.size <- AA+Aa+aa

#Matrix to keep track of genotypes over the generations
evolution.matrix <- matrix(nrow=3,ncol=Generations)
rownames(evolution.matrix) <- c("AA","Aa","aa")
colnames(evolution.matrix) <- paste("Generation",1:Generations)
evolution.matrix[,1] <- c(AA,Aa,aa) #initialize the matrix



##Loop that simulates evolution. Children choose their parents, allele combinations after that are random
for(i in 2:Generations){
  for(j in 1:Total.pop.size){
    
    
    
    
  }
  
  
  
}






