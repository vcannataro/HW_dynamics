##################
#Code that controls evolution
#Author: Vincent Cannataro
#WORK IN PROGRESS
##################

#The total population size of the individuals
Total.pop.size <- AA+Aa+aa

#Matrix to keep track of genotypes over the generations
evolution.matrix <- matrix(nrow=3,ncol=Generations, data=0)
rownames(evolution.matrix) <- c("AA","Aa","aa")
colnames(evolution.matrix) <- paste("Generation",1:Generations)
evolution.matrix[,1] <- c(AA,Aa,aa) #initialize the matrix


Offspring.Genotype <- function(){
  if(parent.1==1 & parent.2==1){
    output <- 1
  }
  if(parent.1==3 & parent.2==3){
    output <- 3
  }
  if(xor(parent.1==1 & parent.2==2 , parent.1==2 & parent.2==1)){
    output <- sample(size=1,x=c(1,2),prob=c(0.5,0.5))
  }
  if(parent.1==2 & parent.2==2){
    output <- sample(size=1,x=c(1,2,3),prob=c(0.25,0.5,0.25))
  }
  if(xor(parent.1==1 & parent.2==3 , parent.1==3 & parent.2==1)){
    output <- 2
  }
  if(xor(parent.1==2 & parent.2==3 , parent.1==3 & parent.2==2)){
    output <- sample(size=1,x=c(2,3),prob=c(0.5,0.5))
  }
  return(output)
}





##Loop that simulates evolution. Children choose their parents, allele combinations after that are random
for(i in 2:Generations){
  for(j in 1:Total.pop.size){
    
    parent.1 <- sample(size=1,x=c(1,2,3),prob=c((evolution.matrix[1,i-1]/Total.pop.size),
                                                (evolution.matrix[2,i-1]/Total.pop.size),
                                                (evolution.matrix[3,i-1]/Total.pop.size)))
    if(parent.1==1){
      parent.2 <- sample(size=1,x=c(1,2,3),prob=c(((evolution.matrix[1,i-1]-1)/Total.pop.size),
                                                  (evolution.matrix[2,i-1]/Total.pop.size),
                                                  (evolution.matrix[3,i-1]/Total.pop.size)))
    }
    if(parent.1==2){
      parent.2 <- sample(size=1,x=c(1,2,3),prob=c((evolution.matrix[1,i-1]/Total.pop.size),
                                                  ((evolution.matrix[2,i-1]-1)/Total.pop.size),
                                                  (evolution.matrix[3,i-1]/Total.pop.size)))
    }
    if(parent.1==3){
      parent.2 <- sample(size=1,x=c(1,2,3),prob=c((evolution.matrix[1,i-1]/Total.pop.size),
                                                  (evolution.matrix[2,i-1]/Total.pop.size),
                                                  ((evolution.matrix[3,i-1]-1)/Total.pop.size)))
    }
    
    offspring <- Offspring.Genotype()
    evolution.matrix[offspring,i] <- evolution.matrix[offspring,i]+1
    
    
  }
  
  
  
}





