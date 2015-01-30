##################
#Code that controls evolution
#Author: Vincent Cannataro
#WORK IN PROGRESS
##################

###In the code genotype choices are sometimes represented by numbers
###AA=1, Aa=2, aa=3

#The total population size of the individuals
Total.pop.size <- AA+Aa+aa

#Matrix to keep track of genotypes over the generations
evolution.matrix <- matrix(nrow=3,ncol=Generations, data=0)
rownames(evolution.matrix) <- c("AA","Aa","aa")
colnames(evolution.matrix) <- paste("Generation",1:Generations)
evolution.matrix[,1] <- c(AA,Aa,aa) #initialize the matrix

#Function saying, given your two parents genotypes draw your resultant genotype
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
  
  #Did a mutation happen? 
  
  if(output==1){
    #Here, we flip 2 coins and see if each allele was mutated
    no.of.mut.events.1 <- rbinom(1,1,A.to.a) 
    no.of.mut.events.2 <- rbinom(1,1,A.to.a)
    if(no.of.mut.events.1==1 & no.of.mut.events.2==1){
      output <- 3
    }
    if(xor((no.of.mut.events.1==1 & no.of.mut.events.2==0),(no.of.mut.events.1==0 & no.of.mut.events.2==1))){
      output <- 2
    }
    return(output)
  }
  if(output==2){
    
    #Here, we flip 2 coins and see if each allele was mutated in the heterozygote 
    no.of.mut.events.1 <- rbinom(1,1,A.to.a) 
    no.of.mut.events.2 <- rbinom(1,1,a.to.A)
    
    if(no.of.mut.events.1==1 & no.of.mut.events.2==1){
      output <- 2 #If both alleles are mutated in the heterozygote, it's still a heterozygote! 
    }
    if(no.of.mut.events.1==1 & no.of.mut.events.2==0){
      output <- 3 
    }
    if(no.of.mut.events.1==0 & no.of.mut.events.2==1){
      output <- 1
    }
    return(output)
  }
  if(output==3){
    #Here, we flip 2 coins and see if each allele was mutated
    no.of.mut.events.1 <- rbinom(1,1,a.to.A) 
    no.of.mut.events.2 <- rbinom(1,1,a.to.A)
    if(no.of.mut.events.1==1 & no.of.mut.events.2==1){
      output <- 1
    }
    if(xor((no.of.mut.events.1==1 & no.of.mut.events.2==0),(no.of.mut.events.1==0 & no.of.mut.events.2==1))){
      output <- 2
    }
    return(output)
    
    
  }
  
  return(output)
  
}





##Loop that simulates evolution. Children choose their parents, allele combinations after that are based on parent genotype combinations
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
  
  
  
  
  print(paste("Generation:",i,"out of",Generations))
  
}



plot(evolution.matrix[1,]/Total.pop.size,type="l",lwd=5,ylim=c(0,1),ylab="Genotype percent of total population",xlab="Generation")
lines(evolution.matrix[2,]/Total.pop.size,lwd=5,col="red")
lines(evolution.matrix[3,]/Total.pop.size,lwd=5,col="blue")
legend("topleft", c("AA","Aa","aa"),col=c("black","red","blue") ,lwd=5 )


p.initial <- (evolution.matrix[1,1] + 0.5*evolution.matrix[2,1])/Total.pop.size
q.initial <- (evolution.matrix[3,1] + 0.5*evolution.matrix[2,1])/Total.pop.size

AA.expected <- p.initial^2
Aa.expected <- 2*p.initial*q.initial
aa.expected <- q.initial^2

p.avg <- (mean(evolution.matrix[1,10:Generations]) + 0.5*mean(evolution.matrix[2,10:Generations]))/Total.pop.size
q.avg <- (mean(evolution.matrix[3,10:Generations]) + 0.5*mean(evolution.matrix[2,10:Generations]))/Total.pop.size

AA.avg <- round((mean(evolution.matrix[1,10:Generations]))/Total.pop.size,4)
Aa.avg <- round((mean(evolution.matrix[2,10:Generations]))/Total.pop.size,4)
aa.avg <- round((mean(evolution.matrix[3,10:Generations]))/Total.pop.size,4)

# 
print(paste("Expected AA=",AA.expected,"|","Expected Aa=",Aa.expected,"|","Expected aa=",aa.expected))

print(paste("Average AA=",AA.avg,"|","Average Aa=",Aa.avg,"|","Average aa=",aa.avg))

# print(paste("Final p^2=",p^2,"|","Final 2*p*q=",2*p*q,"|","Final q^2=",q^2))




