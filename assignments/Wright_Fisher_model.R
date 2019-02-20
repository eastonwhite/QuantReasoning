
# Wright-Fisher model with genetic drift and mutation
# Created by Easton R. White
# Last edited: 19-Feb-2019

# Setting up parameter values
N=100        # population size 
p_ini= 0.1   # the initial frequency of the A allele
u = 0.1      # mutation rate from a to A
max.time=100 # time to run simulations


sample.population <- function(p,N){
  next_pop = vector('character',length = N)
  for (i in 1:N){
    new_allele = sample(x = c('A','a'),size = 1,prob=c(p,1-p))
    #if (new_allele=='a' & rbinom(1,1,prob = u)){new_allele='A'} # Optional line to include mutation
    next_pop[i] = as.character(new_allele)
  }
  return(sum(next_pop=='A')/N) # returns the value for p
}



p = vector(mode = 'numeric',length = max.time)
p[1]=p_ini

for (t in 1:(max.time-1)){
  p[t+1] = sample.population(p[t],N)
}

plot(1:max.time,p,ylim=c(0,1),type='l',las=1,ylab='Freq(A)',xlab='Time')



# Questions 

# 1. What is the effect of population size (N) on the probability of extinction of A allele?

# 2. How does the initial frequency of the A allele affect the probability it will reach fixation?


# 3. Let's now study the combined effects of mutation and genetic drift. Modify the above code to include a probability, u, that if a small "a" allele is chosen, it will mutate to be a big A allele. How does this affect our findings in questinos 1 and 2?


