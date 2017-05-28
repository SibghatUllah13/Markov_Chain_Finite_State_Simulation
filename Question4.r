

set.seed(1)
result = matrix(nrow = 3, ncol = 3)
# Function to Simulate Markov Chain ---------------------------------------
simulate_markov_chain= function (starting_state)
{
  S=c(1,2,3)
  tran_matrix = matrix(c(0,1/2,1/2,5/8,1/8,1/4,2/3,1/3,0),nrow = 3, ncol = 3,byrow = T)
  nsample<-1000
  chain<-rep(NA,nsample+1) # vector that will hold
  chain[1]<-starting_state
  for(t in 1:nsample){
    chain[t+1]<-sample(S,size=1,prob=tran_matrix[chain[t],])
  }
  chain
}
# Part a ------------------------------------------------------------------
nsample = 1000
chain = simulate_markov_chain(1)
plot(chain,ylim=c(0,4),col='blue')
plot(chain,ylim=c(0,4),type="b",col='purple')
# Part b ------------------------------------------------------------------
result[,1] = c(table(chain)[1]/nsample,table(chain)[2]/nsample,table(chain)[3]/nsample)
c(table(chain)[1]/nsample,table(chain)[2]/nsample,table(chain)[3]/nsample)
# Part c ------------------------------------------------------------------
nmarkov = 500
final_states = rep(NA,nmarkov)
for (i in 1:nmarkov)
{
  chain = simulate_markov_chain(1)
  final_states[i] = chain[length(chain)]
  
}
plot(final_states,ylim=c(0,4),col='blue')
plot(final_states,ylim=c(0,4),col='purple')
result[,2] = c(table(final_states)[1]/nmarkov,table(final_states)[2]/nmarkov,table(final_states)[3]/nmarkov)
c(table(final_states)[1]/nmarkov,table(final_states)[2]/nmarkov,table(final_states)[3]/nmarkov)
# Part d ------------------------------------------------------------------
S=c(1,2,3)        
tran_matrix = matrix(c(0,1/2,1/2,5/8,1/8,1/4,2/3,1/3,0),nrow = 3, ncol = 3,byrow = T)
pi <- eigen(t(tran_matrix))$vector[,1]/sum(eigen(t(tran_matrix))$vector[,1])
pi
# Part e ------------------------------------------------------------------
result[,3] = pi
result = as.data.frame(result)
#While Xo= 1
`colnames<-`(result,c('Overall Relative_Frequency','Final_State RF','Stationary_Distribution'))
# Part f ------------------------------------------------------------------
chain = simulate_markov_chain(1)
result[,1] = c(table(chain)[1]/nsample,table(chain)[2]/nsample,table(chain)[3]/nsample)
nmarkov = 500
final_states = rep(NA,nmarkov)
for (i in 1:nmarkov)
{
  chain = simulate_markov_chain(1)
  final_states[i] = chain[length(chain)]
  
}
result[,2] = c(table(final_states)[1]/nmarkov,table(final_states)[2]/nmarkov,table(final_states)[3]/nmarkov)
result[,3] = pi
result = as.data.frame(result)
#While X0 = 2
`colnames<-`(result,c('Overall Relative_Frequency','Final_State RF','Stationary_Distribution'))





