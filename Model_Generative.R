## Dictionary ##
V <- c("like","eat","broccoli","banana","spinach","smoothie","breakfast","chinchillas","kitten","cute","adopt","yesterday","look","hamster","munch","piece")
## initializing ##
M <- 5
K <- 2
library(MCMCpack)
n_d <- c(4,5,3,3,6)
Docs <- matrix(nrow = M, ncol = max(n_d))
## Creating Theta Matrix
Theta <- matrix(nrow = M, ncol = K)
Theta<- rdirichlet(M, rep(0.5,K))
## Creating Phi Matrix
Phi <- matrix(nrow = K, ncol = length(V))
Phi<- rdirichlet(K, rep(0.5,length(V)))
## Generating words
for(m in 1:M){
    for(d in 1:n_d[m]){
      Z_m_d <-sample(c(1:K),1,replace = TRUE,prob = Theta[m,])
      W_m_d <-sample(c(1:length(V)),1,replace = TRUE,prob = Phi[Z_m_d,])
      Docs[m,d]<-V[W_m_d]
    }
}

