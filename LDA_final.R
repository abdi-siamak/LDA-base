#################################################
# (C) Siamak Abdi - 2020
# Implementation of the collapsed Gibbs sampling for Latent Dirichlet Allocation (LDA) model in R.
#################################################
Path <- setwd("D:/Program Files/RStudio/projects/lda-Base/")
K = 4                                                     # number of topics/clusters
alpha = 1
beta = 1
iteration = 5000                                        
burn = 2500
#################################################         # creating (topics / percent) Matrix
na <-c()
topics <- matrix(0,nrow = W, ncol = K)
for(k in 1:K){
  na <-append(na,paste0("#topic-",k),after = length(na))
}
colnames(topics)<- na
percent <- matrix(nrow = M, ncol = K)
colnames(percent)<- na
#################################################         # finding duplicate words
R <- which(duplicated(gibbs[,1]) | duplicated(gibbs[,1], fromLast = TRUE))  # repeat words
U <- setdiff(R,which(duplicated(gibbs[,1])))                                # base words that have repeat
R_N <- setdiff(which(gibbs[,1]==gibbs[,1]),R)             # words that have not repeat
V <- gibbs[union(U,R_N),1]                                # Dictionary
#################################################         # row names for topics matrix
rownames(topics)<- gibbs[1:W]
#################################################
gibbs[,3] <- sample(c(1:K),W,replace = TRUE)              # initialize of Z
##################################################        # Estimated matrix
PHI <- matrix(0,nrow = K, ncol = length(V))
colnames(PHI)<- V
THETA <- matrix(0,nrow = M, ncol = K)
#################################################         # Theta matrix
Theta <- matrix(0,nrow = M, ncol = K)
##################################################        # Phi matrix
Phi <- matrix(0,nrow = K, ncol = length(V)) 
colnames(Phi)<- V
##################################################        # initialize Phi and Theta and N_R matrix
N_R <- array(0,dim = K)
for(x in 1:W){
  Theta[as.integer(gibbs[x,2]),as.integer(gibbs[x,3])]<-Theta[as.integer(gibbs[x,2]),as.integer(gibbs[x,3])]+1
  Phi[as.integer(gibbs[x,3]),gibbs[x,1]] <- Phi[as.integer(gibbs[x,3]),gibbs[x,1]] + 1
  N_R[as.integer(gibbs[x,3])] <- N_R[as.integer(gibbs[x,3])] + 1
  #cat("\r N_M: %",(x/W)*100)
}
##################################################       ## iterations ##
##################################################
n_m <- numeric(K)
n_r <- numeric(K)
n_v <- numeric(K)
n_d <- numeric(K)
until <- 0
for(i in 1:iteration){
  #startTime <- Sys.time()
  Update <- FALSE
  for(z in 1:W){  
    #startTime <- Sys.time()
    if(z==1){                                             # loading new data
      n_m <-Theta[as.integer(gibbs[z,2]),] 
      n_m[as.integer(gibbs[z,3])] <- n_m[as.integer(gibbs[z,3])] - 1
      n_r <- N_R[]
      n_r[as.integer(gibbs[z,3])] <- n_r[as.integer(gibbs[z,3])] - 1
      n_d <-docs_len[as.integer(gibbs[z,2])]
      n_v <-Phi[,gibbs[z,1]]
      n_v[as.integer(gibbs[z,3])] <- n_v[as.integer(gibbs[z,3])] - 1
      n_m <- n_m + alpha 
      n_r <- n_r + ((length(V))*beta)
      n_v <- n_v + beta
      n_d <- n_d + (K*alpha)
    }else if(z!=1){
      if((gibbs[z,2]==gibbs[z-1,2])&&Update){              # in a document
        n_m <-Theta[as.integer(gibbs[z,2]),] 
        n_m[as.integer(gibbs[z,3])] <- n_m[as.integer(gibbs[z,3])] - 1
        n_m <- n_m + alpha 
      }else if(gibbs[z,2]!=gibbs[z-1,2]){                  # enterring a new document
        n_m <-Theta[as.integer(gibbs[z,2]),] 
        n_m[as.integer(gibbs[z,3])] <- n_m[as.integer(gibbs[z,3])] - 1
        n_m <- n_m + alpha 
        n_d <-docs_len[as.integer(gibbs[z,2])]
        n_d <- n_d + (K*alpha)
      }
      if(Update){
        n_r <- N_R[]
        n_r[as.integer(gibbs[z,3])] <- n_r[as.integer(gibbs[z,3])] - 1
        n_v <-Phi[,gibbs[z,1]]
        n_v[as.integer(gibbs[z,3])] <- n_v[as.integer(gibbs[z,3])] - 1
        n_r <- n_r + ((length(V))*beta)
        n_v <- n_v + beta
        Update <- FALSE
      }
    }
    p_Z <- (n_m/n_d) * (n_v/n_r)
    #p_Z <- p_Z/sum(p_Z)                                  # normalizing
    prob <- sample(c(1:K),1,prob = p_Z)                   # sampling (Draw)
    #prob <-which(p_Z==max(p_Z))
    if(gibbs[z,3]!=prob){                                 # updating Theta / Phi / N_R 
      Update <-TRUE
      Theta[as.integer(gibbs[z,2]),prob]<-Theta[as.integer(gibbs[z,2]),prob]+1
      Theta[as.integer(gibbs[z,2]),as.integer(gibbs[z,3])]<-Theta[as.integer(gibbs[z,2]),as.integer(gibbs[z,3])]-1
      N_R[prob]<-N_R[prob]+1
      N_R[as.integer(gibbs[z,3])]<-N_R[as.integer(gibbs[z,3])]-1
      Phi[prob,gibbs[z,1]]<-Phi[prob,gibbs[z,1]]+1
      Phi[as.integer(gibbs[z,3]),gibbs[z,1]]<-Phi[as.integer(gibbs[z,3]),gibbs[z,1]]-1
      gibbs[z,3] <- prob                                     # update z
    }
    if(i > burn){                                        # update topics matrix
      v_4 <- vector("numeric", K)
      v_4[as.integer(gibbs[z,3])]<- 1
      topics[z,] <-topics[z,] + v_4
    }
  }
  #cat("\r learning: ",i)
  if(i > burn){
    #################################################           # estimating PHI matrix (parameter estimation)
    for(e in 1:K){
      for(r in 1:length(V)){
        PHI[e,r]<-PHI[e,r] + ((Phi[e,r]+beta)/(N_R[e]+(length(V)*beta)))
      }
    }
    #################################################           # estimating THETA matrix (parameter estimation)          
    for(j in 1:M){
      for(h in 1:K){
        THETA[j,h]<-THETA[j,h] + ((Theta[j,h]+alpha)/((docs_len[j])+(K*alpha)))
      }
    }
  }
  cat("\r learning: %",(i/iteration)*100)
  #endTime <- Sys.time() - startTime
#  if(i == 1){
#    until <- i + until
#    cat("\r \n ", until)
#    i <- 0
#  }
}
##################################################         # get Topic/Cluster Label 
for(r in 1:W){                                           
  count <- which(topics[r,]==max(topics[r,]))
  if(length(count)>1){
    gibbs[r,4] <- sample(as.integer(count),1,prob = rep((1/length(count)),length(count)))
  }else{
    gibbs[r,4] <- count
  }
}
##################################################          # Topic percentage of each document
for(p in 1:M){
  KK <- c()
  x<-subset(gibbs, gibbs[,2]==p)                            # subset of gibbs matrix
  for(k in 1:K){
    KK[k]<-length(which(x[,4] == paste0(k)))
  }
  percent[p,] <- (KK/sum(KK))*100
}
#################################################           # estimating PHI matrix (parameter estimation)
for(i in 1:K){
  for(r in 1:length(V)){
    PHI[i,r]<-PHI[i,r]/(iteration-burn)
  }
}
#################################################           # estimating THETA matrix (parameter estimation)          
for(j in 1:M){
  for(i in 1:K){
    THETA[j,i]<-THETA[j,i]/(iteration-burn)
  }
}
#################################################
#################################################           # getting (Top words) and (Top topics)
TopPHI <- matrix(nrow = length(V), ncol = K)
TopTHETA <- matrix(nrow = M, ncol = K)
colnames(TopPHI)<- na
colnames(TopTHETA)<- na
rownames(THETA)<- c(1:M)
PHI <- t(PHI)
for(i in 1:K) {
  PHI <- PHI[order(PHI[,i],decreasing = TRUE),]
  TopPHI[,i] <- row.names(PHI)
}
for(i in 1:K) {
  THETA <- THETA[order(THETA[,i],decreasing = TRUE),]
  TopTHETA[,i] <- row.names(THETA)
}

#write.csv(TopPHI,file="top10-PHI.csv",row.names = FALSE)
#######################################################     # document_term_matrix
document_term_matrix <- matrix(0,nrow = M, ncol = length(V))
colnames(document_term_matrix) <- V
for (t in 1:W) {
  document_term_matrix[as.integer(gibbs[t,2]),gibbs[t,1]] <- document_term_matrix[as.integer(gibbs[t,2]),gibbs[t,1]] + 1
}
#######################################################
## output matrix: gibbs, percent, PHI, THETA, TopPHI


