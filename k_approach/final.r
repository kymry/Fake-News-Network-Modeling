# load and install packages
requiredPackages <- c("igraph", "ggplot2", "data.table")
for (pac in requiredPackages) {
  if(!require(pac,  character.only=TRUE)){
    install.packages(pac, repos="http://cran.rstudio.com")
    library(pac,  character.only=TRUE)
  } 
}
rm(pac)
rm(requiredPackages)
options(scipen=999)

# import data
setwd("/Users/kymryburwell/Google Drive/UPC/Fall 2018/CSN/Final Project/FakeNewsNet-master/Data/")
newsUser <- read.table('PolitiFact/PolitiFactNewsUser.txt', col.names=c("newsId","userId","times"))
userUser <- as.matrix(read.table('PolitiFact/PolitiFactUserUser.txt', col.names=c("userId1","userId2")))

# extract users who shared fake news (news id = 200)
shared <- newsUser[newsUser$newsId==200,2]

# obtain subgraph from user network
userGraph <- graph_from_edgelist(userUser, directed=FALSE)
vertex_attr(userGraph, "label", index = shared) <- 9 
userSubGraphNodes <- unique(unlist(ego(userGraph, order=1, nodes=shared)))
userSubgraph <- simplify(induced_subgraph(userGraph, vids = userSubGraphNodes))
shared <- which(V(userSubgraph)$label == 9)

# get pagerank of shared users
pagerank <- unlist(page_rank(userSubgraph, directed=FALSE, vids=(shared))[1], use.names=FALSE)
shared <- cbind(shared, pagerank)
shared <- shared[order(pagerank, decreasing=TRUE),]

# data structure for infected
adj <- as_adj_list(userSubgraph)
infectList <- numeric(length(V(userSubgraph)))
infected <- shared[1:4,1]
for(i in 1:4){infectList[shared[i]] = 1}

#-------- simulate an epidemic --------#

# determines if user becomes infected
infect <- function(b, a, id, infectList, adj){

        # check if already infected
      if(infectList[id] == 1){
        return(infectList)
      }
  
      # get number of infected neighbors
      n = myunlist(adj[id])
      nb = 0
      for(i in 1:length(n)){
        if(infectList[n[i]] == 1){
          nb = nb +1
        }
      }

      # get number of fact checking neighbors
      nf = max(floor(rnorm(1,mean=length(n)/2+1, sd = 1)), 1)
      
      # probability of becoming infected
      p = b * ( (nb*(1+a)) / (nb*(1+a) + nf * (1-a)) )
      
      # determine if becomes infected
      ip = runif(1, 0, 1)
      if(ip < p){ # infected
        return(id)
      }
      
      return()

}

# simulates SI variation
spread <- function(b, a, t, infectList, adj, infected){
  
  # hold infected count over time
  vec <- c()
  
  # for 48 time steps
  for(time in 1:t){
    newInfectList = c()
    
    # for each infected 
    for(i in 1:length(infected)){
      n = myunlist(adj[infected[i]])
      # for each neighbor of infected
      for (j in 1:length(n)){
        newInfectList = c(newInfectList,infect(b, a/time, n[j], infectList, adj))
      }
      
    }
    for(i in 1:length(newInfectList)){ 
      infectList[newInfectList[i]] = 1 
    }
    vec <- c(vec, sum(infectList))
    print(vec)
    
  }
  
  return(infectList)
  
}

# Simulated epidemic with input parameters (beta, alpha, hours, ds to hold infected, adj matrix of network, starting infected)
infectList <- spread(0.5,0.2,20, infectList, adj, infected)


#-------- Auxilary Unlist Function --------#
myunlist <- function(l){
  names <- names(l)
  vec <- unlist(l, F, F)
  reps <- unlist(lapply(l, length), F, F)
  names(vec) <- rep(names, reps)
  vec
}









