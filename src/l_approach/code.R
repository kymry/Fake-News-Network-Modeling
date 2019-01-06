# Set WD and load data
wd = getwd()
if(grepl("nora", wd)) {
    setwd("~/git/fake-news-project/src/l_approach")
} else {
    # Set anyone's else project directory path, if needed be.
    #setwd()
}
rm(wd)

########################
source("config.R")
source("functions.R")
########################

# Load data
news.names <- read.table("../../data/News.txt", quote="\"", comment.char=""); news.names <- news.names$V1
user.names <- read.table("../../data/User.txt", quote="\"", comment.char=""); user.names <- user.names$V1
user.user.df <- read.delim("../../data/PolitiFactUserUser.txt", header=FALSE)
news.user.df <- read.delim("~/git/fake-news-project/data/PolitiFactNewsUser.txt", header=FALSE)

# Set cols names
names(news.user.df) <- c("FakeNewsId", "SharedBy", "Times shared") # Do we need the times shared?


# Construct whole network user-user
users.graph = graph_from_data_frame(user.user.df, directed=F) # For simplicity, we are going to work with an undirected graph.
is.connected(users.graph) # Wasn't it supposed to be fully connected?



simulateSI <- function(g, tmax, beta, verbose=F, fakeNewsId, news.user.df) {
  # Given a graph, a maximum time and a beta, performs an SI simulation.
  ts = 1:tmax
  
  edgelist <- as.data.frame(as_edgelist(g), stringsAsFactors = F)
  E = length(E(g))
  N = length(V(g))
  
  efficiency <- getEfficiencyOfFakeNews(tmax) # Consider this as an inverse damping factor.
  
  set.seed(321)
  nums = runif(tmax * E, 0, 1)
  nToInfect <- 3
 
   # Start with infected nodes
  vertices.infected = data.table(vId = as.numeric(V(g)$name), infected = rep(FALSE, N))
  vToInfect <- selectSourceOfFakeNews(fakeNewsId, g, nToInfect, news.user.df)
  
  #Infect them
  for (vi in vToInfect) {
      vertices.infected[vertices.infected$vId == vi, ]$infected = TRUE        
  }
  
  
  # For each timestep...
  nrInfected = c()
  for (x in ts) {
      
      beta.t <- efficiency[x]*beta
      inf.prev.step <- vertices.infected[vertices.infected$infected == TRUE,]$vId
      
      # For each infected
      x.i <- 1 # subindex to get random prob.
      for (inf in inf.prev.step) {
          
          # we get the susceptibles related to the infected
          v.susceptible.1 = edgelist[edgelist$V1 == inf, ]$V2
          v.susceptible.2 = edgelist[edgelist$V2 == inf, ]$V1
          v.susc <- append(v.susceptible.1, v.susceptible.2)
          
          # Try to infect them if not infected
          for(vs in v.susc){
              alpha = nums[x + x.i]
              if(!vertices.infected[vertices.infected$vId == vs,]$infected & (alpha <= beta.t)){
                  vertices.infected[vertices.infected$vId == vs,]$infected = TRUE
              }
              x.i <- x.i + 1
          }
          
      }
      
      if(verbose){
          cat("It", x, ", nr. infected: ", nrow(vertices.infected[vertices.infected$infected == T,]), "/", N,"\n")   
      }
      nrInfected <- append(nrInfected,  nrow(vertices.infected[vertices.infected$infected == T,]))
  }
  return(nrInfected)
}


fakeNewsId = selectNetworkWithLowerNrUsers(news.user.df, 1, 9)
fake.news.subgraph = createFakeNewsSubgraphFromId(news.user.df, fakeNewsId)


beta = 0.3 # Prob of 10% of getting infected.
tmax = 48 # Each step is an hour. So tops 48 hours.

infected.progression <- simulateSI(fake.news.subgraph, tmax, beta, fakeNewsId, news.user.df)
plotEvolutionRatio(infected.progression, length(V(fake.news.subgraph)), "Infected/susceptible ratio")
plotEvolution(infected.progression, length(V(fake.news.subgraph)), "Infected/susceptible ratio")


