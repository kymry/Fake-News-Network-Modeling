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


createFakeNewsSubgraphFromId <- function(news.user.df, fakeNewsId, neighOrder=2, verbose=T) {
    # Given a fake news id, user-user network and a neighborhood order, a subgraph is created from the user-user network containing all those infected users + neighbors 2 jumps away.
  
  infected.rows <- news.user.df[news.user.df$FakeNewsId == fakeNewsId,]
  if(verbose){
      cat("Nr. infected rows for fakeNews ", fakeNewsId, ": ", nrow(infected.rows), "\n")    
  }
  
  fake.news.subgraphs.array <- make_ego_graph(users.graph, order=neighOrder, nodes=infected.rows$SharedBy)
  
  fake.news.subgraph = make_empty_graph(directed = F)
  for (x in length(fake.news.subgraphs.array)) {
      g = fake.news.subgraphs.array[[x]]    
      fake.news.subgraph = fake.news.subgraph + g
  }
  if(verbose){
      cat("Nr. users in network: ", length(V(fake.news.subgraph)), "\n")   
  }
  return(fake.news.subgraph)
}

progressBar <- function(current, upperBound){
    # Prints a progress bar
    
    currentStr = str_c(rep("#", current), collapse="")
    voidBound = upperBound - current
    voidStr = str_c(rep(".", voidBound), collapse="")
    print(paste(currentStr, voidStr, " [", current, "/", upperBound, "]", sep = ""))
}

selectNetworkWithLowerNrUsers <- function(news.user.df, lowerBound=1, upperBound=10) {
  # Given two bounds for fake news ids, this function returns the network with
  # the lowest number of users. Useful for testing purposes.
  info.table <- data.table(fakeNewsId = numeric(), nrVertices = numeric())
  for (x in lowerBound:upperBound) {
      progressBar(x, upperBound)
      fake.news.subgraph = createFakeNewsSubgraphFromId(news.user.df, x, verbose=F)
      info.table <- rbind(info.table, list(x, length(V(fake.news.subgraph))))    
  }
  cat("\n Fake news with lowest number of vertices: \n")
  minRow <- info.table[info.table$nrVertices == min(info.table$nrVertices), ]
  print(minRow)
  return(minRow$fakeNewsId)
}

fakeNewsId = selectNetworkWithLowerNrUsers(news.user.df, 1, 9)
fake.news.subgraph = createFakeNewsSubgraphFromId(news.user.df, fakeNewsId)
