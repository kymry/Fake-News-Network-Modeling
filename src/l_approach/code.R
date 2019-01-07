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
# news.names <- read.table("../../data/News.txt", quote="\"", comment.char=""); news.names <- news.names$V1
# user.names <- read.table("../../data/User.txt", quote="\"", comment.char=""); user.names <- user.names$V1
user.user.df <- read.delim("../../data/PolitiFactUserUser.txt", header=FALSE)
news.user.df <- read.delim("~/git/fake-news-project/data/PolitiFactNewsUser.txt", header=FALSE)

# Set cols names
names(news.user.df) <- c("FakeNewsId", "SharedBy", "Times shared") # Do we need the times shared?


# Construct whole network user-user
users.graph = graph_from_data_frame(user.user.df, directed=F) # For simplicity, we are going to work with an undirected graph.
is.connected(users.graph) # Wasn't it supposed to be fully connected?





fakeNewsId = selectNetworkWithLowerNrUsers(news.user.df, 1, 9)
fake.news.subgraph = createFakeNewsSubgraphFromId(news.user.df, fakeNewsId)


beta = 0.35 # Prob of 10% of getting infected.
tmax = 48 # Each step is an hour. So tops 48 hours.

infected.progression <- simulateSI(fake.news.subgraph, tmax, beta, fakeNewsId, news.user.df)
plotEvolutionRatio(infected.progression, length(V(fake.news.subgraph)), "Infected/susceptible ratio")
plotEvolution(infected.progression, length(V(fake.news.subgraph)), "Infected/susceptible ratio")


