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
user.user.df <- read.delim("../../data/PolitiFactUserUser.txt", header=FALSE)
news.user.df <- read.delim("~/git/fake-news-project/data/PolitiFactNewsUser.txt", header=FALSE)

# Set cols names
names(news.user.df) <- c("FakeNewsId", "SharedBy", "Times shared") 


# Construct whole network user-user
users.graph = graph_from_data_frame(user.user.df, directed=F) # For simplicity, we are going to work with an undirected graph.

# For testing purposes
fakeNewsId = selectNetworkWithLowerNrUsers(news.user.df, 1, 9)
fake.news.subgraph = createFakeNewsSubgraphFromId(news.user.df, fakeNewsId)

# A baseline example
fakeNewsId = 4
g <- createFakeNewsSubgraphFromId(news.user.df, fakeNewsId)
tmax=48
beta=0.075
set.seed(2)
inf <- simulateSIBaseline(g, tmax, beta, T)

plot(1:49, inf, main="Baseline infected evo., beta = 0.075, fake news = 4", xlab="t", ylab ="# infected nodes")
lines(inf)
abline(h = 19, col="red", lty=c(2))
legend("bottomright", legend = c("Infected nodes", "Infected original"),
       lty = 1, lwd = 2,col = c("black", "red"))

## Comparison

chooseBetaBaselineTrainingFakeNews <- function() {
  real.infected = c()
  fit.infected = c()
  betas = c()
  for (fn in 10:19) {
      inf.real.fn = nrow(news.user.df[news.user.df$FakeNewsId == fn,])
      real.infected = append(real.infected, inf.real.fn)
      res = chooseFittingBetaForBaseline(fn, news.user.df)    
      res.inf = res[2]
      beta.r = res[1]
      fit.infected = append(fit.infected, res.inf)
      betas= append(betas, beta.r)
  }
}


q.baseline = computeQualityMetric(fit.infected, real.infected)

best.betas = c(0.08, 0.04, 0.035, 0.1, 0.06, 0.08, 0.035, 0.04, 0.1)
beta.avg.baseline = mean(best.betas)

simulateBaselineForIds <- function(news.user.df, beta, ids) {
  
    ratios = list()
    for(id in ids){
        fakeNewsId = id
        g <- createFakeNewsSubgraphFromId(news.user.df, fakeNewsId)
        tmax=48
        beta=beta.avg.baseline
        inf <- simulateSIBaseline(g, tmax, beta, T)
        ratios = append(ratios, (inf / (length(V(g)) - inf)))
    }
    return(ratios)
  
}
set.seed(42)
res = simulateBaselineForIds(news.user.df, beta.avg.baseline, c(1, 2))
ratio1=res[1:49]
ratio2=res[50:98]
plotRatioSimInfBaseline(ratio1, ratio2)


news.test.id <- c(25, 26, 28, 29, 31, 32, 41, 42, 43, 44)
real.infected = c()
fit.infected = c()
for (fn in news.test.id) {
    inf.real.fn = nrow(news.user.df[news.user.df$FakeNewsId == fn,])
    g <- createFakeNewsSubgraphFromId(news.user.df, fn)
    real.infected = append(real.infected, inf.real.fn)
    res.inf <- simulateSIBaseline(g, tmax, beta.avg.baseline, T)
    fit.infected = append(fit.infected, res.inf[length(res.inf)])
}




## 2nd model

beta = 0.015 # Prob of 10% of getting infected.
tmax = 48 # Each step is an hour. So tops 48 hours.

if(F){
    infected.progression <- simulateSI(fake.news.subgraph, tmax, beta, fakeNewsId, news.user.df, T)
    plotEvolutionRatio(infected.progression, length(V(fake.news.subgraph)), "Infected/susceptible ratio")
    plotEvolution(infected.progression, length(V(fake.news.subgraph)), "Infected/susceptible ratio")
}
plotEfficiencyFakeNews(tmax)

# Choose best beta given a fake news id
chooseFittingBeta(fakeNewsId, news.user.df)


# TRAIN
chooseBeta2ndModelTrainingFakeNews <- function() {
    real.infected = c()
    fit.infected = c()
    betas = c()
    for (fn in 10:19) {
        cat("fake news id:", fn, "\n")
        inf.real.fn = nrow(news.user.df[news.user.df$FakeNewsId == fn,])
        real.infected = append(real.infected, inf.real.fn)
        res = chooseFittingBeta(fn, news.user.df)    
        res.inf = res[2]
        beta.r = res[1]
        fit.infected = append(fit.infected, res.inf)
        betas= append(betas, beta.r)
    }
    print(real.infected)
    print(fit.infected)
    print(betas)
}
chooseBeta2ndModelTrainingFakeNews()
real.infected.2nd.app = c(26, 32, ) # Same as before
fit.infected.2nd.app = c(11 , 12, ) 
betas.2nd.app = c(0.001, 0.001, )


#### Mitigation


beta = 0.015 # Prob of 1.5% of getting infected.
tmax = 48 # Each step is an hour. So tops 48 hours.
fakeNewsId = 4
g = createFakeNewsSubgraphFromId(news.user.df, fakeNewsId)

obj = simulateSIWithMitigation(fake.news.subgraph, tmax, beta, fakeNewsId, news.user.df, T)
infected = obj[[1]]
immune = obj[[2]]

infected.progression <- simulateSI(fake.news.subgraph, tmax, beta, fakeNewsId, news.user.df, T)

plotEvolution(infected, length(V(fake.news.subgraph)), "Infected/susceptible ratio")
plotEvolution(infected.progression, length(V(fake.news.subgraph)), "Infected/susceptible ratio")

g = make_star(10, mode="undirected") %>% set.vertex.attribute("name", value=1:10)
g = add_edges(g, c(9, 8))
obj = simulateSIWithMitigation(g, tmax, beta, fakeNewsId, news.user.df, T)

