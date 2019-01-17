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
beta=0.12
set.seed(4)
inf <- simulateSIBaseline(g, tmax, beta, T)

plot(1:49, inf, main="Baseline infected evo., beta = 0.12, fake news = 4", xlab="t", ylab ="# infected nodes", ylim=c(0, 21))
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
  print(real.infected)
  print(unlist(fit.infected))
  print(unlist(betas))
  
  q.baseline = computeQualityMetric(fit.infected, real.infected)
  print(q.baseline)
}


real.infected = c(26,  32,  44,  66,  32, 166,  24, 179,  33,  44)
fit.infected = c(3.0, 23.4, 22.8, 19.2,  9.2, 95.8, 44.8,  3.0, 50.4, 50.2)
best.betas = c( 0.00010000, 0.03071429, 0.03071429, 0.03071429, 0.01000000, 0.05142857, 0.03071429, 0.00010000, 0.03071429, 0.03071429)
beta.glob.baseline = createWeightedSelectionBetas(fit.infected, real.infected, best.betas)

plotBetaInfectedCompareBaseline(best.betas, real.infected)
plotBestBetaComparisonBaseline(best.betas, fit.infected, real.infected)

simulateBaselineForIds <- function(news.user.df, beta, ids) {
  
    ratios = list()
    for(id in ids){
        fakeNewsId = id
        g <- createFakeNewsSubgraphFromId(news.user.df, fakeNewsId)
        tmax=48
        inf <- simulateSIBaseline(g, tmax, beta, T)
        ratios = append(ratios, (inf / (length(V(g)) - inf)))
    }
    return(ratios)
  
}
set.seed(42)
res = simulateBaselineForIds(news.user.df, beta.glob.baseline, c(1, 2))
ratio1=unlist(res[1:49])
ratio2=unlist(res[50:98])
plotRatioSimInfBaseline(ratio1, ratio2)


# Validation set
news.test.id <- c(25, 26, 28, 29, 31, 32, 41, 42, 43, 44)
real.infected = c()
fit.infected = c()
times = c()
for (fn in news.test.id) {
    inf.real.fn = nrow(news.user.df[news.user.df$FakeNewsId == fn,])
    g <- createFakeNewsSubgraphFromId(news.user.df, fn)
    real.infected = append(real.infected, inf.real.fn)
    
    start_time = Sys.time()
    res.inf <- simulateSIBaseline(g, tmax, beta.glob.baseline, T)
    end_time = Sys.time()
    
    fit.infected = append(fit.infected, res.inf[length(res.inf)])
    times = append(times, as.double(difftime(end_time, start_time, units="secs")))    
}
real.infected = c(13, 90, 27, 35, 65, 12, 56, 100, 25, 77)
fit.infected = c(3, 6, 12,  8,  3,  3,  3, 3,  3,  3)

plotModelTestBaseline()

## 2nd model

beta = 0.015 # Prob of 10% of getting infected.
tmax = 48 # Each step is an hour. So tops 48 hours.

if(F){
    infected.progression <- simulateSI(fake.news.subgraph, tmax, beta, fakeNewsId, news.user.df, T)
    plotEvolutionRatio(infected.progression, length(V(fake.news.subgraph)), "Infected/susceptible ratio")
    plotEvolution(infected.progression, length(V(fake.news.subgraph)), "Infected/susceptible ratio")
}
plotEfficiencyFakeNews(tmax)


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

real.infected.2nd.app = c(26,  32,  44,  66,  32, 166,  24, 179,  33,  44) # Same as before
fit.infected.2nd.app = c(24.4 , 11.0 , 18.8,  58.8,  32.8, 146.2,  13.6, 276.2,  11.0,  37.2) 
betas.2nd.app = c(0.003635714, 0.007878571, 0.010000000, 0.030714286, 0.004342857 ,0.030714286, 0.010000000 ,0.030714286 ,0.010000000, 0.010000000)
plotBetaInfectedCompare2ndApp(betas.2nd.app, real.infected.2nd.app)
beta.glob.2nd.approach = createWeightedSelectionBetas(fit.infected.2nd.app, real.infected.2nd.app, betas.2nd.app)

## Plot ratio
ratios = list()
for(id in 1:2){
    fakeNewsId = id
    g <- createFakeNewsSubgraphFromId(news.user.df, fakeNewsId)
    tmax=48
    inf <- simulateSI(g, 48, beta.glob.2nd.approach, fakeNewsId, news.user.df, T)
    ratios = append(ratios, (inf / (length(V(g)) - inf)))
}
ratio1 = unlist(ratios[1:49])
ratio2 = unlist(ratios[50:98])
plotRatioSimInf2ndApproach(ratio1, ratio2)



news.test.id <- c(25, 26, 28, 29, 31, 32, 41, 42, 43, 44)
real.infected.2nd = c()
fit.infected.2nd = c()
times = c()
for (fn in news.test.id) {
    inf.real.fn = nrow(news.user.df[news.user.df$FakeNewsId == fn,])
    g <- createFakeNewsSubgraphFromId(news.user.df, fn)
    real.infected.2nd = append(real.infected.2nd, inf.real.fn)
    
    start_time <- Sys.time()
    res.inf <-  simulateSI(g, tmax, beta.glob.2nd.approach, fn, news.user.df, T)
    end_time <- Sys.time()

    fit.infected.2nd = append(fit.infected.2nd, res.inf[length(res.inf)])
    times = append(times, as.double(difftime(end_time, start_time, units="secs")))    
}
fit.infected.2nd
real.infected.2nd

plotModelTest2ndApproach(fit.infected.2nd, real.infected.2nd)

computeQualityMetric(fit.infected.2nd, real.infected.2nd)




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

