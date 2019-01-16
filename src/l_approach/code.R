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




if(FALSE){
fakeNewsId = selectNetworkWithLowerNrUsers(news.user.df, 1, 9)
fake.news.subgraph = createFakeNewsSubgraphFromId(news.user.df, fakeNewsId)


fakeNewsId = 4
g <- createFakeNewsSubgraphFromId(news.user.df, fakeNewsId)
tmax=48
beta=0.075
set.seed(2)
inf <- simulateSIBaseline(g, tmax, beta, T)

## Comparison

if(F){
    
real.infected = c()
fit.infected = c()
for (fn in 10:19) {
    inf.real.fn = nrow(news.user.df[news.user.df$FakeNewsId == fn,])
    real.infected = append(real.infected, inf.real.fn)
    res = chooseFittingBetaForBaseline(fn, news.user.df)    
    res.inf = res[2]
    
    fit.infected = append(fit.infected, res.inf)
}

}

real.infected = c(26, 32, 44, 66, 32, 166, 24, 179, 33, 44)
fit.infected = c(3.0, 3.1, 38.1, 3.0, 3.0, 3.0, 6.9, 234.2, 42.4, 33.1)
q.baseline = computeQualityMetric(fit.infected, real.infected)

# Baseline plot
plot(1:49, inf, main="Baseline infected evo., beta = 0.03, fake news = 4", xlab="t", ylab ="# infected nodes")
lines(inf)
abline(h = 19, col="red", lty=c(2))
legend("bottomright", legend = c("Infected nodes", "Infected original"),
       lty = 1, lwd = 2,col = c("black", "red"))
}

beta = 0.015 # Prob of 10% of getting infected.
tmax = 48 # Each step is an hour. So tops 48 hours.

infected.progression <- simulateSI(fake.news.subgraph, tmax, beta, fakeNewsId, news.user.df, T)

plotEfficiencyFakeNews(tmax)
plotEvolutionRatio(infected.progression, length(V(fake.news.subgraph)), "Infected/susceptible ratio")
plotEvolution(infected.progression, length(V(fake.news.subgraph)), "Infected/susceptible ratio")

# Choose best beta given a fake news id
chooseFittingBeta(fakeNewsId, news.user.df)

if(F){
    fake.news.ids <- c(2, 4, 6)
    for (f in fake.news.ids) {
        cat("fake news id:", f, "\n")
        chooseFittingBeta(f, news.user.df)
    }
}

# Results:
#     - fakeNewsId: 2
#         - Best beta: 0.0175 
#         - Avg. infected: 45.9
#         - Original infected: 47
#         
#         
#     - fakeNewsId: 4
#         - Best beta: 0.01
#         - Avg. infected: 24
#         - Original infected: 19
#
#         
#     - fakeNewsId: 6
#         - Best beta: 0.0255/0.026
#         - Avg. infected: 70, 70
#         - Original infected: 70

#### Mitigation


beta = 0.015 # Prob of 1.5% of getting infected.
tmax = 48 # Each step is an hour. So tops 48 hours.
fakeNewsId = 4
g = createFakeNewsSubgraphFromId(news.user.df, fakeNewsId)

simulateSIWithMitigation <- function(g, tmax, beta, fakeNewsId, news.user.df, verbose=F) {
    # Given a graph, a maximum time and a beta, performs an SI simulation.
    ts = 1:tmax
    
    infected.originally = nrow(news.user.df[news.user.df$FakeNewsId == fakeNewsId,])
    edgelist <- as.data.frame(as_edgelist(g), stringsAsFactors = F)
    E = length(E(g))
    N = length(V(g))
    
    efficiency <- getEfficiencyOfFakeNews(tmax) # Consider this as an inverse damping factor.
    efficiencyTrueNews <- getEfficiencyOfTrueNews(tmax)
    
    # New: Mitigation! Users that are immune to the fake news!
    vertices.infected.imm = data.table(vId = as.numeric(V(g)$name), infected = rep(FALSE, N), immune = rep(FALSE, N))
    nToInfect <- 3
    vToInfect <- selectSourceOfFakeNews(fakeNewsId, g, nToInfect, news.user.df)
    
    #Infect them
    for (vi in vToInfect) {
        vertices.infected.imm[vertices.infected.imm$vId == vi, ]$infected = TRUE        
    }
    
    # Compute centralization degrees of all nodes
    centr.degree = centralization.degree(g, mode = "all")
    degree.df <- data.table(vId = as.numeric(V(g)$name), degree=centr.degree$res)
    degree.df <- degree.df[order(-degree.df$degree),] # Order descendingly by degree
    
    # For each timestep...
    nrInfected = c(0)
    nrImmune = c(0)
    for (x in ts) {
        
        beta.t <- efficiency[x]*beta
        beta.imm.t <- efficiencyTrueNews[x]*beta
        inf.prev.step <- vertices.infected.imm[vertices.infected.imm$infected == TRUE,]$vId
        
        # For each infected
        for (inf in inf.prev.step) {
            
            # we get the susceptibles related to the infected
            v.susceptible.1 = edgelist[edgelist$V1 == inf, ]$V2
            v.susceptible.2 = edgelist[edgelist$V2 == inf, ]$V1
            # Susceptible without taking into account if they are infected or not
            v.susc <- append(v.susceptible.1, v.susceptible.2); v.susc = unique(v.susc)
            
            ### 1. INFECT
            # Purge them until they are all not infected and not immune yet
            # 1.1 First take all the susceptible nodes
            non.infected.immune.prev <- vertices.infected.imm[vertices.infected.imm$infected == FALSE,]$vId
            v.susc = v.susc[v.susc  %in% non.infected.immune.prev] # Keeping only those nodes that are not yet infected & not immune
            
            # Infect nodes
            nr.v.infect.at.x = floor(beta.t * length(v.susc)) # Take beta prop. of susc nodes to infect
            v.infect.at.x = sample(v.susc, nr.v.infect.at.x, prob = NULL)
            
            for (to.infect in v.infect.at.x){
                # 1.2 Infect only those that are not immunized
                if(!vertices.infected.imm[vertices.infected.imm$vId == to.infect, ]$immune){
                    vertices.infected.imm[vertices.infected.imm$vId == to.infect, ]$infected = TRUE    
                }
                
            }
            
            ### 2. IMMUNIZE
            # We get again those that are not infected and not immune yet
            non.infected.immune.prev <- vertices.infected.imm[vertices.infected.imm$infected == FALSE & vertices.infected.imm$immune == FALSE,]$vId
            
            # Immunize nodes
            nr.v.imm.at.x = floor(beta.imm.t * length(non.infected.immune.prev)) # Take beta prop. of susc nodes to infect
            v.imm.at.x = subset(degree.df, degree.df$vId %in% non.infected.immune.prev) # Immunize those with higher degree
            v.imm.at.x = v.imm.at.x$vId[1:nr.v.imm.at.x]
            
            for (to.imm in v.imm.at.x){
                vertices.infected.imm[vertices.infected.imm$vId == to.infect, ]$immune = TRUE
            }
        }
        
        if(verbose){
            cat("It", x, ", beta.t:", beta.t, ", nr. infected: ", nrow(vertices.infected.imm[vertices.infected.imm$infected == T,]), "/", N,"\n")   
            cat("It", x, ", beta.t:", beta.t, ", nr. immune: ", nrow(vertices.infected.imm[vertices.infected.imm$immune == T,]), "/", N,"\n")   
            cat("-----------\nInfected originally: ", infected.originally, "\n")   
        }
        currInf <-  nrow(vertices.infected.imm[vertices.infected.imm$infected == T,])
        currImm <-  nrow(vertices.infected.imm[vertices.infected.imm$immune == T,])
        nrInfected <- append(nrInfected, currInf)
        nrImmune <- append(nrImmune, currImm)
        
        if(beta.t < 0.001){
            remaining = tmax - x
            nrInfected <- append(nrInfected, rep(currInf, remaining))
            nrImmune <- append(nrImmune, rep(currImm, remaining))
            if(verbose){
                cat(" *** \n beta.t < 0.001. Stopping. \n")       
            }
            break
        }
    }
    return(list(nrInfected, nrImmune))
}
obj = simulateSIWithMitigation(fake.news.subgraph, tmax, beta, fakeNewsId, news.user.df, T)
infected = obj[[1]]
immune = obj[[2]]

infected.progression <- simulateSI(fake.news.subgraph, tmax, beta, fakeNewsId, news.user.df, T)

plotEvolution(infected, length(V(fake.news.subgraph)), "Infected/susceptible ratio")
plotEvolution(infected.progression, length(V(fake.news.subgraph)), "Infected/susceptible ratio")

g = make_star(10, mode="undirected") %>% set.vertex.attribute("name", value=1:10)
g = add_edges(g, c(9, 8))
obj = simulateSIWithMitigation(g, tmax, beta, fakeNewsId, news.user.df, T)

