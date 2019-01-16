#-------- Directory Settings --------#

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

# import data (politifact)
setwd("/Users/kymryburwell/Google Drive/UPC/Fall 2018/CSN/Final Project/FakeNewsNet-master/Data/")
newsUser <- read.table('PolitiFact/PolitiFactNewsUser.txt', col.names=c("newsId","userId","times"))
userUser <- as.matrix(read.table('PolitiFact/PolitiFactUserUser.txt', col.names=c("userId1","userId2")))

# import data (buzzfeed)
# setwd("/Users/kymryburwell/Google Drive/UPC/Fall 2018/CSN/Final Project/FakeNewsNet-master/Data/")
# newsUser <- read.table('BuzzFeed/BuzzFeedNewsUser.txt', col.names=c("newsId","userId","times"))
# userUser <- as.matrix(read.table('BuzzFeed/BuzzFeedUserUser.txt', col.names=c("userId1","userId2")))



#-------- Set News Article Data Structures --------#

# set news id here
currentNewsId = 30

# extract users who shared fake news (news id = 4)
shared <- newsUser[newsUser$newsId==currentNewsId,2]

# obtain subgraph from user network
userGraph <- graph_from_edgelist(userUser, directed=FALSE)
vertex_attr(userGraph, "label", index = shared) <- 9 
userSubGraphNodes <- unique(unlist(ego(userGraph, order=2, nodes=shared)))
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


#-------- Simulate Epidemic --------#

# get best beta
efficiency <- getEfficiencyBeta()
bestBeta <- findBestBeta(efficiency, 0.5, 48)

# get infected/susceptible ratio
ratio <- spreadReturnRatioInfSus(efficiency, 0.195, 0.5, 48, infectList, adj, infected, ratio) 

# get infected list spread(beta, alpha, hours, ds to hold infected, adj matrix of network, starting infected)
efficiency <- getEfficiencyBeta()
infectList <- spread(efficiency, 0.1, 0.5, 48, infectList, adj, infected)

# get number of infected after simulation
numInfected <- spreadReturnNumInfected(efficiency, 0.1472, 0.5, 48, infectList, adj, infected)


#-------- simulate an epidemic --------#

# determines if a user becomes infected
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

# simulates SI variation and returns infected list
spread <- function(efficiency, b, a, t, infectList, adj, infected){
  
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
        newInfectList = c(newInfectList,infect(efficiency[i]*b, a, n[j], infectList, adj)) # change to a/time to scale a dynamicallys
      }
      
    }
    for(i in 1:length(newInfectList)){ 
      infectList[newInfectList[i]] = 1 
    }
    vec <- c(vec, sum(infectList))
    #print(vec)
    
  }
  
  return(infectList)
  
}

# simulates SI variation and returns the number of infected
spreadReturnNumInfected <- function(efficiency, b, a, t, infectList, adj, infected){
  
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
        newInfectList = c(newInfectList,infect(efficiency[i]*b, a, n[j], infectList, adj)) # change to a/time to scale a dynamicallys
      }
      
    }
    for(i in 1:length(newInfectList)){ 
      infectList[newInfectList[i]] = 1 
    }
    vec <- c(vec, sum(infectList))
  }
  
  # return total number of infected
  return(sum(infectList != 0))
  
}

# simulates SI variation and returns the ratio of infected/susceptible
spreadReturnRatioInfSus <- function(efficiency, b, a, t, infectList, adj, infected, vec){
  
  # for 48 time steps
  for(time in 1:t){
    newInfectList = c()
    
    # for each infected 
    for(i in 1:length(infected)){
      n = myunlist(adj[infected[i]])
      # for each neighbor of infected
      for (j in 1:length(n)){
        newInfectList = c(newInfectList,infect(efficiency[i]*b, a, n[j], infectList, adj)) # change to a/time to scale a dynamicallys
      }
      
    }
    for(i in 1:length(newInfectList)){ 
      infectList[newInfectList[i]] = 1 
    }
    vec <- c(vec, sum(infectList)/length(infectList))
  }
  
  # return total number of infected
  return(vec)
  
}

# finds the best fit beta for a given fake news article
findBestBeta <- function(efficiency, alpha, hours){
  
  # data structure for infected
  infectList <- numeric(length(V(userSubgraph)))
  infected <- shared[1:4,1]
  for(i in 1:4){infectList[shared[i]] = 1}
  
  # set range of beta to search
  betaRange <- seq(0.02, 0.5, by = 0.025)
  
  actualInfected <- length(shared)/2
  difference <- 100000
  bestBeta <- 0
  
  # find best beta
  for (i in betaRange){
    
    numInfected <- spreadReturnNumInfected(efficiency, i, alpha, hours, infectList, adj, infected)
    if (abs(actualInfected - numInfected) < difference){
      bestBeta <- i
      difference <- abs(actualInfected - numInfected)
    }
    
    print(abs(actualInfected - numInfected))
    print(bestBeta)
    print("-----")
    
    if ( difference <= 1){
      return(bestBeta)
    }
  }
  
  return(bestBeta)
}


--------------------------------------
--------------------------------------
#-------- Auxilary Functions --------#
--------------------------------------
--------------------------------------
  
myunlist <- function(l){
  names <- names(l)
  vec <- unlist(l, F, F)
  reps <- unlist(lapply(l, length), F, F)
  names(vec) <- rep(names, reps)
  vec
}

getEfficiencyBeta <- function(tmax=48){
  # Kymry's understanding of the effectives of fake news spread over time
  # Peak is around the 24 hour mark 
  
  t <- 1:tmax
  x <- c( (tmax/2):1, 1:(tmax/2) )
  y <- dnorm(x, mean = 1.5, sd = 10)
  y <- y/(max(y)) # Scale y....
  return(y)
}


----------------------------
----------------------------
#-------- Graphics --------#
----------------------------
----------------------------
  
plotEfficiencyKymry <- function(tmax=48){
  # Kymry's understanding of the effectives of fake news spread over time
  # Peak is around the 24 hour mark 
  
  t <- 1:tmax
  x <- c( (tmax/2):1, 1:(tmax/2) )
  y <- dnorm(x, mean = 1.5, sd = 10)
  y <- y/(max(y)+0.01) # Scale y....
  
  df = data.frame(y, t)
  ggplot(data = df) +
    aes(x = t, y = y) +
    geom_line(color = '#e31a1c') +
    labs(title = 'Beta evolution over time',
         x = 'time (hours)',
         y = 'Infection probability',
         subtitle = '(Efficiency of spread of fake news as time progresses)') +
    theme_minimal()
}

plotEfficiencyLaura <- function(tmax=48){
  # Laura understanding of how effective are fake news on spreading as time progresses. 
  # Their peak is at the 2nd and 3rd hours.
  # They slowly decline as time progresses. After 48h, it just dies out.
  
  x.seq.max <- 10
  x <- seq(0, x.seq.max, by = .1)
  y <- dnorm(x, mean = 1.5, sd = 5)
  x <-x*tmax/x.seq.max # Scale x...
  y <- y/max(y)# Scale y....
  df = data.frame(x, y)
  ggplot(data = df) +
    aes(x = x, y = y) +
    geom_line(color = '#e31a1c') +
    labs(title = 'Beta evolution over time',
         x = 'time (hours)',
         y = 'Infection probability',
         subtitle = '(Efficiency of spread of fake news as time progresses)') +
    theme_minimal()
  
}


plotEvolutionRatio  <- function(nrInfected, N, title){
  nrSusceptible = N - nrInfected
  ts = seq(length(nrInfected))
  plot(ts,(nrInfected/nrSusceptible), type='l', col="blue", main=title, ylab = "ratio", xlab="t")
  
  grid()
}

plotEvolution  <- function(nrInfected, N, title){
  nrSusceptible = N - nrInfected
  ts = seq(length(nrInfected))
  plot(ts,nrSusceptible, type='l', ylim = c(0, N), col="blue", main=title, ylab = "# nodes", xlab="t")
  lines(nrInfected, col="red")
  grid()
  legend("topright", legend = c("Infected", "Susceptible"),
         lty = 1, lwd = 2,col = c("red", "blue"))
}

progressBar <- function(current, upperBound){
  # Prints a progress bar
  
  currentStr = str_c(rep("#", current), collapse="")
  voidBound = upperBound - current
  voidStr = str_c(rep(".", voidBound), collapse="")
  print(paste(currentStr, voidStr, " [", current, "/", upperBound, "]", sep = ""))
}


plotBetaInfectedCompare  <- function(){
  
  beta <- c(0.12, 0.12, 0.195, 0.095, 0.195, 0.07, 0.045, 0.195, 0.22, 0.245, 0.12)
  infected <- c(24, 32, 43, 62,  31, 160, 24, 33, 42,  11, 170)
  df <- data.frame(beta, infected)
  df <- df[order(df$infected),,drop=TRUE]
  
  ggplot(df, aes(x = infected, y = beta)) +
    theme_minimal() +
    geom_line(size=0.5, color="#00AFBB") +
    geom_point(size = 0.5,show.legend = TRUE) +
    labs(title="Best Fit Beta per Fake News Article", y="beta", x="number of infected at t=48")+
    theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90, hjust = 1) )
}

plotRatioSimInf  <- function(){
  
  news10 <- c(0.12, 0.12, 0.195, 0.095, 0.195, 0.07, 0.045, 0.195, 0.22, 0.245, 0.12)
  infected <- c(24, 32, 43, 62,  31, 160, 24, 33, 42,  11, 170)
  df <- data.frame(beta, infected)
  df <- df[order(df$infected),,drop=TRUE]
  
  ggplot(df, aes(x = infected, y = beta)) +
    theme_minimal() +
    geom_line(size=0.5, color="#00AFBB") +
    geom_point(size = 0.5,show.legend = TRUE) +
    labs(title="Best Fit Beta per Fake News Article", y="beta", x="number of infected at t=48")+
    theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90, hjust = 1) )
}

plotRatioSimInf  <- function(){
  
  news10 <- ratio[1:48]
  news11 <- ratio[49:96]
  df <- data.frame(cbind(news10, news11))
  df <- melt(df)
  df <- cbind(df, time=1:48)
  
  # plot ratio of infected/susceptible
  ggplot(df, aes(y=value, x=time, color=variable)) +
    geom_line(size=0.5) +
    theme_minimal()+
    labs(title="Ratio of Infected/Susceptible", y="ratio", x="time (hours)")+
    theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90, hjust = 1) )+
    scale_color_hue(labels = c("Fake News 1", "Fake news 2"), name="")
    
}

