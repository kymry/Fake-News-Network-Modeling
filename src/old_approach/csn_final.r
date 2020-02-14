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
currentNewsId = 177

# extract users who shared fake news (news id = 4)
shared <- newsUser[newsUser$newsId==currentNewsId,2]

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
length(V(userSubgraph))
shared

#-------- Simulate Epidemic --------#

# get best beta
efficiency <- getEfficiencyBeta()
bestBetaFinal <- findBestBeta(efficiency, 0.5, 48)

# get infected/susceptible ratio
ratio <- spreadReturnRatioInfSus(efficiency, 0.195, 0.5, 48, infectList, adj, infected, ratio) 

# get infected list spread(beta, alpha, hours, ds to hold infected, adj matrix of network, starting infected)
efficiency <- getEfficiencyBeta()
infectList <- spread(efficiency, 0.1, 0.5, 48, infectList, adj, infected)

# get number of infected after simulation
numInfected <- spreadReturnNumInfected(efficiency, 0.0533, 0.5, 48, infectList, adj, infected)


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
  
  finalBeta <- 0
  finalInfected <- 0
  
  for (j in 1:10){
    
        # data structure for infected
        infectList <- numeric(length(V(userSubgraph)))
        infected <- shared[1:4,1]
        for(i in 1:4){infectList[shared[i]] = 1}
        
        # set range of beta to search
        betaRange <- seq(0.02, 0.3, by = 0.025)
        
        actualInfected <- length(shared)/2
        difference <- 100000
        bestBeta <- 0
        
        # find best beta
        for (i in betaRange){
          
          numInfected <- spreadReturnNumInfected(efficiency, i, alpha, hours, infectList, adj, infected)
          if (abs(actualInfected - numInfected) < difference){
            bestBeta <- i
            difference <- abs(actualInfected - numInfected)
            bestInfected <- numInfected
          }
          
          print(abs(actualInfected - numInfected))
          print(bestBeta)
          print("-----")
          
          if ( difference <= 1){
            finalBeta <- finalBeta + bestBeta
            finalInfected <- finalInfected + bestInfected
            break
          }
          if (abs(actualInfected - numInfected) > 100){
            finalBeta <- finalBeta + bestBeta
            finalInfected <- finalInfected + bestInfected
            break
          }
        }
  }
  vec1 <- c(finalBeta/3, finalInfected/3)
  return(vec1)
  #return(bestBeta)
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
  
  beta <- c(0.12, 0.12, 0.195, 0.095, 0.195, 0.07, 0.045, 0.12, 0.195, 0.22)
  infected <- c(24, 32, 43, 62,  31, 160, 24, 170, 33, 42)
  
  df <- data.frame(beta, infected)
  df <- df[order(df$infected),,drop=TRUE]
  
  ggplot(df, aes(x = infected, y = beta)) +
    theme_minimal() +
    geom_line(size=0.5, color="#00AFBB") +
    geom_point(size = 0.5,show.legend = TRUE) +
    labs(title="Best Fit Beta per Fake News Article", y="beta", x="number of infected at t=48")+
    theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90, hjust = 1) )
}

plotbestBeta  <- function(){
  
  beta <- c(0.036, 0.045,  0.078, .078, 0.045, 0.07, 0.02, 0.013, 0.045, .103)
  infected <- c(24,  38, 38, 67, 36, 160, 27,188,  32, 43  )
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

plotModelTest <- function{
  
    Real <- c(13, 90, 27, 35, 65, 12,56, 100, 25, 77)
    Model <- c(14, 42, 39, 34, 47, 24, 89, 45, 26, 56 )
    news <- c('25', '26', '28', '29', '31', '32', '41', '42', '43', '44')
    
    df <- data.frame(Real, Model, news)
    df <- melt(df, id.vars='news')
    head(df)
    
    ggplot(df, aes(x=news, y=value, fill=variable)) +
      theme_minimal()+
      geom_bar(stat='identity', position='dodge')+
      labs(title="Infected Nodes - Model Results vs. Real Data", y="number of infected (at t=48)", x="fake news article id")+
      theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90, hjust = 1) )+
      guides(fill=guide_legend(title="", values = c("Actual infected", "Simulated Infected") ))
  
}

plotGraphSize <- function{
  
  gSize <- c(176, 2398, 399, 735, 2051, 186, 2313, 2014,  583, 1325)
  news <- c('25', '26', '28', '29', '31', '32', '41', '42', '43', '44') 
  
  df <- data.frame(gSize, news)
  df <- melt(df, id.vars='news')
  head(df)
  
  ggplot(df, aes(x=news, y=value, fill=variable)) +
    theme_minimal()+
    geom_bar(stat='identity', position='dodge')+
    labs(title="Fake News User Subgraph Size", y="number of nodes", x="fake news article id")+
    theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90, hjust = 1) )+
    guides(fill=FALSE)
}


plotTimeCompare  <- function(){
  
  model3 <- c(1.1192, 2.2283, 2.2221, 2.0002, 10.9928, 1.0092, 4.1029,30.2993, 2.0012, 6.2202) 
  model2 <- c(0.10492182, 2.90344834, 0.11267209, 0.28113031, 0.14145470, 0.05655098, 0.19740319, 8.33758116, 0.13309479, 0.91461468)
  model1 <- c(0.08845639,  1.20024180,  0.10450482,  0.20301700,  0.10681677,  0.08243585,  0.23237038, 18.25728559,  0.11121535,  0.25872064)
  newsId <- c('25', '26', '28', '29', '31', '32', '41', '42', '43', '44') 
  df <- data.frame(cbind(model1, model2,model3))
  df <- melt(df)
  df <- cbind(df, newsId)
  
  # plot ratio of infected/susceptible
  ggplot(df, aes(y=value, x=newsId, color=variable, group=variable)) +
    geom_line(size=0.5) +
    theme_minimal()+
    labs(title="Simulation CPU Time (seconds)", y="time (seconds)", x="Fake News Id")+
    theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90, hjust = 1) )+
    scale_color_hue(labels = c("Model 1 (Baseline)", "Model 2", "Model 3"), name="")
  
}

## Quality metrics

computeQualityMetric <- function(fitted, real){
  n = length(fitted)
  numerator = (1/n) * sum((fitted - real)**2)
  denom = var(real)
  
  q = numerator/denom
  return(q)
}

# Baseline
real.infected = c(13, 90, 27, 35, 65, 12, 56, 100, 25, 77)
fit.infected = c(3, 6, 12,  8,  3,  3,  3, 3,  3,  3)
q.baseline = computeQualityMetric(fit.infected, real.infected); q.baseline

#2nd approach
fit.inf.2nd <- c(9, 138,  18,  29 , 14  , 9 , 18 ,220 , 20 , 40)
real.inf.2nd <- c(13 , 90,  27,  35,  65,  12,  56, 100,  25 , 77)
q.2nd = computeQualityMetric(fit.inf.2nd, real.inf.2nd); q.2nd


# 3rd approach
Real <- c(13, 90, 27, 35, 65, 12,56, 100, 25, 77)
Model <- c(14, 42, 39, 34, 47, 24, 89, 45, 26, 56 )
q.3rd = computeQualityMetric(Model, Real); q.3rd
