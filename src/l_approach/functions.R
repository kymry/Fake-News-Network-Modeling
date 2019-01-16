#############################
####### FUNCTIONS ###########
#############################

#-------------------------
###### --- UTILS  --- ####
#-------------------------
progressBar <- function(current, upperBound){
    # Prints a progress bar
    
    currentStr = str_c(rep("#", current), collapse="")
    voidBound = upperBound - current
    voidStr = str_c(rep(".", voidBound), collapse="")
    print(paste(currentStr, voidStr, " [", current, "/", upperBound, "]", sep = ""))
}


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

computeQualityMetric <- function(fitted, real){
    n = length(fitted)
    numerator = (1/n) * sum((fitted - real)**2)
    denom = var(real)
    
    q = numerator/denom
    return(q)
}


plotModelTestBaseline <- function(){
    
    Real <- c(13, 90, 27, 35, 65, 12, 56, 100, 25, 77 )
    Model <- c(3, 3, 7520, 3, 3, 3, 7796, 3, 3, 3)
    
    
    news <- c('25', '26', '28', '29', '31', '32', '41', '42', '43', '44')
    
    df <- data.frame(Real, Model, news)
    df <- melt(df, id.vars='news')
    head(df)
    
    ggplot(df, aes(x=news, y=value, fill=variable)) +
        theme_minimal()+
        geom_bar(stat='identity', position='dodge')+
        labs(title="Infected Nodes - Baseline model results vs. Real Data", y="number of infected (at t=48)", x="fake news article id")+
        theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90, hjust = 1) )+
        guides(fill=guide_legend(title="", values = c("Actual infected", "Simulated Infected") ))
    
}
plotModelTestBaseline()



plotBetaInfectedCompareBaseline  <- function(){
    
    beta <- c(0.08, 0.04, 0.035, 0.1, 0.06, 0.08, 0.035, 0.01, 0.04, 0.1)
    infected <- c(26, 32, 44, 66, 32, 166, 24, 179, 33, 44)
    df <- data.frame(beta, infected)
    df <- df[order(df$infected),,drop=TRUE]
    
    ggplot(df, aes(x = infected, y = beta)) +
        theme_minimal() +
        geom_line(size=0.5, color="#00AFBB") +
        geom_point(size = 0.5,show.legend = TRUE) +
        labs(title="Best Fit Beta per Fake News Article on Baseline model", y="beta", x="number of infected at t=48")+
        theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90, hjust = 1) )
}


plotRatioSimInfBaseline  <- function(ratio1, ratio2){
    
    df <- data.frame(cbind(r1 = ratio1,r2= ratio2)) #, time = as.character(1:49)))
#    df <- melt(df,  id.vars='time')
    df <- cbind(df, time=1:49)
    
    # plot ratio of infected/susceptible
    ggplot(df, aes(y=r1, x=time)) +
        geom_line(size=0.5, col='blue') +
        theme_minimal()+
        labs(title="Ratio of Infected/Susceptible Baseline", y="ratio", x="time (hours)")+
        theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90, hjust = 1) )+
        scale_color_hue(labels = c("Fake News 1", "Fake news 2"), name="")
    
}

#-------------------------
###### --- SIMS  --- #######
#-------------------------

###### --- v.1  --- #######
## Just a baseline. 
##  - Infects 3 nodes at the start randomly.
##  - Uses a static beta that establishes whether to infect a user or not.
simulateSIBaseline <- function(g, tmax, beta, verbose=F) {
    # Given a graph, a maximum time and a beta, performs an SI simulation.
    # It picks 3 random nodes as infected.
    # Beta is static.
    
    ts = 1:tmax
    
    beta.damp.func <- function(){
        beta <- c()
        for (t in 1:48) {
            beta <- append(beta, exp(1-(0.1*t)))
        }
        return(beta)
    }
    beta.damp = beta.damp.func()
    beta.damp = beta.damp/max(beta.damp)
    #plot(1:48, beta.damp, main = "Beta damping factor (lambda)", xlab = "t", ylab = "lambda", xlim = c(1, 48))
    #lines(beta.damp)
    
    
    edgelist <- as.data.frame(as_edgelist(g), stringsAsFactors = F)
    E = length(E(g))
    N = length(V(g))
    
    # Start with infected nodes
    p = 3
    infected = rep(FALSE, N)
    infected.start = sample(N, p, prob=NULL)
    infected[infected.start] = TRUE
    
    # Data frame with vertices id, since they are identified already.
    vertices.infected = data.table(vId = as.numeric(V(g)$name), infected)
    
    nrInfected = c(3)
    # For each timestep...
    for (x in ts) {
        
        beta.t <- beta.damp[x] * beta
        inf.prev.step <- vertices.infected[vertices.infected$infected == TRUE,]$vId
        
        # For each infected
        for (inf in inf.prev.step) {
            
            # we get the susceptibles related to the infected
            v.susceptible.1 = edgelist[edgelist$V1 == inf, ]$V2
            v.susceptible.2 = edgelist[edgelist$V2 == inf, ]$V1
            v.susc <- append(v.susceptible.1, v.susceptible.2); v.susc = unique(v.susc)
            
            # Try to infect all the susceptible ones.
             nr.v.infect.at.x = floor(beta.t * length(v.susc))
            #nr.v.infect.at.x = floor(beta.t * N) # Take beta prop. of susc nodes to infect
            #nr.v.infect.at.x = min(length(v.susc), nr.v.infect.at.x)
            v.infect.at.x = sample(v.susc, nr.v.infect.at.x, prob = NULL)
            
            for (to.infect in v.infect.at.x){
                vertices.infected[vertices.infected$vId == to.infect, ]$infected = TRUE
            }
        }
        
        if(verbose){
            cat("It", x, ", nr. infected: ", nrow(vertices.infected[vertices.infected$infected == T,]), "/", N,"\n")   
        }
        currInf <-  nrow(vertices.infected[vertices.infected$infected == T,])
        nrInfected <- append(nrInfected, currInf)
        if(currInf == N){
            cat("All nodes infected. Stopping. \n")
            break
        }
    }
    return(nrInfected)
    
}


chooseFittingBetaForBaseline <- function(fakeNewsId, news.user.df) {
    
    ## Choosing fitting beta
    infected.originally = nrow(news.user.df[news.user.df$FakeNewsId == fakeNewsId,])
    g = createFakeNewsSubgraphFromId(news.user.df, fakeNewsId)
    thres.break.40 <- infected.originally + infected.originally*0.2 # Threshold of +0.2% nodes extra to stop
    betas = c(0.001, 0.005); betas = append(betas, seq(0.01, 0.1, by = 0.005)) 
    avg.infect <- c()
    pb <- 1
    for (beta in betas){
        set.seed(2)
        progressBar(pb, length(betas))
        nrInfected.10 <- c()
        for(x in 1:10){
            infected.progression <- simulateSIBaseline(g, tmax, beta, F)
            nrInfected.10 <- append(nrInfected.10, infected.progression[length(infected.progression)])
        }
        avg.infect <- append(avg.infect, mean(nrInfected.10))
        pb <- pb + 1
        if(mean(nrInfected.10) >= thres.break.40){
            print("More than 120% of nodes. No need to up beta")
            break
        }
    }
    avg.infect <- append(avg.infect, rep(9999, length(betas) - length(avg.infect))) 
    df = data.frame(betas, avg.infect,  diff=abs(avg.infect - infected.originally))
    #print(df)
    minDiffRow <- df[min(df$diff) == df$diff,]
    cat(" ---------------- \n")
    cat(" * Best beta: ", minDiffRow$betas, "\n")
    cat(" * Avg. inf. : " , minDiffRow$avg.infect,"\n")
    cat(" * Org. nr. inf: ", infected.originally, "\n")
    
    return(list(minDiffRow$betas[1], minDiffRow$avg.infect[1]))
}



###### --- v.2  --- #######
## Assumes fake news efficiency on spreading follows a distrubtion, with a peak
## and a decline.
##  - Infects n*2/3 with highest degree nodes + 1/3 of randomly picked nodes.
##  - Picks beta.t*susceptible nodes each time iteration and infects them (does not use runif).

getEfficiencyOfFakeNews <- function(tmax=48){
    # Laura understanding of how effective are fake news on spreading as time progresses. 
    # They slowly decline as time progresses. After 48h, it just dies out.
    
    x.seq.max <- tmax
    x <- seq(0, x.seq.max, by = 1)
    y <- dnorm(x, mean = 1.5, sd = 5)
    y <- y/max(y)
    return(y)
}

getEfficiencyOfTrueNews <- function(tmax=48){
    # Laura understanding of how effective are True news on spreading as time progresses. 
    # They slowly decline as time progresses. 
    
    x.seq.max <- tmax
    x <- seq(0, x.seq.max, by = 1)
    y <- dnorm(x, mean = 2.5, sd = 7)
    y <- y/max(y)
    y <- y - 0.05
    return(y)
}



selectSourceOfFakeNews <- function(fakeNewsId, g, n, news.user.df){
    # given a fakeNewsId, number of nodes to pick and dataset related to it
    # this function picks n vertices that could potentially be
    # the ones to first start spreading the fake news
    
    # Use centralization degree to pick 2/3 of the nodes + 1/3 as random pick
    centr = centralization.degree(g, mode = "all")
    degree.df <- data.table(vId = as.numeric(V(g)$name), degree=centr$res)
    degree.df <- degree.df[order(-degree.df$degree),] # Order descendingly by degree
    
    ## TODO: Try to take into account the number of times a user has shared some fake news?
    if(n > 2){
        n.top <- floor((n*2)/3)    
        toInfect <- degree.df[1:n.top, ]$vId # Most well connected vertices to infect
        n.rand <- n - n.top # Pick last rand nodes to reach n
        v.rand <- sample(degree.df$vId, n.rand, prob=NULL)
        toInfect <- append(toInfect, v.rand)
    } else {
        toInfect <- degree.df[1:n, ]$vId
    }
    return(toInfect)
}

simulateSI <- function(g, tmax, beta, fakeNewsId, news.user.df, verbose=F) {
    # Given a graph, a maximum time and a beta, performs an SI simulation.
    ts = 1:tmax
    
    infected.originally = nrow(news.user.df[news.user.df$FakeNewsId == fakeNewsId,])
    edgelist <- as.data.frame(as_edgelist(g), stringsAsFactors = F)
    E = length(E(g))
    N = length(V(g))
    
    efficiency <- getEfficiencyOfFakeNews(tmax) # Consider this as an inverse damping factor.
    
    # Start with infected nodes
    vertices.infected = data.table(vId = as.numeric(V(g)$name), infected = rep(FALSE, N))
    nToInfect <- 3
    vToInfect <- selectSourceOfFakeNews(fakeNewsId, g, nToInfect, news.user.df)
    
    #Infect them
    for (vi in vToInfect) {
        vertices.infected[vertices.infected$vId == vi, ]$infected = TRUE        
    }
    
    
    # For each timestep...
    nrInfected = c(0)
    for (x in ts) {
        
        beta.t <- efficiency[x]*beta
        inf.prev.step <- vertices.infected[vertices.infected$infected == TRUE,]$vId
        
        # For each infected
        for (inf in inf.prev.step) {
            
            # we get the susceptibles related to the infected
            v.susceptible.1 = edgelist[edgelist$V1 == inf, ]$V2
            v.susceptible.2 = edgelist[edgelist$V2 == inf, ]$V1
            # Susceptible without taking into account if they are infected or not
            v.susc <- append(v.susceptible.1, v.susceptible.2); v.susc = unique(v.susc)
            
            # Purge them until they are all not infected
            # TODO: Check this.
            # non.infected.prev <- vertices.infected[vertices.infected$infected == FALSE,]$vId
            # v.susc = v.susc[v.susc  %in% non.infected.prev] # Keeping only those nodes that are not yet infected
            
            nr.v.infect.at.x = floor(beta.t * length(v.susc)) # Take beta prop. of susc nodes to infect
            v.infect.at.x = sample(v.susc, nr.v.infect.at.x, prob = NULL)
            
            for (to.infect in v.infect.at.x){
                vertices.infected[vertices.infected$vId == to.infect, ]$infected = TRUE
            }
        }
        
        if(verbose){
            cat("It", x, ", beta.t:", beta.t, ", nr. infected: ", nrow(vertices.infected[vertices.infected$infected == T,]), "/", N,"\n")   
            cat("-----------\nInfected originally: ", infected.originally, "\n")   
        }
        currInf <-  nrow(vertices.infected[vertices.infected$infected == T,])
        nrInfected <- append(nrInfected, currInf)
        
        if(beta.t < 0.001){
            remaining = tmax - x
            nrInfected <- append(nrInfected, rep(currInf, remaining))
            if(verbose){
                cat(" *** \n beta.t < 0.001. Stopping. \n")       
            }
            break
        }
    }
    return(nrInfected)
}


chooseFittingBeta <- function(fakeNewsId, news.user.df) {
    
    ## Choosing fitting beta
    infected.originally = nrow(news.user.df[news.user.df$FakeNewsId == fakeNewsId,])
    thres.break.40 <- infected.originally + infected.originally*0.2 # Threshold of +0.2% nodes extra to stop
    betas = c(0.001, 0.005); betas = append(betas, seq(0.01, 0.03, by = 0.0005))
    avg.infect <- c()
    pb <- 1
    for (beta in betas){
        set.seed(123)
        progressBar(pb, length(betas))
        nrInfected.10 <- c()
        for(x in 1:10){
            infected.progression <- simulateSI(fake.news.subgraph, tmax, beta, fakeNewsId, news.user.df, F)
            nrInfected.10 <- append(nrInfected.10, infected.progression[length(infected.progression)])
        }
        avg.infect <- append(avg.infect, mean(nrInfected.10))
        pb <- pb + 1
        if(mean(nrInfected.10) >= thres.break.40){
            print("More than 120% of nodes. No need to up beta")
            break
        }
    }
    avg.infect <- append(avg.infect, rep(9999, length(betas) - length(avg.infect))) 
    df = data.frame(betas, avg.infect,  diff=abs(avg.infect - infected.originally))
    #print(df)
    minDiffRow <- df[min(df$diff) == df$diff,]
    cat(" ---------------- \n")
    cat(" * Best beta: ", minDiffRow$betas, "\n")
    cat(" * Avg. inf. : " , minDiffRow$avg.infect,"\n")
    cat(" * Org. nr. inf: ", infected.originally, "\n")
}

#-------------------------
###### --- GRAPHICS  --- ####
#-------------------------
plotEfficiencyFakeNews <- function(tmax=48){
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

plotEfficiencyTrueNews <- function(tmax=48){
    # Laura understanding of how effective are fake news on spreading as time progresses. 
    # Their peak is at the 2nd and 3rd hours.
    # They slowly decline as time progresses. After 48h, it just dies out.
    
    x.seq.max <- 10
    x <- seq(0, x.seq.max, by = .1)
    
    y <- dnorm(x, mean = 3, sd = 5)
    y <- y/max(y)# Scale y....
    y.inf <- dnorm(x, mean = 2.5, sd = 7)
    y.inf <- y.inf/max(y.inf); y.inf <- y.inf - 0.05
    
    x <-x*tmax/x.seq.max # Scale x...
    df = data.frame(x, infection=y, immunization=y.inf)
    ggplot(data = df) +
        aes(x = x) +
        geom_line(aes(y = immunization, color = "immunization")) +
        geom_line(aes(y = infection, color = 'infection')) +
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
