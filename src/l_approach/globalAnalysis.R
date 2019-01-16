# Network global evaluation/analysis

# Set WD and load data
wd = getwd()
if(grepl("nora", wd)) {
    setwd("~/git/fake-news-project/src/l_approach")
} else {
    # Set anyone's else project directory path, if needed be.
    #setwd()
}
rm(wd)
source("config.R")

user.user.df <- read.delim("../../data/PolitiFactUserUser.txt", header=FALSE)
news.user.df <- read.delim("~/git/fake-news-project/data/PolitiFactNewsUser.txt", header=FALSE)

computeSummaryTableForGraph <- function(g){
    
    
    table <- data.table(
                        "N" = numeric(),
                        "E" = numeric(),
                        "k" = numeric(),
                        "delta" = numeric(),
                        stringsAsFactors = FALSE)
    
    
    E = length(E(g))
    N = length(V(g))
    k = 2*E/N
    delta = 2*E/(N * (N-1))
    
    table <- rbind(table, list( N, E, round(k, 2), round(delta, 6)))
    
    return(table)
}

names(news.user.df) <- c("FakeNewsId", "SharedBy", "Times shared") # Do we need the times shared?


# Construct whole network user-user
users.graph = graph_from_data_frame(user.user.df, directed=F) # For simplicity, we are going to work with an undirected graph.

computeSummaryTableForGraph(users.graph)

ds = degree(users.graph)
ds = as.vector(ds)
length(ds[ds == 0])
hist(ds, main='Graph degree distribution', xlab = "k", ylab = "Frequency", breaks = 100, col = "black")


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
fit.infected = c(3, 3, 7520, 3, 3, 3, 7796, 3, 3, 3)
q.baseline = computeQualityMetric(fit.infected, real.infected); q.baseline

#2nd approach


# 3rd approach
Real <- c(13, 90, 27, 35, 65, 12, 56, 100, 25, 77 )
Model <- c(12, 93, 24,  31, 105, 10, 147, 94, 23,  70)

q.3rd = computeQualityMetric(Model, Real); q.3rd
