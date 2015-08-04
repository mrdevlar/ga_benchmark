library(GA)
library(TSP)
source("plotTour.R")


data("USCA312", package="TSP")
D = as.matrix(USCA312)
maxCities = attr(USCA312, "Size")


tourLength = function(tour, distMatrix) {
  tour    = c(tour, tour[1])
  route   = embed(tour, 2)[,2:1]
  output  = sum(distMatrix[route])
  
  return(output)
}

tspFitness = function(tour, distMatrix) {
  output = 1 / tourLength(tour, distMatrix)
  
  return(output)
}




# seLection = "gaperm_lrSelection"
# seLection = "gaperm_nlrSelection"
seLection = "gaperm_rwSelection"
# seLection = "gaperm_tourSelection"

crossOver = "gaperm_cxCrossover"
# crossOver = "gaperm_oxCrossover"
# crossOver = "gaperm_pbxCrossover"
# crossOver = "gaperm_pmxCrossover"


# muTation  = "gaperm_simMutation"
# muTation  = "gaperm_ismMutation"
# muTation  = "gaperm_swMutation"
muTation  = "gaperm_dmMutation"
# muTation  = "gaperm_scrMutation"

plotTitle = paste("USCA312", seLection, crossOver, muTation, sep="__")
outFile = paste0(plotTitle, ".txt")

sink(outFile, type="output", append = TRUE)

ptm <- proc.time()

GA <- ga(type = "permutation", 
         fitness = tspFitness, 
         distMatrix = D,
         min = 1, max = maxCities, 
         popSize = 500, 
         maxiter = 5000,
         run = 500, 
         pmutation = 0.2,
         # monitor = plot,
         selection = seLection,
         crossover = crossOver,
         mutation  = muTation)

proc.time() - ptm
sink()

