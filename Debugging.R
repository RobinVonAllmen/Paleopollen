library(devtools)

load_all()

#Debug Data:

taxaList = c("Pinus", "Betula", "Poaceae")
groupingList = c("Trees", "Shrubs", "Grasses")

(debugAssignedGroup <- assignGroupToTaxa(taxaList, groupingList))
(debugColorMap <- createColorMap(debugAssignedGroup, colorblind = T))


#Note that load_all() has made the strsplit1() function available, although it does not exist in the global environment.
exists("assignGroupToTaxa", where = globalenv(), inherits = FALSE)
