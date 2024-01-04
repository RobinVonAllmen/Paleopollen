#' @title Assigns each taxa variable with a group variable
#' @description This function creates the grouping value for plotting, if no grouping variable is supplied, the taxa will
#' be grouped based on their own taxa name.
#'
#' @param ListOfTaxa A vector containing a list of all taxa in the analysis. All values are converted into Strings.
#' @param AssignedGroup A vector that matches each taxa to a group, or if not provided each taxa forms it's own group.
#'
#' @return A named vector matching each taxa to a group.
#' @export
#'
#' @examples
#' taxaList <- c("Pinus", "Betula", "Poaceae", "Picea", "Artemisia")
#' groupList <- c("Trees", "Shrubs", "Grasses", "Trees", "Herbs")
#' assignGroupToTaxa(taxaList, groupList)
#'
#' #>   Pinus    Betula   Poaceae     Picea      Artemisia
#' #>   "Trees"  "Shrubs" "Grasses"   "Trees"   "Herbs"
#'
#' assignGroupToTaxa(taxaList)
#'
#' #>  Pinus      Betula     Poaceae       Picea   Artemisia
#' #>  "Pinus"    "Betula"   "Poaceae"     "Picea" "Artemisia"


assignGroupToTaxa <- function (ListOfTaxa, AssignedGroup = NULL){
  ListOfTaxa <- paste(ListOfTaxa)

  groupAssignments <- vector(length = length(ListOfTaxa))

  if(is.null(AssignedGroup)){
    groupAssignments <- ListOfTaxa
  }
  else if(length(ListOfTaxa)!= length(AssignedGroup)){
    warning(call.=FALSE, immediate. = TRUE, sprintf("The list of taxa (%d) and the list
    of assigned group(%d) are not equal in length. Group variable is based on Taxa name!", length(ListOfTaxa), length(AssignedGroup)))

    groupAssignments <- ListOfTaxa
  }

  else{
    groupAssignments <- AssignedGroup
  }

  groupAssignments <- setNames(groupAssignments, ListOfTaxa)
  return(groupAssignments)
}
