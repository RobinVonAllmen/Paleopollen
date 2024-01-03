#---------------------------------------------------------------------------------------------------------
# This function creates the grouping value for plotting, if no grouping variable is supplied, the taxa will
# be grouped based on their own taxa name.
#---------------------------------------------------------------------------------------------------------

assignGroupToTaxa <- function (ListOfTaxa, AssignedGroup){
  ListOfTaxa <- paste(ListOfTaxa)

  groupAssignments <- vector(length = length(ListOfTaxa))

  if(length(ListOfTaxa)!= length(AssignedGroup)){
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
