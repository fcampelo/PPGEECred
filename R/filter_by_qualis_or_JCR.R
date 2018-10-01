#' Filter a lattes.list by qualis extract / impact factor
#'
#' Keeps only papers from journals that are ranked into specific Qualis levels
#' or have impact factors greater than a given threshold.
#'
#' @param lattes.list Lattes list (generated internally in [lattes_to_list()])
#' @param which.qualis vector of desired Qualis levels
#' @param min.IF lower threshold for impact factor
#'
#' @return list containing parsed information on accepted journal papers

filter_by_qualis_or_JCR <- function(lattes.list,
                                    which.qualis = c("A1", "A2", "B1"),
                                    min.IF       = 1.5){

  for (j in 1:length(lattes.list)){
    mydoc <- lattes.list[[j]]
    n.pub <- length(mydoc$Papers.pub)
    n.acc <- length(mydoc$Papers.acc)
    if(n.pub){
      names(mydoc$Papers.pub) <- 1:n.pub
      for(k in 1:n.pub){
        myQualis <- mydoc$Papers.pub[[k]]$Qualis
        if (is.na(myQualis)){
          myIF <- mydoc$Papers.pub[[k]]$JCR
          if(is.na(myIF) || myIF < min.IF){
            names(mydoc$Papers.pub)[k] <- "ToRemove"
          }
        } else {
          if(!(myQualis %in% which.qualis)){
            names(mydoc$Papers.pub)[k] <- "ToRemove"
          }
        }
      }
      toRemove <- which(names(mydoc$Papers.pub) == "ToRemove")
      if(length(toRemove)) mydoc$Papers.pub <- mydoc$Papers.pub[-toRemove]
    }

    if(n.acc){
      names(mydoc$Papers.acc) <- 1:n.acc
      for(k in 1:n.acc){
        myQualis <- mydoc$Papers.acc[[k]]$Qualis
        if (is.na(myQualis)){
          myIF <- mydoc$Papers.acc[[k]]$JCR
          if(is.na(myIF) || myIF < min.IF){
            names(mydoc$Papers.acc)[k] <- "ToRemove"
          }
        } else {
          if(!(myQualis %in% which.qualis)){
            names(mydoc$Papers.acc)[k] <- "ToRemove"
          }
        }
      }
      toRemove <- which(names(mydoc$Papers.acc) == "ToRemove")
      if(length(toRemove)) mydoc$Papers.acc <- mydoc$Papers.acc[-toRemove]
    }
    lattes.list[[j]] <- mydoc
  }
  return(lattes.list)
}
