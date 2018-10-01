#' Filter a lattes.list by year
#'
#' Removes papers that fall outside a given time window from a given
#' `lattes.list` object.
#'
#' @param lattes.list Lattes list (generated internally in [lattes_to_list()])
#' @param start.year initial year of the interval
#' @param end.year final year of the interval
#'
#' @return list containing parsed information on accepted journal papers

filter_by_year <- function(lattes.list,
                           start.year = 0,
                           end.year   = 10000){

  for (j in 1:length(lattes.list)){
    mydoc <- lattes.list[[j]]
    n.pub <- length(mydoc$Papers.pub)
    n.acc <- length(mydoc$Papers.acc)
    if(n.pub){
      names(mydoc$Papers.pub) <- 1:n.pub
      for(k in 1:n.pub){
        myear <- as.numeric(mydoc$Papers.pub[[k]]$Year)
        if(myear < start.year || myear > end.year) {
          names(mydoc$Papers.pub)[k] <- "ToRemove"
        }
      }
      toRemove <- which(names(mydoc$Papers.pub) == "ToRemove")
      if(length(toRemove)) mydoc$Papers.pub <- mydoc$Papers.pub[-toRemove]
    }

    if(n.acc){
      names(mydoc$Papers.acc) <- 1:n.acc
      for(k in 1:n.acc){
        myear <- as.numeric(mydoc$Papers.acc[[k]]$Year)
        if(myear < start.year || myear > end.year) {
          names(mydoc$Papers.acc)[k] <- "ToRemove"
        }
      }
      toRemove <- which(names(mydoc$Papers.acc) == "ToRemove")
      if(length(toRemove)) mydoc$Papers.acc <- mydoc$Papers.acc[-toRemove]
    }
    lattes.list[[j]] <- mydoc
  }
  return(lattes.list)
}
