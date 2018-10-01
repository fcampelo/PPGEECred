#' Apply filters
#'
#' Apply filters by Qualis / JCR / Year / UseAccepted
#'
#' @param lattes.list a Lattes list object created using [lattes_to_list()]
#' @param use.accepted strategy to use with accepted papers
#'
#' @return
#' Returns a filtered _lattes.list_
#'
#' @export

apply_filters <- function(lattes.list, use.accepted, strict.qualis = TRUE){
  filtered.list <- lattes.list
  for (j in seq(lattes.list)){
    mydoc <- lattes.list[[j]]
    n.pub <- length(mydoc$Papers.pub)
    n.acc <- length(mydoc$Papers.acc)

    # Part I: filter by use.accepted
    if(n.acc){
      if (use.accepted == "yes"){
        mydoc$Papers.pub <- c(mydoc$Papers.pub, mydoc$Papers.acc)
      } else if (use.accepted == "byDOI"){
        for (k in 1:n.acc) {
          if(!(is.na(mydoc$Papers.acc[[k]]$DOI) || mydoc$Papers.acc[[k]]$DOI == "")){
            n.pub <- n.pub + 1
            mydoc$Papers.pub[[n.pub]] <- mydoc$Papers.acc[[k]]
            mydoc$Papers.pub[[n.pub]]$isPublished <- FALSE
          }
        }
      }
      mydoc$Papers.acc <- NULL
    }

    # Part II: filter by year
    if(n.pub){
      toKeep <- sapply(mydoc$Papers.pub,
                       FUN = function(x){x$ValidYear})
      mydoc$Papers.pub <- mydoc$Papers.pub[which(toKeep)]
    }

    # Part III: filter by Qualis || JCR
    n.pub <- length(mydoc$Papers.pub)
    if(n.pub){
      HasQualis <- sapply(mydoc$Papers.pub,
                          FUN = function(x){!is.na(x$Qualis)})
      QualisOK  <- sapply(mydoc$Papers.pub,
                          FUN = function(x){x$ValidQualis})
      IFOK      <- sapply(mydoc$Papers.pub,
                          FUN = function(x){x$ValidJCR})
      if (strict.qualis){
        toKeep <- (HasQualis & QualisOK) | (!HasQualis & IFOK)
      } else {
        toKeep <- (HasQualis & QualisOK) | IFOK
      }

      mydoc$Papers.pub <- mydoc$Papers.pub[which(toKeep)]
    }

    filtered.list[[j]] <- mydoc
  }

  return(filtered.list)

}
