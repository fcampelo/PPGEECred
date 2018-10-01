#' Function to calculate the accreditation scores
#'
#' Calculate the accreditation scores for PPGEE/UFMG
#'
#' @param AllProductions data frame returned by [find_coauthorships()]
#' @param ignore.list list of full names to ignore when calculating scores.
#'
#' @return Updated `Allproductions` plus data frame with scores for each
#' professor.
#'
#' @export

calculate_scores <- function(AllProductions, ignore.list = character(0)){
  # Filter out names in ignore.list
  indx  <- sapply(AllProductions$Prof.Name,
                  FUN = function(x){return(!(x %in% ignore.list))})
  niter <- length(grep("Paper Value", names(AllProductions)))
  AllProductions <- cbind(AllProductions, 0)
  nc <- ncol(AllProductions)
  names(AllProductions)[nc] <- "V1"

  for (k in seq(nrow(AllProductions))){
    item <- AllProductions[k, ]
    if(indx[k] || niter == 0){
      mycoauths <- unlist(item$Coauthorship.Indices)
      mycoauths <- mycoauths[which(sapply(mycoauths,
                                          function(x){x %in% which(indx)}))]
      item$Effective.Coauthorships <- list(mycoauths)
      item$Effective.n.Authors     <- length(mycoauths)
      item$V1 <- 1 / item$Effective.n.Authors
    } else {
      item$Effective.Coauthorships <- NA
      item$Effective.n.Authors     <- NA
      item$V1 <- NA
    }
    AllProductions[k, ] <- item
  }
  names(AllProductions)[nc] <- paste0("Paper Value (Iteration ",
                                      niter + 1, ")")

  # Calculate score for each professor
  Prof.Scores <- as.data.frame(cbind(N.papers = rep(1, nrow(AllProductions)),
                                     `Total Score` = AllProductions[, nc]),
                               stringsAsFactors = FALSE)


  Prof.Scores <- stats::aggregate(x   = Prof.Scores,
                                  by  = list(AllProductions$Prof.Name),
                                  FUN = sum)
  names(Prof.Scores)[1] <- "Prof.Name"
  return(list(AllProductions = AllProductions,
              Prof.Scores    = Prof.Scores))
}
