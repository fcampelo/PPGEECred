#' Function to find and tag duplicated entries (coauthorships)
#'
#' Find and tag duplicated entries (coauthorships) based on:
#'
#' 1. DOI
#' 2. Title (using partial matching accepting up to 5% differences in the names)
#'
#' @param filtered.list list of productions filtered by year, Qualis/JCR, and
#' publication status (accepted vs. published)
#'
#' @return Data frame summarizing the productions and co-authorship information
#'
#' @export

find_coauthorships <- function(filtered.list, match.treshold = 0.05){

  # Assemble data frame with publication information
  AllProductions <- data.frame(Prof.ID        = numeric(),
                               Prof.Name      = character(),
                               Paper.ID       = character(),
                               Paper.Authors  = character(),
                               Paper.Title    = character(),
                               Paper.Year     = numeric(),
                               Paper.DOI      = character(),
                               Journal.Name   = character(),
                               Journal.Volume = character(),
                               Journal.ISSN   = character(),
                               Journal.IF     = numeric(),
                               Journal.Qualis = character(),
                               Title.Redux    = character(),
                               Journal.Redux  = character(),
                               ISSN.Redux     = character())
  tmp.df <- AllProductions

  # Assemble data frame with all productions
  for (j in seq(filtered.list)){
    mydoc <- filtered.list[[j]]
    n.pub <- length(mydoc$Papers.pub)

    # Extract relevant info and populate dataframe
    if(n.pub){
      for (k in seq(n.pub)){
        mydf  <- tmp.df
        item  <- mydoc$Papers.pub[[k]]
        item$Authors <- paste0(item$Authors, collapse = "; ")
        mydf[1, "Prof.ID"]    <- j
        mydf$Prof.Name        <- mydoc$Nome
        mydf$Paper.Authors    <- item$Authors
        mydf$Paper.Title      <- item$Title
        mydf$Paper.Year       <- item$Year
        mydf$Paper.DOI        <- item$DOI
        mydf$Journal.Name     <- item$Journal
        mydf$Journal.Volume   <- ifelse(is.null(item$Volume),
                                                yes = "NP",
                                                no  = item$Volume)
        mydf$Journal.ISSN     <- item$ISSN
        mydf$Journal.IF       <- item$JCR
        mydf$Journal.Qualis   <- item$Qualis
        mydf$Title.Redux      <- gsub("[^[:alnum:]]", "", toupper(item$Title))
        mydf$Journal.Redux    <- item$Journal.Redux
        mydf$ISSN.Redux       <- item$ISSN.Redux

        AllProductions <- rbind(AllProductions, mydf)
      }
    }
  }
  AllProductions$Paper.ID <- 1:nrow(AllProductions)

  # Find co-occurrences by DOI
  cat("\nMapping coauthorships by DOI")
  coAuth.byDOI <- sapply(AllProductions$Paper.DOI,
                         FUN = function(x){
                           as.list(which(AllProductions$Paper.DOI == x))
                         })
  names(coAuth.byDOI) <- NULL
  null.indx <- which(AllProductions$Paper.DOI == "") # Get empty DOIs
  coAuth.byDOI[null.indx] <- as.list(null.indx)      # and fix false positives


  # Find co-occurrences by Title
  cat("\nMapping coauthorships by Title")
  coAuth.byNames <- sapply(X   = AllProductions$Title.Redux,
                           FUN = function(x){
                             # Do partial matching to account for subtle
                             # differences in input. Up to 5% differences are
                             # considered acceptable.
                             as.list(agrep(x, AllProductions$Title.Redux,
                                           max.distance = match.treshold))})
  names(coAuth.byNames) <- NULL

  # Merge DOI and name information and extract the detected co-Authorships
  coAuth.indices <- apply(cbind(coAuth.byDOI, coAuth.byNames),
                          MARGIN = 1 , FUN = function(x){unique(unlist(x))})

  AllProductions$Coauthorship.Indices    <- coAuth.indices
  AllProductions$n.Authors               <- sapply(coAuth.indices, length)
  AllProductions$Effective.Coauthorships <- coAuth.indices
  AllProductions$Effective.n.Authors     <- AllProductions$n.Authors

  return(AllProductions)
}
