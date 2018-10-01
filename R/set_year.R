#' Set ValidYear flags
#'
#' Set the `ValidYear` flags for each paper in `lattes.list`, based on the
#' start and end years from `years`.
#'
#' @param lattes.list a Lattes list object created using [lattes_to_list()]
#' @param years vector of start and end years,
#'        `years = c(start.year, end.year)`.
#'
#' @return
#' Returns the original _lattes.list_ object updated with the `ValidYear` flag
#' for each paper
#'
#' @export

set_year <- function(lattes.list, years){

  for (j in 1:length(lattes.list)){
    mydoc <- lattes.list[[j]]
    n.pub <- length(mydoc$Papers.pub)
    n.acc <- length(mydoc$Papers.acc)
    if(n.pub){
      mydoc$Papers.pub <- lapply(X      = mydoc$Papers.pub,
                                 FUN    = setYear,
                                 years  = years)
    }
    if(n.acc){
      mydoc$Papers.acc <- lapply(X      = mydoc$Papers.acc,
                                 FUN    = setYear,
                                 years  = years)
    }
    lattes.list[[j]] <- mydoc
  }
  return(lattes.list)
}


setYear <- function(x, years){
  if (years[1] <= x$Year && years[2] >= x$Year){
    x$ValidYear <- TRUE
  }
  return(x)
}

