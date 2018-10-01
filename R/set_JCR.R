#' Obtain JCR information
#'
#' Attribute JCR information to each paper in `lattes.list`. The journal
#' information available in each entry of `lattes.list` is matched
#' against the information in `JCR.file`: first by ISSN and, if no match is
#' found or the ISSN is not informed in `lattes.list`, by name. Journal names
#' are reduced (removal of all spaces and punctuations) and upper-cased
#' before matching, to reduce the chances of name mismatches.
#'
#' @param lattes.list a Lattes list object created using [lattes_to_list()]
#' @param JCR.file CSV file containing the ranking of journals according to
#' the QUALIS classification system for a certain area. See `JCR File` for
#'  details.
#'
#' @section JCR File:
#' The JCR file must be a CSV file with commas as separators and UTF-8
#' encoding. To ensure these properties, follow the steps:
#' - Obtain the relevant XLS file (e.g., from
#' [https://www.researchgate.net/post/New_Impact_factors_2017_for_Journals_are_released_now](https://www.researchgate.net/post/New_Impact_factors_2017_for_Journals_are_released_now) or directly from the Clarivate Analytics Website
#' (if you have access). It is important to obtain the file containing ALL
#' listed journals.
#' - Open the file using your reader of choice, remove the headers and footers
#' and copy the relevant information (including column names).
#' - Open a new spreadsheet on Google Docs, [https://docs.google.com/spreadsheets/u/0/](https://docs.google.com/spreadsheets/u/0/)
#' - Paste everything into the Google Docs Spreadsheet
#' - Download the resulting file as a comma-separated file (**File** -> **Download as** -> **Comma-separated values (.csv, current sheet)**)
#'
#' @return
#' Returns the original _lattes.list_ object updated with JCR information
#' for each paper
#'
#' @export

set_JCR <- function(lattes.list, JCR.file, min.IF, alias.list){

  # read JCR file
  JCR <- utils::read.csv(JCR.file,
                         header = TRUE, sep = ";",
                         encoding = "UTF-8",
                         stringsAsFactors = FALSE)
  invisible(utils::capture.output(aliases <- utils::read.csv(alias.list,
                                                             header = TRUE, sep = ";",
                                                             encoding = "UTF-8",
                                                             stringsAsFactors = FALSE)))

  # Set a "redux" version of the journal titles - avoids conflicts due to
  # punctuation, capitalization, and spacing
  JCR$Titulo.Redux <- gsub("[^[:alnum:]]", "",
                           toupper(JCR$Full.Journal.Title))
  aliases$Titulo.Redux <- gsub("[^[:alnum:]]", "",
                               toupper(aliases$lattes.name))
  aliases$alias.Redux  <- gsub("[^[:alnum:]]", "",
                               toupper(aliases$alias))


  for (j in 1:length(lattes.list)){
    cat("\nRecovering JCR information for CV ", j, "/", length(lattes.list))
    mydoc <- lattes.list[[j]]
    n.pub <- length(mydoc$Papers.pub)
    n.acc <- length(mydoc$Papers.acc)
    if(n.pub){
      mydoc$Papers.pub <- lapply(X      = mydoc$Papers.pub,
                                 FUN    = setJCR,
                                 JCR.df = JCR,
                                 min.IF = min.IF,
                                 aliases = aliases)
    }
    if(n.acc){
      mydoc$Papers.acc <- lapply(X       = mydoc$Papers.acc,
                                 FUN     = setJCR,
                                 JCR.df  = JCR,
                                 min.IF  = min.IF,
                                 aliases = aliases)
    }
    lattes.list[[j]] <- mydoc
  }
  return(lattes.list)
}


setJCR <- function(x, JCR.df, min.IF, aliases){
  # try: matching by journal name (redux)
  jname <- gsub("PRINT$|ONLINE$", "", x$Journal.Redux)
  indx  <- which(gsub("PRINT$|ONLINE$", "", JCR.df$Titulo.Redux) == jname)
  if (length(indx) != 0){
    x$JCR <- as.numeric(JCR.df$Journal.Impact.Factor[indx[1]])
    if (x$JCR >= min.IF) x$ValidJCR <- TRUE
  } else {
    x$JCR <- NA
  }

  # Check for aliases
  if (is.na(x$JCR)){
    alias.indx <- which(jname == gsub("PRINT$|ONLINE$", "", aliases$Titulo.Redux))
    if(length(alias.indx)){
      indx  <- which(gsub("PRINT$|ONLINE$", "", JCR.df$Titulo.Redux) == gsub("PRINT$|ONLINE$", "", aliases$alias.Redux[alias.indx]))
      if (length(indx) != 0){
        x$JCR <- as.numeric(JCR.df$Journal.Impact.Factor[indx[1]])
        if (x$JCR >= min.IF) x$ValidJCR <- TRUE
      }
    }
  }



  return(x)
}

