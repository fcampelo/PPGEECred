#' Obtain QUALIS information
#'
#' Attribute QUALIS information to each paper in `lattes.list`. The journal
#' information available in each entry of `lattes.list` is matched
#' against the information in `qualis.file`: first by ISSN and, if no match is
#' found or the ISSN is not informed in `lattes.list`, by name. Journal names
#' are reduced (removal of all spaces and punctuations) and upper-cased
#' before matching, to reduce the chances of name mismatches.
#'
#' @param lattes.list a Lattes list object created using [lattes_to_list()]
#' @param qualis.file CSV file containing the ranking of journals according to
#' the QUALIS classification system for a certain area. See `Qualis File` for details.
#' @param which.qualis vector of valid qualis levels (to flag each paper's
#' `ValidQualis` entry)
#'
#' @section Qualis File:
#' The qualis file must be a CSV file with commas as separators and UTF-8
#' encoding. To ensure these properties, follow the steps:
#' - Generate the relevant file from
#' [https://sucupira.capes.gov.br/sucupira/public/consultas/coleta/veiculoPublicacaoQualis/listaConsultaGeralPeriodicos.jsf](https://sucupira.capes.gov.br/sucupira/public/consultas/coleta/veiculoPublicacaoQualis/listaConsultaGeralPeriodicos.jsf),
#' selecting only the fields _Evento de Classificacao_ and _Area de Avaliacao_
#' (leave the others blank).
#' - Download the resulting **xls** file.
#' - Open the file using your reader of choice, select all and copy
#' - Open a new spreadsheet on Google Docs, [https://docs.google.com/spreadsheets/u/0/](https://docs.google.com/spreadsheets/u/0/)
#' - Paste everything into the Google Docs Spreadsheet
#' - Download the resulting file as a comma-separated file (**File** -> **Download as** -> **Comma-separated values (.csv, current sheet)**)
#'
#' @return
#' Returns the original _lattes.list_ object updated with Qualis information
#' for each paper
#'
#' @export

set_qualis <- function(lattes.list, qualis.file, which.qualis, sep = ";"){

  # read qualis file
  qualis <- utils::read.csv(qualis.file,
                            header = TRUE, sep = sep,
                            encoding = "UTF-8",
                            stringsAsFactors = FALSE)

  # Set a "redux" version of the journal titles - avoids conflicts due to
  # punctuation, capitalization, and spacing
  qualis$Titulo.Redux <- gsub("[^[:alnum:]]", "", toupper(qualis$`Título`))
  qualis$ISSN.Redux   <- gsub("[^[:alnum:]]", "", qualis$ISSN)

  for (j in 1:length(lattes.list)){
    mydoc <- lattes.list[[j]]
    n.pub <- length(mydoc$Papers.pub)
    n.acc <- length(mydoc$Papers.acc)
    if(n.pub){
      mydoc$Papers.pub <- lapply(X            = mydoc$Papers.pub,
                                 FUN          = setqualis,
                                 qualis.df    = qualis,
                                 which.qualis = which.qualis)
    }
    if(n.acc){
      mydoc$Papers.acc <- lapply(X            = mydoc$Papers.acc,
                                 FUN          = setqualis,
                                 qualis.df    = qualis,
                                 which.qualis = which.qualis)
    }
    lattes.list[[j]] <- mydoc
  }
  return(lattes.list)
}


setqualis <- function(x, qualis.df, which.qualis){
  if (!("ISSN.Redux" %in% names(x))){
    x$ISSN.Redux    <- gsub("[^[:alnum:]]", "", x$ISSN)
  }
  if (!("Journal.Redux" %in% names(x))){
    x$Journal.Redux <- gsub("[^[:alnum:]]", "", toupper(x$Journal))
  }
  if (!("Title.Redux" %in% names(x))){
    x$Title.Redux <- gsub("[^[:alnum:]]", "", toupper(x$Title))
  }
  x$ValidQualis <- FALSE

  if(x$ISSN != ""){ # first try: matching by ISSN
    jISSN <- x$ISSN.Redux
    indx  <- which(qualis.df$ISSN.Redux == jISSN)
    if (length(indx) != 0){
      x$Qualis <- sort(qualis.df$Estrato[indx])[1] # get highest qualis in case of ambiguity
      if(x$Qualis %in% which.qualis) {
        x$ValidQualis <- TRUE
      }
    } else x$Qualis <- NA
  }

  # Second try: exact match by journal name
  if(is.na(x$ValidQualis) || x$ValidQualis == FALSE){
    jname <- x$Journal.Redux
    indx  <- which(qualis.df$Titulo.Redux == jname)
    if (length(indx) != 0){
      x$Qualis <- sort(qualis.df$Estrato[indx])[1] # get highest qualis in case of ambiguity
      if(x$Qualis %in% which.qualis) {
        x$ValidQualis <- TRUE
      }
    } else x$Qualis <- NA
  }
  return(x)
}
