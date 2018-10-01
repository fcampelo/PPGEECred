#' Extract accepted journal papers
#'
#' Extracts accepted journal papers from Lattes list.
#'
#' This function extracts relevant information on accepted journal papers
#' from a Lattes list.
#'
#' @param x Lattes list (generated internally in [lattes_to_list()])
#'
#' @return list containing parsed information on accepted journal papers
#'
#' @export

get_accepted_papers <- function(x){

  # Get number of items
  n.items <- length(x$`PRODUCAO-BIBLIOGRAFICA`$`ARTIGOS-ACEITOS-PARA-PUBLICACAO`)

  if (n.items){
    outlist <- vector(mode = "list", length = n.items)
    for (j in 1:n.items){
      # Isolate item and count authors
      item <- lapply(FUN = as.list,
        X = x$`PRODUCAO-BIBLIOGRAFICA`$`ARTIGOS-ACEITOS-PARA-PUBLICACAO`[[j]])
      authindx  <- which(names(item) == "AUTORES")
      n.authors <- length(authindx)

      # prepare output structure
      outlist[[j]] <- list(Authors     = character(n.authors),
                           Title       = character(1),
                           Journal     = character(1),
                           ISSN        = character(1),
                           Year        = character(1),
                           DOI         = character(1),
                           Qualis      = "Z",
                           JCR         = 0,
                           ValidYear   = FALSE,
                           ValidJCR    = FALSE,
                           ValidQualis = FALSE)

      outlist[[j]]$Year    <- item$`DADOS-BASICOS-DO-ARTIGO`$`ANO-DO-ARTIGO`
      outlist[[j]]$Title   <- item$`DADOS-BASICOS-DO-ARTIGO`$`TITULO-DO-ARTIGO`
      outlist[[j]]$Journal <- item$`DETALHAMENTO-DO-ARTIGO`$`TITULO-DO-PERIODICO-OU-REVISTA`
      outlist[[j]]$ISSN    <- item$`DETALHAMENTO-DO-ARTIGO`$ISSN
      outlist[[j]]$DOI     <- item$`DADOS-BASICOS-DO-ARTIGO`$DOI
      for (k in seq(n.authors)){
        outlist[[j]]$Authors[k] <- item[[authindx[k]]]$`NOME-COMPLETO-DO-AUTOR`
      }
      outlist[[j]]$Journal.Redux <- gsub("[^[:alnum:]]", "",
                                         toupper(outlist[[j]]$Journal))
      outlist[[j]]$ISSN.Redux    <- gsub("[^[:alnum:]]", "",
                                         outlist[[j]]$ISSN)
      outlist[[j]]$Title.Redux   <- gsub("[^[:alnum:]]", "",
                                         outlist[[j]]$Title)
    }
    return(outlist)
  } else return(vector(mode = "list", length = 0))
}
