#' Collection of functions to assist in analyses
#' @author carlga ~ Carlos Gallardo
#'


#' Makes duplicated elements in a vector unique (numbers all duplicates)
#' @param x A character vector
#' @param sep A separator character to append numbering
#' @return Deduplicated character vector
#' 
#' @author carlga
#' 
uniquify <- function(x, sep = '.') {
  res <- ave(x, x, FUN = function(x) {
    if(length(x) > 1) {
      paste(x, 1:length(x), sep = sep)
    } 
    else x
  })
  
  return(res)
}


#' Parses metadata from GEO series matrix files
#' @param file Path to file
#' @return Data frame with extracted metadata
#' 
#' @author carlga
#' 
parseGEOSeriesMatrix <- function(file) {
  lines <- grep('^!Sample_', readLines(file))
  start <- min(lines)
  end <- max(lines)
  
  res <- read.table(
    file = file,
    stringsAsFactors = FALSE,
    sep = '\t',
    header = FALSE,
    fill = FALSE,
    quote = '\"',
    skip = start-1,
    nrows = end-start+1
  )
  rownames(res) <- gsub('^!Sample_(.*)$', '\\1', uniquify(res$V1, sep = '_'))
  res <- as.data.frame(t(res[,-1]))
  rownames(res) <- NULL
  
  return(res)
}


