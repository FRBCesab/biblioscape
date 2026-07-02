title2query <- function(
  x,
  th = 3,
  cat = "[Title]",
  format = c("pubmed", "hal")
) {
  format <- match.arg(format)

  y <- unlist(strsplit(x, " "))
  y <- y[nchar(y) > th]

  if (format == "pubmed") {
    return(paste(paste0(y, "[Title]"), collapse = " AND "))
  } else {
    return(hal_query(as.list(y), field = "title_t"))
  }
}


cleantitle <- function(x) {
  # remove all punctuation
  x <- gsub("[[:punct:]]", " ", tolower(x))
  # remove created triple space
  x <- gsub("   ", " ", x)
  # or double space
  x <- gsub("  ", " ", x)
  # remove ending space if any
  x <- gsub(" $", "", x)
  return(x)
}
