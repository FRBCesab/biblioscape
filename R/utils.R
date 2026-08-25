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

firstup <- function(x) {
  x <- tolower(x)
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  return(x)
}

noempty <- function(x) {
  if (length(x) == 0) {
    x <- ""
  }
  return(x)
}


breaks_string <- function(x, each = 50, linemax = 3) {
  S <- strsplit(x, " ")
  return(sapply(S, add_br, each = each, linemax = linemax))
}

add_br <- function(s, each = 50, linemax = 3) {
  cs <- cumsum(nchar(s))

  if (max(cs) < each) {
    out <- paste(s, collapse = " ")
  } else {
    nbr <- ifelse(max(cs) > 2 * each, 3, 2)
    th1 <- max(cs) / nbr
    if (nbr == 2) {
      out <- paste(
        paste(s[cs <= th1], collapse = " "),
        paste(s[cs > th1], collapse = " "),
        sep = "<br>"
      )
    } else {
      th2 <- 2 * max(cs) / nbr
      out <- paste(
        paste(s[cs <= th1], collapse = " "),
        paste(s[cs > th1 & cs <= th2], collapse = " "),
        paste(s[cs > th2], collapse = " "),
        sep = "<br>"
      )
    }
  }
  return(out)
}
