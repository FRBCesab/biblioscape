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

pval <- function(
  x,
  d,
  mode = c("all", "high", "low"),
  bk = c(0, 0.001, 0.01, 0.05, 0.1, 1),
  lab = c("***", "**", "*", ".", " "),
  na.rm = TRUE
) {
  match.arg(mode, c("all", "high", "low"))

  if (na.rm) {
    d <- d[!is.na(d)]
  }
  n <- length(d)

  if (mode == "all") {
    comp <- c(sum(d > x) / n, sum(d < x) / n)
    pval <- 2 * min(comp)
    sig <- cut(pval, bk, lab, include.lowest = TRUE)
    sig <- ifelse(
      pval < bk[length(bk) - 1],
      paste0(c("+", "-")[which.min(comp)], sig),
      as.character(sig)
    )
  }
  if (mode == "high") {
    pval <- sum(d > x) / n
    sig <- cut(pval, bk, lab, include.lowest = TRUE)
  }
  if (mode == "low") {
    pval <- sum(d < x) / n
    sig <- cut(pval, bk, lab, include.lowest = TRUE)
  }
  return(data.frame("pval" = pval, "sig" = as.character(sig)))
}

panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...) {
  #usr <- par("usr")
  usr <- par()$usr
  on.exit(par(usr = usr))
  par(usr = c(0, 1, 0, 1))
  r <- cor(x, y, use = "complete.obs")
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste(prefix, txt, sep = "")
  if (missing(cex.cor)) {
    cex <- 0.5 / strwidth(txt)
  }

  test <- cor.test(x, y, use = "complete.obs")
  # borrowed from printCoefmat
  Signif <- symnum(
    test$p.value,
    corr = FALSE,
    na = FALSE,
    cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
    symbols = c("***", "**", "*", ".", " ")
  )

  xtxt <- ifelse(par()$xlog, 10^0.5, 0.5)
  ytxt <- ifelse(par()$ylog, 10^0.5, 0.5)
  xstar <- ifelse(par()$xlog, 10^0.8, 0.8)
  ystar <- ifelse(par()$ylog, 10^0.8, 0.8)
  text(xtxt, ytxt, txt, cex = cex * abs(r))
  text(xstar, ystar, Signif, cex = cex, col = 2)
}
