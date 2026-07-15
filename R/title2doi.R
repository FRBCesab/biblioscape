# create new columns (for testing)
title2doi <- function(
  df,
  simplified = FALSE,
  word_limit = 4,
  limit = 20,
  th_year = 2,
  th_dist = 10,
  th_score = 70,
  query_title = TRUE
) {
  # check data.frame
  # check columns
  out <- list()
  for (i in 1:nrow(df)) {
    outi <- data.frame(
      "Title" = df$Title[i],
      "Year" = df$Year[i],
      "DOI" = df$DOI[i],
      "score" = NA,
      "title_dist" = NA,
      "new_title" = NA
    )
    if (is.na(outi$DOI)) {
      # look for crossref
      outi <- title2doi_crossref(
        df$Title[i],
        df$Year[i],
        simplified = simplified,
        word_limit = word_limit,
        limit = limit,
        th_year = th_year,
        th_dist = th_dist,
        th_score = th_score
      )
    }
    # if not found in crossref, look at pubmed
    if (is.na(outi$DOI) & require(rentrez)) {
      # look for pubmed
      outi <- title2doi_pubmed(
        df$Title[i],
        df$Year[i],
        simplified = simplified,
        query_title = query_title,
        word_limit = word_limit,
        th_year = th_year,
        th_dist = th_dist,
      )
    }
    # if not found in pubmed, look at HAL
    if (is.na(outi$DOI) & require(odyssey)) {
      # look for pubmed
      outi <- title2doi_hal(
        df$Title[i],
        df$Year[i],
        simplified = simplified,
        word_limit = word_limit,
        th_year = th_year,
        th_dist = th_dist,
      )
    }
    out[[i]] <- outi
  }
  # lapply(out, names)
  complete <- do.call(rbind, out)
  # add other data
  complete <- cbind(complete, df[, !names(df) %in% names(complete)])
  return(complete)
}


title2doi_crossref <- function(
  x,
  y = NULL,
  simplified = FALSE,
  word_limit = 4,
  limit = 50,
  th_year = 1,
  th_dist = 10,
  th_score = 70
) {
  #todo: check x is a string
  out <- data.frame(
    "Title" = x,
    "Year" = y,
    "DOI" = NA,
    "score" = NA,
    "title_dist" = NA,
    "new_title" = NA
  )
  x <- cleantitle(x)
  if (length(strsplit(x, " ")[[1]]) < word_limit) {
    return(out)
  }
  if (!is.null(y)) {
    cri <- rcrossref::cr_works(
      query = x,
      limit = limit,
      sort = "score",
      filter = c(
        from_pub_date = y - th_year,
        until_pub_date = y + th_year
      )
    )
  } else {
    # check y is numeric
    cri <- rcrossref::cr_works(
      query = x,
      limit = limit,
      sort = "score"
    )
  }

  if ("title" %in% names(cri$data)) {
    title_dist <- utils::adist(cleantitle(cri$data$title), x)

    # find the winner based on score and distance
    w <- which.min(title_dist / as.numeric(cri$data$score)**2)
    score_crit <- (title_dist[w] < th_dist | cri$data$score[w] > th_score)

    if (!is.null(y)) {
      # need to have the year of publication
      yrC <- grep("published", names(cri$data))[1]
      if (!is.na(yrC) & !is.na(cri$data[w, yrC])) {
        yi <- as.numeric(substr(cri$data[w, yrC], 1, 4))
        # keep if good year, high score or very low distance
        year_crit <- abs(y - yi) <= th_year
        if (year_crit & score_crit) {
          out$DOI <- cri$data$doi[w]
          out$score <- cri$data$score[w]
          out$title_dist <- title_dist[w]
          out$new_title <- cri$data$title[w]
        }
      }
    } else {
      if (score_crit) {
        out$DOI <- cri$data$doi[w]
        out$score <- cri$data$score[w]
        out$title_dist <- title_dist[w]
        out$new_title <- cri$data$title[w]
      }
    }
  }
  if (simplified) {
    return(out[, c("Title", "Year", "DOI")])
  } else {
    return(out)
  }
}


title2doi_pubmed <- function(
  x,
  y = NULL,
  simplified = FALSE,
  query_title = TRUE,
  word_limit = 4,
  limit = 50,
  th_year = 1,
  th_dist = 10
) {
  #todo: check x is a string of more than five words
  out <- data.frame(
    "Title" = x,
    "Year" = y,
    "DOI" = NA,
    "score" = NA,
    "title_dist" = NA,
    "new_title" = NA
  )
  # simplify title
  x <- cleantitle(x)
  if (length(strsplit(x, " ")[[1]]) < word_limit) {
    return(out)
  }
  #todo: check query_title is binary
  query <- ifelse(query_title, title2query(x), x)

  if (!is.null(y)) {
    #todo: check y is numeric between 1900 and today
    yr_qy <- ifelse(
      th_year == 0,
      paste0(y, "[DP]"),
      paste0(y - th_year, ":", y + th_year, "[DP]")
    )
    query <- paste(query, yr_qy)
  }
  # make the query
  ri <- rentrez::entrez_search(db = "pubmed", term = query)

  if (ri$count > 0) {
    sumi <- rentrez::entrez_summary(
      db = "pubmed",
      id = ri$ids,
      always_return_list = TRUE
    )
    title_dist <- utils::adist(cleantitle(sapply(sumi, function(z) z$title)), x)
    # find the smallest distance
    w <- which.min(title_dist)
    score_crit <- (title_dist[w] < th_dist)

    if (!is.null(y)) {
      if (!is.na(sumi[[w]]$pubdate)) {
        yi <- as.numeric(substr(sumi[[w]]$pubdate, 1, 4))
        # keep if good year, high score or very low distance
        year_crit <- abs(y - yi) <= th_year
        sumi$articleids[which(sumi$articleids$idtype == "doi"), "value"]
        if (year_crit & score_crit) {
          tempi <- sumi[[w]]$articleids
          if ("doi" %in% tempi$idtype) {
            out$DOI <- tempi[which(tempi$idtype == "doi"), "value"]
            out$title_dist <- title_dist[w]
            out$new_title <- sumi[[w]]$title
          }
        }
      }
    } else {
      if (score_crit) {
        tempi <- sumi[[w]]$articleids
        if ("doi" %in% tempi$idtype) {
          out$DOI <- tempi[which(tempi$idtype == "doi"), "value"]
          out$title_dist <- title_dist[w]
          out$new_title <- sumi[[w]]$title
        }
      }
    }
  }
  if (simplified) {
    return(out[, c("Title", "Year", "DOI")])
  } else {
    return(out)
  }
}


title2doi_hal <- function(
  x,
  y = NULL,
  simplified = FALSE,
  word_limit = 4,
  limit = 50,
  th_year = 1,
  th_dist = 10
) {
  #todo: check x is a string of more than five words
  out <- data.frame(
    "Title" = x,
    "Year" = y,
    "DOI" = NA,
    "score" = NA,
    "title_dist" = NA,
    "new_title" = NA
  )
  x <- cleantitle(x)
  if (length(strsplit(x, " ")[[1]]) < word_limit) {
    return(out)
  }

  #todo: check query_title is binary
  query <- odyssey::hal_query(paste0("\"", x, "\"")) |>
    odyssey::hal_select("doiId_s", "producedDateY_i", "title_s")

  if (!is.null(y)) {
    #todo: check y is numeric between 1900 and today
    query <- odyssey::hal_filter(
      query,
      as.character(y - th_year) %TO% as.character(y + th_year),
      "producedDateY_i"
    )
  }
  # make the query
  hi <- try(odyssey::hal_search(query, limit = limit, verbose = FALSE))

  if (hi$response$numFound > 0) {
    hi_title <- sapply(hi$response$docs, function(z) z$title_s[[1]])
    title_dist <- utils::adist(cleantitle(hi_title), x)
    # find the smallest distance
    w <- which.min(title_dist)
    score_crit <- (title_dist[w] < th_dist)

    if (!is.null(y)) {
      if (!is.na(hi$response$docs[[w]]$producedDateY_i)) {
        yi <- as.numeric(hi$response$docs[[w]]$producedDateY_i)
        # keep if good year, high score or very low distance
        year_crit <- abs(y - yi) <= th_year
        if (year_crit & score_crit & !is.null(hi$response$docs[[w]]$doiId_s)) {
          out$DOI <- hi$response$docs[[w]]$doiId_s
          out$title_dist <- title_dist[w]
          out$new_title <- hi$response$docs[[w]]$title_s[[1]]
        }
      }
    } else {
      if (score_crit & !is.null(hi$response$docs[[w]]$doiId_s)) {
        out$DOI <- hi$response$docs[[w]]$doiId_s
        out$title_dist <- title_dist[w]
        out$new_title <- hi$response$docs[[w]]$title_s[[1]]
      }
    }
  }
  if (simplified) {
    return(out[, c("Title", "Year", "DOI")])
  } else {
    return(out)
  }
}
