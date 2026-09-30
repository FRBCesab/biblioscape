# remotes::install_github("FRBCesab/zoteror")
library(zoteror)
library(igraph)

devtools::load_all()


in_data <- here::here("data", "raw-data", "cesab")
out_data <- here::here("data", "derived-data")

## Import the liste of Cesab members to cross with the list of authors
members <- read.csv2("data/derived-data/cesab_members.csv")
members$group %>% unique() # 73 groups in total

# create a bipartite graph of cesab members and cesab groups, link if member belongs to a group
netBI <- igraph::graph_from_data_frame(
  members[, c("name", "group")]
)
# distinguish between the 2 levels: some are groups (TRUE), some are members (FALSE)
V(netBI)$type <- V(netBI)$name %in% members$group
  
# assign blue squares to groups, red dots to members
V(netBI)$color <- ifelse(V(netBI)$type, "blue", "red")
V(netBI)$shape <- ifelse(V(netBI)$type, "square", "dot")

extra <- ifelse(
  V(netBI)$type,
  "",
  infoV[match(V(netBI)$name, names(infoV))]
)
V(netBI)$title <- paste(
  V(netBI)$name,
  extra,
  sep = "<br>"
)

# remove members that are not connected to multiple groups
# for visualization purpose only
showBI <- delete_vertices(netBI, degree(netBI) == 1)
E(showBI)$arrow.mode <- 0

visNetwork::visIgraph(
  showBI,
  randomSeed = 25,
  layout = "layout_with_fr", #layout_with_kk, layout_nicely
  smooth = TRUE
) |>
  visNetwork::visOptions(
    highlightNearest = TRUE
  )


igraph::degree(netBI, mode = "out") %>% sort(., decreasing = TRUE) %>% barplot()
plot(degree_distribution(netBI, cumulative = TRUE),type="l")


# Graph showing only member vertices that are linked if they share a group ?
g <- bipartite_projection(netBI, multiplicity = TRUE)
g_members <- g$proj1
g_groups <- g$proj2


g_show_mem <- bipartite_projection(showBI, multiplicity = TRUE)$proj1

  
  
visNetwork::visIgraph(
  g_show_mem,
  randomSeed = 25,
  layout = "layout_with_fr", #layout_with_kk, layout_nicely
  smooth = TRUE
) |>
  visNetwork::visOptions(
    highlightNearest = TRUE
  )



## Cesab authorship network : extract all authors from the Cesab publication list, distinguish 2 groups based on whether or not authors are part of a Cesab group
# vertices = authors, links = co-authors, clusters = cesab groups ?
# vertices = publications, links = co-authors, clusters = cesab groups ?

## Get the list of Cesab publications and remove Core
cesab_pub <- zoteror::get_zotero_data("~/Zotero")
df <- cesab_pub |> dplyr::filter(
  collection != "_Core",
  category %in% c("bookSection", "journalArticle"),
  !is.na(doi)
)

## Clean the author names
df <- df |> 
  dplyr::select(collection, year, category, author, journal, doi)

## homogenize full names by keeping the last name and reducing all remaining into initials

# keep original names too
original_names <- sapply(df$author, function(x){x |> stringr::str_split(" ; ")})

initials <- function(name) {
  surname <- stringr::str_extract(name, "^[^,]+") # everything before comma
  firstname   <- stringr::str_extract(name, "(?<=,\\s).*") # everything after comma + whitespace

  init <- firstname |>
    stringr::str_extract_all("[A-Z]", simplify = TRUE) |> # extract all capital letters as a matrix
    paste(collapse = " ")

  paste(surname, init)
}


# apply initials function to each line au co-authors from the publication df:
authors <- sapply(df$author, function(x){
  x |>
  stringr::str_split(" ; ") |>
  unlist() |>
  purrr::map_chr(initials) |>
  purrr::map_chr(clean_string)
})

# Put df in long format such that for a given article, 1 author = 1 line
authors <- lapply(1: length(authors), function(x){
  tab <- data.frame(
    doi = df$doi[[x]],
    collection = df$collection[[x]],
    category = df$category[[x]],
    author = original_names[[x]],
    author_simple = authors[[x]])
    rownames(tab) <- NULL
  return(tab)
}) 

# make dataframe from list
authors <- do.call(rbind, authors)

# add simplified author names to long df
df <- dplyr::left_join(
  df |> dplyr::select(-author),
  authors
)

# now try this with the members names:
members$names_simple <- paste(members$lastname, members$firstname, sep = ", ")
members$names_simple <- sapply(members$names_simple, initials) |>
  sapply(X = _, clean_string)

# add column is_cesab
df$is_cesab <- ifelse(
  df$author_simple %in% members$names_simple,
  TRUE, FALSE
)

# vertices = authors, links = co-authors, clusters = cesab groups ?
# create a bipartite graph of authors, link if co-authors
net_coauth <- igraph::graph_from_data_frame(
  df[, c("author_simple", "doi")]
)

net_c <- delete_vertices(net_coauth, degree(net_coauth) == 1)
E(net_c)$arrow.mode <- 0

visNetwork::visIgraph(
  net_c,
  randomSeed = 25,
  layout = "layout_with_fr", #layout_with_kk, layout_nicely
  smooth = TRUE
) |>
  visNetwork::visOptions(
    highlightNearest = TRUE
  )

# distinguish between the 2 levels: some are groups (TRUE), some are members (FALSE)
V(net_coauth)$type <- V(net_coauth)$name_simple %in% df$doi
  
# assign blue squares to groups, red dots to members
V(net_coauth)$color <- ifelse(V(net_coauth)$type, "blue", "red")
V(net_coauth)$shape <- ifelse(V(net_coauth)$type, "square", "dot")


V(net_coauth)$title <- paste(
  V(net_coauth)$name,
  extra,
  sep = "<br>"
)

# remove members that are not connected to multiple groups
# for visualization purpose only
showBI <- delete_vertices(net_coauth, degree(net_coauth) == 1)
E(showBI)$arrow.mode <- 0

visNetwork::visIgraph(
  showBI,
  randomSeed = 25,
  layout = "layout_with_fr", #layout_with_kk, layout_nicely
  smooth = TRUE
) |>
  visNetwork::visOptions(
    highlightNearest = TRUE
  )


igraph::degree(net_coauth, mode = "out") %>% sort(., decreasing = TRUE) %>% barplot()
plot(degree_distribution(net_coauth, cumulative = TRUE),type="l")









## Get the list of references for each publication ----
# remove incomplete DOI
doilist <- df$doi[!is.na(df$doi)]

## fetching records from openalex
# oa <- openalexR::oa_fetch(
#   doi = doilist,
#   entity = "works",
#   output = 'list',
#   verbose = FALSE
# )

# save output
# save(oa, file = file.path(out_data, "cesab_openalex_cc.rdata"))


load("data/derived-data/cesab_openalex_cc.rdata")
essai <- oa[[1]]
essai |> names()
essai$referenced_works



x <- oa[[1]]

oa_df <- oa |> 
  lapply(., function(x){
    tab <- data.frame(
      oa_id = x$ids$openalex,
      doi = x$doi,
      title = x$title,
      year = x$publication_year,
      type = x$type,



      dplyr::select())


  }

)
  do.call(rbind, .) |>
  as.data.frame()
dim(oa_df)


x |> names()
# merge OA info with the frb_cesab data:

df <- dplyr::left_join(frb_cesab, oa_df)


oadata <- file.path(out_data, "cesab_openalex_cc.rdata")

M <- bibliometrix::convert2df(
  file = oadata,
  dbsource = "openalex_api",
  format = "api"
)





