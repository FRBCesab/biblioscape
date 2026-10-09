# remotes::install_github("FRBCesab/zoteror")
library(zoteror)
library(igraph)

devtools::load_all()


in_data <- here::here("data", "raw-data", "cesab")
out_data <- here::here("data", "derived-data")

## Import the liste of Cesab members to cross with the list of authors
cesab <- read.csv2(here::here(out_data, "cesab_members.csv"))
cesab$group %>% unique() # 73 groups in total


# remove guest and stagiaire
cesab <- cesab[!cesab$function_in_group %in% c("Guest", "Stagiaire"), ]

# remove non active members (and keep NA)
cesab <- cesab[!cesab$still_member %in% 0, ]

# add a column with merged Liastname and Firtsname
cesab$name <- paste(cesab$lastname, cesab$firstname, sep = " ")

# remove duplicates (mostly for Pelagic 1 and 2)
cesab <- cesab[!duplicated(cesab[, c("group", "name")]), ]

infoV <- tapply(cesab$group, cesab$name, paste, collapse = "<br>")

nmem <- table(cesab$group)
nproj <- table(table(cesab$name))

# create a bipartite graph of cesab cesab and cesab groups, link if member belongs to a group
netBI <- igraph::graph_from_data_frame(
  cesab[, c("name", "group")]
)
# distinguish between the 2 levels: some are groups (TRUE), some are cesab (FALSE)
V(netBI)$type <- V(netBI)$name %in% cesab$group
  
# assign blue squares to groups, red dots to cesab
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

# remove cesab that are not connected to multiple groups
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
g_cesab <- g$proj1
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
# vertices = authors, links = co-authors, colors = cesab groups ?
# aim : to plot a network where 2 authors are linked if co-authors and see if external colaborators are clustered together, 
# or if they are linked to the Cesab community by only 1 link ?


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

# now try this with the cesab names:
cesab$names_simple <- paste(cesab$lastname, cesab$firstname, sep = ", ")
cesab$names_simple <- sapply(cesab$names_simple, initials) |>
  sapply(X = _, clean_string)

# add column is_cesab
df$is_cesab <- ifelse(
  df$author_simple %in% cesab$names_simple,
  TRUE, FALSE
)



# vertices = authors, links = co-authors, clusters = cesab groups ?
# create a bipartite graph of authors, link if co-authors
net_coauth <- igraph::graph_from_data_frame(
  df[, c("author_simple", "doi")]
) 
 
# distinguish between the 2 levels: some are cesab cesab (TRUE), some are external (FALSE)
V(net_coauth)$is_cesab <- ifelse(
  V(net_coauth)$name %in% cesab$names_simple,
  TRUE, FALSE
)

# assign blue dots to cesab members, red dots to external, green squares for papers
V(net_coauth)$color <- ifelse(V(net_coauth)$name %in% cesab$names_simple, "blue", "red")
V(net_coauth)$color[which(V(net_coauth)$name %in% df$doi)] <- "green"
V(net_coauth)$shape <- ifelse(V(net_coauth)$name %in% df$doi, "square", "dot")


# simplify network and remove unique links: see whether interconnections are more intense between cesab memebers or with the outside ?
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




# now make 2 separate bipartite networks: 
# - 1 for publications (DOIs). Vertices
# - 1 for authors


V(net_coauth)$type <- V(net_coauth)$name %in% df$author_simple
split_coauth <- bipartite_projection(net_coauth, multiplicity = TRUE)

netpub <- split_coauth$proj1

# plot network showing both types of vertices : 
netpub_c <- delete_vertices(netpub, degree(netpub) == 1)
E(netpub_c)$arrow.mode <- 0

visNetwork::visIgraph(
  netpub_c,
  randomSeed = 25,
  layout = "layout_with_fr", #layout_with_kk, layout_nicely
  smooth = TRUE
) |>
  visNetwork::visOptions(
    highlightNearest = TRUE
  )






netauth <- split_coauth$proj2

# assign blue squares to groups, red dots to cesab
V(netauth)$color <- ifelse(V(netauth)$is_cesab, "blue", "red")
V(netauth)$shape <- ifelse(V(netauth)$is_cesab, "square", "dot")

# plot network showing both types of vertices : 
netauth_c <- delete_vertices(netauth, degree(netauth) == 1)
E(netauth_c)$arrow.mode <- 0

visNetwork::visIgraph(
  netauth_c,
  randomSeed = 25,
  layout = "layout_with_fr", #layout_with_kk, layout_nicely
  smooth = TRUE
) |>
  visNetwork::visOptions(
    highlightNearest = TRUE
  )



# remove cesab that are not connected to multiple groups
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
igraph::degree(netauth, mode = "out") %>% sort(., decreasing = TRUE) %>% barplot()
plot(degree_distribution(net_coauth, cumulative = TRUE),type="l")






### Openalex : retrive fields and keywords
options(openalexR.mailto = "camille.coux@fondationbiodiversite.fr")

dois <- unique(cesab_pub$doi)[1:10]

res <- openalexR::oa_fetch(
  entity  = "works",
  doi     = dois,
  verbose = TRUE
)

# Topics are in long format: one row per hierarchy level
# (topic, subfield, field, domain), so pivot to one row per topic
topics_df <- res |>
  dplyr::select(doi, topics) |>
  tidyr::unnest(topics) |>
  dplyr::select(doi, i, type, display_name) |>
  tidyr::pivot_wider(names_from = type, values_from = display_name)

topics <- unique(topics_df$topic)
subfield <- unique(topics_df$subfield)
field <- unique(topics_df$field)


keywords_df <- res |>
  dplyr::select(doi, keywords) |>
  tidyr::unnest(keywords)

kw_ex <- keywords_df |>
  dplyr::filter(doi ==  "https://doi.org/10.1111/gcb.13201") |>
  dplyr::select(display_name) |> 
  as.vector()

table(keywords_df$doi) |> as.vector()


concepts_df <- res |>
  dplyr::select(doi, concepts) |>
  tidyr::unnest(concepts)

concepts_ex <- concepts_df |>
  dplyr::filter(doi ==  "https://doi.org/10.1111/gcb.13201") |>
  dplyr::select(display_name) |> 
  as.vector()










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





