#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| label: setup
#| echo: false
devtools::load_all()
library(igraph)
library(visNetwork)
library(plotly)
library(bibliometrix) # to build citation networks

out_data <- here::here("data", "derived-data")
# citation dataset
M <- readRDS(file.path(out_data, "cesab_bibliometrix.rds"))

M$shorttitle <- substr(firstup(M$display_name), 1, 30)
M$breaktitle <- breaks_string(firstup(M$display_name))
M$linkDOI <- paste0(
  "<a href=\'",
  paste0("https://www.doi.org/", M$DI),
  "\'>",
  M$DI,
  "</a>"
)
#
#
#
#| label: build-network
#| echo: false

# DE: Author keywords
# remove brackets
M$DE <- gsub("\\((.+?)\\)", "", M$DE)
M$DE <- tolower(gsub(" ;", ";", M$DE))
# KeyOcc1 <- bibliometrix::biblioNetwork(
#   M,
#   analysis = "co-occurrences",
#   network = "author_keywords",
#   n = NULL,
#   short = FALSE,
#   sep = ";"
# )
# dim(KeyOcc1) # 933
# dimnames(KeyOcc1)

# CS1 <- conceptualStructure(
#   M,
#   field = "DE",
#   method = "MCA",
#   minDegree = 1,
#   clust = "auto",
#   stemming = FALSE,
#   labelsize = 15,
#   documents = 20,
#   graph = FALSE
# )
# dim(CS1$net) # 405, 933

# manual building of bipartite network
keysplit1 <- sapply(M$DE, strsplit, "; ")
ukey1 <- sort(unique(unlist(keysplit1)))
refkey1 <- sapply(keysplit1, function(x) {
  as.numeric(ukey1 %in% x)
})
dimnames(refkey1)[[1]] <- ukey1
dimnames(refkey1)[[2]] <- M$shortname
nref1 <- rowSums(refkey1)
nkey1 <- colSums(refkey1)

# ID: OpenAlex keywords
# KeyOcc2 <- bibliometrix::biblioNetwork(
#   M,
#   analysis = "co-occurrences",
#   network = "keywords",
#   n = NULL,
#   short = FALSE,
#   sep = ";"
# )
# dim(KeyOcc2) # 590
# dimnames(KeyOcc2)

# try KW_Merged : same as DE
# M$KW_Merged <- gsub("\\((.+?)\\)", "", M$KW_Merged)

# manual building of bipartite network with ID
keysplit2 <- sapply(M$ID, strsplit, "; ")
ukey2 <- sort(unique(unlist(keysplit2)))
refkey2 <- sapply(keysplit2, function(x) {
  as.numeric(ukey2 %in% x)
})
dimnames(refkey2)[[1]] <- ukey2
dimnames(refkey2)[[2]] <- M$shortname
nref2 <- rowSums(refkey2)
nkey2 <- colSums(refkey2)
#
#
#
#
#
#
#| label: tbl-keywords
#| tbl-cap: "Most popular keywords from authors and OpenAlex."

top1 <- nref1[order(nref1, decreasing = TRUE)[1:10]]
top2 <- nref2[order(nref2, decreasing = TRUE)[1:10]]
info <- data.frame(
  "Author_KW" = names(top1),
  "Author_N" = as.numeric(top1),
  "OpenAlex_KW" = names(top2),
  "OpenAlex_N" = as.numeric(top2)
)

# summary(nkey1)
# summary(nkey2)
# err <- which(refkey1["ECOLOGY", ] + refkey2["ECOLOGY", ] == 1)
# M$ID[err]
# M$DE[err]

info
#
#
#
#
#
#
#
#| label: fig-binet
#| fig-cap: "Bipartite network containing the articles (nodes in blue square) and their keywords (nodes in red circle). We kept only keywords that were used by at least 1% of the articles, and articles that used at least 2 keywords."

# simplified bipartite network
# remove keywords used by a single ref,
refkeys1 <- refkey1[nref1 > ncol(refkey1) * 0.01, ]
# remove papers with a single keyword
refkeys1 <- refkeys1[, colSums(refkeys1) > 1]

netKO <- igraph::graph_from_biadjacency_matrix(
  refkeys1,
  mode = "all"
)

V(netKO)$type <- V(netKO)$name %in% dimnames(refkeys1)[[1]]
V(netKO)$color <- ifelse(V(netKO)$type, "blue", "red")
V(netKO)$shape <- ifelse(V(netKO)$type, "square", "dot")

extra <- ifelse(
  V(netKO)$type,
  "",
  M$project[match(V(netKO)$name, M$shortname)]
)

V(netKO)$title <- paste(
  V(netKO)$name,
  extra,
  sep = "<br>"
)

visNetwork::visIgraph(
  netKO,
  layout = "layout_with_fr",
  randomSeed = 25,
  smooth = FALSE,
) |>
  visOptions(
    highlightNearest = TRUE
  )
#
#
#
splitBI <- bipartite_projection(netKO)
netREF <- splitBI$proj1
netKW <- splitBI$proj2
#
#
#
#
#
#| label: fig-pcakey
#| fig-cap: "Loadings of the keywords in the PCA analysis on bipartite network."

# conceptualStructure
# based on termExtraction() then bibliometrix:::factorial()
#  uses on ca::ca and ca::mjca

# MCA : discarded because kept FALSE and TRUE factors : duplicated keywords
# refkeys1_fac <- data.frame(apply(refkeys1, 1, factor), stringsAsFactors = TRUE)
# mca1 <- ade4::dudi.acm(refkeys1_fac, scannf = FALSE, nf = 3)
# mca1 <- ca::mjca(refkeys1_fac)

# CA: using chi-square distances
# not bad but ugly shaped
# ca1 <- ade4::dudi.coa(refkeys1, scannf = FALSE, nf = 3)
# plot(ca1$co[, 1:2], pch = 16)
# plot(ca1$li[, 1:2], pch = 16)

# t-SNE: discarded because no bivariate projection
# https://www.datanovia.com/learn/machine-learning/dimension-reduction/t-sne
# install.packages("Rtsne")
# tsne1 <- Rtsne::Rtsne(
#   refkeys1,
#   check_duplicates = FALSE,
#   perplexity = mean(nkey1),
#   max_iter = 5000
# )
# plot(tsne1$Y, pch = 16)

# PCA
pca1 <- ade4::dudi.pca(
  t(refkeys1),
  #vegan::decostand(t(refkeys1), "hellinger"),
  center = TRUE,
  scale = TRUE,
  scannf = FALSE,
  nf = 2
)

# visual checks
# plot(pca1$co[, 1:2], pch = 16)
# plot(pca1$li[, 1:2], pch = 16)
# plot(pca1$co[, 1], rowSums(refkeys1), pch = 16)
# plot(pca1$li[, 1], colSums(refkeys1), pch = 16)
# barplot(pca1$eig) # 5 dimension would be best

# Low variance explained
# (pca1$eig / sum(pca1$eig)) * 100

# cluster the keywords with modularity from KW network
# nclust <- NbClust::NbClust(
#   pca1$co,
#   distance = "euclidean",
#   method = "kmeans"
# )
# clu <- kmeans(pca1$co, centers = 9)
modKW <- cluster_louvain(netKW) # 0.15
cluKW <- membership(modKW)
# palclu <- colorspace::qualitative_hcl(
#   n = length(unique(cluKW)),
#   palette = "Dark 3"
# )

key_df <- data.frame(
  pca1$co,
  "N" = rowSums(refkeys1),
  "clu" = as.factor(cluKW)
  # "color" = palclu[cluKW]
)

p1 <- plot_ly(key_df) |>
  add_markers(
    x = ~Comp1,
    y = ~Comp2,
    size = ~N,
    color = ~clu,
    #marker = list(color = ~color, line = list(color = ~color)),
    text = row.names(key_df),
    hoverinfo = "text"
  ) |>
  layout(title = "Keywords loadings", showlegend = FALSE) |>
  config(
    modeBarButtons = list(list("toImage")),
    displaylogo = FALSE
  )

p1
#
#
#
project <- M$project[match(colnames(refkeys1), M$shortname)]
pp <- strsplit(project, ", ")
npp <- sapply(pp, length)

palproj <- colorspace::qualitative_hcl(
  n = length(unique(unlist(pp))),
  palette = "Dark 3"
)

ref_df <- data.frame(
  "shortname" = rep(colnames(refkeys1), npp),
  "project" = unlist(pp),
  "PC1" = rep(pca1$li[, 1], npp),
  "PC2" = rep(pca1$li[, 2], npp),
  "color" = palproj[as.factor(unlist(pp))],
  "N" = rep(colSums(refkeys1), npp)
)
ref_df$label <- paste(ref_df$shortname, ref_df$project, sep = "<br>")

p2 <- plot_ly(ref_df) |>
  add_markers(
    x = ~PC1,
    y = ~PC2,
    size = ~N,
    marker = list(color = ~color, line = list(color = ~color)),
    # color = ~project,
    text = ~label,
    hoverinfo = "text",
    # legendgroup = ~project
  ) |>
  layout(title = "References scores") |>
  config(
    modeBarButtons = list(list("toImage")),
    displaylogo = FALSE
  )

p2

# subplot(p1, p2) |>
# layout(showlegend = FALSE, title = 'Side By Side Subplots') |>
# config(
#   modeBarButtons = list(list("toImage")),
#   displaylogo = FALSE
# )
#
#
#
#
#
#

# visNetwork::visIgraph(
#   netKW,
#   layout = "layout_with_fr",
#   idToLabel = FALSE,
#   randomSeed = 25,
#   # smooth = TRUE,
#   type = "full"
# ) |>
#   visOptions(
#     highlightNearest = TRUE
#   )

centP <- data.frame(
  "degree" = degree(netKW),
  "pagerank" = page_rank(netKW)$vector,
  "closeness" = closeness(netKW),
  "betweenness" = betweenness(netKW),
  "lab" = V(netKW)$name
)

plot_ly(centP) |>
  add_markers(
    x = ~betweenness,
    y = ~pagerank,
    size = ~degree,
    #marker = list(color = ~color, line = list(color = ~color)),
    text = ~lab,
    hoverinfo = "text"
  ) |>
  layout(title = "Project centrality") |>
  config(
    modeBarButtons = list(list("toImage")),
    displaylogo = FALSE
  )
#
#
#
#
#
thematicMapResults <- thematicMap(
  M, # 405, 62
  field = "DE",
  n = 250,
  minfreq = 13,
  stemming = FALSE,
  size = 0.3,
  n.labels = 3,
  repel = TRUE,
  cluster = "louvain"
)

plot_ly(thematicMapResults$clusters) |>
  add_markers(
    x = ~centrality,
    y = ~density,
    size = ~freq,
    text = ~words,
    hoverinfo = "text"
  ) |>
  layout(
    title = "Thematic map" # ,
    # shapes = list(
    #   hline(median(thematicMapResults$clusters$density)),
    #   vline(median(thematicMapResults$clusters$centrality))
    # )
  ) |>
  config(
    modeBarButtons = list(list("toImage")),
    displaylogo = FALSE
  )
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| eval: false
#| code-fold: false

tall::tall()
#
#
#
