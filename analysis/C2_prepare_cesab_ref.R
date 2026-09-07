# Script to clean references, fetch open alex, and prepare bibliometrix format
#
# input:
#   raw-data/cesab/FRB-CESAB.rdf : export from Zotero in rdf format
#     to keep folder structure of the references
# output:
#   derived-data/cesab_zotero.csv : simplified Zotero export
#   derived-data/cesab_openalex.rdata : openalex data from Cesab DOI
#   derived-data/cesab_bibliometrix.rds: bibliometrix formated dataset
#

# 0. Set-up --------------------------------------
library(xml2)
devtools::load_all()

in_data <- here::here("data", "raw-data", "cesab")
out_data <- here::here("data", "derived-data")

# 1. Simplify Zotero output ----------------------
# Load the RDF file to get the folder structure
rdf <- xml2::read_xml(file.path(in_data, "FRB-CESAB.rdf"))
# Fetch XML namespaces
ns <- xml_ns(rdf)
# Get the collection : folder structure of the references
collections <- xml_find_all(rdf, ".//z:Collection", ns)
col_list <- lapply(collections, function(x) {
  folder_name <- xml_text(xml_find_first(x, ".//dc:title", ns))
  item_nodes <- xml_find_all(x, ".//dcterms:hasPart", ns)
  item_keys <- xml_attr(item_nodes, "resource")
  if (length(item_keys) == 0) {
    item_keys <- ""
  }
  data.frame(
    folder = folder_name,
    item_id = item_keys,
    stringsAsFactors = FALSE
  )
})
col_df <- do.call(rbind, col_list)
# dim(col_df) # 456 raws
# projects with no references yet
# col_df$folder[col_df$item_id == ""]

# Get the article information
articles <- xml_find_all(rdf, ".//bib:Article", ns)
art_list <- lapply(articles, function(x) {
  titles <- xml_find_all(x, ".//dc:title", ns)
  data.frame(
    item_id = noempty(xml_attr(x, "about")),
    item_year = noempty(xml_text(xml_find_all(x, ".//dc:date", ns))),
    item_title = noempty(xml_text(titles[length(titles)])),
    item_keys = noempty(xml_text(xml_find_all(x, ".//z:citationKey", ns)))
  )
})
art_df <- do.call(rbind, art_list)

# Get DOI
journals <- xml_find_all(rdf, ".//bib:Journal", ns)
art_doi <- sapply(journals, function(x) {
  allid <- xml_text(xml_find_all(x, ".//dc:identifier", ns))
  allid <- gsub("^DOI ", "", allid[grep("^DOI ", allid)])
  return(ifelse(length(allid) == 0, "", allid))
})
art_df$DOI <- art_doi

# add Report, BookSection, Book, and Thesis
reports <- xml_find_all(rdf, ".//bib:Report", ns) #3
rep_list <- lapply(reports, function(x) {
  titles <- xml_find_all(x, ".//dc:title", ns)
  id <- xml_attr(x, "about")
  findoi <- regexpr("doi.org/", id)
  data.frame(
    item_id = noempty(xml_attr(x, "about")),
    item_year = noempty(xml_text(xml_find_all(x, ".//dc:date", ns))),
    item_title = noempty(xml_text(titles[length(titles)])),
    item_keys = noempty(xml_text(xml_find_all(x, ".//z:citationKey", ns))),
    DOI = ifelse(findoi != -1, substr(id, findoi + 8, nchar(id)), "")
  )
})
rep_df <- do.call(rbind, rep_list)

books <- xml_find_all(rdf, ".//bib:Book", ns) #1
book_list <- lapply(books, function(x) {
  titles <- xml_find_all(x, ".//dc:title", ns)
  id <- xml_attr(x, "about")
  findoi <- regexpr("doi.org/", id)
  data.frame(
    item_id = noempty(xml_attr(x, "about")),
    item_year = noempty(xml_text(xml_find_all(x, ".//dc:date", ns))),
    item_title = noempty(xml_text(titles[length(titles)])),
    item_keys = noempty(xml_text(xml_find_all(x, ".//z:citationKey", ns))),
    DOI = ifelse(findoi != -1, substr(id, findoi + 8, nchar(id)), "")
  )
})
book_df <- do.call(rbind, book_list)

chapters <- xml_find_all(rdf, ".//bib:BookSection", ns) #22
chapt_list <- lapply(chapters, function(x) {
  titles <- xml_find_all(x, ".//dc:title", ns)
  id <- xml_attr(x, "about")
  findoi <- regexpr("doi.org/", id)
  data.frame(
    item_id = noempty(xml_attr(x, "about")),
    item_year = noempty(xml_text(xml_find_all(x, ".//dc:date", ns))),
    item_title = noempty(xml_text(titles[length(titles)])),
    item_keys = noempty(xml_text(xml_find_all(x, ".//z:citationKey", ns))),
    DOI = ifelse(findoi != -1, substr(id, findoi + 8, nchar(id)), "")
  )
})
chapt_df <- do.call(rbind, chapt_list)

thesis <- xml_find_all(rdf, ".//bib:Thesis", ns) #3
phd_list <- lapply(thesis, function(x) {
  titles <- xml_find_all(x, ".//dc:title", ns)
  id <- xml_attr(x, "about")
  findoi <- regexpr("doi.org/", id)
  data.frame(
    item_id = noempty(xml_attr(x, "about")),
    item_year = noempty(xml_text(xml_find_all(x, ".//dc:date", ns))),
    item_title = noempty(xml_text(titles[length(titles)])),
    item_keys = noempty(xml_text(xml_find_all(x, ".//z:citationKey", ns))),
    DOI = ifelse(findoi != -1, substr(id, findoi + 8, nchar(id)), "")
  )
})
phd_df <- do.call(rbind, phd_list)

# merge all information
art_df <- rbind(art_df, rep_df, book_df, chapt_df, phd_df)

# merge both information
# keeping collection structure to keep duplicated articles
m0 <- match(col_df$item_id, art_df$item_id)
df <- cbind(col_df, art_df[m0, -1])

# simplify DOI with lower case
df$DOI <- tolower(df$DOI)


# clean-up project names
# table(df$folder)
df$folder <- gsub("PPR Oceans", "PPR Océans", df$folder)
# keep only references in CESAB group (or in Core)
# remove Desybel
df <- df[!df$folder %in% "Desybel", ]
# table(df$item_id != "" & is.na(df$item_title))
# View(df)

# Export as csv
write.csv(df, file.path(out_data, "cesab_zotero.csv"), row.names = FALSE)


# 2. Fecth OpenAlex data ----------------------
df <- read.csv(file.path(out_data, "cesab_zotero.csv"))
# remove incomplete DOI
# table(is.na(ref$DOI))
doilist <- df$DOI[!is.na(df$DOI) & df$DOI != ""]

# fetching records from openalex
oa <- openalexR::oa_fetch(
  doi = doilist,
  entity = "works",
  output = 'list',
  verbose = FALSE
)

# save output
save(oa, file = file.path(out_data, "cesab_openalex.rdata"))


# 3. Format as bibliometrix object -----------------
oadata <- file.path(out_data, "cesab_openalex.rdata")

M <- bibliometrix::convert2df(
  file = oadata,
  dbsource = "openalex_api",
  format = "api"
)

# length(M$DI) # 1035
# table(tolower(doilist) %in% M$DI)
Last1 <- sapply(strsplit(M$AU_CORR, " "), function(x) x[[length(x)]])
M$shortname <- paste(Last1, M$PY, sep = "_")
dup <- duplicated(M$shortname)
M$shortname[dup] <- paste(M$shortname[dup], 2, sep = "_")
dup <- duplicated(M$shortname)
M$shortname[dup] <- gsub("_2$", "_3", M$shortname[dup])
# sum(duplicated(M$shortname)) # 0

# add projects per reference
projdoi <- tapply(df$folder, df$DOI, paste, collapse = ", ")
M$project <- projdoi[match(M$DI, names(projdoi))]
# table(M$project)

# add journal abbreviation
# remotes::install_github("patrickbarks/abbrevr")
# library(abbrevr)
conv <- data.frame(
  "ori" = sort(unique(M$SO))
)
conv$abb <- sapply(firstup(conv$ori), abbrevr::AbbrevTitle, USE.NAMES = FALSE)
# conv <- conv[!duplicated(conv), ]
mconv <- match(tolower(M$SO), tolower(conv$ori))

M$shortjournal <- conv$abb[match(M$SO, conv$ori)]


# export
saveRDS(M, file.path(out_data, "cesab_bibliometrix.rds"))

# 4.(extra) Look-up for all citations in OA -----------------
# not working properly yet
# missing the citations in oa
# needs oa_snowball()
#
# oa[[1]]$cited_by_count
# oa[[1]]$referenced_works
# oa[[1]]$related_works
# = length(strsplit(M$CR[1], "; ")[[1]])

## openalexR::oa_snowball doesn't work ...
idlist <- sapply(oa, function(x) gsub("https://openalex.org/", "", x$id))
# df <- read.csv(file.path(out_data, "cesab_zotero.csv"))
# doilist <- df$DOI[!is.na(df$DOI) & df$DOI != ""]
#
# snow_1 <- openalexR::oa_snowball(
#   identifier = oa[[1]]$id
# )
#
# snow_2 <- openalexR::oa_snowball(doi = doilist)
# with idlist or with doilist
#   6.     └─openalexR (local) `<fn>`(...)
#   7.       └─openalexR::oa_request(...)
#   8.         └─openalexR:::api_request(...)
#   9.           └─jsonlite::fromJSON(m, simplifyVector = FALSE)
#  10.             └─jsonlite:::stop("Argument 'txt' must be a JSON string, URL or file.")

# snowball <- openalexR::oa_snowball(identifier = idlist[ncite > 0])

# citing_works <- openalexR::oa_fetch(
#   entity = "works",
#   cites = idlist[ncite > 0]
# )
# ncite <- sapply(oa, function(x) x$cited_by_count)
# # Error:
# # ! Argument 'txt' must be a JSON string, URL or file.

# # save output
# save(snowball, file = file.path(out_data, "cesab_snowball_openalex.rdata"))
