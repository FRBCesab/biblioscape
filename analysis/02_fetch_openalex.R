# Load functions and needed package
devtools::load_all()

out_data <- here::here("data", "derived-data")

# load reference list
ref <- read.csv(file.path(out_data, "mte_references_completed.csv"))

# remove incomplete DOI
# table(is.na(ref$DOI))
doilist <- ref$DOI[!is.na(ref$DOI)]

# fetching records from openalex
oa <- openalexR::oa_fetch(
  doi = doilist,
  entity = "works",
  output = 'list',
  verbose = FALSE
)

# save output
save(oa, file = file.path(out_data, "mte_references_oa.rdata"))


# CESAB reference list
ref <- read.csv(here::here(
  "data",
  "raw-data",
  "cesab",
  "Zotero_FRB-CESAB.csv"
))

# remove incomplete DOI
# table(is.na(ref$DOI))
doilist <- ref$DOI[!is.na(ref$DOI)]

# fetching records from openalex
oa <- openalexR::oa_fetch(
  doi = doilist,
  entity = "works",
  output = 'list',
  verbose = FALSE
)

# save output
save(oa, file = file.path(out_data, "cesab_references_oa.rdata"))

oadata <- file.path(out_data, "cesab_references_oa.rdata")
M <- bibliometrix::convert2df(
  file = oadata,
  dbsource = "openalex_api",
  format = "api"
)

# remove duplicated : from 1047 to 1035 rows
M <- M[M$DI != "", ]
# length(M$DI) # 1035
# table(doilist %in% M$DI)
Last1 <- sapply(strsplit(M$AU_CORR, " "), function(x) x[[length(x)]])
M$shortname <- paste(Last1, M$PY, sep = "_")
dup <- duplicated(M$shortname)
M$shortname[dup] <- paste(M$shortname[dup], 2, sep = "_")
dup <- duplicated(M$shortname)
M$shortname[dup] <- gsub("_2$", "_3", M$shortname[dup])
# sum(duplicated(M$shortname)) # 0

saveRDS(M, file.path(out_data, "cesab_bibliometrix.rds"))
