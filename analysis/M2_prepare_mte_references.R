# Script to clean references, get DOI from title,
#  fetch open alex, and prepare bibliometrix format
#
# input:
#   raw-data/mte/XXX_raw.csv : files containing the citations from proposal
# output:
#   derived-data/mte_references_completed.csv : completed reference list
#   derived-data/mte_references_oa.rdata : openalex data from DOI reference list
#   derived-data/mte_bibliometrix.rds: bibliometrix formated dataset
#

# Load functions and needed package
devtools::load_all()

# 1 From TITLE + YEARS to DOI -------------------
# set folder directory
in_data <- here::here("data", "raw-data", "mte")
out_data <- here::here("data", "derived-data")

# read all raw reference files
ref_list <- list.files(in_data, "_raw.csv$", full.names = TRUE)
df_list <- lapply(ref_list, function(x) {
  read.csv(x)[, c("Project", "DOI", "Title", "Year", "Relation")]
})
df <- do.call(rbind, df_list)
# nrow(df) # 1306 references
# length(unique(df$Project)) # 24 projects

# remove duplicates, and keep Output>Top5>Proposal
df$Relation <- factor(
  df$Relation,
  levels = c("Proposal", "Top5", "Output"),
  ordered = TRUE
)
df <- df[order(df$Project, df$Relation, df$Title, decreasing = TRUE), ]
# remove duplicates # sum(duplicated(ref))
df <- df[!duplicated(df[, c("Project", "DOI", "Title", "Year")]), ]
# nrow(df) # 1255 references without duplicates

# table(df$Project)
# table(df$Project, df$Year)
# table(df$Project, df$Relation)

# Replace missing DOI by NA
df$DOI[df$DOI == ""] <- NA

# check if valid DOI, else replace with NA
# can take some time ...
has_doi <- !is.na(df$DOI)
check_doi <- rcrossref::cr_agency(df$DOI[has_doi]) |> suppressWarnings()
valid_doi <- sapply(check_doi, function(x) "agency" %in% names(x))
df$DOI[has_doi][!valid_doi] <- NA

# Remove missing project or missing DOI or missing title
# useless check in this case
keepR <- !is.na(df$DOI) | (df$Title != "" & !is.na(df$Year))

df <- df[keepR, ]

# how many missing doi
# sum(is.na(df$DOI)) # 752
# missing DOI per project
tapply(is.na(df$DOI), df$Project, sum)

# keep only complete
# table(df$Year, useNA = "ifany")

system.time({
  complete <- title2doi(df, limit = 20, th_year = 2)
})
# takes 15 min with 752 missing DOI, limit = 20, with 92% completion rate
prop.table(table(is.na(complete$DOI)))

write.csv(
  complete,
  file.path(out_data, "mte_references_completed.csv"),
  row.names = FALSE
)


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


oadata <- file.path(out_data, "mte_references_oa.rdata")
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

saveRDS(M, file.path(out_data, "mte_bibliometrix.rds"))
