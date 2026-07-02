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
nrow(df) # 1291 references
length(unique(df$Project)) # 24 projects

# remove duplicates, and keep Output>Top5>Proposal
df$Relation <- factor(
  df$Relation,
  levels = c("Proposal", "Top5", "Output"),
  ordered = TRUE
)
df <- df[order(df$Project, df$Relation, df$Title, decreasing = TRUE), ]
# remove duplicates # sum(duplicated(ref))
df <- df[!duplicated(df[, c("Project", "DOI", "Title", "Year")]), ]
nrow(df) # 1239 references without duplicates

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
sum(is.na(df$DOI)) # 792
# missing DOI per project
tapply(is.na(df$DOI), df$Project, sum)

# keep only complete
system.time({
  complete <- title2doi(df, limit = 20, th_year = 2)
})
# takes 25 min with 491 missing DOI, limit = 20, with 92% completion rate
prop.table(table(is.na(complete$DOI)))

write.csv(
  complete,
  file.path(out_data, "mte_references_completed.csv"),
  row.names = FALSE
)
