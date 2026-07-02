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
