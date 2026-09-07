# Script to clean membership from FRB-MTE-OFB projects
#
# input:
#   raw-data/mte/FRB-MTE-OFB_Members.xlsx
#   raw-data/mte/CESAB_MTE_Members.xlsx
#     from FRB-Sharepoint and Coline's Onedrive
# output:
#   derived-data/mte_members.csv : simplified membership dataset
#

# Load functions and needed package
devtools::load_all()

# 1 From TITLE + YEARS to DOI -------------------
# set folder directory
in_data <- here::here("data", "raw-data", "mte")
out_data <- here::here("data", "derived-data")

cesab <- readxl::read_xlsx(file.path(in_data, "CESAB_MTE_Members.xlsx"))
frb <- readxl::read_xlsx(file.path(in_data, "FRB-MTE-OFB_Members.xlsx"))

# remove accent
frb <- as.data.frame(
    apply(frb, 2, iconv, from = "UTF-8", to = "ASCII//TRANSLIT")
)
cesab <- as.data.frame(
    apply(cesab, 2, iconv, from = "UTF-8", to = "ASCII//TRANSLIT")
)

# all CESAB's group match
cesab$group[!cesab$group %in% frb$group]

# assumption: the CESAB list is more accurate
mte <- rbind(cesab, frb[!frb$group %in% cesab$group, ])
dim(mte) # 245 members

mte <- mte[order(mte$lastname, mte$firstname), ]
write.csv(
    mte,
    file.path(out_data, "mte_members.csv"),
    row.names = FALSE
)

# mte <- read.csv(file.path(out_data, "mte_members.csv"))
# table(mte$group, mte$function_in_group)
