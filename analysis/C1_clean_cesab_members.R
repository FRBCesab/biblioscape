# Script to clean CESAB_Members.xlsx and CESAB_Groups.xlsx
# input:
#   raw-data/cesab/CESAB_Members.xlsx
#   raw-data/cesab/CESAB_Groups.xlsx
#     from FRB Sharepoint
#
# output:
#   derived-data/cesab_members.csv : simplified membership dataset
#

# 0. Set-up --------------------------------------
devtools::load_all()

in_data <- here::here("data", "raw-data", "cesab")
out_data <- here::here("data", "derived-data")

# 1. Clean CESAB_Members.xlsx -------------------
cesab <- readxl::read_xlsx(file.path(in_data, "CESAB_Members.xlsx")) |>
  data.frame() # I hate tibbles

# remove workshop participation
cesab <- cesab[, 1:15]

# simplify affiliation
# table(cesab$affiliation[grepl("univ", tolower(cesab$affiliation))])
cesab$affiliation[grepl("univ", tolower(cesab$affiliation))] <- "University"
cesab$affiliation[cesab$affiliation == "UM"] <- "University"

# Membres fondateurs
mf <- c("CIRAD", "CNRS", "FRB", "IFREMER", "INRAE", "IRD", "MNHN", "OFB")
in_mf <- toupper(cesab$affiliation) %in% mf
cesab$affiliation[in_mf] <- toupper(cesab$affiliation[in_mf])

# Other
cesab$affiliation[!cesab$affiliation %in% c("University", mf)] <- "Other"

# should I keep only actual members
# table(cesab$still_member)

# should I remove guest ?
#fmt: skip
cesab$function_in_group[cesab$function_in_group%in%c("Guesi", "guest")] <- "Guest"
#fmt: skip
cesab$function_in_group[cesab$function_in_group%in%c("Membre")] <- "Member"
#fmt: skip
cesab$function_in_group[cesab$function_in_group%in%c("Post-doc")] <- "Post-Doc"
# table(cesab$function_in_group)

# check names and duplicates
cesab$name <- paste(cesab$lastname, cesab$firstname)

dupname <- duplicated(cesab$name)
dupemail <- duplicated(cesab$emails) & !is.na(cesab$emails)
# table(dupname, dupemail)
# changes of emails : ok
# error <- (dupname & !dupemail)
# ceserr <- cesab[cesab$name %in% cesab$name[error], ]
# ceserr[order(ceserr$name), ]
# errors in name spelling : to be corrected
error <- (dupemail & !dupname)
ceserr <- cesab[cesab$emails %in% cesab$emails[error], ]
# ceserr[order(ceserr$name), c("emails", "name")]
cesab$name <- gsub("Guerin Joana", "Guerrin Joana", cesab$name)
cesab$name <- gsub("Rush Adrien", "Rusch Adrien", cesab$name)
cesab$name <- gsub("^Shin Yunne$", "Shin Yunne-Jai", cesab$name)
cesab$name <- gsub("Thackeray Steeve", "Thackeray Stephen", cesab$name)
cesab$name <- gsub("Guillemot Joannès", "Guillemot Joannes", cesab$name)
cesab$name <- gsub("Shatz Bertrand", "Schatz Bertrand", cesab$name)

# simplify Pelagic 1 and 2
cesab$group[grep("^Pelagic", cesab$group)] <- "Pelagic"


# 2. Add starting year from CESAB_Groups.xlsx -------------------
groups <- readxl::read_xlsx(
  file.path(in_data, "CESAB_Groups.xlsx"),
  skip = 1
) |>
  data.frame() # I still hate tibbles

m0 <- match(tolower(cesab$group), tolower(groups$Acronym))

cesab$start_year <- groups[m0, "Année.appel"]

# missing PPR Oceans, 2022
# cesab$group[is.na(m0)]
cesab$start_year[is.na(cesab$start_year)] <- "2022"

# replace NC by 2026
cesab$start_year[cesab$start_year == "NC"] <- "2026"
# table(cesab$start_year)

# 3. Export file
write.csv(
  cesab,
  here::here("data", "derived-data", "cesab_members.csv"),
  row.names = FALSE
)
