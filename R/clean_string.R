clean_string <- function(x){
  sapply(x, function(y){
    stringi::stri_trans_general(
      str = y, id = "Latin-ASCII") |>
      tolower() |>
      gsub("-|_|\\.", " ", x=_) |>
      gsub(",", "", x=_) |>
      gsub(" ​ ", " ", x=_) |>
      gsub(" ​", " ", x=_) |>
      gsub("^\\s+|\\s+$|^\\t+|\\t+$", "", x=_) |>
      gsub("\\s{2,}", " ", x=_)  })
}
