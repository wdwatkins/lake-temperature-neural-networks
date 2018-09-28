# function to sprintf a bunch of key-value (string-variableVector) pairs, then
# paste them together with a good separator for constructing remake recipes
psprintf <- function(..., sep='\n      ') {
  args <- list(...)
  strs <- mapply(function(string, variables) {
    spargs <- if(string == '') list(variables) else c(list(string), as.list(variables))
    do.call(sprintf, spargs)
  }, string=names(args), variables=args)
  paste(strs, collapse=sep)
}

get_site_ids <- function(file, comment = "#") {
  sites <- read_csv(file, comment = comment)
  return(sites$site_id)
}

lookup_lake_name <- function(lake_site_id, priority_lakes) {
  lake_name_row <- priority_lakes %>% filter(site_id == lake_site_id)
  lake_name <- unique(as.character(lake_name_row$lake_name))
  if(length(lake_name) == 0) {
    lake_name <- NA_character_
  }
  assertthat::assert_that(length(lake_name) == 1)
  return(lake_name)
}
