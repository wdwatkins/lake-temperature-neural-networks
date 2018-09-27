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

lookup_lake_name <- function(lake_site_id) {
  #switch to full lake name crosswalk in the future
  lake_name <- read_csv("lib/crosswalks/pipeline_3_lakes.csv", comment = "#") %>% filter(site_id == lake_site_id) %>%
    .$name %>% unique()
  assertthat::assert_that(length(lake_name) == 1)
  return(lake_name)
}
