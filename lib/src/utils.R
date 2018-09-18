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
  read_csv(file, comment = comment)$site_id
}
