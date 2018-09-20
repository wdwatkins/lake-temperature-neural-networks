evaluate_model <- function(model_list_ind, dat_ind, rmd_file, site_id, output_html) {
  model_list <- as_data_file(model_list_ind)
  dat_ind <- as_data_file(dat_ind)
  lake_name <- lookup_lake_name(site_id)
  rmarkdown::render(
    input = rmd_file,
    output_format = "html_document",
    output_file = output_html,
    output_dir = "2_model/doc")
}
