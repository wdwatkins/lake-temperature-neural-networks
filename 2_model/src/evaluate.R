evaluate_model <- function(model_list, dat, rmd_file, lake_name) {
  rmarkdown::render(
    input = rmd_file,
    output_format = "html_document",
    output_file = sprintf('model_%03d.html', model_list$id),
    output_dir = "2_model/doc")
}
