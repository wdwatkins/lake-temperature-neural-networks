#split task name into site id (NHD) and model function
get_model_func_site_id <- function(task_name) {
  model_func <- gsub(pattern = "nhd_[1-9]*_", replacement = "", x = task_name)
  site_id <- str_extract(pattern = "nhd_[1-9]*", string = task_name)
  return(list(site_id, model_func))
}

create_model_task_plan <- function(site_ids, model_function_names, ind_dir) {
  model_function_names <- as.character(model_function_names)
  task_steps <- list(
    create_task_step(step_name = "model_output_ind",
                     target_name = function(task_name, ...) {
                       sprintf("2_model/out/%s.output.rds.ind", task_name)
                     },
                     command = function(task_name, ...) {
                       c(site_id, model_func) %<-% get_model_func_site_id(task_name)
                       sprintf("%s(1_format/out/%s_split_scaled.rds.ind)", model_func, site_id)
                     }),
    create_task_step(step_name = "model_output_rds",
                     target_name = function(steps, ...) {
                       as_data_file(steps[[1]]$target_name)
                     },
                     command = "gd_get(target_name)"),
    create_task_step(step_name = "evaluate",
                     target_name = function(task_name, ...) {
                       sprintf("2_model/doc/%s.html", task_name)
                     },
                     command = function(task_name, steps, ...) {
                       c(site_id, model_func) %<-% get_model_func_site_id(task_name)
                       model_output <- steps[[1]]$target_name
                       formatted_data <- sprintf("1_format/out/%s_spli_scaled.rds.ind", site_id)
                       sprintf("evaluate_model(%s, %s, rmd_file='2_model/src/assessment.Rmd')", model_output, formatted_data)
                     }))
  print(length(model_function_names))
  print(model_function_names)
  task_names <- expand.grid(site_ids, model_function_names) %>%
    unite(col = "task_names", Var1, Var2, sep = "_") %>% .$task_names
  saveRDS(task_names, "task_names.rds")
  model_task_plan <- create_task_plan(
    task_names = task_names,
    task_steps = task_steps,
    final_steps = "evaluate",
    ind_dir = "2_model/log",
    add_complete = FALSE)

  return(model_task_plan)
}

create_model_task_makefile <- function(task_plan, makefile, ...) {
  create_task_makefile(
    task_plan = task_plan,
    makefile = makefile,
    packages = c("dplyr", "tidyr", "keras"),
    sources = c("lib/src/require_local.R", "2_model/src/models.R"),
    include = "2_model.yml",
    ...)
}
