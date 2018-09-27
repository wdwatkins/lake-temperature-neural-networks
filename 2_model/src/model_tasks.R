#split task name into site id (NHD) and model function
get_model_func_site_id <- function(task_name) {
  model_func <- gsub(pattern = "nhd_[0-9]*_", replacement = "", x = task_name)
  site_id <- str_extract(pattern = "nhd_[0-9]*", string = task_name)
  return(list(site_id, model_func))
}

create_model_task_plan <- function(site_ids, model_function_names) {
  model_function_names <- as.character(model_function_names)
  task_steps <- list(
    create_task_step(step_name = "model_output_ind",
                     target_name = function(task_name, ...) {
                       sprintf("2_model/out/%s.output.rds.ind", task_name)
                     },
                     command = function(task_name, ...) {
                       c(site_id, model_func) %<-% get_model_func_site_id(task_name)
                       sprintf("%s(out_ind = target_name, data_ind = \'1_format/out/%s_split_scaled.rds.ind\')", model_func, site_id)
                     }),
    create_task_step(step_name = "model_output_rds",
                     target_name = function(steps, ...) {
                       as_data_file(steps[[1]]$target_name)
                     },
                     command = function(steps, ...) {
                       sprintf("require_local(\'%s\')", steps[[1]]$target_name)
                     }),
    create_task_step(step_name = "evaluate",
                     target_name = function(task_name, ...) {
                       sprintf("2_model/doc/%s.html", task_name)
                     },
                     command = function(task_name, steps, ...) {
                       c(site_id, model_func) %<-% get_model_func_site_id(task_name)
                       model_output <- steps[[1]]$target_name
                       formatted_data <- sprintf("1_format/out/%s_split_scaled.rds.ind", site_id)
                       sprintf("evaluate_model(output_html = target_name, model_list_ind = \'%s\', dat_ind = \'%s\', rmd_file='2_model/src/assessment.Rmd', site_id = I(\'%s\'))",
                               model_output, formatted_data, site_id)
                       },
                     depends = function(steps, ...) {
                       steps[[2]]$target_name
                     }))
  browser()
  task_names <- expand.grid(site_ids$site_id, model_function_names) %>% arrange(Var1) %>%
    unite(col = "task_names", Var1, Var2, sep = "_") %>% .$task_names

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
    packages = c("dplyr", "tidyr", "keras", "scipiper"),
    source = c("lib/src/require_local.R", "2_model/src/models.R", "lib/src/utils.R", "lib/src/require_local.R"),
    include = "remake.yml",
    ...)
}
