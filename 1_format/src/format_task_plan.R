lookup_meteo_file <- function(site_id) {
  #once the table exists, will lookup there
  sprintf("1_format/in/drivers/%s_meteo.feather.ind", site_id)
}
get_input_file_names <- function(task_name) {
  site_id <- gsub(pattern = "format_", replacement = "", x = task_name)
  obs_file <- sprintf("1_format/in/obs/%s_obs.csv.ind", site_id)
  glm_preds_file <- sprintf('1_format/in/glm_preds/%s_output.nc.ind', site_id)
  meteo_file <- lookup_meteo_file(site_id)
  return(list(site_id = site_id, obs_file=obs_file, glm_preds_file=glm_preds_file,
              meteo_file = meteo_file))
}

create_format_task_plan <- function(site_ids, ind_dir) {

  step1_combined <- create_task_step(step_name = "combine_nn_data",
    command = function(task_name, ...){
      c(site_id, obs_file, glm_preds_file, meteo_file) %<-% get_input_file_names(task_name)
      sprintf("combine_nn_data(obs_ind = \'%s\', glm_preds_ind=\'%s\', meteo_ind = \'%s\')",
              obs_file, glm_preds_file, meteo_file)
      },
    depends = function(task_name, ...) {
      c(site_id, obs_file, glm_preds_file, meteo_file) %<-% get_input_file_names(task_name)
      c(obs_file, glm_preds_file, meteo_file)
    })

  # step2_formatted <- create_task_step()
  # step3_split_scale <- create_task_step()
  format_task_plan <- create_task_plan(task_names = paste0("format_", site_ids),
                                       task_steps = list(step1_combined),
                                       final_steps="combine_nn_data",
                                       ind_dir = "1_format/log")
  return(format_task_plan)
}
#
# create_format_makefile <- function(makefile, format_task_plan) {
#   packages <-
#   include <-
#   sources <-
#   create_task_makefile()
# }
