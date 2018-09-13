lookup_meteo_file <- function(site_id) {
  #once the table exists, will lookup there
  sprintf("1_format/in/drivers/%s_meteo.feather.ind", site_id)
}
get_input_file_names <- function(site_id = task_name) {
  obs_file <- sprintf("1_format/in/obs/%s_obs.csv.ind", site_id)
  glm_preds_file <- sprintf('1_format/in/glm_preds/%s_output.nc.ind', site_id)
  meteo_file <- lookup_meteo_file(site_id)
  return(list(obs_file=obs_file, glm_preds_file=glm_preds_file,
              meteo_file = meteo_file))
}

#assume task_name == site_id
create_format_task_plan <- function(site_ids, ind_dir) {

  step1_combined <- create_task_step(step_name = "combine_nn_data",
    command = function(task_name, ...){
      c(obs_file, glm_preds_file, meteo_file) %<-% get_input_file_names(task_name)
      sprintf("combine_nn_data(obs_ind = \'%s\', glm_preds_ind=\'%s\', meteo_ind = \'%s\')",
              obs_file, glm_preds_file, meteo_file)
      },
    depends = function(task_name, ...) {
      c(obs_file, glm_preds_file, meteo_file) %<-% get_input_file_names(task_name)
      c(obs_file, glm_preds_file, meteo_file)
    })

  step2_formatted <- create_task_step(step_name = "format_nn_data",
                                      command = function(task_name, ...) {
                                        sprintf("format_nn_data(%s_combine_nn_data, structure=I('NN'))",task_name)
                                      })
  step3_split_scale <- create_task_step(step_name = "split_scale_nn_data",
                                        command = function(task_name, ...) {
                                          sprintf("split_scale_nn_data(%s_format_nn_data)", task_name)
                                        })
  format_task_plan <- create_task_plan(task_names = paste0(site_ids),
                                       task_steps = list(step1_combined, step2_formatted,
                                                         step3_split_scale),
                                       final_steps="split_scale_nn_data",
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
