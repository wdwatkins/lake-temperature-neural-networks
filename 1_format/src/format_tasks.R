get_site_ids <- function(file, comment = "#") {
  read_csv(file, comment = comment)$site_id
}

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
create_format_task_plan <- function(site_ids, ind_dir, settings_yml = "lib/cfg/settings.yml") {
  # combine_nn_data has 3 .ind files as arguments but right now only uses them
  # to determine the names of data files, which I promise exist. These are really
  # just placeholders until we can connect up the start of this pipeline so that
  # data files come from other pipelines, at which point the I() around those .ind
  # files will no longer be appropriate and we should call sc_retrieve within
  # combine_nn_data.
  step1_combined <- create_task_step(step_name = "combine_nn_data",
    command = function(task_name, ...){
      c(obs_file, glm_preds_file, meteo_file) %<-% get_input_file_names(task_name)
      sprintf("combine_nn_data(obs_ind = I(\'%s\'), glm_preds_ind=I(\'%s\'),
              meteo_ind = I(\'%s\'))",
              obs_file, glm_preds_file, meteo_file)
      },
    depends = function(task_name, ...) {
      c(obs_file, glm_preds_file, meteo_file) %<-% get_input_file_names(task_name)
      as_data_file(c(obs_file, glm_preds_file, meteo_file))
    })

  step2_formatted <- create_task_step(step_name = "format_nn_data",
                                      command = function(task_name, ...) {
                                        sprintf("format_nn_data(%s_combine_nn_data, structure=I('NN'))",task_name)
                                      },
                                      depends = settings_yml)
  step3_split_scale <- create_task_step(step_name = "split_scale_nn_data",
                                        target_name = function(task_name, ...) {
                                          sprintf("1_format/out/%s_split_scaled.rds.ind", task_name)
                                        },
                                        command = function(task_name, target_name, ...) {
                                          sprintf("split_scale_nn_data(%s_format_nn_data, ind_file = \'%s\')",
                                                  task_name, target_name)
                                        },
                                        depends = settings_yml)
  format_task_plan <- create_task_plan(task_names = site_ids,
                                       task_steps = list(step1_combined, step2_formatted,
                                                         step3_split_scale),
                                       final_steps="split_scale_nn_data",
                                       ind_dir = "1_format/log")
  return(format_task_plan)
}

create_format_task_makefile <- function(task_plan, makefile, ...) {
  packages <- c("dplyr", "tidyr")
  sources <- "1_format/src/combine_nn_inputs.R"
  create_task_makefile(task_plan = task_plan, makefile = makefile,
                       packages = packages, sources = sources, ...)
}
