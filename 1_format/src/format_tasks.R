lookup_meteo_file <- function(site_id) {
  #once the table exists, will lookup there
  sprintf("in/driver-data/%s_meteo.feather.ind", site_id)
}

get_input_file_names <- function(site_id = task_name) {
  obs_file <- sprintf("1_format/tmp/%s_separated_obs.feather.ind", site_id)
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
  task_steps <- list(
    # step1a_separate_obs_ind
    create_task_step(
      step_name = "separate_obs_ind",
      target_name = function(task_name, ...) {
        sprintf("1_format/tmp/%s_separated_obs.feather.ind", task_name)
      },
      command = function(task_name, ...) {
        sprintf("separate_obs(out_ind = target_name, site_id = I(\'%s\'))", task_name)
      }),
    # step1b_separate_obs_feather
    create_task_step(
      step_name = "separate_obs_feather",
      target_name = function(steps, ...) {
        as_data_file(steps[['separate_obs_ind']]$target_name)
      },
      command = function(steps, ...) {
        sprintf("require_local(\'%s\')", steps[['separate_obs_ind']]$target_name)
      }),
    # step2a_combine_ind
    create_task_step(
      step_name = "combine_ind",
      target_name = function(task_name, ...) {
        sprintf("1_format/tmp/%s_combined.rds.ind", task_name)
      },
      command = function(task_name, ...){
        c(obs_file, glm_preds_file, meteo_file) %<-% get_input_file_names(task_name)
        psprintf(
          "combine_nn_data(",
          "combined_ind = target_name,",
          "obs_ind = I('%s')," = obs_file, # we should take away the I() if we ever actually have these .ind files
          "glm_preds_ind = I('%s')," = glm_preds_file,
          "meteo_ind = I('%s')," = meteo_file,
          "combine_cfg = combine_cfg)")
      }),
    # step2b_combine_rds
    create_task_step(
      step_name = "combine_rds",
      target_name = function(steps, ...) {
        as_data_file(steps[['combine_ind']]$target_name)
      },
      command = function(steps, ...) {
        sprintf("require_local('%s')", steps[['combine_ind']]$target_name)
      }),

    # step3a_format_ind
    create_task_step(
      step_name = "format_ind",
      target_name = function(task_name, ...) {
        sprintf("1_format/tmp/%s_formatted.rds.ind", task_name)
      },
      command = function(steps,...) {
        psprintf(
          "format_nn_data(",
          "formatted_ind = target_name,",
          "combined_ind = '%s'," = steps[['combine_ind']]$target_name,
          "format_cfg = format_cfg)")
      }),
    # step3b_format_rds
    create_task_step(
      step_name = "format_rds",
      target_name = function(steps, ...) {
        as_data_file(steps[['format_ind']]$target_name)
      },
      command = function(steps, ...) {
        sprintf("require_local('%s')", steps[['format_ind']]$target_name)
      }),

    # step4a_split_scale_ind
    create_task_step(
      step_name = "split_scale_ind",
      target_name = function(task_name, ...) {
        sprintf("1_format/out/%s_split_scaled.rds.ind", task_name)
      },
      command = function(steps, target_name, ...) {
        psprintf(
          "split_scale_nn_data(",
          "ind_file = target_name,",
          "formatted_ind = '%s',"=steps[['format_ind']]$target_name,
          "split_scale_cfg = split_scale_cfg)")
      })
    # leave the gd_get for 2_model_tasks.yml
  )

  format_task_plan <- create_task_plan(
    task_names = site_ids,
    task_steps = task_steps,
    final_steps = "split_scale_ind",
    ind_dir = "1_format/log",
    add_complete = FALSE)

  return(format_task_plan)
}

create_format_task_makefile <- function(task_plan, makefile, ...) {
  create_task_makefile(
    task_plan = task_plan,
    makefile = makefile,
    packages = c("dplyr", "tidyr"),
    sources = c("lib/src/require_local.R", "1_format/src/combine_nn_inputs.R"),
    include = "1_format.yml",
    ...)
}

separate_obs <- function(site_id, out_ind, all_obs_file = "in/merged_temp_data_daily.feather") {
  #assuming data is already fully clean
  site_obs <- read_feather(all_obs_file) %>% filter(.data$site_id == site_id)
  write_feather(x = site_obs, path = as_data_file(out_ind))
  sc_indicate(ind_file = out_ind, data_file = as_data_file(out_ind))
}
