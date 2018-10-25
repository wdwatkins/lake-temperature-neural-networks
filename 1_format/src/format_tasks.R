#assume task_name == site_id
create_format_task_plan <- function(priority_lakes, ind_dir, settings_yml = "lib/cfg/settings.yml") {
  task_steps <- list(
    create_task_step(
      step_name = "yeti_download_ind",
      target_name = function(task_name, ...) {
        sprintf("1_format/in/glm_preds/%s_output.nc.ind", task_name)
      },
      command = "syncr_indicate(out_ind = target_name, glm_yeti_path = glm_yeti_path_cfg)"
    ),
    create_task_step(
      step_name = "yeti_download_nc",
      target_name = function(steps, ...) {
        as_data_file(steps[['yeti_download_ind']]$target_name)
      },
      command = function(steps, ...) {
        sprintf("require_local('%s')", steps[['yeti_download_ind']]$target_name)
      }
    ),
    # step1a_separate_obs_ind
    create_task_step(
      step_name = "separate_obs_ind",
      target_name = function(task_name, ...) {
        sprintf("1_format/tmp/%s_separated_obs.rds.ind", task_name)
      },
      command = function(task_name, ...) {
        sprintf("separate_obs(out_ind = target_name, site_id = I(\'%s\'), all_obs_file_ind = 'in/merged_temp_data_daily.feather.ind')", task_name)
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
        c(obs_file, glm_preds_file, meteo_file) %<-%
          ( priority_lakes %>% filter(site_id == task_name) %>%
              select(obs_file, glm_preds_file, meteo_file))
        psprintf(
          "combine_nn_data(",
          "combined_ind = target_name,",
          "obs_ind = '%s'," = obs_file,
          "glm_preds_ind = '%s'," = glm_preds_file,
          "meteo = '%s'," = meteo_file,
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
    task_names = priority_lakes$site_id,
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
    packages = c("dplyr", "tidyr", "syncr"),
    sources = c("lib/src/require_local.R", "1_format/src/combine_nn_inputs.R"),
    include = "1_format.yml",
    ...)
}

separate_obs <- function(site_id, out_ind, all_obs_file_ind) {
  #assuming data is already fully clean
  site_obs <- read_feather(as_data_file(all_obs_file_ind)) %>% filter(nhd_id == site_id) %>%
    rename(DateTime = date) %>% as.data.frame() #need these col names for glmtools::resample_to_field
  saveRDS(object = site_obs, file = as_data_file(out_ind))
  sc_indicate(ind_file = out_ind, data_file = as_data_file(out_ind))
}

syncr_indicate <- function(out_ind, glm_yeti_path) {
  user <- Sys.info()[['user']] #assumes this is your yeti login
  out_data_file <- as_data_file(out_ind)
  src <- sprintf('%s@yeti.cr.usgs.gov:%s/%s', user, glm_yeti_path, basename(out_data_file))
  dest <- sprintf('1_format/in/glm_preds/%s_output.nc', site_id)
  syncr(src, dest)
  sc_indicate(ind_file = out_ind, data_file = out_data_file)
}
