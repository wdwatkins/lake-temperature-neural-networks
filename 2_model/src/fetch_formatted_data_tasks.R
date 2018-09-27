create_fetch_formatted_data_task_plan <- function(site_ids, ind_dir) {
  task <- list(create_task_step(
    step_name = "split_scale_rds",
    target_name = function(steps, task_name, ...) {
      sprintf("1_format/out/%s_split_scaled.rds", task_name)
    },
    command = function(steps, task_name, ...) {
      sprintf("gd_get('%s')", sprintf("1_format/out/%s_split_scaled.rds.ind", task_name))
    }))
  fetch_formatted_data_task_plan <- create_task_plan(
    task_names = site_ids,
    task_steps = task,
    ind_dir = "2_model/log",
    add_complete = FALSE)
  return(fetch_formatted_data_task_plan)
}


