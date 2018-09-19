create_model_and_eval_steps <- function(model_functions) {
  model_steps <- NULL
  for(func in model_functions) {
    print(func)
    run_step <- create_task_step(step_name = paste0(func, "_ind"),
                                 target_name = function(steps, task_name, ...) {
                                   sprintf("2_model/out/%s_%s_model_out.rds.ind", steps[[1]]$step_name, task_name)
                                 },
                                 command = function(steps, ...) {
                                   sprintf("%s(%s)", func, steps[['split_scale_rds']]$target_name)
                                 })
    run_get_local_step <- create_task_step(step_name = paste0(func, "_rds"),
                                           target_name = function(steps, task_name, func = func, ...) {
                                             sprintf("2_model/out/%s_%s_model_out.rds", func, task_name)
                                           },
                                           command = function(steps, ...) {
                                             #print(lapply(steps, `[`, "step_name"))
                                             run_ind_step <- paste0(func, "_ind")
                                             #print(steps[[run_ind_step]]$target_name)
                                             #print(run_ind_step)
                                             sprintf("gd_get(%s)", steps[[run_ind_step]]$target_name)
                                           })
    # eval_step <- create_task_step(step_name = paste0("evaluate_", func),
    #                               target_name = function(task_name, ...) {
    #                                 sprintf("2_model/doc/%s_")
    #                               })
    if(!is.null(model_steps)) {
      model_steps <- c(model_steps, list(run_step, run_get_local_step))
    } else {
      model_steps <- list(run_step, run_get_local_step)
    }

  }
  return(model_steps)
}


create_model_task_plan <- function(site_ids, settings, ind_dir) {
  task_steps <- c(
    list(create_task_step(
      step_name = "split_scale_rds",
      target_name = function(steps, task_name, ...) {
        sprintf("1_format/out/%s_split_scaled.rds", task_name)
      },
      command = function(steps, task_name, ...) {
        sprintf("gd_get('%s')", sprintf("1_format/out/%s_split_scaled.rds.ind", task_name))
      })),
    create_model_and_eval_steps(settings[['model_functions']])
  )
  saveRDS(task_steps, 'task_steps.rds')
  model_task_plan <- create_task_plan(
    task_names = site_ids,
    task_steps = task_steps,
    #final_steps = "",
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
