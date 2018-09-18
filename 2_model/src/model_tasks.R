# the model tasks plan will probably include this task_step:
# create_task_step(
#   step_name = "split_scale_rds",
#   target_name = function(steps, ...) {
#     sprintf("1_format/out/%s_split_scaled.rds", task_name)
#   },
#   command = function(steps, ...) {
#     sprintf("gd_get('%s')", sprintf("1_format/out/%s_split_scaled.rds.ind", task_name))
#   })
