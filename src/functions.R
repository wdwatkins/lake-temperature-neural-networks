#attempting to identify bad observations (i.e. violating increasing density w/ depth)
#This is different from cummin() in that cummin throws out everything after a deviation from 
#monotonically increasing. When this function encounters a point that is not monotonic, it discards it,
#but then compares to the next one. It was an attempt to not throw out entire temp profiles based on 
#a single bad observation
#currently not used.
remove_out_of_order <- function(vec){
  keep <- rep(TRUE, length(vec))
  mx <- vec[1]
  for(i in seq_along(vec)){
    if(vec[i] < mx){
      keep[i] <- FALSE
    } else {
      mx <- vec[i]
    }
  }
  return(keep)
}

augment_features <- function(df){
  df %>% select(-time) %>% 
    mutate(yday = lubridate::yday(DateTime),
           Snow = ifelse(Snow > 0, yes = 1, no = 0),
           freezing = ifelse(AirTemp <= 0, yes = 1, no = 0),
           gdd = AirTemp - 10, gdd = ifelse(gdd < 0, yes = 0, no = gdd
           )) 
}

#diff temp plot isn't working - switched to ggplot instead
plot_heatmap_hack <- function(glm_temp, nn_temp, diff_temp){
  glmtools:::.stacked_layout(TRUE, num_divs = 3)
  clim <- c(-2,30)
  xaxis = list(time_form = "%Y", x_lab = "Hours", lim = as.Date(c("1990-06-29", "2005-10-28")), 
               vis_time = seq.Date(from = as.Date("1990-01-01"), to = as.Date("2006-01-01"), by = 'year'))
  glmtools:::.plot_df_heatmap(glm_temp, bar_title="GLM", xaxis = xaxis, col_lim = clim)
  glmtools:::.plot_df_heatmap(nn_temp, bar_title="NN", xaxis = xaxis, col_lim = clim)
  glmtools:::.plot_df_heatmap(diff_temp, bar_title="NN - GLM", xaxis = xaxis, col_lim = clim)
}


#downsample dates to approx nobs from paper - only keep every fourth date
# dates_keep <- unique(obs$DateTime)[c(TRUE,FALSE,FALSE,FALSE)]
# obs <- obs %>% filter(DateTime %in% dates_keep)
#TODO: eliminate obs that are physically inconsistent?

# obs_density <- obs %>% mutate(rho = rLakeAnalyzer::water.density(temp)) %>%
#   group_by(DateTime) %>% arrange(DateTime, Depth) %>%
#   mutate(rank = min_rank(rho), keep = identify_out_of_order(rho)) %>%
#   mutate(keep = identify_out_of_order(rank))
# 
# dates_consistent_density <- obs_density %>% summarize(keep = all(keep)) %>% filter(keep)
# obs <- obs %>% filter(DateTime %in% dates_consistent_density$DateTime)