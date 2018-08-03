temp_to_density <- function(x) {
  1000*(1 - ((x + 288.9414)*(x - 3.9863)^2)/(508929.2 * (x + 68.12963)))
}

identify_out_of_order <- function(vec){
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

plot_heatmap_hack <- function(glm_temp, nn_temp, diff_temp){
  glmtools:::.stacked_layout(TRUE, num_divs = 3)
  clim <- c(-2,30)
  xaxis = list(time_form = "%Y", x_lab = "Hours", lim = as.Date(c("1990-06-29", "2005-10-28")), 
               vis_time = seq.Date(from = as.Date("1990-01-01"), to = as.Date("2006-01-01"), by = 'year'))
  glmtools:::.plot_df_heatmap(glm_temp, bar_title="GLM", xaxis = xaxis, col_lim = clim)
  glmtools:::.plot_df_heatmap(nn_temp, bar_title="NN", xaxis = xaxis, col_lim = clim)
  .plot_df_heatmap_na_custom(diff_temp, bar_title="NN - GLM", xaxis = xaxis, col_lim = clim)
}


.plot_df_heatmap_na_custom <- function(data, bar_title, num_cells, palette, title_prefix=NULL, overlays=NULL, xaxis=NULL, col_lim){
  
  z_out <- rLakeAnalyzer::get.offsets(data)
  reference = ifelse(substr(names(data)[2],1,3) == 'elv', 'bottom', 'surface')
  
  if (missing(col_lim))
    col_lim = range(data[, -1], na.rm = TRUE)
  if (missing(palette))
    palette <- colorRampPalette(c("violet","blue","cyan", "green3", "yellow", "orange", "red"), 
                                bias = 1, space = "rgb")
  
  col_subs <- head(pretty(col_lim, 6), -1)
  levels <- sort(unique(c(col_subs, pretty(col_lim, 15))))
  colors <- palette(n = length(levels)-1)
  dates <- data[, 1]
  matrix_var <- data.matrix(data[, -1])
  if(is.null(xaxis)){
    xaxis <- get_xaxis(dates)
  }
  
  yaxis <- glmtools:::get_yaxis_2D(z_out, reference, prefix=title_prefix)
  glmtools:::plot_layout(xaxis, yaxis, add=TRUE)
  glmtools:::.filled.contour(x = dates, y = z_out, z =matrix_var,
                  levels= levels,
                  col=colors)
  overlays # will plot any overlay functions
  axis_layout(xaxis, yaxis) #doing this after heatmap so the axis are on top
  
  color_key(levels, colors, subs=col_subs, col_label = bar_title)
}
#downsample dates to approx nobs from paper - only keep every fourth date
# dates_keep <- unique(obs$DateTime)[c(TRUE,FALSE,FALSE,FALSE)]
# obs <- obs %>% filter(DateTime %in% dates_keep)
#TODO: eliminate obs that are physically inconsistent?

# obs_density <- obs %>% mutate(rho = temp_to_density(temp)) %>%
#   group_by(DateTime) %>% arrange(DateTime, Depth) %>%
#   mutate(rank = min_rank(rho), keep = identify_out_of_order(rho)) %>%
#   mutate(keep = identify_out_of_order(rank))
# 
# dates_consistent_density <- obs_density %>% summarize(keep = all(keep)) %>% filter(keep)
# obs <- obs %>% filter(DateTime %in% dates_consistent_density$DateTime)