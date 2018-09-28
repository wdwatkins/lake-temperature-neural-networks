#' @section Structural Classes
#'
#'   There are four classes of neural network structures that we might try for
#'   which the format of the inputs and targets can differ.
#'
#'   1. NN: Non-recurrent, non-spatial NNs where depth is given as an input
#'   variable and one corresponding temperature (at that depth) is predicted by
#'   the model. For these models we only need the GLM predictions and
#'   meteorological drivers as subset to the depths and times of the
#'   observations.
#'
#'   2. SNN: Non-recurrent, spatially explicit NNs where depths are represented
#'   by separate input nodes at each standard depth (for GLM predictions) and by
#'   separate output nodes at each standard depth (the NN predictions). For
#'   these models we need GLM predictions subset to a consistent vector of
#'   depths, and we need those GLM predictions and the meteorological drivers
#'   for only those days on which we have observations at one or more of those
#'   depths.
#'
#'   3. RNN: Recurrent, non-spatial NNs. For these models I think we want GLM
#'   predictions on every day at a fixed grid of depths, meteorological data on
#'   every day, and optionally a single GLM prediction at each depth and date
#'   for which we have a matching observation.
#'
#'   4. RSNN: Recurrent, spatially explicit NNs. For these models we need a
#'   complete grid of depths and days for the GLM predictions, a complete
#'   timeseries of days for the meteorological data, and optionally a single GLM
#'   prediction at each depth and date for which we have a matching observation.
#'
#' @param combined_ind name of indicator file where output should be
#'   saved/indicated
#' @param obs_ind indicator file of observations, with at least columns
#'   `DateTime`, `depth`, and `temp`
#' @param glm_preds_ind indicator file of temperature predictions from GLM
#' @param meteo_ind indicator file of meteorological data
#' @param combine_cfg list containing configuration information, with 2 elements
#'   so far: min_obs_per_depth = minimum number of observations at a depth for
#'   that depth to qualify as a "common" depth and to be included in the
#'   resampling of GLM predictions to specific depths. min_obs_per_date =
#'   minimum number of observations at the common depths on a date to justify
#'   including that date in the input.
#' @param common_depths as an alternative to `combine_cfg$min_obs_per_depth`, a vector of
#'   common depths (in m) can be specified directly. If `common_depths` is given
#'   then `combine_cfg$min_obs_per_depth` will be ignored
combine_nn_data <- function(combined_ind, obs_ind, glm_preds_ind, meteo_file,
  combine_cfg = list(min_obs_per_depth=100, min_obs_per_date=1),
  common_depths) {

  # convert ind files to data files
  glm_preds_file <- scipiper::as_data_file(glm_preds_ind)
  obs_file <- scipiper::as_data_file(obs_ind)

  # read and format the temperature observations
  obs <- readRDS(obs_file) %>% select(date = DateTime, depth, ObsTemp=temp)

  # determine which depths are sufficiently commonly observed to deserve their
  # own NN node in spatially explicit networks
  if(missing(common_depths)) {
    common_depths <- obs %>%
      group_by(depth) %>%
      summarize(NumObs = length(ObsTemp)) %>%
      filter(NumObs >= combine_cfg$min_obs_per_depth) %>%
      pull(depth)
  }
  # identify the "common dates", those dates that include at least N
  # observations at the common depths
  common_dates <- obs %>%
    filter(depth %in% common_depths) %>%
    group_by(date) %>%
    summarize(NumObs = length(ObsTemp)) %>%
    filter(NumObs >= combine_cfg$min_obs_per_date) %>%
    pull(date)
  obs_dates <- unique(obs$date)

  # amend the temperature observations with info about their match to the grid
  obs <- obs %>%
    mutate(
      IsGriddepth = depth %in% common_depths,
      IsGridDate = date %in% common_dates)

  # read in the glm predictions so they can be matched to (1) the depths and
  # dates of the temperature observations, for NNs, (2) a temporally subset grid
  # of depths and days where only those days with observations at grid depths
  # are included, for SNNs, (3) a complete grid of depths and days, for RNNs and
  # RSNNs. the actual matching happens in format_nn_data()

  print(glm_preds_file)
  glm_preds_by_obs <- glmtools::resample_to_field(nc_file = glm_preds_file, field_file = obs_file) %>%
    mutate(date = as.Date(DateTime)) %>%
    mutate(IsObsMatch = TRUE) %>%
    select(date, depth = Depth, GLMTemp=Modeled_temp, IsObsMatch) %>%
    as_data_frame() %>%
    filter(!is.na(GLMTemp))
  glm_preds_by_dep <- glmtools::get_temp(glm_preds_file, reference = "surface", z_out = common_depths) %>%
    tidyr::gather(var_depth, GLMTemp, -DateTime) %>%
    tidyr::separate(var_depth, c('Var','depth'), sep="_") %>%
    mutate(depth = as.numeric(depth), date = as.Date(DateTime)) %>%
    mutate(IsGriddepth = TRUE) %>%
    select(date, depth, GLMTemp, IsGriddepth) %>%
    as_data_frame() %>%
    filter(!is.na(GLMTemp))
  glm_preds <- full_join(glm_preds_by_obs, glm_preds_by_dep, by=c('date','depth','GLMTemp')) %>%
    mutate(
      IsObsMatch = ifelse(is.na(IsObsMatch), FALSE, IsObsMatch),
      IsGriddepth = ifelse(is.na(IsGriddepth), FALSE, IsGriddepth),
      IsGridObsDate = date %in% common_dates,
      IsAnyObsDate = date %in% obs_dates) %>%
    arrange(date, depth)

  # read and format the meteorological (drivers) data so they can be subset to
  # (1) drivers matched to the dates of the temperature observations, for NNs
  # and SNNs, or (2) a complete timeseries of drivers for all days, for RNNs and
  # RSNNs
  print(meteo_file)
  meteo <- read_csv(meteo_file) %>%
    mutate(date = as.Date(time)) %>%
    select(-time) %>%
    select(date, everything()) %>%
    augment_met_features() %>%
    filter(date %in% unique(glm_preds$date)) %>%
    select(-Snow) %>% # replaced by the logical SnowTF
    mutate(
      IsGridObsDate = date %in% common_dates,
      IsAnyObsDate = date %in% obs_dates) %>%
    arrange(date)# %>%
   # remove_rain_offset()

  # amend the observations one more time to truncate to match glm and meteo data
  obs_in_glm <- inner_join(glm_preds_by_obs, obs, by=c('date','depth')) %>%
    select(date, depth, ObsTemp)
  obs_in_met <- filter(obs, date %in% meteo$date) %>%
    select(date, depth, ObsTemp)
  obs <- inner_join(obs_in_glm, obs_in_met, by=c('date','depth','ObsTemp')) %>%
    left_join(obs, by=c('date','depth','ObsTemp'))

  # package everything into a list
  inputs <- list(
    obs=obs,
    glm_preds=glm_preds,
    meteo=meteo)

  # write and indicate (locally)
  combined_rds <- as_data_file(combined_ind)
  saveRDS(inputs, combined_rds)
  sc_indicate(combined_ind, data_file=combined_rds)
}

#' add those features reported by the paper Anuj led
augment_met_features <- function(meteo) {
  meteo %>%
    mutate(
      YDay = lubridate::yday(date),
      SnowTF = ifelse(Snow > 0, yes = 1, no = 0),
      FreezeTF = ifelse(AirTemp <= 0, yes = 1, no = 0),
      Year = lubridate::year(date)) %>%
    group_by(Year) %>%
    mutate(
      GDD = cumsum(pmax(AirTemp - 10, 0))) %>%
    ungroup() %>%
    select(-Year)
}

#' estimate the offset amount and YDay interval based on the minimum rain on
#' each yday over all years of the dataset
remove_rain_offset <- function(meteo) {
  # estimate the offset amount based on the minimum rain on each yday over all
  # years of the dataset
  minrain <- meteo %>%
    group_by(YDay) %>%
    summarize(MinRain = min(Rain))
  offset <- minrain %>%
    filter(MinRain > 0) %>%
    pull(MinRain) %>%
    table() %>%
    which.max() %>%
    names() %>%
    as.numeric()
  # a little error checking to notice when our algorithm is likely to have failed
  if(length(offset) != 1) stop("couldn't identify a single unique rain offset")

  # estimate the offset YDay interval
  offset_by_yday <- minrain %>%
    mutate(IsOffset = MinRain >= offset)
  # a little more error checking to notice when our algorithm is likely to have failed
  offset_rle <- as_data_frame(unclass(rle(setNames(offset_by_yday$IsOffset, offset_by_yday$YDay))))
  if(nrow(filter(offset_rle, values==TRUE)) != 1) stop("couldn't find a single run of dates with obvious rain offset")
  # if we pass the error check, proceed to identify the offset YDays
  offset_dates <- offset_by_yday %>%
    filter(IsOffset) %>%
    pull(YDay)

  # subtract the offset on the specified YDays
  return(meteo %>%
    mutate(Rain = ifelse(YDay %in% offset_dates, Rain - offset, Rain)))
}

#' Format a site's data for the specified neural network
#'
#' @section Structural Classes
#'
#'   There are four classes of neural network structures that we might try for
#'   which the format of the inputs and targets can differ.
#'
#'   1. NN: Non-recurrent, non-spatial NNs where depth is given as an input
#'   variable and one corresponding temperature (at that depth) is predicted by
#'   the model. For these models we only need the GLM predictions and
#'   meteorological drivers as subset to the depths and times of the
#'   observations.
#'
#'   2. SNN: Non-recurrent, spatially explicit NNs where depths are represented
#'   by separate input nodes at each standard depth (for GLM predictions) and by
#'   separate output nodes at each standard depth (the NN predictions). For
#'   these models we need GLM predictions subset to a consistent vector of
#'   depths, and we need those GLM predictions and the meteorological drivers
#'   for only those days on which we have observations at one or more of those
#'   depths.
#'
#'   3. RNN: Recurrent, non-spatial NNs. For these models I think we want GLM
#'   predictions on every day at a fixed grid of depths, meteorological data on
#'   every day, and optionally a single GLM prediction at each depth and date
#'   for which we have a matching observation.
#'
#'   4. RSNN: Recurrent, spatially explicit NNs. For these models we need a
#'   complete grid of depths and days for the GLM predictions, a complete
#'   timeseries of days for the meteorological data, and optionally a single GLM
#'   prediction at each depth and date for which we have a matching observation.
#'
#' @param formatted_ind name of indicator file where output should be
#'   saved/indicated
#' @param combined_ind indicator file of rds file containing list of glm_preds,
#'   meteo, and obs data_frames
#' @param format_cfg list containing configuration information, with 1 element
#'   so far: structure = type of NN to format for
format_nn_data <- function(formatted_ind, combined_ind, format_cfg) {

  # read in the model/data configuration
  stopifnot(format_cfg$structure %in% c('NN','SNN','RNN','RSNN'))
  structure <- format_cfg$structure # simplify b/c we'll use it a lot

  # read in the data
  inputs <- readRDS(sc_retrieve(combined_ind, '1_format_tasks.yml'))

  glm_preds <- inputs$glm_preds
  if(structure == 'NN') {
    glm_in <- filter(glm_preds, IsObsMatch) %>%
      select(date, depth, GLMTemp)
  } else if(structure == 'SNN') {
    glm_in <- filter(glm_preds, IsGriddepth & IsGridObsDate) # IsAnyObsDate
  } else if(structure %in% c('RNN','RSNN')) {
    glm_in <- filter(glm_preds, IsGriddepth) # | IsObsMatch
  }
  if(structure %in% c('SNN','RNN','RSNN')) {
    glm_in <- glm_in %>%
      mutate(depthColname = sprintf('GLMTemp_%04.1fm', depth)) %>%
      select(date, depthColname, GLMTemp) %>%
      tidyr::spread(depthColname, GLMTemp)
    if(structure == 'RNN') {
      glm_dep <- filter(glm_preds, IsObsMatch)
      glm_in <- full_join(glm_dep, glm_in, by='date')
    }
  }

  meteo <- inputs$meteo
  met_in <- switch(
    structure,
    'NN' = filter(meteo, IsAnyObsDate),
    'SNN' = filter(meteo, IsGridObsDate),
    'RNN' =, 'RSNN' = meteo) %>%
    select(-IsAnyObsDate, -IsGridObsDate)

  all_in <- full_join(met_in, glm_in, by='date')
  mat_in <- as.matrix(select(all_in, -date))
  rownames(mat_in) <- format(all_in$date, '%Y%m%d')

  obs <- inputs$obs
  obs_out <- switch(
    structure,
    'NN' = obs,
    'SNN' =, 'RNN' =, 'RSNN' = filter(obs, IsGriddepth)) %>%
    select(date, depth, ObsTemp)
  if(structure %in% c('NN','RNN')) {
    all_out_names <- sprintf("%s_%04.1fm", format(obs_out$date, '%Y%m%d'), obs_out$depth)
    all_out <- select(obs_out, ObsTemp)
  } else if(structure %in% c('SNN','RSNN')) {
    all_out <- obs_out %>%
      mutate(depthColname = sprintf('ObsTemp_%04.1fm', depth)) %>%
      select(date, depthColname, ObsTemp) %>%
      tidyr::spread(depthColname, ObsTemp)
    all_out_names <- format(all_out$date, '%Y%m%d')
    all_out <- select(all_out, -date)
  }
  mat_out <- as.matrix(all_out)
  rownames(mat_out) <- all_out_names

  formatted <- list(
    input = mat_in,
    target = mat_out
  )

  # write and indicate
  formatted_rds <- as_data_file(formatted_ind)
  saveRDS(formatted, formatted_rds)
  sc_indicate(formatted_ind, data_file=formatted_rds)
}

#' Divide the NN inputs and targets into training, development (validation), and
#' test fractions
#'
#' @param formatted_ind indicator file of rds file containing list with matrix
#'   elements `input` and `target` containing the model-ready neural network
#'   inputs and outputs, respectively
#' @param ind_file name of indicator file where output should be saved/indicated
#' @param split_scale_cfg list of configuration options. currently contains 2
#'   elements: dev_frac =  fraction of dataset to assign to development
#'   (validation). test_frac = fraction of dataset to reserve for testing
split_scale_nn_data <- function(formatted_ind, ind_file, split_scale_cfg=list(dev_frac=0.2, test_frac=0.2)) {

  # read in the data
  formatted <- readRDS(sc_retrieve(formatted_ind, '1_format_tasks.yml'))

  # unpack the config list and make sure the fractions are reasonable
  dev_frac <- split_scale_cfg$dev_frac
  test_frac <- split_scale_cfg$test_frac
  if(dev_frac + test_frac >= 1) stop('dev_frac + test_frac must be < 1')
  train_frac <- 1 - dev_frac - test_frac

  # split the data into train, dev, and test sets. with respect to time, nest
  # the training data in the center, with dev data as its shoulders, with test
  # data as dev's shoulders
  n_obs <- nrow(formatted$input)
  breaks <- data_frame(
    test_run = as.integer(n_obs * test_frac / 2),
    dev_run = as.integer(n_obs * dev_frac / 2),
    train_run = n_obs - 2*(test_run + dev_run),
    test1_start = 1,
    test1_end = test1_start + test_run - 1,
    dev1_start = test1_end + 1,
    dev1_end = dev1_start + dev_run - 1,
    train_start = dev1_end + 1,
    train_end = train_start + train_run - 1,
    dev2_start = train_end + 1,
    dev2_end = dev2_start + dev_run - 1,
    test2_start = dev2_end + 1,
    test2_end = n_obs)
  dat <- with(breaks, list(
    test_input = formatted$input[c(test1_start:test1_end, test2_start:test2_end), ],
    test_target = formatted$target[c(test1_start:test1_end, test2_start:test2_end), ],
    dev_input = formatted$input[c(dev1_start:dev1_end, dev2_start:dev2_end), ],
    dev_target = formatted$target[c(dev1_start:dev1_end, dev2_start:dev2_end), ],
    train_input = formatted$input[train_start:train_end, ],
    train_target = formatted$target[train_start:train_end, ]
  ))

  # scale all three input datasets according to the means and sds of the training data
  dat$train_input <- scale(dat$train_input)
  dat$centers <- attr(dat$train_input, 'scaled:center')
  dat$scales <- attr(dat$train_input, 'scaled:scale')
  dat$test_input <- scale(dat$test_input, center=dat$centers, scale=dat$scales)
  dat$dev_input <- scale(dat$dev_input, center=dat$centers, scale=dat$scales)

  # save and post
  saveRDS(object = dat, file = as_data_file(ind_file))
  gd_put(remote_ind = ind_file)
}

