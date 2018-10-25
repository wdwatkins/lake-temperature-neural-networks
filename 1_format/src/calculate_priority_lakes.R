#' @param temp_dat all daily temperature observations from pipeline #1
#' @param n_min minimum number of total observations a lake must have to be considered a priority lake. This
#' filter is applied before n_years, years_with_7months, years_with_10days, and n_days.
#' @param n_years number of years of observation which qualifies a lake for priority status
#' @param years_with_7months number of years for which a lake has observations in 7 or more months, which qualifies a lake for priority status
#' @param years_with_10days number of years for which a lake has 10 or more days of observations, which qualifies a lake for priority status
#' @param n_days number of observation days a lake has over the entire period or record which qualifies a lake for priority status
calc_priority_lakes <- function(temp_dat, n_min, n_years, years_with_7months, years_with_10days, n_days) {
  # This function calculates priority lakes based on data availability
  all_dat <- feather::read_feather(temp_dat)

  stats <- all_dat %>%
    mutate(year = lubridate::year(date),
           month = lubridate::month(date)) %>%
    group_by(nhd_id, year, month, date) %>% # first group by date to compress depths
    summarize(n_depths = n()) %>% # keep track of n depths to get total obs
    group_by(nhd_id, year) %>% # now flatten to yearly stats
    summarize(mean_ndepths = mean(n_depths), # avg n depths measured per lake-date
              ntotal = sum(n_depths), # total obs per lake-year
              ndays_peryear = n(), # days of observation per lake-year
              nmonths_peryear = length(unique(month))) %>% # n months of obs per lake-year
    group_by(nhd_id) %>% # now flatten to lake stats
    summarize(mean_ndepths = mean(mean_ndepths),
              ntotal = sum(ntotal), # total obs per lake
              mean_ndays_peryear = mean(ndays_peryear), # avg number of obs days per lake-year
              ndays_total = sum(ndays_peryear), # total days of monitoring per lake
              nyears = n(), # total years of monitoring per lake
              nyears_10days = length(which(ndays_peryear>=10)), # n years with >= 10 days of monitoring per lake
              mean_nmonths_peryear = mean(nmonths_peryear), # avg number of monitoring months across all years per lake
              nyears_7months = length(which(nmonths_peryear>=7))) # n years with >= 7 months of monitoring per year

  first_pass <- filter(stats, ntotal > n_min) # first, lakes must meet n_min criteria

  lakes_years <- filter(first_pass, nyears >= n_years)
  lakes_months <- filter(first_pass, mean_nmonths_peryear >= nyears_7months)
  lakes_years_10days <- filter(first_pass, nyears_10days >= years_with_10days)
  lakes_days <- filter(first_pass, ndays_total >= n_days)

  # combine and find unique lakes that meet each criteria
  priority_lakes <- unique(c(lakes_years$nhd_id, lakes_months$nhd_id, lakes_years_10days$nhd_id, lakes_days$nhd_id))

  return(priority_lakes)

}

combine_priorities <- function(priority_lakes_by_choice, priority_lakes_by_data, nldas_crosswalk_file,
                               truncate_lakes_for_dev = FALSE) {

  crosswalk <- readRDS(nldas_crosswalk_file)
  all_lakes <- unique(c(priority_lakes_by_choice, priority_lakes_by_data))

  all_lakes_names <-  crosswalk %>%
    filter(site_id %in% all_lakes) %>%
    mutate(obs_file = sprintf("1_format/tmp/%s_separated_obs.rds.ind", site_id),
           glm_preds_file = sprintf('1_format/in/glm_preds/%s_output.nc.ind', site_id),
           meteo_file = sprintf("in/driver-data/NLDAS_time[0.346848]_x[%s]_y[%s].csv", nldas_coord_x, nldas_coord_y),
           lake_name = gsub('\\d+$', '', GNIS_Nm)) %>%
    #other columns from NLDAS file or master lake list could be saved here if useful
    select(site_id, lake_name, obs_file, glm_preds_file, meteo_file) %>%
    distinct()

  choice_lakes_dont_quality <- priority_lakes_by_choice[!priority_lakes_by_choice %in% priority_lakes_by_data]
  if(length(choice_lakes_dont_quality) > 0) {
    warning(length(choice_lakes_dont_quality), " chosen lakes don't meet data criteria: ", choice_lakes_dont_quality)
  }

  reg_sheet <- googlesheets::gs_key('1gCfesykjlTDQvdNlWJDo1GkCTOufRJOZdZx7Kzbp0vM')
  missing_names <- googlesheets::gs_read(ss = reg_sheet)

  all_lakes_names_fixed <- left_join(all_lakes_names, missing_names, by = 'site_id') %>%
    mutate(lake_name = ifelse(is.na(lake_name.x), lake_name.y, lake_name.x)) %>%
    select(site_id, lake_name, obs_file, glm_preds_file, meteo_file)

  if (any(is.na(all_lakes_names_fixed$lake_name))) {
    warning(paste0('Some NHD ids are still missing lake names (site_id = ', paste(all_lakes_names_fixed$site_id[is.na(all_lakes_names_fixed$lake_name)], sep = ', '), '). Update the google sheet lake-temperature-neural-networks/in/missing_names_crosswalk.'))
  }
  if(truncate_lakes_for_dev) {
     all_lakes_names_fixed <- all_lakes_names_fixed %>% filter(site_id %in% c("nhd_2360642", "nhd_13293262"))
  }

  return(all_lakes_names_fixed)
}
