#' Gets Updated Open Chart Stats
#'
#' Returns tibble with an observation for each day in date range 2021-09-01
#' through tthe current date. Saves as data.
#'
#' All data references a 30-day rolling window.
#'
#' Variables:
#' - Open24Hr: Number of charts within past thirty days open for more than 24 hours.
#' - Open4Day: Number of charts open for more than 4 days.
#' - Open8Day: Number of charts open for more than 8 days.
#' - AllIncomplete: Total number of incomplete charts
#' - Incomplete30D: Total number of incomplete charts
#' - total_calls: Total charts within past thirty days
#' - incomplete30d_prop: Proportion of all calls within past thirty days taht are open
#' - Over24Hr_Prop: Proportion of all calls within past thirty days open for more than 24 hours
#' - Over4D_prop: Proportion of all calls within past thirty days open for more than 4 days
#' - Over8D_prop: Proportion of all calls within past thirty days open for more than 8 days#'
#' @return tibble of stats
#' @export

get_open_chart_stats_alt <- function() {
  library(magrittr)
  library(tidyverse)
  library(groverr)
  library(lubridate)

  # Get new incidents
  if (fs::file_exists(here::here("data/open_calls.rds"))) {
    open_chart_stats <- read_rds(here::here("data/open_calls.rds"))
    open_chart_stats <- bind_rows(open_chart_stats,
                                  import_new_data(start_date = max(open_chart_stats$testDate) + days(1) - days(30),
                                                  end_date = floor_date(today(), unit="month")-days(1)) %>%
                                    get_chart_completion_stats_alt() %>%
                                    filter(testDate >= max(open_chart_stats$testDate) + days(1)))
  } else {
    open_chart_stats <- import_new_data(start_date = "2021-09-01",
                                                  end_date = floor_date(today(), unit="month")-days(1)) %>%
                                    get_chart_completion_stats() %>%
                                    filter(testDate >= "2021-09-01")
  }

  open_chart_stats %>% write_rds(here::here("data/open_calls.rds"))
}


#' Get Chart Completion stats by day
#'
#' Returns tibble with an observation for each day in the incidents data.
#' All data references a 30-day rolling window.
#'
#' Variables:
#' - Open24Hr: Number of charts within past thirty days open for more than 24 hours.
#' - Open4Day: Number of charts open for more than 4 days.
#' - Open8Day: Number of charts open for more than 8 days.
#' - AllIncomplete: Total number of incomplete charts
#' - Incomplete30D: Total number of incomplete charts
#' - total_calls: Total charts within past thirty days
#' - incomplete30d_prop: Proportion of all calls within past thirty days taht are open
#' - Over24Hr_Prop: Proportion of all calls within past thirty days open for more than 24 hours
#' - Over4D_prop: Proportion of all calls within past thirty days open for more than 4 days
#' - Over8D_prop: Proportion of all calls within past thirty days open for more than 8 days
#'
#' @param incidents incidents range to compute
#'
#' @return tibble
#' @export
#'
get_chart_completion_stats_alt <- function(incidents) {
  library(furrr)
  plan("future::multisession")
  dates <- seq.POSIXt(from=min(floor_date(incidents$dtDate,unit="day")+hours(7)+minutes(0)+seconds(0)),
                      to=max(ceiling_date(incidents$dtDate,unit="day")),by="day")

  lead_providers <- incidents %>% pull(Lead) %>% unique()

  incidents_by_lead <- incidents |> group_by(Lead) |> nest()

  pb <- progress::progress_bar$new(total = length(dates)*length(lead_providers), clear = FALSE,
                                   format = "Calculating chart completion stats for :lead_provider :testDate [:bar] :percent eta: :eta")


  output <- future_map_dfr(incidents_by_lead, call_completion_stats_by_provider_alt, dates, pb,
                           .progress = TRUE,
                           .options = furrr_options(seed = 123))

  #output <- map_dfr(dates, call_completion_stats, incidents, pb)
  plan(sequential)
  output %>% mutate(incomplete30d_prop = Incomplete30D/total_calls,
                    Over24Hr_prop = Open24Hr/total_calls,
                    Over4D_prop = Open4Day/total_calls,
                    Over8D_prop = Open8Day/total_calls)
}

#' Gets call completion stats by provider
#'
#' @param lead_provider Lead provider
#' @param incidents incidents tibble
#' @param dates Vector of dates
#' @param pb progress bar
#'
#' @return Table of stats by day
call_completion_stats_by_provider_alt <- function(incidents_by_lead, dates, pb) {
  incidents_by_lead |> unnest()
  lead_provider <- incidents_by_lead$Lead[1]
  map_dfr(dates, call_completion_stats_alt, incidents_by_lead, lead_provider, pb)
}

#' Gets Call Completion Stats
#'
#' Internal function used to tick getting call completion stats for by day
#'
#' @param testDate Date to look at
#' @param incidents Incidents table
#' @param lead_provider Lead provider
#' @param pb progress bar
#'
#' @return Stats for given day grouped by lead
call_completion_stats_alt <- function(testDate, incidents, lead_provider, pb) {
  pb$tick(tokens = list(testDate = testDate, lead_provider = lead_provider))
  # Filter if they are open at this point in time
  incomplete <- incidents %>% filter(testDate %within% open_interval)
  total_incomplete <- incomplete %>% tally() %>% pull(n)

  # Additional filter for incident closed within 30 days
  filter_incomplete_30d <- incomplete |>  filter(incidentClosed >= testDate - days(30))

  total_incomplete_30d <-  filter_incomplete_30d %>% tally() %>% pull(n)


  all_calls <- incidents %>% filter(incidentClosed >= testDate - days(30), incidentClosed <= testDate)

  total_calls_30d <- all_calls %>% tally() %>% pull(n)

  # Calculate hours from incident closed to either test date or chart closed, whichever is first.
  all_calls %>%  # Filter for incidents within past 30
    mutate(test_interval = as.integer(int_length(interval(incidentClosed,
                                                          if_else((firstLockDate>testDate) | is.na(firstLockDate),
                                                                  testDate,firstLockDate)
    ))/3600
    )) %>%
    mutate(Open24Hr = if_else(test_interval > 24, 1, 0),
           Open4Day = if_else(test_interval > 24*4, 1, 0),
           Open8Day = if_else(test_interval > 24*8,1,0),
           testDate = testDate) %>%
    group_by(Lead, testDate) %>% summarize(Open24Hr = sum(Open24Hr), Open4Day = sum(Open4Day), Open8Day = sum(Open8Day)) %>%
    mutate(AllIncomplete = total_incomplete,
           Incomplete30D = total_incomplete_30d,
           total_calls = total_calls_30d)
}
