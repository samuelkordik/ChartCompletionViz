

#' Loads Incidents for date range
#'
#' Includes incident information and crew information, along with timeframe chart was open for (interval)
#'
#' @param start_date Date to start search
#' @param end_date Date to end search
#' @param source "custom" or "eso"
#'
#' @return Tibble of incidents
#' @export
#'
import_new_data <- function(start_date, end_date, source = "custom") {

  switch (source,
    "custom" = import_new_data_custom(start_date, end_date),
    "eso" = import_new_data_eso(start_date, end_date)
  )
}

import_new_data_eso <- function(start_date, end_date) {
  incidents <- get_eso_by_date(start_date, end_date, "Incidents")

  personnel <- get_eso_by_date(start_date, end_date, "Personnel") %>% unite(full_name, `Crew Member Last Name`, `Crew Member First Name`, sep=", ") %>%
    dplyr::select(PatientCareRecordId, full_name, `Crew Member Role`) %>% filter(`Crew Member Role` %in% c("Lead", "Driver"))

  personnel <- personnel %>% unique() %>% spread(key=`Crew Member Role`, value=full_name)

  incidents %>% left_join(personnel, by="PatientCareRecordId") -> incidents

  incidents %>% mutate(firstLockDate = mdy_hms(`First Lock Date`),
                       incidentClosed = mdy_hms(`Incident Closed Time`)) %>%
    select(-`First Lock Date`, -`Incident Closed Time`) %>%
    mutate(open_interval = interval(incidentClosed, firstLockDate))
}

import_new_data_custom <- function(start_date, end_date) {

  library(tidyverse)

  dataset <- readr::read_csv(here::here("data/completion_data.csv"))

  dataset |> select(`Incident Date`, `Incident Number`, `Lock Date`, `Sync Date`, `Is Lock`,
                    `First Lock Date`, `Run Type`, `Crew Member First Name`, `Crew Member Last Name`, `Crew Member Role`) |> distinct() -> incidents

  incidents <- incidents |> mutate(dtDate = lubridate::mdy_hms(`Incident Date`),
                                   firstLockDate = lubridate::mdy_hms(`First Lock Date`),
                                   incidentClosed = mdy_hms(`Incident Date`))

  incidents |> unite(full_name, `Crew Member Last Name`, `Crew Member First Name`, sep = ", ") |> filter(`Crew Member Role` %in% c("Lead", "Driver")) |>
    pivot_wider(names_from = `Crew Member Role`, values_from = full_name) |>
    select(-`First Lock Date`) |> mutate(open_interval = interval(dtDate, firstLockDate))
}
