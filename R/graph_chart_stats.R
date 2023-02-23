#' Graph Chart Stats
#'
#' Creates a chart of open chart rates for all times with gems showing specific current metrics.
#'
#'
#' @param remove_worst_performers (integer) optionally removes the top n worst performers, base on most recent % charts open more than 8 days.
#'
#' @return Plot item
#' @export
#'
graph_chart_stats <- function(remove_worst_performers = NULL) {
  library(tidyverse)
  library(ggplot2)
  open_chart_stats <- get_open_chart_stats()

  stopifnot(max(open_chart_stats %>% count(testDate) %>% pull(n))==1)

  if (!is.null(remove_worst_performers)) {
    open_chart_stats |> filter(testDate == max(open_chart_stats$testDate, na.rm=TRUE)) |>
      filter(total_calls > 1) |>
      select(Lead, total_calls,
             `% Open More Than 8 Days` = Over24Hr_prop) |>
      filter(total_calls > 0) |> arrange(desc(`% Open More Than 8 Days`)) %>%
      select(Lead) %>% head(remove_worst_performers) %>% pull(Lead) -> worst_performers

    open_chart_stats %>% filter(!(Lead %in% worst_performers)) -> open_chart_stats
  }

  # output %>% write_rds(here::here("data/open_calls.rds"))
  #
  # range(output$testDate)
  # range(open_chart_stats$testDate)

  open_chart_stats_long <- open_chart_stats %>% ungroup() %>%
    select(testDate, `% Open More Than 24 Hours` = Over24Hr_prop,
           `% Open More Than 4 Days` = Over4D_prop,
           `% Open More Than 8 Days` = Over8D_prop,
           `Total % Incomplete Charts` = incomplete30d_prop) |>
    group_by(testDate) |> summarize(across(where(is.numeric),  ~ mean(.x, na.rm=TRUE))) |>
    filter(testDate > min(testDate) + days(30)) %>% pivot_longer(!testDate, names_to="Measure", values_to = "Value")


  open_chart_stats_long$Measure <- factor(open_chart_stats_long$Measure, levels = c("Total % Incomplete Charts",
                                                                                    "% Open More Than 24 Hours",
                                                                                    "% Open More Than 4 Days",
                                                                                    "% Open More Than 8 Days"))

  open_chart_stats_long %>%
    ggplot() + geom_line(aes(x=testDate, y=Value, group=Measure, colour=Measure), size=1) + ggtitle("Chart Completion performance") +
    theme_light() +
    scale_colour_manual(values=c(
      "% Open More Than 24 Hours" = "firebrick4",
      "% Open More Than 4 Days" = "darkorange3",
      "% Open More Than 8 Days" = "darkolivegreen3",
      "Total % Incomplete Charts" = "gray70"
    )) +
    scale_alpha_manual(values=c(
      "% Open More Than 24 Hours" = 1,
      "% Open More Than 4 Days" = 1,
      "% Open More Than 8 Days" = 1,
      "Total % Incomplete Charts" = .6
    )) +
    scale_y_continuous(breaks=seq(0,1,.01), labels=scales::percent) +
    scale_x_datetime(name="Date", date_breaks="1 month", date_labels="%m, %Y", date_minor_breaks="1 week") +
    theme(legend.position="bottom") +
    guides(colour=guide_legend(title="Measure")) -> p1

  curr_date <-  max(open_chart_stats$testDate, na.rm=TRUE) |> format.Date("%Y-%m-%d")

  open_chart_stats %>% arrange(testDate) %>% ungroup() |>
    select(testDate, `% Open More Than 24 Hours` = Over24Hr_prop,
           `% Open More Than 4 Days` = Over4D_prop,
           `% Open More Than 8 Days` = Over8D_prop,
           `Total % Incomplete Charts` = incomplete30d_prop) %>%
    group_by(testDate) |> summarize(across(where(is.numeric), mean, na.rm=TRUE)) |>
    slice_tail(n=1) |>
    pivot_longer(!testDate, names_to="Measure", values_to = "Value") %>%
    mutate(Value = paste0(round(Value, 4)*100, "%")) %>%
    ggplot(aes(x=1, y=4:1)) +
    geom_text(aes(label=Measure), size=5) +
    geom_text(aes(label=Value), nudge_y = .3, size=8, fontface="bold") +
    theme_void() + ggtitle(paste0("Current Performance, as of ", curr_date)) -> p2

  library(patchwork)

  p1+p2+plot_layout(ncol = 2, widths = c(4,1))
  ggsave(here::here(paste0(curr_date, "_open_calls.pdf")), width=17, height=10, dpi=120)
  p1+p2+plot_layout(ncol = 2, widths = c(4,1))
}
