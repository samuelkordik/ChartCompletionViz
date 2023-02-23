#' Graph Performance by Provider
#'
#' Graphs each individual provider's performance over time as a ridgeline plot
#' Sorted by % open more than 8 days, with % open more than 24 hours included to side.
#'
#' @return plot element
#' @export
#'
graph_chart_by_provider <- function() {
  open_chart_stats <- get_open_chart_stats()
  open_chart_stats |> filter(testDate == max(open_chart_stats$testDate, na.rm=TRUE)) |>
    filter(total_calls > 1) |>
    select(Lead, total_calls,
           `% Open More Than 24 Hours` = Over24Hr_prop,
           `% Open More Than 4 Days` = Over4D_prop,
           `% Open More Than 8 Days` = Over8D_prop,
           `Total % Incomplete Charts` = incomplete30d_prop) |>
    filter(total_calls > 0) |> arrange(desc(`% Open More Than 8 Days`)) -> recent_lead_performance


  #install.packages("ggridges")
  library(ggridges)


  # Ridgeline plot

  open_chart_stats_long_lead <- open_chart_stats %>%
    select(testDate, Lead, `% Open More Than 24 Hours` = Over24Hr_prop,
           `% Open More Than 4 Days` = Over4D_prop,
           `% Open More Than 8 Days` = Over8D_prop,
           `Total % Incomplete Charts` = incomplete30d_prop) |>
    filter(testDate > min(testDate) + days(30)) %>% pivot_longer(where(is.numeric), names_to="Measure", values_to = "Value")


  open_chart_stats_long_lead$Measure <- factor(open_chart_stats_long_lead$Measure, levels = c("Total % Incomplete Charts",
                                                                                              "% Open More Than 24 Hours",
                                                                                              "% Open More Than 4 Days",
                                                                                              "% Open More Than 8 Days"))

  # left_join(recent_lead_performance |> select(Lead, total_calls,
  #         now_open24 = `% Open More Than 24 Hours`,
  #                 now_open4 = `% Open More Than 4 Days` ,
  #                 now_open8 = `% Open More Than 8 Days` ,
  #                 now_open_in = `Total % Incomplete Charts` ), by="Lead")
  #
  recent_lead_performance <- recent_lead_performance |> arrange(`% Open More Than 24 Hours`)
  open_chart_stats_long_lead$Lead24 <- factor(open_chart_stats_long_lead$Lead, levels = recent_lead_performance$Lead)

  open_chart_stats_long_lead$Lead24 <- fct_relevel(open_chart_stats_long_lead$Lead24, recent_lead_performance$Lead)

  recent_lead_performance$Lead <- factor(recent_lead_performance$Lead, levels = recent_lead_performance$Lead)

  open_chart_stats_long_lead |> filter(testDate >= max(testDate) - days(90)) |> filter(!is.na(Lead24)) -> ocsll

  library(gghighlight)

  ocsll |>
    #left_join(recent_lead_performance, by=c("Lead24" = "Lead")) |>
    ggplot(aes(x = testDate, y = Lead24, height = Value, fill = Measure)) + geom_ridgeline() +
    geom_text(aes(testDate + days(5), y = Lead24, label = sprintf('%0.1f%%', Value*100),
                  color = ),
              data = ocsll |> filter(Measure == "% Open More Than 24 Hours") |> slice_tail( n = 1),
              position = position_nudge(30, 0.5)) +
    #facet_grid(. ~ Measure, scales = "free") +
    theme_light() +
    scale_fill_manual(values=c(
      "% Open More Than 24 Hours" = "firebrick4",
      "% Open More Than 4 Days" = "darkorange3",
      "% Open More Than 8 Days" = "darkolivegreen3",
      "Total % Incomplete Charts" = "gray70"
    )) +
    scale_x_datetime(name="Date", date_breaks="1 month", date_labels="%m, %Y", date_minor_breaks="1 week") +
    theme(legend.position="bottom") +
    guides(colour=guide_legend(title="Measure"))+
    xlab("Lead Provider") + ylab("Date")+ggtitle("Per Provider Performance, Past 90 Days")

}
