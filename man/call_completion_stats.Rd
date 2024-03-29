% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stats.R
\name{call_completion_stats}
\alias{call_completion_stats}
\title{Gets Call Completion Stats}
\usage{
call_completion_stats(testDate, incidents, lead_provider, pb)
}
\arguments{
\item{testDate}{Date to look at}

\item{incidents}{Incidents table}

\item{lead_provider}{Lead provider}

\item{pb}{progress bar}
}
\value{
Stats for given day grouped by lead
}
\description{
Internal function used to tick getting call completion stats for by day
}
