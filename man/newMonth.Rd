% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/main.R
\name{newMonth}
\alias{newMonth}
\title{newMonth}
\usage{
newMonth(
  filename_ = paste0(format(Sys.Date(), "\%Y\%m"), ".Rmd"),
  template_ = "~/month.Rmd"
)
}
\description{
Copy the month template and execute the marked section.
}
