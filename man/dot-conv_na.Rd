% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fcts_cleanup.R
\name{.conv_na}
\alias{.conv_na}
\title{Set non-informative values to NA}
\usage{
.conv_na(x)
}
\arguments{
\item{x}{variable to be cleaned}
}
\value{
variable with values such as "na" set to NA
}
\description{
Set non-informative values to NA
}
\details{
Replaced strings: NA, nan, inf, "NA", "NAN", "null" with NA
}
