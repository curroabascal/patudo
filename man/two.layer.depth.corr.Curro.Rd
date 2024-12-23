% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/two.layer.depth.corr.Curro.R
\name{two.layer.depth.corr.Curro}
\alias{two.layer.depth.corr.Curro}
\title{Function to convert light at depth to light at surface
using a slight modification of the function in Rlibrary trackit.}
\usage{
two.layer.depth.corr.Curro(datetime, light, depth, weekbyweek = FALSE, D0 = 50)
}
\arguments{
\item{datetime}{vector of datetime as POSIXct}

\item{light}{vector with light levels}

\item{depth}{vector with depths}

\item{weekbyweek}{calculate attenuation coefficients per week or for whole time series}
}
\value{
Returns a vector with corrected light levels
}
\description{
Function to convert light at depth to light at surface
using a slight modification of the function in Rlibrary trackit.
}
