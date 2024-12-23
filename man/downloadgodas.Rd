% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/downloadgodas.R
\name{downloadgodas}
\alias{downloadgodas}
\title{This is a function to download data from GODAS ncep using opendap.}
\usage{
downloadgodas(
  product = c("mld", "temp"),
  year,
  minlon,
  maxlon,
  minlat,
  maxlat,
  maxdepth,
  outputfolder
)
}
\arguments{
\item{product}{Product to download. Mixed layer depth (mld) or Potential temperature (temp)}

\item{year}{year (numeric)}

\item{minlon}{Longitude in decimal degrees}

\item{maxlon}{Latitude in decimal degrees}

\item{minlat}{Longitude in decimal degrees}

\item{maxlat}{Latitude in decimal degrees}

\item{maxdepth}{Maximum depth to download}

\item{outputfolder}{folder to store downloads}
}
\value{
Saves the download as an RDS file in the output folder
}
\description{
This is a function to download data from GODAS ncep using opendap.
}
\examples{
dir.create('oceanodata')
downloadgodas(product='temp',year=1999,minlon=120,maxlon=240,minlat=-40,maxlat=40,maxdepth=500,outputfolder = 'oceanodata')
downloadgodas(product='mld',year=2000,minlon=120,maxlon=240,minlat=-40,maxlat=40,outputfolder = 'oceanodata')
}
