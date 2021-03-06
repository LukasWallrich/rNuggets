% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/misc.R
\name{gcd.hf}
\alias{gcd.hf}
\title{Geosedic distance between two points.}
\source{
Adapted from
  https://www.r-bloggers.com/great-circle-distance-calculations-in-r/
}
\usage{
gcd.hf(long1, lat1, long2, lat2)
}
\arguments{
\item{long1}{Longitude of point 1 in degrees.}

\item{lat1}{Latitude of point 1 in degrees.}

\item{long2}{Longitude of point 2 in degrees.}

\item{lat2}{Latitude of point 2 in degrees.}
}
\value{
The distance betwen the two points in kilometers.
}
\description{
Calculates the geodesic distance between two points specified by their
longitude and latitude in degrees calculated using the Haversine formula.
This should give accurate results for small and medium distances, but assumes
a spherical earth. Algorithms that accurately model an ellipsoid earth are
slower, but should be used for long distances.
}
