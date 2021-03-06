% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/swatches.R
\name{extract_swatch}
\alias{extract_swatch}
\title{Extract swatch RGB values from a raw RGB array}
\usage{
extract_swatch(arr, xmin, xmax, ymin, ymax)
}
\arguments{
\item{arr}{The 3D RGB array to use}

\item{xmin}{The position of the leftmost pixel column}

\item{xmax}{The position of the rightmost pixel column}

\item{ymin}{The position of the topmost pixel row}

\item{ymax}{The position of the bottom pixel row}
}
\value{
a 3-column matrix with numeric values for red, green, and blue between 0 and 255
}
\description{
Extract swatch RGB values from a raw RGB array
}
