% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{symmetry_analysis}
\alias{symmetry_analysis}
\title{Image Symmetry}
\usage{
symmetry_analysis(images)
}
\arguments{
\item{images}{Folder where images are stored}
}
\value{
Returns a dataframe with horizontal, vertical, and diagonal symmetry
}
\description{
Gets image symmetry
}
\details{
Detects symmetry in an image along three axes.

Closer to zero means more symmetrical, positive means the image has more ink left or up, negative the opposite

The diagonal method has defocused areas along the line y=x, the priority is reading symmetry not in the center of the image
}
\examples{
symmetry(here("Images/"))
}
