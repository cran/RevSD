\name{revsd}
\alias{revsd}
\title{Visualizing the Standard Deviation via Revolution}
\usage{
revsd(data, shouldInstallAllPackages)
}
\arguments{
  \item{data}{The data that a user inputs, usually a vector of values.}
  \item{shouldInstallAllPackages}{An optional argument, takes a boolean value as input which decides whether or not to install the following packages which are required to launch the application: shiny, shinydashboard, plotrix, and shinyWidgets. FALSE by default.}
}
\value{
No return value, the function will open a new window and display a shiny interactive application.
}
\description{
We visualize the standard deviation of a data set by revolving the empirical cumulative distribution function about the vertical line through the mean. The total volume of the collection of cylinders is equated to the volume of a single cylinder of height 1. The radius of this single cylinder is the standard deviation.
}
\examples{
  revsd(c(10,18,23,30,36), shouldInstallAllPackages = FALSE)
}

