% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/test-functions.R
\name{predatorprey.plot0}
\alias{predatorprey.plot0}
\title{predatorprey.plot0}
\usage{
predatorprey.plot0(x1, x2, x3, x4, title = NULL)
}
\arguments{
\item{x1}{The growth rate of prey}

\item{x2}{The decay rate of predator}

\item{x3}{The predating effect on prey}

\item{x4}{The predating effect on predator}

\item{title}{The optional plot title. May be omited.}
}
\value{
An ggplot2 object
}
\description{
Generate a plot for the predator-prey ODE output.
}
\examples{
\dontrun{
 predatorprey.plot0(1.351888, 1.439185, 1.337083, 0.9079049)
}

}
