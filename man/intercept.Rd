% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/integrations.R
\name{intercept}
\alias{intercept}
\title{Wraps a plotting function (e.g. plot) so that its output is intercepted
by GoFigr.}
\usage{
intercept(plot_func)
}
\arguments{
\item{plot_func}{function to intercept}
}
\value{
intercepted function
}
\description{
Wraps a plotting function (e.g. plot) so that its output is intercepted
by GoFigr.
}
\examples{
gf_plot <- intercept(base::plot)
}
