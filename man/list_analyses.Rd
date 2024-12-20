% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/analysis.R
\name{list_analyses}
\alias{list_analyses}
\title{Lists analyses under a workspace.}
\usage{
list_analyses(gf, workspace_id = NULL)
}
\arguments{
\item{gf}{GoFigr client}

\item{workspace_id}{API id of the workspace}
}
\value{
list of analyses
}
\description{
Lists analyses under a workspace.
}
\examples{
list_analyses() # will use default workspace as specified in the GoFigr config
list_analyses("59da9bdb-2095-47a9-b414-c029f8a00e0e")
}
