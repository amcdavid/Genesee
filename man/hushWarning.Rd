% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utility.R
\name{hushWarning}
\alias{hushWarning}
\title{Selectively muffle warnings based on output}
\usage{
hushWarning(expr, regexp)
}
\arguments{
\item{expr}{an expression}

\item{regexp}{a regexp to be matched (with str_detect)}
}
\value{
the result of expr
}
\description{
Selectively muffle warnings based on output
}
\examples{
Genesee:::hushWarning(warning('Beware the rabbit'), 'rabbit')
Genesee:::hushWarning(warning('Beware the rabbit'), 'hedgehog')
}
