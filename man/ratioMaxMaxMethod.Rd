% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/metricsMethods.R
\name{ratioMaxMaxMethod}
\alias{ratioMaxMaxMethod}
\title{Ratio of profiles maximal peaks between two profiles}
\usage{
ratioMaxMaxMethod(profile1, profile2, threshold = 1)
}
\arguments{
\item{profile1}{a \code{vector} of \code{numeric} values, the first profile
containing the alignment depth for each position. The \code{profile1} and 
\code{profile2} should have the same length.}

\item{profile2}{a \code{vector} of \code{numeric} values, the second profile
containing the alignment depth for each position. The \code{profile1} and 
\code{profile2} should have the same length.}

\item{threshold}{a \code{numeric}, the minimum peak value accepted 
to calculate a ratio.}
}
\value{
The calculated ratio or \code{NA} if one peak value is inferior to
the threshold.
}
\description{
Calculate and return the ratio of profiles maximal peaks
between two profiles covering the same range. The  
maximal peak of the first profile is always divided by the maximal peak of
the second profile.If one peak value is inferior to the threshold, the 
function returns \code{NA}.
}
\seealso{
\itemize{
\item \code{\link{MetricFactory}} {for using the recommanded interface to 
calculate all available metrics separately or togheter.}
}
}
\author{
Astrid Deschenes, Elsa Bernatchez
}
