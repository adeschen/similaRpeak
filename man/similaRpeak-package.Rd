% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/similaRpeak.R
\docType{package}
\name{similaRpeak-package}
\alias{similaRpeak-package}
\alias{similaRpeak}
\title{similaRpeak: Metrics to estimate a level of similarity between two 
ChIP-Seq profiles}
\description{
This package is calculating six differents metrics to estimate a 
level of similarity between two ChIP-Seq profiles.
}
\details{
The \code{\link{similarity}} function calculates six differents metrics:
\itemize{
\item RATIO_AREA: The ratio between the areas. The larger value is always 
divided by the smaller value.
\item DIFF_POS_MAX: The difference between the maximal peaks positions. The 
difference is always a positive value.
\item RATIO_MAX_MAX: The ratio between the maximal peaks values. The 
larger value is always divided by the smaller value. 
\item RATIO_INTERSECT: The ratio between the intersection area and the 
total area.
\item RATIO_NORMALIZED_INTERSECT: The ratio between the intersection area 
and the total area of two normalized profiles. The profiles are normalized 
by divinding them by their average value.
\item SPEARMAN_CORRELATION: The Spearman's rho statistic between profiles.  
}

The function \code{\link{similarity}} also reports basic information about
each ChIP profile such as the number of positions, the area, the maximum 
value and the position of the maximum value.

To learn more about \pkg{similaRpeak} package see:
\url{https://github.com/adeschen/similaRpeak/wiki}
}
\seealso{
\itemize{
\item \code{\link{MetricFactory}} {for using a interface to calculate all 
available metrics separately.}
\item \code{\link{similarity}} {for calculating all available metrics 
between two ChIP-Seq profiles.}
}
}
\author{
Astrid Deschenes,
Elsa Bernatchez,
Charles Joly Beauparlant,
Fabien Claude Lamaze,
Rawane Samb,
Pascal Belleau and
Arnaud Droit

Maintainer:
Astrid Deschenes <adeschen@hotmail.com>
}
\keyword{package}
