\docType{package}
\name{ChIPprofileSimilarity}
\alias{ChIPprofileSimilarity-package}
\alias{ChIPprofileSimilarity}
\title{
ChIPprofileSimilarity: Metrics to estimate a level of similarity between two ChIP profiles
}
\description{
\pkg{ChIPprofileSimilarity} is calculing four differents metrics to estimate a level of similarity between two ChIP profiles.
}
\details{
The R function \code{\link{similarity}} calculates four differents metrics: 

\itemize{
        \item RATIO_AREA: The ratio between the areas. The larger 
                          value is always divided by the smaller value.
        \item DIFF_POS_MAX: The difference between the maximal peaks positions. 
                            The difference is always a positive value.
        \item RATIO_MAX_MAX: The ratio between the maximal peaks values. 
                            The larger value is always divided by the smaller
                            value. 
        \item RATIO_INTERSECT: The ratio between the intersection area and the 
                                total area. 
}

The function \code{\link{similarity}} also reports basic information about each ChIP profile such as the number of positions, the area, the maximum value and the position of the maximum value.

To learn more about \pkg{ChIPprofileSimilarity} package see:

\url{https://github.com/adeschen/ChIPprofileSimilarity/wiki}
}
\author{
A.L. Deschenes, 
E. Bertnachez,
C. Joly Beauparlant,
F.C. Lamaze,
R. Samb,
P. Belleau and
A. Droit 

Maintainer: Astrid Louise Deschenes <Astrid-Louise.Deschenes@crchuq.ulaval.ca>
}
\seealso{
\code{\link{similarity}}
}
\keyword{package}
