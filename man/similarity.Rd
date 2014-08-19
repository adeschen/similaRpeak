\name{similarity}
\alias{similarity}

\title{
  Calculate 4 metrics which estimate the level of similarity between two ChIP-Seq profiles.
}

\description{
  Return a list containing information about both ChIP-Seq profiles and a \code{list} of all four similarity 
  metrics: the ratio of the maximum values, the ratio of the areas, the ratio between the intersection 
  area and the total area, the difference between two profiles maximal peaks positions.
}

\usage{
  similarity(
    profile1, 
    profile2, 
    ratioAreaThreshold=1, 
    ratioMaxMaxThreshold=1, 
    ratioIntersectThreshold=1)
}

\arguments{
  \item{profile1}{ Vector containing the RPM values of the first ChIP-Seq profile 
    for each position of the selected region.}
    
  \item{profile2}{ Vector containing the RPM values of the second ChIP-Seq profile 
    for each position of the selected region.}

  \item{ratioAreaThreshold}{ The minimum denominator accepted 
    to calculate the ratio of the area between both profiles. Default = 1.}

  \item{ratioMaxMaxThreshold}{ The minimum denominator accepted 
    to calculate the ratio of the maximal peaks values between both profiles. 
    Default = 1.}
    
  \item{ratioIntersectThreshold}{ The minimum denominator accepted 
    to calculate the ratio of the intersection area of both profiles over
    the total area. Default = 1.}
}

\details{
  \code{similarity} uses the two vectors passed as arguments to
  calculate the metrics. When the metric is a ratio, it always verify
  that the threshold for the denominator is respected. If the threshold
  is not respected, the metric is assigned the \code{NA} value.
}

\value{
  \code{similarity} returns a \code{list} which contains the
  information about both ChIP-Seq profiles and a \code{list} of 
  all four metrics.

  The data structure is a \code{list}  of \code{list}.

  The first level contain the following items:
  \itemize{
    \item nbrPosition: The number of positions included in each profile.
    \item areaProfile1: The area of the first profile.
    \item areaProfile2: The area of the second profile.
    \item maxProfile1: The maximum value in the first profile.
    \item maxProfile2: The maximum value in the second profile.
    \item maxPositionProfile1: The position of the maximum value in the 
        first profile.
    \item maxPositionProfile2: The position of the maximum value in the 
        second profile.
    \item metrics: A \code{list} with the
      following items:
      \itemize{
        \item RATIO_AREA: The ratio between the areas. The larger 
                          value is always divided by the smaller value.
                          \code{NA} if minimal threshold is not
                          respected.
        \item DIFF_POS_MAX: The difference between the maximal peaks positions. 
                            The difference is always a positive value.
        \item RATIO_MAX_MAX: The ratio between the maximal peaks values. 
                            The larger value is always divided by the smaller
                            value. \code{NA} if minimal threshold is not
                            respected.
        \item RATIO_INTERSECT: The ratio between the intersection area and the 
                                total area. \code{NA} if minimal threshold is not
                                respected.
        }
  }
}

\author{
  Astrid Louise Deschenes
  <Astrid-Louise.Deschenes@crchudequebec.ulaval.ca> 
  
  Elsa Bernatchez
  <elsa.bernatchez.1@ulaval.ca>
}

\examples{
# Example with vector directly used as inputs -----------------------------------------
ChIPprofileSimilarity:::similarity(c(3,59,6,24,65,34,15,4,53,22),
        c(15,9,46,44,9,39,27,34,34,4))

# Example with variables used as inputs and customised threshold ----------------------
profile1<-c(3,59,6,24,65,34,15,4,53,22,21,12,11)
profile2<-c(15,9,46,44,9,39,27,34,34,4,3,4,2)
ChIPprofileSimilarity:::similarity(profile1, profile2, 
        ratioAreaThreshold=5, ratioMaxMaxThreshold=5, ratioIntersectThreshold=12)

}
\keyword{similarity}
