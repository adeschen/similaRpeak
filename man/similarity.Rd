\name{similarity}
\alias{similarity}

\title{
    Calculate metrics which estimate the level of similarity between two 
    ChIP-Seq profiles
}

\description{
    Return a list containing information about both ChIP-Seq profiles and a 
    \code{list} of all similarity metrics: the ratio of the maximum values, 
    the ratio of the areas, the ratio between the intersection area and the 
    total area (for normalized and non-normalized profiles), the difference 
    between two profiles maximal peaks positions and the Spearman's rho 
    statistic.
}

\usage{
    similarity(
        profile1, 
        profile2, 
        ratioAreaThreshold=1, 
        ratioMaxMaxThreshold=1, 
        ratioIntersectThreshold=1,
        ratioNormalizedIntersectThreshold=1,
        diffPosMaxThresholdMinValue=1, 
        diffPosMaxThresholdMaxDiff=100, 
        diffPosMaxTolerance=0.01,
        spearmanCorrSDThreashold=1e-8)
}

\arguments{
    \item{profile1}{ Vector containing the RPM values of the first ChIP-Seq 
        profile for each position of the selected region.}
    
    \item{profile2}{ Vector containing the RPM values of the second ChIP-Seq 
        profile for each position of the selected region.}

    \item{ratioAreaThreshold}{ The minimum denominator accepted 
        to calculate the ratio of the area between both profiles. The value 
        has to be positive. Default = 1.}

    \item{ratioMaxMaxThreshold}{ The minimum denominator accepted 
        to calculate the ratio of the maximal peaks values between both 
        profiles. The value has to be positive. Default = 1.}
    
    \item{ratioIntersectThreshold}{ The minimum denominator accepted 
        to calculate the ratio of the intersection area of both profiles over
        the total area. The value has to be positive. Default = 1.}
    
    \item{ratioNormalizedIntersectThreshold}{ The minimum denominator accepted 
        to calculate the ratio of the intersection area of both normalized 
        profiles over the total area. The value has to be positive. 
        Default = 1.
        }
    
    \item{diffPosMaxThresholdMinValue}{ The minimum peak accepted to calculate 
        the metric. The value has to be positive. Default = 1.}
    
    \item{diffPosMaxThresholdMaxDiff}{ The maximum distance accepted between 2 
        peaks positions in one profile to calculate the metric. The value has 
        to be positive. Default=100.}
        
    \item{diffPosMaxTolerance}{ The maximum of variation accepted 
        on the maximum value to consider a position as a peak position. The 
        value can be between 0 and 1. Default=0.01.}
        
    \item{spearmanCorrSDThreashold}{ The minimum standard deviation accepted 
        on both profiles to calculate the metric. Default=1e-8.}
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
    all metrics.

    The data structure is a \code{list}  of \code{list}.

    The first level contain the following items:
    \itemize{
        \item nbrPosition: The number of positions included in each profile.
        \item areaProfile1: The area of the first profile.
        \item areaProfile2: The area of the second profile.
        \item maxProfile1: The maximum value in the first profile.
        \item maxProfile2: The maximum value in the second profile.
        \item maxPositionProfile1: The list of positions of the maximum value 
            in the first profile.
        \item maxPositionProfile2: The list of positions of the maximum value 
            in the second profile.
        \item metrics: A \code{list} with the
            following items:
        \itemize{
            \item RATIO_AREA: The ratio between the areas. The larger 
                        value is always divided by the smaller value.
                        \code{NA} if minimal threshold is not
                        respected.
            \item DIFF_POS_MAX: The difference between the maximal peaks 
                        positions. The difference is always the first profile 
                        value minus the second profile value. \code{NA} is 
                        returned if minimal peak value is not 
                        respected. A profile can have more than one 
                        position with the maximum value. In that case, 
                        the median position is used. A threshold argument 
                        can be set to consider all positions within a 
                        certain range of the maximum value. A threshold 
                        argument can also be set to ensure that the 
                        distance between two maximum values is not too 
                        wide. When this distance is not respected, 
                        it is assumed that more than one peak is present 
                        in the profile  and \code{NA} is returned.
            \item RATIO_MAX_MAX: The ratio between the maximal peaks values. 
                        The first profile is always divided by the second 
                        profile. \code{NA} if minimal threshold is not
                        respected.
            \item RATIO_INTERSECT: The ratio between the intersection area and 
                        the total area. \code{NA} if minimal threshold is 
                        not respected.
            \item RATIO_NORMALIZED_INTERSECT: The ratio between the 
                        intersection area and the total area of normalized 
                        profiles. \code{NA} if minimal threshold is not 
                        respected.
            \item SPEARMAN_CORRELATION: The Spearman's rho statistic between 
                        profiles. \code{NA} if minimal threshold is 
                        not respected or when no complete element pair is 
                        present between both profiles.
        }
  }
}

\author{
    Astrid Louise Deschenes
    <Astrid-Louise.Deschenes@crchudequebec.ulaval.ca> 
  
    Elsa Bernatchez
}

\examples{
# Defining two CHiP-Seq profiles 
profile1<-c(3,59,6,24,65,34,15,4,53,22,21,12,11)
profile2<-c(15,9,46,44,9,39,27,34,34,4,3,4,2)

# Example usign default thresholds
similarity(profile1, profile2)

# Example using customised thresholds
similarity(profile1, profile2, 
                        ratioAreaThreshold=5, 
                        ratioMaxMaxThreshold=5, 
                        ratioIntersectThreshold=12,
                        ratioNormalizedIntersectThreshold=2.2,
                        diffPosMaxThresholdMinValue=2, 
                        diffPosMaxThresholdMaxDiff=130, 
                        diffPosMaxTolerance=0.03,
                        spearmanCorrSDThreashold=1e-3)

# You can refer to the vignette to see exemples using ChIP-Seq profiles
# extractedfrom the Encyclopedia of DNA Elements (ENCODE) data.
}
\seealso{
    \itemize{
        \item \code{\link{MetricFactory}} for using a interface to calculate 
                                all available metrics separately.
        \item \code{\link[=../doc/similaRpeak.html]{similarRpeak vignette}}
    }
}
\keyword{similarity}
