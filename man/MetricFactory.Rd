\name{MetricFactory-class}
\docType{class}

\alias{MetricFactory-class}
\alias{MetricFactory}

\title{
     MetricFactory object
}

\description{
    An object which is a interface to calculate all available metrics 
    separately.
}

\section{Constructor}{
    \describe{
        \item{}{
            \code{MetricFactory$new(
                    ratioAreaThreshold=1, 
                    ratioMaxMaxThreshold=1, 
                    ratioIntersectThreshold=1,
                    ratioNormalizedIntersectThreshold=1,
                    diffPosMaxThresholdMinValue=1, 
                    diffPosMaxThresholdMaxDiff=100, 
                    diffPosMaxTolerance=0.01,
                    spearmanCorrSDThreashold=1e-8)}:
    Create a MetricFactory object.
    \describe{
        \item{\code{ratioAreaThreshold}}{ The minimum denominator accepted to 
                        calculate the ratio of the area between both profiles. 
                        Default = 1.}
        \item{\code{ratioMaxMaxThreshold}}{ The minimum denominator accepted 
                        to calculate the ratio of the maximum values between 
                        both profiles. Default = 1.}
        \item{\code{ratioIntersectThreshold}}{ The minimum denominator 
                        accepted to calculate the ratio of the intersection 
                        area of both profiles and the total area. 
                        Default = 1.}
        \item{\code{ratioIntersectThreshold}}{ The minimum denominator 
                        accepted to calculate the ratio of the intersection 
                        area of both profiles and the total area for 
                        normalized profiles. Default = 1.}
        \item{\code{diffPosMaxThresholdMinValue}}{ The minimum peak accepted 
                        to calculate the metric. Default = 1.}
        \item{\code{diffPosMaxThresholdMaxDiff}}{ The maximum distance 
                        accepted between 2 peaks positions in one profile to 
                        calculate the metric. Default=100.}
        \item{\code{diffPosMaxTolerance}}{The maximum variation accepted on 
                        the maximum value to consider a position as a peak 
                        position. Default=0.01.}
        \item{\code{spearmanCorrSDThreashold}}{The minimum standard deviation
                        accepted on both profiles to consider to calculate 
                        the metric. Default=1e-8.}
    }
  }
 }
}

\details{
    The \code{MetricFactory} object is inspired from the factory design 
    pattern. Only one instance of \code{MetricFactory} object is necessary to 
    calculate all available metrics for different profiles, as long as the 
    thresholds set in the \code{MetricFactory} instance are appropriate for 
    the calculation. The thresholds are set during the \code{MetricFactory} 
    object creation and cannot be changed afterwards. If different thresholds 
    are needed, a new \code{MetricFactory} object, with the new thresholds, 
    must be instantiated.
}

\value{
    \code{MetricFactory$new} returns a \code{MetricFactory} object which 
    contains the information about the thresholds used to calculate each 
    metric. It can be used, as many times needed, to calculate the specified 
    metrics. 
}

\author{
    Astrid Louise Deschenes
    <Astrid-Louise.Deschenes@crchudequebec.ulaval.ca> 
}

\examples{
## Initialized the factory object
factory = MetricFactory$new(ratioAreaThreshold=100,
                            ratioIntersectThreshold=20,
                            diffPosMaxTolerance=0.04)

## Define 2 ChIP-Seq profiles
profile1=c(1,59,6,24,65,34,15,4,53,22)
profile2=c(15,9,46,44,9,39,27,34,34,4)

## Use the factory object to calculate each metric separatly
ratio_max_max <- factory$createMetric(metricType="RATIO_MAX_MAX", 
                                profile1, profile2)
ratio_max_max

diff_pos_max <- factory$createMetric(metric="DIFF_POS_MAX", profile1, profile2)
diff_pos_max

## Example using ChIP-Seq profiles of H3K27ac (DCC accession: ENCFF000ASG) 
## and H3K4me1 (DCC accession: ENCFF000ARY) from the Encyclopedia of DNA  
## Elements (ENCODE) for the region 
data(demoProfiles)

## Visualize ChIP-Seq profiles 
plot(demoProfiles$chr3.73159773.73160145$H3K27ac,
        type="l", col="blue", xlab="", ylab="", ylim=c(0, 125),
        main="chr3:73159773-73160145")
par(new=TRUE)
plot(demoProfiles$chr3.73159773.73160145$H3K4me1,
        type="l", col="darkgreen", xlab="Position", 
        ylab="Coverage in reads per million (RPM)",  ylim=c(0, 125))
legend("topright", c("H3K27ac","H3K4me1"), cex=1.2, 
        col=c("blue","darkgreen"), lty=1)

## Calculate metrics using factory object 

ratio_norm_intersect <- factory$createMetric(metricType = 
                        "RATIO_NORMALIZED_INTERSECT", 
                        profile1=demoProfiles$chr3.73159773.73160145$H3K4me1, 
                        profile2=demoProfiles$chr3.73159773.73160145$H3K27ac)
ratio_norm_intersect

ratio_area <- factory$createMetric(metricType="RATIO_AREA", 
                        profile1=demoProfiles$chr3.73159773.73160145$H3K4me1, 
                        profile2=demoProfiles$chr3.73159773.73160145$H3K27ac)
ratio_area
}
\seealso{
    \itemize{
        \item \code{\link{similarity}} for calculating all available metrics 
                            between two ChIP-Seq profiles.
        \item \code{\link{demoProfiles}} {for more informations about ChIP-Seq
                profiles present in the demoProfiles data.}
    }
}
\keyword{MetricFactory}
