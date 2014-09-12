\name{MetricFactory-class}
\docType{class}

\alias{MetricFactory-class}
\alias{MetricFactory}

\title{
  MetricFactory object.
}

\description{
    An object which is a interface to calculate all available metrics separately.
}

\section{Constructor}{
    \describe{
        \item{}{
            \code{MetricFactory$new(
    ratioAreaThreshold=1, 
    ratioMaxMaxThreshold=1, 
    ratioIntersectThreshold=1,
    diffPosMaxThresholdMinValue=1, 
    diffPosMaxThresholdMaxDiff=100, 
    diffPosMaxTolerance=0.01)}:
    Create a MetricFactory object.
    \describe{
        \item{\code{ratioAreaThreshold}}{TODO}
        \item{\code{ratioMaxMaxThreshold}}{TODO}
        \item{\code{ratioIntersectThreshold}}{TODO}
        \item{\code{diffPosMaxThresholdMinValue}}{TODO}
        \item{\code{diffPosMaxThresholdMaxDiff}}{TODO}
        \item{\code{diffPosMaxTolerance}}{TODO}
    }
  }
 }
}



\details{
  TODO
}

\value{
  \code{MetricFactory$new} returns a \code{MetricFactory} object which contains the
  information about the thresholds used to calculate each metric. It can be used, as
  many times needed, to calculate the specified metrics.
}

\author{
  Astrid Louise Deschenes
  <Astrid-Louise.Deschenes@crchudequebec.ulaval.ca> 
}

\examples{
# Initialized the factory object ------------------------------------------------------------
factory = MetricFactory$new(diffPosMaxTolerance=0.04)

# Use the factory object to calculate each metric separatly ---------------------------------
ratio_max_max <- factory$createMetric(metricType="RATIO_MAX_MAX", profile1=c(1,59,6,24,65,34,15,4,53,22), profile2=c(15,9,46,44,9,39,27,34,34,4))
ratio_max_max
ratio_area <- factory$createMetric(metricType="RATIO_AREA", profile1=c(1,59,6,24,65,34,15,4,53,22), profile2=c(15,9,46,44,9,39,27,34,34,4))
ratio_area
ratio_intersect <- factory$createMetric(metricType="RATIO_INTERSECT", profile1=c(1,59,6,24,65,34,15,4,53,22), profile2=c(15,9,46,44,9,39,27,34,34,4))
ratio_intersect
diff_pos_max <- factory$createMetric(metricType="DIFF_POS_MAX", profile1=c(1,59,6,24,65,34,15,4,53,22), profile2=c(15,9,46,44,9,39,27,34,34,4))
diff_pos_max
}
\keyword{MetricFactory}
