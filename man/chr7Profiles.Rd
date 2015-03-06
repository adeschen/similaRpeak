\name{chr7Profiles}
\docType{data}
\alias{chr7Profiles}
\title{
ChIP-Seq profiles of region chr7:61968807-61969730 related to enhancers 
H3K27ac and H3K4me1 (for demonstration purpose)
}
\description{
ChIP-Seq profiles of region chr7:61968807-61969730 of two histone 
post-transcriptional modifications linked to highly active enhancers H3K27ac 
(DCC accession: ENCFF000ASG) and H3K4me1 (DCC accession: ENCFF000ARY) from 
the Encyclopedia of DNA Elements (ENCODE) data (Dunham I et al. 2012).
}
\usage{data(chr7Profiles)}
\format{
    A list with 1 entry. The entry is a list of 2 ChIP-Seq profiles, one per
    active enhancer (H3K27ac and H3K4me1).The 2 ChIP-Seq profiles are of 
    identical length and specific to a genomic region. Each ChiP-Seq profile 
    is a numerical vector containing the profiles values at each position, as 
    reported in reads per million (RPM),
  \describe{
    \item{\code{chr7Profiles}}{ a list containing 1 entry }
    \item{\code{chr7Profiles$chr7.61968807.61969730}}{ a list containing
                2  ChIP-Seq profiles for the genomic region 
                chr7:6196880-61969730 }
    \item{\code{demoProfiles$chr7.61968807.61969730$H3K27ac}}{ a numeric 
                vector containing the profiles values related to the enhancer 
                H3K27ac, as reported in reads per million (RPM). The first 
                entry of the vector is for position chr7:61968807 while the 
                last entry is for position chr7:61969730 }
    \item{\code{demoProfiles$chr7.61968807.61969730$H3K4me1}}{ a numeric 
                vector containing the profiles values related to the 
                enhancer H3K4me1, as reported in reads per million (RPM). The 
                first entry of the vector is for position chr7:61968807 while 
                the last entry is for position chr7:61969730 }
  }
}
\source{
The Encyclopedia of DNA Elements (ENCODE) data
}
\references{
Dunham I, Kundaje A, Aldred SF, et al. An integrated encyclopedia of DNA 
elements in the human genome. Nature. 2012 Sep 6;489(7414):57-74.
}
\examples{
data(chr7Profiles)

## Calculating all metrics for the "chr7.61968807.61969730" region 
metrics <- similarity(chr7Profiles$chr7.61968807.61969730$H3K4me1, 
                        chr7Profiles$chr7.61968807.61969730$H3K27ac, 
                        ratioAreaThreshold=10, 
                        ratioMaxMaxThreshold=4, 
                        ratioIntersectThreshold=5, 
                        ratioNormalizedIntersectThreshold=2,
                        diffPosMaxThresholdMinValue=10, 
                        diffPosMaxThresholdMaxDiff=100, 
                        diffPosMaxTolerance=0.10)
metrics


## You can refer to the vignette to see more examples using ChIP-Seq profiles
## extracted from the Encyclopedia of DNA Elements (ENCODE) data.
}\seealso{
    \itemize{
        \item \code{\link{MetricFactory}} {for using a interface to calculate 
                        all available metrics separately.}
        \item \code{\link{similarity}} {for calculating all available metrics 
                        between two ChIP-Seq profiles.}
    }
}
\keyword{datasets}
