\name{chr7_profiles}
\alias{chr7_profiles}
\docType{data}
\title{
ChIP-Seq profiles of region chr7:61968807-61969730 related to enhancers 
H3K27ac and H3K4me1 (for demonstration purpose)
}
\description{
ChIP-Seq profiles of region chr7:61968807-61969730 of two histone 
post-transcriptional modifications linked to highly active enhancers H3K27ac 
and H3K4me1 (for demonstration purpose)
}
\usage{data("chr7_profiles")}
\format{
  Two vectors of length 924.
  \describe{
    \item{\code{H3K27ac_chr7_profile}}{a numeric vector containing the
                profiles values related to the enhancer H3K27ac, as reported 
                in reads per million (RPM), for the region 
                chr7:61968807-61969730. The first entry of the vector is for
                position chr7:61968807 while the last entry is for position
                chr7:61969730.}
    \item{\code{H3K4me1_chr7_profile}}{a numeric vector containing the
                profiles values related to the  enhancer H3K4me1, as reported 
                in reads per million (RPM), for the region 
                chr7:61968807-61969730. The first entry of the vector is for
                position chr7:61968807 while the last entry is for position
                chr7:61969730.}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
Dunham I, Kundaje A, Aldred SF, et al. An integrated encyclopedia of DNA 
elements in the human genome. Nature. 2012 Sep 6;489(7414):57-74.
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(chr7_profiles)

## Metrics calculation using chr7_profiles data and default parameters
metrics <- similarity(H3K27ac_chr7_profile, H3K4me1_chr7_profile)
metrics
}
\keyword{datasets}
