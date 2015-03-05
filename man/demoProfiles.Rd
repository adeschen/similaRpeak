\name{demoProfiles.Rd}
\alias{demoProfiles.Rd}
\docType{data}
\title{
Selected ChIP-seq profiles related to enhancers 
H3K27ac and H3K4me1 (for demonstration purpose)
}
\description{
ChIP-Seq profiles of two histone post-transcriptional modifications 
linked to highly active enhancers H3K27ac (DCC accession: ENCFF000ASG) and 
H3K4me1 (DCC accession: ENCFF000ARY) from the Encyclopedia of DNA Elements 
(ENCODE) data (Dunham I et al. 2012).
}
\usage{data("demoProfiles")}
\format{
  A list with 4 entries. Each entry is a list of 2 ChIP-Seq profiles, one per
  active enhancer (H3K27ac and H3K4me1).The 2 ChIP-Seq profiles are of 
  identical length and specific to a genomic region. Each ChiP-Seq profile is a 
  numerical vector containing the profiles values at each position, as reported
  in reads per million (RPM),
  \describe{
    \item{\code{demoProfiles}}{ a list containing all demo ChIP-Seq profiles }
    \item{\code{demoProfiles$chr2.70360770.70361098}}{ a list containing
            2  ChIP-Seq profiles for the genomic region chr2:70360770-70361098 }
    \item{\code{demoProfiles$chr2.70360770.70361098$H3K27ac}}{ a numeric vector 
                containing the
                profiles values related to the enhancer H3K27ac, as reported 
                in reads per million (RPM). The first entry of the vector is for
                position chr1:70360770 while the last entry is for position
                chr2:70361098 }
    \item{\code{demoProfiles$chr2.70360770.70361098$H3K4me1}}{ a numeric vector 
                containing the
                profiles values related to the enhancer H3K4me1, as reported 
                in reads per million (RPM). The first entry of the vector is for
                position chr1:70360770 while the last entry is for position
                chr2:70361098 }
    \item{\code{demoProfiles$chr3.73159773.73160145$H3K4me1}}{ a list containing
            2  ChIP-Seq profiles for the genomic region chr3:73159773-73160145 }
    \item{\code{demoProfiles$chr3.73159773.73160145$H3K27ac}}{ a numeric vector 
                containing the
                profiles values related to the enhancer H3K27ac, as reported 
                in reads per million (RPM). The first entry of the vector is for
                position chr2:73159773 while the last entry is for position
                chr3:73160145 }
    \item{\code{demoProfiles$chr3.73159773.73160145$H3K4me1}}{ a numeric vector 
                containing the
                profiles values related to the enhancer H3K4me1, as reported 
                in reads per million (RPM). The first entry of the vector is for
                position chr3:73159773 while the last entry is for position
                chr3:73160145 }
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
data(demoProfiles)

}
\keyword{datasets}
