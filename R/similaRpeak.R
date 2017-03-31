#' similaRpeak: Metrics to estimate a level of similarity between two 
#' ChIP-Seq profiles
#'
#' This package is calculating six differents metrics to estimate a 
#' level of similarity between two ChIP-Seq profiles.
#'
#' The \code{\link{similarity}} function calculates six differents metrics:
#' \itemize{
#' \item RATIO_AREA: The ratio between the areas. The larger value is always 
#' divided by the smaller value.
#' \item DIFF_POS_MAX: The difference between the maximal peaks positions. The 
#' difference is always a positive value.
#' \item RATIO_MAX_MAX: The ratio between the maximal peaks values. The 
#' larger value is always divided by the smaller value. 
#' \item RATIO_INTERSECT: The ratio between the intersection area and the 
#' total area.
#' \item RATIO_NORMALIZED_INTERSECT: The ratio between the intersection area 
#' and the total area of two normalized profiles. The profiles are normalized 
#' by divinding them by their average value.
#' \item SPEARMAN_CORRELATION: The Spearman's rho statistic between profiles.  
#' }
#' 
#' The function \code{\link{similarity}} also reports basic information about
#' each ChIP profile such as the number of positions, the area, the maximum 
#' value and the position of the maximum value.
#'
#' To learn more about \pkg{similaRpeak} package see:
#' \url{https://github.com/adeschen/similaRpeak/wiki}
#' 
#' @docType package
#'
#' @name similaRpeak-package
#'
#' @aliases similaRpeak-package similaRpeak
#'
#' @author  Astrid Deschenes,
#' Elsa Bernatchez,
#' Charles Joly Beauparlant,
#' Fabien Claude Lamaze,
#' Rawane Samb,
#' Pascal Belleau and
#' Arnaud Droit
#'
#' Maintainer:
#' Astrid Deschenes <adeschen@@hotmail.com>
#' 
#' @seealso
#' \itemize{
#' \item \code{\link{MetricFactory}} {for using a interface to calculate all 
#' available metrics separately.}
#' \item \code{\link{similarity}} {for calculating all available metrics 
#' between two ChIP-Seq profiles.}
#' }
#'
#' @keywords package
NULL

#' ChIP-Seq profiles of region chr7:61968807-61969730 related to enhancers 
#' H3K27ac and H3K4me1 (for demonstration purpose)
#' 
#' ChIP-Seq profiles of region chr7:61968807-61969730 of two histone 
#' post-transcriptional modifications linked to highly active enhancers H3K27ac 
#' (DCC accession: ENCFF000ASG) and H3K4me1 (DCC accession: ENCFF000ARY) from 
#' the Encyclopedia of DNA Elements (ENCODE) data (Dunham I et al. 2012).
#'
#' @name chr7Profiles
#'
#' @docType data
#'
#' @aliases chr7Profiles
#'
#' @format A \code{list} with 1 entry. The entry is a list of 2 ChIP-Seq 
#' profiles, one per active enhancer (H3K27ac and H3K4me1).The 2 ChIP-Seq 
#' profiles are of identical length and specific to a genomic region. Each 
#' ChiP-Seq profile is a numerical vector containing the profiles values 
#' at each position, as reported in reads per million (RPM).
#' \itemize{
#' \item{\code{chr7Profiles}}{ a \code{list} containing all demo ChIP-Seq 
#' profiles }
#' \item \code{chr7Profiles$chr7.61968807.61969730} { a \code{list} containing  
#' 2 ChIP-Seq profiles for the genomic region chr7:6196880-61969730 }
#' \item \code{demoProfiles$chr7.61968807.61969730$H3K27ac} { a numeric vector 
#' containing the profiles values related to the enhancer H3K27ac, as reported 
#' in reads per million (RPM). The first entry of the vector is for position 
#' chr7:61968807 while the last entry is for position chr7:61969730 }
#' \item \code{demoProfiles$chr7.61968807.61969730$H3K4me1} { a numeric vector 
#' containing the profiles values related to the enhancer H3K4me1, as reported 
#' in reads per million (RPM). The first entry of the vector is for position 
#' chr7:61968807 while the last entry is for position chr7:61969730 }
#' }
#' 
#' @source The Encyclopedia of DNA Elements (ENCODE) (DCC accession:
#' ENCFF000MZT)
#'
#' @references
#' \itemize{
#' \item Dunham I, Kundaje A, Aldred SF, et al. An integrated encyclopedia
#' of DNA elements in the human genome. Nature. 2012 Sep 6;489(7414):57-74.
#' }
#'
#' @seealso
#' \itemize{
#' \item \code{\link{demoProfiles}} { ChIP-seq profiles related to enhancers 
#' H3K27ac and H3K4me1 (for demonstration purpose)}
#' \item \code{\link{MetricFactory}} {for using a interface to calculate all 
#' available metrics separately.}
#' \item \code{\link{similarity}} {for calculating all available metrics 
#' between two ChIP-Seq profiles.}
#' }
#'
#' @usage data(chr7Profiles)
#'
#' @keywords datasets
#'
#' @examples
#'
#' data(chr7Profiles)
#'
#' ## Calculating all metrics for the "chr7.61968807.61969730" region 
#' metrics <- similarity(chr7Profiles$chr7.61968807.61969730$H3K4me1, 
#' chr7Profiles$chr7.61968807.61969730$H3K27ac, 
#'     ratioAreaThreshold=10, 
#'     ratioMaxMaxThreshold=4,
#'     ratioIntersectThreshold=5, 
#'     ratioNormalizedIntersectThreshold=2,
#'     diffPosMaxThresholdMinValue=10, 
#'     diffPosMaxThresholdMaxDiff=100, 
#'     diffPosMaxTolerance=0.10)
#' metrics
#' 
#' ## You can refer to the vignette to see more examples using ChIP-Seq profiles
#' ## extracted from the Encyclopedia of DNA Elements (ENCODE) data.
#'
NULL

#' ChIP-Seq profiles of region chr7:61968807-61969730 related to enhancers 
#' H3K27ac and H3K4me1 (for demonstration purpose)
#' 
#' ChIP-Seq profiles of region chr7:61968807-61969730 of two histone 
#' post-transcriptional modifications linked to highly active enhancers H3K27ac 
#' (DCC accession: ENCFF000ASG) and H3K4me1 (DCC accession: ENCFF000ARY) from 
#' the Encyclopedia of DNA Elements (ENCODE) data (Dunham I et al. 2012).
#'
#' @name demoProfiles
#'
#' @docType data
#'
#' @aliases demoProfiles
#'
#' @format A \code{list} with 1 entry. The entry is a \code{list} of 2 
#' ChIP-Seq profiles, one per active enhancer (H3K27ac and H3K4me1).The 2 
#' ChIP-Seq profiles are of identical length and specific to a 
#' genomic region. Each ChiP-Seq profile is 
#' a numerical vector containing the profiles values at each position, as 
#' reported in reads per million (RPM).
#' \itemize{
#' \item \code{demoProfiles} { a \code{list} containing all demo ChIP-Seq 
#' profiles }
#' \item \code{demoProfiles$chr2.70360770.70361098} { a list containing 2 
#' ChIP-Seq profiles for the genomic region chr2:70360770-70361098 }
#' \item \code{demoProfiles$chr2.70360770.70361098$H3K27ac} { a numeric vector 
#' containing the profiles values related to the enhancer H3K27ac, as reported 
#' in reads per million (RPM). The first entry of the vector is for position 
#' chr1:70360770 while the last entry is for position chr2:70361098 }
#' \item \code{demoProfiles$chr2.70360770.70361098$H3K4me1} { a numeric vector 
#' containing the profiles values related to the enhancer H3K4me1, as reported 
#' in reads per million (RPM). The first entry of the vector is for position 
#' chr1:70360770 while the last entry is for position chr2:70361098 }
#' \item \code{demoProfiles$chr3.73159773.73160145$H3K4me1} { a list containing 
#' 2 ChIP-Seq profiles for the genomic region chr3:73159773-73160145 }
#' \item \code{demoProfiles$chr3.73159773.73160145$H3K27ac} { a numeric vector 
#' containing the profiles values related to the enhancer H3K27ac, as reported 
#' in reads per million (RPM). The first entry of the vector is for position 
#' chr2:73159773 while the last entry is for position chr3:73160145 }
#' \item \code{demoProfiles$chr3.73159773.73160145$H3K4me1} { a numeric vector 
#' containing the profiles values related to the enhancer H3K4me1, as reported 
#' in reads per million (RPM). The first entry of the vector is for position 
#' chr3:73159773 while the last entry is for position chr3:73160145 }
#' }
#' 
#' @source The Encyclopedia of DNA Elements (ENCODE) (DCC accession:
#' ENCFF000MZT)
#'
#' @references
#' \itemize{
#' \item Dunham I, Kundaje A, Aldred SF, et al. An integrated encyclopedia
#' of DNA elements in the human genome. Nature. 2012 Sep 6;489(7414):57-74.
#' }
#'
#' @seealso
#' \itemize{
#' \item \code{\link{chr7Profiles}} { ChIP-Seq profiles of region 
#' chr7:61968807-61969730 related to enhancers H3K27ac and H3K4me1 
#' (for demonstration purpose)}
#' \item \code{\link{MetricFactory}} {for using a interface to calculate all 
#' available metrics separately.}
#' \item \code{\link{similarity}} {for calculating all available metrics 
#' between two ChIP-Seq profiles.}
#' }
#'
#' @usage data(demoProfiles)
#'
#' @keywords datasets
#'
#' @examples
#'
#' data(demoProfiles)
#'
#' # Calculate metrics for the "chr3:73159773-73160145" region
#' metrics <- similarity(demoProfiles$chr3.73159773.73160145$H3K27ac, 
#'     demoProfiles$chr3.73159773.73160145$H3K4me1)
#' metrics
#' 
#' ## You can refer to the vignette to see more examples using ChIP-Seq profiles
#' ## extracted from the Encyclopedia of DNA Elements (ENCODE) data.
#'
NULL