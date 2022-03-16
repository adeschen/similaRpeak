#' @title Calculate metrics which estimate the level of similarity between two 
#' ChIP-Seq profiles
#' 
#' @description It returns a list containing information about both ChIP-Seq 
#' profiles and a \code{list} of all similarity metrics: the ratio of the
#' maximum values, the ratio of the areas, the ratio between the intersection 
#' area and the total area (for normalized and non-normalized profiles), the 
#' difference between two profiles maximal peaks positions and the Spearman's
#' rho statistic.
#' 
#' @param profile1 Vector containing the RPM values of the first ChIP-Seq 
#' profile for each position of the selected region.
#' 
#' @param profile2 Vector containing the RPM values of the second ChIP-Seq 
#' profile for each position of the selected region.
#' 
#' @param ratioAreaThreshold The minimum denominator accepted to calculate 
#' the ratio of the area between both profiles. The value has to be positive. 
#' Default = 1.
#' 
#' @param ratioMaxMaxThreshold The minimum denominator accepted to calculate 
#' the ratio of the maximal peaks values between both profiles. The value 
#' has to be positive. Default = 1.
#' 
#' @param ratioIntersectThreshold The minimum denominator accepted to 
#' calculate the ratio of the intersection area of both profiles over the 
#' total area. The value has to be positive. Default = 1.
#' 
#' @param ratioNormalizedIntersectThreshold The minimum denominator accepted 
#' to calculate the ratio of the intersection area of both normalized profiles 
#' over the total area. The value has to be positive. Default = 1.
#' 
#' @param diffPosMaxThresholdMinValue The minimum peak accepted to calculate 
#' the metric. The value has to be positive. Default = 1.
#' 
#' @param diffPosMaxThresholdMaxDiff The maximum distance accepted between 2 
#' peaks positions in one profile to calculate the metric. The value has to be 
#' positive. Default=100.
#' 
#' @param diffPosMaxTolerance The maximum of variation accepted on the 
#' maximum value to consider a position as a peak position. The value can be 
#' between 0 and 1. Default=0.01.
#' 
#' @param spearmanCorrSDThreashold The minimum standard deviation accepted 
#' on both profiles to calculate the metric. Default=1e-8.
#' 
#' @details
#' \code{similarity} uses the two vectors passed as arguments to
#' calculate the metrics. When the metric is a ratio, it always verify
#' that the threshold for the denominator is respected. If the threshold
#' is not respected, the metric is assigned the \code{NA} value.
#' 
#' @return A \code{list} containing :
#' \itemize{
#' \item \code{nbrPosition} The number of positions included in each profile.
#' \item \code{areaProfile1} The area of the first profile.
#' \item \code{areaProfile2} The area of the second profile.
#' \item \code{maxProfile1} The maximum value in the first profile.
#' \item \code{maxProfile2} The maximum value in the second profile.
#' \item \code{maxPositionProfile1} The list of positions of the maximum value 
#' in the first profile.
#' \item \code{maxPositionProfile2} The list of positions of the maximum value 
#' in the second profile.
#' \item \code{metrics} A \code{list} with thefollowing items:
#' \itemize{
#' \item \code{RATIO_AREA} The ratio between the areas. The larger value is 
#' always divided by the smaller value.\code{NA} if minimal threshold is not
#' respected.
#' \item \code{DIFF_POS_MAX} The difference between the maximal peaks 
#' positions. The difference is always the first profile value minus the 
#' second profile value. \code{NA} is returned if minimal peak value is not 
#' respected. A profile can have more than one position with the maximum 
#' value. In that case, the median position is used. A threshold argument 
#' can be set to consider all positions within a certain range of the maximum 
#' value. A threshold argument can also be set to ensure that the distance 
#' between two maximum values is not too wide. When this distance is not 
#' respected, it is assumed that more than one peak is present in the profile 
#' and \code{NA} is returned.
#' \item \code{RATIO_MAX_MAX} The ratio between the maximal peaks values. The 
#' first profile is always divided by the second profile. \code{NA} if minimal 
#' threshold is not respected.
#' \item \code{RATIO_INTERSECT} The ratio between the intersection area and the 
#' total area. \code{NA} if minimal threshold is not respected.
#' \item \code{RATIO_NORMALIZED_INTERSECT} The ratio between the intersection 
#' area and the total area of normalized profiles. \code{NA} if minimal 
#' threshold is not respected.
#' \item \code{SPEARMAN_CORRELATION} The Spearman's rho statistic between 
#' profiles. \code{NA} if minimal threshold is not respected or when no 
#' complete element pair is present between both profiles.
#' }
#' }
#' 
#' @examples
#' 
#' ## Defining two CHiP-Seq profiles 
#' profile1<-c(3,59,6,24,65,34,15,4,53,22,21,12,11)
#' profile2<-c(15,9,46,44,9,39,27,34,34,4,3,4,2)
#' 
#' ## Example usign default thresholds
#' similarity(profile1, profile2)
#' 
#' ## Example using customised thresholds
#' similarity(profile1, profile2, 
#'     ratioAreaThreshold=5, 
#'     ratioMaxMaxThreshold=5, 
#'     ratioIntersectThreshold=12,
#'     ratioNormalizedIntersectThreshold=2.2,
#'     diffPosMaxThresholdMinValue=2, 
#'     diffPosMaxThresholdMaxDiff=130, 
#'     diffPosMaxTolerance=0.03,
#'     spearmanCorrSDThreashold=1e-3)
#'     
#' ## Example using ChIP-Seq profiles of H3K27ac (DCC accession: ENCFF000ASG) 
#' ## and H3K4me1 (DCC accession: ENCFF000ARY) from the Encyclopedia of DNA  
#' ## Elements (ENCODE) for the region 
#' data(demoProfiles)
#' 
#' ## Visualize ChIP-Seq profiles 
#' plot(demoProfiles$chr2.70360770.70361098$H3K27ac,
#'     type="l", col="blue", xlab="", ylab="", ylim=c(0, 25),
#'     main="chr2:70360770-70361098")
#' par(new=TRUE)
#' plot(demoProfiles$chr2.70360770.70361098$H3K4me1,
#'     type="l", col="darkgreen", xlab="Position", 
#'     ylab="Coverage in reads per million (RPM)",  ylim=c(0, 25))
#' legend("topright", c("H3K27ac","H3K4me1"), cex=1.2, 
#'     col=c("blue","darkgreen"), lty=1)
#'     
#' # Calculate metrics
#' similarity(demoProfiles$chr2.70360770.70361098$H3K4me1, 
#'     demoProfiles$chr2.70360770.70361098$H3K27ac, 
#'     ratioAreaThreshold=15, 
#'     ratioMaxMaxThreshold=5, 
#'     ratioIntersectThreshold=12,
#'     ratioNormalizedIntersectThreshold=2.2,
#'     diffPosMaxThresholdMinValue=2, 
#'     diffPosMaxThresholdMaxDiff=130, 
#'     diffPosMaxTolerance=0.03,
#'     spearmanCorrSDThreashold=0.1)
#'     
#' ## You can refer to the vignette to see more examples using ChIP-Seq profiles
#' ## extracted from the Encyclopedia of DNA Elements (ENCODE) data.
#' 
#' @seealso
#' \itemize{
#' \item \code{\link{MetricFactory}} {for using a interface to calculate all 
#' available metrics separately or togheter.}
#' \item \code{\link{demoProfiles}} {for more informations about ChIP-Seq
#' profiles present in the demoProfiles data.}
#' }
#' 
#' @author Astrid Deschenes, Elsa Bernatchez
#' @export
similarity <- function(profile1, 
                    profile2, 
                    ratioAreaThreshold = 1, 
                    ratioMaxMaxThreshold = 1, 
                    ratioIntersectThreshold = 1, 
                    ratioNormalizedIntersectThreshold = 1,
                    diffPosMaxThresholdMinValue = 1, 
                    diffPosMaxThresholdMaxDiff = 100, 
                    diffPosMaxTolerance = 0.01,
                    spearmanCorrSDThreashold = 1e-8) {
    
    #######################################
    # Test prerequisites
    #######################################
    
    # The profile1 and profile2 arguments are numeric vectors.  
    if (!is.vector(profile1) || !is.numeric(profile1)) {
        stop("The 'profile1' argument must be a numeric vector.")
    }
    if (!is.vector(profile2) || !is.numeric(profile2)) {
        stop("The 'profile2' argument must be a numeric vector.")
    }
    
    # At elements in profile1 and profile2 at to have a value superior to zero 
    if (any(profile1 < 0, na.rm = TRUE)) {
        stop("The 'profile1' argument contains at least one negative number.")
    }
    if (any(profile2 < 0, na.rm = TRUE)) {
        stop("The 'profile2' argument contains at least one negative number.")
    }
    
    # The length of profile1 is equal to the length of profile2
    if (length(profile1) != length(profile2)) {
        stop("Lengths of 'profile1' and 'profile2' vectors aren't equals.")
    }
    
    # The ratioAreaThreshold argument is a positive numeric element
    if (length(ratioAreaThreshold) != 1 || 
            !is.numeric(ratioAreaThreshold) || 
            (ratioAreaThreshold <= 0)) {
        stop("The 'ratioAreaThreshold' must be a positive numeric value.")
    }
    
    # The ratioMaxMaxThreshold argument is a positive numeric element
    if (length(ratioMaxMaxThreshold) != 1 || 
            !is.numeric(ratioMaxMaxThreshold) || 
            (ratioMaxMaxThreshold <= 0)) {
        stop("The 'ratioMaxMaxThreshold' must be a positive numeric value.")
    }

    # The ratioIntersectThreshold argument is a positive numeric element
    if (length(ratioIntersectThreshold) != 1 || 
            !is.numeric(ratioIntersectThreshold) || 
            (ratioIntersectThreshold <= 0)) {
        stop("The 'ratioIntersectThreshold' must be a positive numeric value.")
    }
    
    # The ratioNormalizedIntersectThreshold argument is a positive 
    # numeric element
    if (length(ratioNormalizedIntersectThreshold) != 1 || 
            !is.numeric(ratioNormalizedIntersectThreshold) || 
            (ratioNormalizedIntersectThreshold <= 0)) {
        stop("The 'ratioNormalizedIntersectThreshold' must be",
                " a positive numeric value.")
    }
    
    # The diffPosMaxThresholdMinValue argument is a positive numeric element
    if (length(diffPosMaxThresholdMinValue) != 1 || 
            !is.numeric(diffPosMaxThresholdMinValue) || 
            (diffPosMaxThresholdMinValue <= 0)) {
        stop("The 'diffPosMaxThresholdMinValue' must be a positive ",
                "numeric value.")
    }   
    
    # The diffPosMaxThresholdMaxDiff argument is a positive numeric element
    if (length(diffPosMaxThresholdMaxDiff) != 1 || 
            !is.numeric(diffPosMaxThresholdMaxDiff) || 
            (diffPosMaxThresholdMaxDiff <= 0)) {
        stop("The 'diffPosMaxThresholdMaxDiff' must be a positive ",
                "numeric value.")
    }  
    
    # The diffPosMaxTolerance argument is a positive numeric element between
    # zero and one
    if (length(diffPosMaxTolerance) != 1 || 
            !is.numeric(diffPosMaxTolerance) || 
            (diffPosMaxTolerance < 0) || 
            (diffPosMaxTolerance > 1)) {
        stop("The 'diffPosMaxTolerance' must be a positive numeric ",
                "value between 0 and 1 included.")
    } 
    
    # The spearmanCorrSDThreashold argument is a positive numeric element
    if (length(spearmanCorrSDThreashold) != 1 || 
            !is.numeric(spearmanCorrSDThreashold) || 
            (spearmanCorrSDThreashold <= 0)) {
        stop("The 'spearmanCorrSDThreashold' must be a positive ",
                "numeric value.")
    }  
    
    # Get information about both profiles
    nbrPos <- length(profile1)
    areaProfile1 <- sum(profile1, na.rm=TRUE)
    areaProfile2 <- sum(profile2, na.rm=TRUE)
    maxProfile1 <- max(profile1, na.rm=TRUE)
    maxProfile2 <- max(profile2, na.rm=TRUE)
    maxPositionProfile1 <- which(profile1 == maxProfile1)
    maxPositionProfile2 <- which(profile2 == maxProfile2)
    
    
    # Create a metric factory object
    factory <- MetricFactory$new(ratioAreaThreshold, 
                                ratioMaxMaxThreshold, 
                                ratioIntersectThreshold,
                                ratioNormalizedIntersectThreshold,
                                diffPosMaxThresholdMinValue,
                                diffPosMaxThresholdMaxDiff, 
                                diffPosMaxTolerance,
                                spearmanCorrSDThreashold)
    
    # Generate the list of all metrics availables
    metricList <- factory$createMetric("ALL", profile1, profile2)
    
    # Create a list containing all pertinent information and 
    # a sub-list with all metrics values
    result <- list(nbrPosition=nbrPos, 
                areaProfile1=areaProfile1, 
                areaProfile2=areaProfile2, 
                maxProfile1=maxProfile1,
                maxProfile2=maxProfile2, 
                maxPositionProfile1=maxPositionProfile1, 
                maxPositionProfile2=maxPositionProfile2,
                metrics=metricList)
    
    return(result)
}