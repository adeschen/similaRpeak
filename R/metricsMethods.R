# Calculate and return the area ratio between two ChIP 
# profiles covering the same range. The area from profile1 is always divided by
# area from profile2. If the minimum area between profile1 and profile2 is 
# inferior to the threshold, the function returns NA. 
# The threshold has to be a positive value.
#
# Input:   
#   profile1:    a first profile/vector containing depths. Each position is 
#                associated to a position in particular, which is assumed.
#   profile2:    a second profile/vector containing depths. Each position is 
#                associated to a position in particular, which is assumed.
#   threshold:   the minimum denominator accepted to calculate a ratio.
#
# Output: 
#   The calculated ratio or NA if threshold is not respected.
#
ratioAreaMethod <- function(profile1, profile2, threshold = 1) {

    # Get the total area associated to each profile
    area1 <- sum(profile1, na.rm = TRUE)
    area2 <- sum(profile2, na.rm = TRUE)

    # Get the ratio between area1 and area2
    minimum <- min(area1, area2)
    if (minimum > 0  && threshold <= minimum){
        ratio <- area1 / area2
    }else {
        ratio <- as.numeric(NA)
    }

    return(ratio)
}


#' @title Ratio of profiles maximal peaks between two profiles
#' 
#' @description Calculate and return the ratio of profiles maximal peaks
#' between two profiles covering the same range. The  
#' maximal peak of the first profile is always divided by the maximal peak of
#' the second profile.If one peak value is inferior to the threshold, the 
#' function returns \code{NA}.
#' 
#' @param profile1 a \code{vector} of \code{numeric} values, the first profile
#' containing the alignment depth for each position. The \code{profile1} and 
#' \code{profile2} should have the same length.
#' 
#' @param profile2 a \code{vector} of \code{numeric} values, the second profile
#' containing the alignment depth for each position. The \code{profile1} and 
#' \code{profile2} should have the same length. 
#' 
#' @param threshold a \code{numeric}, the minimum peak value accepted 
#' to calculate a ratio.
#' 
#' @return The calculated ratio or \code{NA} if one peak value is inferior to
#' the threshold.
#' 
#' @seealso
#' \itemize{
#' \item \code{\link{MetricFactory}} {for using the recommanded interface to 
#' calculate all available metrics separately or togheter.}
#' }
#' 
#' @author Astrid Deschenes, Elsa Bernatchez
ratioMaxMaxMethod <- function(profile1, profile2, threshold = 1) {

    # Get the maximum element associated to each profile
    max1 <- max(profile1, na.rm = TRUE)
    max2 <- max(profile2, na.rm = TRUE)

    # Get the ratio between max1 and max2
    minimum <- min(max1, max2)
    if (minimum > 0 && threshold <= minimum){
        ratio <- max1/max2
    }else {
        ratio <- as.numeric(NA)
    }

    return(ratio)
}


#' @title Difference between two profiles maximal peaks positions
#' 
#' @description Calculate and return the difference between two profiles 
#' maximal peaks positions. The difference is always the first profile value 
#' (profile1 parameter) minus the second profile value (profile2 parameter). 
#' When more than one maximual peak is present in one profile, the mediane of
#' the position is calculated and used as the maximal peak position.
#' If one threshold is not respected, the function returns \code{NA}.
#' 
#' @param profile1 a \code{vector} of \code{numeric} values, the first profile
#' containing the alignment depth for each position. The \code{profile1} and 
#' \code{profile2} should have the same length.
#' 
#' @param profile2 a \code{vector} of \code{numeric} values, the second profile
#' containing the alignment depth for each position. The \code{profile1} and 
#' \code{profile2} should have the same length. 
#' 
#' @param threshold a \code{numeric}, the minimum peak value accepted to 
#' calculate the metric. Default = 1.
#' 
#' @param thresholdDist a \code{numeric}, the maximum distance accepted 
#' between two maximum peaks positions in the same profile. Default = 100.
#' 
#' @param tolerance a \code{numeric}, the maximum variation accepted on the 
#' maximum value to consider a position as a peak position. The tolerance must
#  be between 0 and 1. Default = 0.01.
#' 
#' @return The calculated ratio or \code{NA} if not all thresholds are 
#' respected.
#' 
#' @seealso
#' \itemize{
#' \item \code{\link{MetricFactory}} {for using the recommanded interface to 
#' calculate all available metrics separately or togheter.}
#' }
#' 
#' @importFrom stats median
#' @author Astrid Deschenes, Elsa Bernatchez
diffPosMaxMethod <- function(profile1, 
                       profile2, 
                       threshold = 1, 
                       thresholdDist = 100, 
                       tolerance = 0.01) {

    # The profile1 and profile2 arguments are numeric vectors. 
    # If not, NA is returned. 
    if (!is.vector(profile1)      || 
            !is.numeric(profile1) || 
            !is.vector(profile2)  || 
            !is.numeric(profile2) ||
            all(is.na(profile1))  ||
            all(is.na(profile2))) {
        return(as.numeric(NA))
    }

    # Get the position of the maximum element associated to each profile
    max1 <- max(profile1, na.rm = TRUE)
    max2 <- max(profile2, na.rm = TRUE)

    tolerance_multiple <- 1 - tolerance
    toleranceMax1 <- tolerance_multiple * max1
    toleranceMax2 <- tolerance_multiple * max2

    posMax1 <- which(profile1 >= toleranceMax1)
    posMax2 <- which(profile2 >= toleranceMax2)

    # Get the difference between posMax1 and posMax2
    minimum = min(posMax1, posMax2)
    # The metric is only calculated if the minimal peak value is respected
    if ((minimum > 0) && (threshold <= min(max1, max2))) {
        if (length(posMax1) == 1 && length(posMax2) == 1) {
            diff <- posMax1-posMax2    
        } else {
            maxDiff1 <- ifelse(length(posMax1) == 1, 0, 
                                        max(diff(sort(posMax1))))
            maxDiff2 <- ifelse(length(posMax2) == 1, 0, 
                                        max(diff(sort(posMax2))))
            if (max(maxDiff1, maxDiff2) <= thresholdDist) {
                median1 <- median(posMax1)
                median2 <- median(posMax2)
                diff <- median1 - median2
            } else {
                diff <- as.numeric(NA)
            }
        }
    } else {
        diff <- as.numeric(NA)
    }

    return(diff)
}

# Calculate and return the ratio between the intersection area of two profiles 
# and the total area covered by those profiles. If the total area is inferior 
# to the threshold, the function returns NA. The threshold has to be a positive 
# value.
#
# Input:   
#   profile1:    a first curve/vector containing depths. Each position is 
#                associated to a position in particular, which is assumed.
#   profile2:    a second curve/vector containing depths. Each position is 
#                associated to a position in particular, which is assumed.
#   threshold:   the minimum denominator accepted to calculate a ratio.
#
# Output: 
#   The calculated ratio or NA if threshold is not respected.
#
ratioIntersectMethod <- function(profile1, profile2, threshold = 1) {

    # Get the area of the intersection (min of both curves for each position)
    intersect <- sum(unlist(lapply(1:length(profile1), 
                                function(x) min(profile1[x], profile2[x]))), 
                    na.rm = TRUE)

    # Get the total area covered by both curves
    totalArea <- sum(profile1, na.rm = TRUE) +
        sum(profile2, na.rm = TRUE) - intersect

    # Get the ratio between intersect and totArea
    if (totalArea > 0 && threshold <= totalArea) {
        ratio <- intersect/totalArea
    }else {
        ratio <- as.numeric(NA)
    }

    return(ratio)
}


#' @title Spearman's rho statistic of two profiles
#' 
#' @description Calculate and return the Spearman's rho statistic of two 
#' profiles. If one profile has a standard deviation inferior to 
#' the threshold, the function returns \code{NA}. When no complete element 
#' pair are present, \code{NA} is returned.
#' 
#' @param profile1 a \code{vector} of \code{numeric} values, the first profile
#' containing the alignment depth for each position. The \code{profile1} and 
#' \code{profile2} should have the same length.
#' 
#' @param profile2 a \code{vector} of \code{numeric} values, the second profile
#' containing the alignment depth for each position. The \code{profile1} and 
#' \code{profile2} should have the same length. 
#' 
#' @param threshold a \code{numeric}, the minimum denominator accepted 
#' to calculate a ratio.
#' 
#' @return The calculated ratio or \code{NA} if one profile has a standard 
#' deviation inferior to the threshold. If profiles have no complete element 
#' pair, \code{NA} is returned.
#' 
#' @seealso
#' \itemize{
#' \item \code{\link{MetricFactory}} {for using the recommanded interface to 
#' calculate all available metrics separately or togheter.}
#' }
#' 
#' @importFrom stats sd cor complete.cases
#' @author Astrid Deschenes, Elsa Bernatchez
spearmanCorrMethod <- function(profile1, profile2, threshold = 1e-8) {

    # Validate that each profile has at least one complete element pair
    # and that the standard deviation of each profile is superior to threshold
    if (sum(complete.cases(profile1, profile2)) == 0  || 
        (sd(profile1, na.rm = TRUE) < threshold)      || 
        (sd(profile2, na.rm = TRUE) < threshold))  {
        correlation <- as.numeric(NA)
    } else {
        # Spearman correlation
        correlation <- cor(x=profile1, y=profile2, use="complete.obs", 
                       method="spearman")
    }

    return(correlation)
}
