# 
# Calculate and return five metrics which estimates the level of similarity 
# between two profiles (represented as vectors).
#
# Input:   
#   profile1:                   a first profile/vector containing depths. 
#                                 Each position is associated to a position in 
#                                 particular, which is assumed.
#   profile2:                   a second profile/vector containing depths. 
#                                 Each position is associated to a position in 
#                                 particular, which is assumed.
#   ratioAreaThreshold:         the minimum denominator accepted to calculate 
#                                 the ratio of the area between both profiles. 
#                                 Default = 1.
#   ratioMaxMaxThreshold:       the minimum denominator accepted to calculate
#                                 the ratio of the maximum values between both 
#                                 profiles. Default = 1.
#   ratioIntersectThreshold:    the minimum denominator accepted to calculate 
#                                 the ratio of the intersection area of both 
#                                 profiles and the total area. Default = 1.
#   ratioNormalizedIntersectThreshold: the minimum denominator accepted to 
#                                 calculate the ratio of the normalized 
#                                 intersection area of both 
#                                 profiles and the total area. Default = 1.
#   diffPosMaxThresholdMinValue: the minimum peak accepted to calculate the 
#                                 metric. Default = 1.
#   diffPosMaxThresholdMaxDiff:  the maximum distance accepted between 2 peaks 
#                                 positions in one profile to calculate the 
#                                 metric. Default = 100.
#   diffPosMaxTolerance:         the maximum variation accepted on the maximum 
#                                 value to consider a position as a peak 
#                                 position. Default = 0.01.
#
# Prerequisites: 
#   The 'profile1' argument is a numeric vector where no element is less 
#      than zero.
#   The 'profile2' argument is a numeric vector where no element is less 
#      than zero.
#   The length of 'profile1' is equal to the length of 'profile2'.
#   The 'ratioAreaThreshold' argument is a positive numeric value. 
#   The 'ratioMaxMaxThreshold' argument is a positive numeric value. 
#   The 'ratioIntersectThreshold' argument is a positive numeric value. 
#   The 'ratioNormalizedIntersectThreshold' argument is a positive 
#      numeric value.
#   The 'diffPosMaxThresholdMinValue' argument is a positive numeric value.
#   The 'diffPosMaxThresholdMaxDiff' argument is a positive numeric value.
#   The 'diffPosMaxTolerance' argument is a positive numeric value
#      between 0 and 1.
#
# Output: 
#   A list of elements containing information about both profiles and a 
#   list of metrics.
#
similarity <- function(profile1, 
                    profile2, 
                    ratioAreaThreshold = 1, 
                    ratioMaxMaxThreshold = 1, 
                    ratioIntersectThreshold = 1, 
                    ratioNormalizedIntersectThreshold = 1,
                    diffPosMaxThresholdMinValue = 1, 
                    diffPosMaxThresholdMaxDiff = 100, 
                    diffPosMaxTolerance = 0.01) {
    
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
    if (sum(profile1 < 0, na.rm = TRUE) > 0) {
        stop("The profile1 argument contains at least one negative number.")
    }
    if (sum(profile2 < 0, na.rm = TRUE) > 0) {
        stop("The profile2 argument contains at least one negative number.")
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
        stop(paste("The 'ratioNormalizedIntersectThreshold' must be",
                , " a positive numeric value.", sep=""))
    }
    
    # The diffPosMaxThresholdMinValue argument is a positive numeric element
    if (length(diffPosMaxThresholdMinValue) != 1 || 
            !is.numeric(diffPosMaxThresholdMinValue) || 
            (diffPosMaxThresholdMinValue <= 0)) {
        stop(paste("The 'diffPosMaxThresholdMinValue' must be a positive ",
                "numeric value.", sep=""))
    }   
    
    # The diffPosMaxThresholdMaxDiff argument is a positive numeric element
    if (length(diffPosMaxThresholdMaxDiff) != 1 || 
            !is.numeric(diffPosMaxThresholdMaxDiff) || 
            (diffPosMaxThresholdMaxDiff <= 0)) {
        stop(paste("The 'diffPosMaxThresholdMaxDiff' must be a positive ",
                "numeric value.", sep=""))
    }  
    
    # The diffPosMaxTolerance argument is a positive numeric element
    if (length(diffPosMaxTolerance) != 1 || 
            !is.numeric(diffPosMaxTolerance) || 
            (diffPosMaxTolerance < 0) || 
            (diffPosMaxTolerance > 1)) {
        stop(paste("The 'diffPosMaxTolerance' must be a positive numeric ",
                "value between 0 and 1 included.", sep=""))
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
                                diffPosMaxTolerance)
    
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