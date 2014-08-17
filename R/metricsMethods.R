# Calculate and return the area ratio between two ChIP profiles covering the same range. The maximum
# area is always divided by the minimum area.If the minimum area is inferior to the threshold, 
# the function returns NA. The threshold has to be a positive value.
#
# Input:   
#   profile1:                 a first profile/vector containing depths. Each position is 
#                             associated to a position in particular, which is assumed.
#   profile2:                 a second profile/vector containing depths. Each position is 
#                             associated to a position in particular, which is assumed.
#   threshold:                the minimum denominator accepted to calculate a ratio.
#
# Output: 
#   The calculated ratio or NA if threshold is not respected.
#
ratioArea <- function(profile1, profile2, threshold=1){
    
    # Get the total area associated to each profile
    area1 = sum(profile1, na.rm=T)
    area2 = sum(profile2, na.rm=T)
    
    # Get the ratio between area1 and area2
    minimum = min(area1, area2)
    if (minimum > 0  && threshold <= minimum){
        ratio = max(area1, area2)/minimum
    }else {
        ratio = NA
    }
    
    return(ratio)
}

# Calculate and return the ratio between profiles maximal peaks between two ChIP profiles covering
# the same range. The maximum peak is always divided by the minimum peak. If the minimum peak is 
# inferior to the threshold, the function returns NA.
#
# Input:   
#   profile1:                 a first profile/vector containing depths. Each position is 
#                             associated to a position in particular, which is assumed.
#   profile2:                 a second prfole/vector containing depths. Each position is 
#                             associated to a position in particular, which is assumed.
#   threshold                 the minimum denominator accepted to calculate a ratio.
#
# Output: 
#   The calculated ratio or NA if threshold is not respected.
#
ratioMaxMax <- function(profile1, profile2, threshold=1){
    
    # Get the maximum element associated to each profile
    max1 = max(profile1, na.rm=T)
    max2 = max(profile2, na.rm=T)
    
    # Get the ratio between max1 and max2
    minimum = min(max1, max2)
    if (minimum > 0 && threshold <= min(max1, max2)){
        ratio = max(max1, max2)/minimum
    }else {
        ratio = NA
    }
    
    return(ratio)
}

# Calculate and return the difference between two profiles maximal peaks positions. 
# The difference is always a positive value.
#
# Input:   
#   profile1:                 a first profile/vector containing depths. Each position is 
#                             associated to a position in particular, which is assumed.
#   profile2:                 a second profile/vector containing depths. Each position is 
#                             associated to a position in particular, which is assumed.
#   threshold:                the threshold is not used in this function.
#
# Output: 
#   The calculated difference. 
#
diffPosMax <- function(profile1, profile2, threshold=NULL){
    
    # Get the position of the maximum element associated to each profile
    posMax1 = which.max(profile1)
    posMax2 = which.max(profile2)
    
    # Get the absolute difference between posMax1 and posMax2
    diff = abs(posMax1-posMax2)
    
    if(length(diff)==0){diff=NA}
    
    return(diff)
}

# Calculate and return the ratio between the intersection area of two profiles and the 
# total area covered by those profiles. If the total area is inferior to the threshold, 
# the function returns NA. The threshold has to be a positive value.
#
# Input:   
#   profile1:                 a first curve/vector containing depths. Each position is 
#                             associated to a position in particular, which is assumed.
#   profile2:                 a second curve/vector containing depths. Each position is 
#                             associated to a position in particular, which is assumed.
#   threshold:                the minimum denominator accepted to calculate a ratio.
#
# Output: 
#   The calculated ratio or NA if threshold is not respected.
#
ratioIntersect <- function(profile1, profile2, threshold=1){
    
    # Get the area of the intersection (min of both curves for each position)
    intersect = sum(unlist(lapply(1:length(profile1), function(x) min(profile1[x], profile2[x]))), na.rm=T)
    
    # Get the total area covered by both curves
    totalArea = sum(profile1, na.rm=T)+sum(profile2, na.rm=T)-intersect
 
    # Get the ratio between intersect and totArea
    if (totalArea > 0 && threshold <= totalArea){
        ratio = intersect/totalArea
    }else {
        ratio = NA
    }

    return(ratio)
}
