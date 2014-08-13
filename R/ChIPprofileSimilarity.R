# Calculate and return the area ratio between two ChIP profiles covering the same range. The maximum
# area is always divided by the minimum area.If the minimum area is inferior to the threshold, 
# the function returns NA.
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
    area1 = sum(profile1)
    area2 = sum(profile2)
    
    # Get the ratio between area1 and area2
    if (threshold <= min(area1, area2)){
        ratio = max(area1, area2)/min(area1, area2)
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
    max1 = max(profile1)
    max2 = max(profile2)
    
    # Get the ratio between max1 and max2
    if (threshold <= min(max1, max2)){
        ratio = max(max1, max2)/min(max1, max2)
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
    
    return(diff)
}

# Calculate and return the ratio between the intersection area of two profiles and the 
# total area covered by those profiles. If the total area is inferior to the threshold, 
# the function returns NA. 
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
ratioIntersect <- function(profile1, profile2, threshold=NULL){
    
    # Get the area of the intersection (min of both curves for each position)
    intersect = sum(unlist(lapply(1:length(profile1), function(x) min(profile1[x], profile2[x]))))
    
    # Get the total area covered by both curves
    totArea = sum(profile1)+sum(profile2)-intersect
 
    # Get the ratio between intersect and totArea
    if (threshold <= totArea){
        ratio = intersect/totArea
    }else {
        ratio = NA
    }

    return(ratio)
}
