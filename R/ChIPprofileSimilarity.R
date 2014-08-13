# Get the area ratio between two curves 
#
# Input:   
#   profile1:                               a first curve/vector containing depths. Each position is 
#                                         associated to a position in particular, which is assumed.
#   profile2:                               a second curve/vector containing depths. Each position is 
#                                         associated to a position in particular, which is assumed.
#   threshold                             the minimum denominator accepted to calculate a ratio.
#
# Output: 
#   The calculated ratio. 
#
ratioArea <- function(profile1, profile2, threshold){
    
    # Get the total area associated to each curve
    area1 = sum(profile1)
    area2 = sum(profile2)
    
    # Get the ratio between area1 and area2
    if (threshold <= min(area1, area2)){
        ratio = max(area1, area2)/min(area1, area2)
    }else {ratio = NA}
    
    return(ratio)
}

# Get the ratio between two curves' maximal elements. 
#
# Input:   
#   profile1:                               a first curve/vector containing depths. Each position is 
#                                         associated to a position in particular, which is assumed.
#   profile2:                               a second curve/vector containing depths. Each position is 
#                                         associated to a position in particular, which is assumed.
#   threshold                             the minimum denominator accepted to calculate a ratio.
#
# Output: 
#   The calculated ratio. 
#
ratioMaxMax <- function(profile1, profile2, threshold){
    
    # Get the maximum element associated to each curve
    max1 = max(profile1)
    max2 = max(profile2)
    
    # Get the ratio between max1 and max2
    if (threshold <= min(max1, max2)){
        ratio = max(max1, max2)/min(max1, max2)
    }else {ratio = NA}
    
    return(ratio)
}

# Get the difference between two curves' maximal elements' positions. 
#
# Input:   
#   profile1:                               a first curve/vector containing depths. Each position is 
#                                         associated to a position in particular, which is assumed.
#   profile2:                               a second curve/vector containing depths. Each position is 
#                                         associated to a position in particular, which is assumed.
#   threshold                             the minimum denominator accepted to calculate a ratio.
#
# Output: 
#   The calculated difference. 
#
diffPosMax <- function(profile1, profile2, threshold){
    
    # Get the position of the maximum element associated to each curve
    posMax1 = which.max(profile1)
    posMax2 = which.max(profile2)
    
    # Get the absolute difference between posMax1 and posMax2
    diff = abs(posMax1-posMax2)
    
    return(diff)
}

# Get the ratio between the intersection area of two curves and the total area covered by those curves. 
#
# Input:   
#   profile1:                               a first curve/vector containing depths. Each position is 
#                                         associated to a position in particular, which is assumed.
#   profile2:                               a second curve/vector containing depths. Each position is 
#                                         associated to a position in particular, which is assumed.
#   threshold                             the minimum denominator accepted to calculate a ratio.
#
# Output: 
#   The calculated ratio. 
#
ratioIntersect <- function(profile1, profile2, threshold){
    
    # Get the area of the intersection (min of both curves for each position)
    intersect = sum(unlist(lapply(1:length(profile1), function(x) min(profile1[x], profile2[x]))))
    
    # Get the total area covered by both curves
    totArea = sum(profile1)+sum(profile2)-intersect
 
    # Get the ratio between intersect and totArea
    if (threshold <= totArea){
        ratio = intersect/totArea
    }else {ratio = NA}

    return(ratio)
}
