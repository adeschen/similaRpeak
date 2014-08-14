# 
# Calculate and return four metrics which estimates the level of similarity between two profiles 
# (represent as vectors).
#
# Input:   
#   profile1:                               a first profile/vector containing depths. Each position is 
#                                             associated to a position in particular, which is assumed.
#   profile2:                               a second profile/vector containing depths. Each position is 
#                                             associated to a position in particular, which is assumed.
#   ratioAreaThreshold:                     the minimum denominator accepted to calculate the ratio of
#                                             the area between both profiles. Default = 1.
#   ratioMaxMaxThreshold:                   the minimum denominator accepted to calculate the ratio of
#                                             the maximum values between both profiles. Default = 1.
#   ratioIntersectThreshold:                the minimum denominator accepted to calculate the ratio of
#                                             the intersection area of both profiles and the total 
#                                             area. Default = 1.
#
# Prerequisites: 
#   The 'profile1' argument is a numeric vector where at least one element is greater than zero and 
#   none element is less than zero.
#   The 'profile2' argument is a numeric vector where at least one element is greater than zero and 
#   none element is less than zero.
#   The length of 'profile1' is equal to the length of 'profile2'.
#   The 'ratioAreaThreshold' argument is a positive numeric value. 
#   The 'ratioMaxMaxThreshold' argument is a positive numeric value. 
#   The 'ratioIntersectThreshold' argument is a positive numeric value. 
#
# Output: 
#   A list of elements containing information about both profiles and a list of metrics.
#
similarity <- function(profile1, profile2, ratioAreaThreshold=1, ratioMaxMaxThreshold=1, ratioIntersectThreshold=1){
    
    #######################################
    # Test prerequisites
    #######################################
    
    # The profile1 and profile2 arguments are numeric vectors where at least one element is greater 
    # than zero
    if (!is.vector(profile1) | !is.numeric(profile1)) {
        stop("The 'profile1' argument must be a numeric vector.")
    }
    if (!is.vector(profile2) | !is.numeric(profile2)) {
        stop("The 'profile2' argument must be a numeric vector.")
    }
    
    # At least one element in profile1 and profile2 is greater than zero and none element is less 
    # than zero'.
    
    if (sum(profile1)==0 || sum(profile1<0)>0){
<<<<<<< HEAD
        stop("The profile1 argument contains negatives or is made up of zeros only.")
    }
    if (sum(profile2)==0 || sum(profile2<0)>0){
        stop("The profile2 argument contains negatives or is made up of zeros only.")
=======
        stop("The 'profile1' argument contains negatives or NAs or is made up of zeros only.")
    }
    if (sum(profile2)==0 || sum(profile2<0)>0){
        stop("The 'profile2' argument contains negatives or NAs or is made up of zeros only.")
>>>>>>> 5afdda3ccb684c65165a85f70523dd3051caaf85
    }
    
    # The length of profile1 is equal to the length of profile2
    if (length(profile1) != length(profile2)) {
        stop("Lengths of 'profile1' and 'profile2' vectors aren't equals.")
    }
    
    # The ratioAreaThreshold argument is a positive numeric element
    if (length(ratioAreaThreshold)!=1 || !is.numeric(ratioAreaThreshold) || (ratioAreaThreshold <= 0)){
        stop("The 'ratioAreaThreshold' must be a positive numeric value.")
    }
    
    # The ratioMaxMaxThreshold argument is a positive numeric element
    if (length(ratioMaxMaxThreshold)!=1 || !is.numeric(ratioMaxMaxThreshold) || (ratioMaxMaxThreshold <= 0)){
        stop("The 'ratioMaxMaxThreshold' must be a positive numeric value.")
    }

    # The ratioIntersectThreshold argument is a positive numeric element
    if (length(ratioIntersectThreshold)!=1 || !is.numeric(ratioIntersectThreshold) || (ratioIntersectThreshold <= 0)){
        stop("The 'ratioIntersectThreshold' must be a positive numeric value.")
    }
    
    # Get information about both profiles
    nbrPos = length(profile1)
    areaProfile1 = sum(profile1)
    areaProfile2 = sum(profile2)
    maxProfile1 = max(profile1, na.rm=T)
    maxProfile2 = max(profile2, na.rm=T)
    maxPositionProfile1 = which.max(profile1)
    maxPositionProfile2 = which.max(profile2)
    
    
    # Create a metric factory object
    factory = MetricFactory$new(ratioAreaThreshold, ratioMaxMaxThreshold, ratioIntersectThreshold)
    # Generate the list of all metrics availables
    metricList = factory$createMetric("ALL", profile1, profile2)
    
    # Create a list containing all pertinent information and a sub-list with all metrics values
    result = list(nbrPosition=nbrPos, areaProfile1=areaProfile1, areaProfile2=areaProfile2, 
                  maxProfile1=maxProfile1, maxProfile2=maxProfile2, 
                  maxPositionProfile1=maxPositionProfile1, 
                  maxPositionProfile2=maxPositionProfile2,
                  metrics=metricList)
    
    return(result)
}