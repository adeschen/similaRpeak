# Assign a level of similarity between two curves (represent as vectors) by calculating them mean 
# difference and them ratio.
#
# Input:   
#   curve1:                               a first curve/vector containing depths. Each position is 
#                                         associated to a position in particular, which is assumed.
#   curve2:                               a second curve/vector containing depths. Each position is 
#                                         associated to a position in particular, which is assumed.
#   maxDiff                               the maximum mean difference accepted so the ratio will 
#                                         be calculated.
#
# Prerequisites: 
#   The curve1 argument is a numeric vector where at least one element is greater than zero and 
#   none element is less than zero or NA.
#   The curve2 argument is a numeric vector where at least one element is greater than zero and 
#   none element is less than zero or NA.
#   The length of curve1 is equal to the length of curve2.
#   The maxDiff argument is a numeric element. 
#
# Output: 
#   A matrix containing in order: Maximum and minimum depths sum, depths means difference, ratio 
#   and number of positios used. 
#
similarity <- function(curve1, curve2, maxDiff){
    
    #######################################
    # Test prerequisites
    #######################################
    
    # The curve1 and curve2 arguments are numeric vectors where at least one element is greater 
    # than zero
    if (!is.vector(curve1) | !is.numeric(curve1)) {
        stop("The curve1 argument must be a numeric vector.")
    }
    if (!is.vector(curve2) | !is.numeric(curve2)) {
        stop("The curve2 argument must be a numeric vector.")
    }
    
    # At least one element in curve1 and curve2 is greater than zero and none element is less 
    # than zero or NA.
    
    if (sum(is.na(curve1))>0 | sum(curve1)==0 | sum(curve1<0)>0){
        stop("The curve1 argument contains negatives or NAs or is made up of zeros only.")
    }
    if (sum(is.na(curve2))>0 | sum(curve2)==0 | sum(curve2<0)>0){
        stop("The curve2 argument contains negatives or NAs or is made up of zeros only.")
    }
    
    # The length of curve1 is equal to the length of curve2
    if (length(curve1) != length(curve2)) {
        stop("Lengths of curve1 and curve2 vectors aren't equals.")
    }
    
    # The maxDiff argument is a numeric element
    if (length(maxDiff)!=1 | !is.numeric(maxDiff)){
        stop("maxDiff must be a numeric element.")
    }
    
    # Get the sums of curve1 and curve2 arguments
    nbrPos = length(curve1)
    curve1 = sum(curve1)
    curve2 = sum(curve2)
    
    # Get the absolute mean difference between curve1 and curve2
    meanDiff = abs(curve1-curve2)/nbrPos
    
    # Get the ratio between curve1 and curve2
    if (meanDiff <= maxDiff){
        ratio = max(curve1, curve2)/min(curve1, curve2)
    }else {ratio = "Mean difference is greater than accepted limit."}
    
    result = matrix(c(max(curve1, curve2), min(curve1, curve2), meanDiff, ratio, nbrPos))
    rownames(result)=c("Maximum","Minimum","MeanDiff","Ratio","NbrPos")
    
    writeLines(paste(paste("Maximum:", result[1,]),paste("Minimum:", result[2,]),
                     paste("Mean Difference:", result[3,]),paste("Ratio:", result[4,]),
                     paste("Number of positions:", result[5,]),sep='\n'))
    
    return(result)
}