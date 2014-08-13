###################################################
## Test the similarity functions
###################################################

## Test the result of ratioArea
test.getSimilarityMetrics_ratio_area_function<- function() {
    obs <- 1.084291188
    exp <- ratioArea(c(1,59,6,24,65,34,15,4,53,22), c(15,9,46,44,9,39,27,34,34,4))
    checkEqualsNumeric(obs, exp, tolerance = .Machine$double.eps^0.5)
}

## Test the result of ratioMaxMax
test.getSimilarityMetrics_ratio_max_max<- function() {
    obs <- 1.413043478
    exp <- ratioMaxMax(c(1,59,6,24,65,34,15,4,53,22), c(15,9,46,44,9,39,27,34,34,4))
    checkEqualsNumeric(obs, exp, tolerance = .Machine$double.eps^0.5)
}

## Test the result of diffPosMax
test.getSimilarityMetrics_ratio_max_max<- function() {
    obs <- 2
    exp <- diffPosMax(c(1,59,6,24,65,34,15,4,53,22), c(15,9,46,44,9,39,27,34,34,4))
    checkEqualsNumeric(obs, exp, tolerance = .Machine$double.eps^0.5)
}

## Test the result of ratioIntersect
test.getSimilarityMetrics_ratio_Intersect<- function() {
    obs <- 0.346534653
    exp <- ratioIntersect(c(1,59,6,24,65,34,15,4,53,22), c(15,9,46,44,9,39,27,34,34,4))
    checkEqualsNumeric(obs, exp, tolerance = .Machine$double.eps^0.5)
}