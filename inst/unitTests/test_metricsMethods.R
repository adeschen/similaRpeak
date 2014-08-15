###################################################
## Test the similarity functions
###################################################

### {{{ --- Test setup ---

if(FALSE) {
    library( "RUnit" )
    library( "ChiPprofileSimilarity" )
}

### }}}

## Test the result of ratioArea
test.getSimilarityMetrics_ratio_area_function<- function() {
    obs <- 1.084291188
    exp <- ChIPprofileSimilarity:::ratioArea(c(1,59,6,24,65,34,15,4,53,22), c(15,9,46,44,9,39,27,34,34,4))
    checkEqualsNumeric(obs, exp, tolerance = .Machine$double.eps^0.5)
}

## Test the result of ratioMaxMax
test.getSimilarityMetrics_ratio_max_max_function<- function() {
    obs <- 1.413043478
    exp <- ChIPprofileSimilarity:::ratioMaxMax(c(1,59,6,24,65,34,15,4,53,22), c(15,9,46,44,9,39,27,34,34,4))
    checkEqualsNumeric(obs, exp, tolerance = .Machine$double.eps^0.5)
}

## Test the result of diffPosMax
test.getSimilarityMetrics_diff_pos_max_function<- function() {
    obs <- 2
    exp <- ChIPprofileSimilarity:::diffPosMax(c(1,59,6,24,65,34,15,4,53,22), c(15,9,46,44,9,39,27,34,34,4))
    checkEqualsNumeric(obs, exp, tolerance = .Machine$double.eps^0.5)
}

## Test the result of ratioIntersect
test.getSimilarityMetrics_ratio_Intersect_function<- function() {
    obs <- 0.346534653
    exp <- ChIPprofileSimilarity:::ratioIntersect(c(1,59,6,24,65,34,15,4,53,22), c(15,9,46,44,9,39,27,34,34,4))
    checkEqualsNumeric(obs, exp, tolerance = .Machine$double.eps^0.5)
}


#### Using only NA or zeros

## Test the result of ratioArea
test.getSimilarityMetrics_ratio_area_function_na_only<- function() {
    obs <- NA
    exp <- ChIPprofileSimilarity:::ratioArea(c(NA,NA,NA,NA), c(NA,NA,NA,NA))
    checkEqualsNumeric(obs, exp, tolerance = .Machine$double.eps^0.5)
}
test.getSimilarityMetrics_ratio_area_function_0<- function() {
    obs <- NA
    exp <- ChIPprofileSimilarity:::ratioArea(c(0,0,0,0), c(0,0,0,0))
    checkEqualsNumeric(obs, exp, tolerance = .Machine$double.eps^0.5)
}

## Test the result of ratioMaxMax
test.getSimilarityMetrics_ratio_max_max_function_na_only<- function() {
    obs <- NA
    exp <- ChIPprofileSimilarity:::ratioMaxMax(c(NA,NA,NA,NA), c(NA,NA,NA,NA))
    checkEqualsNumeric(obs, exp, tolerance = .Machine$double.eps^0.5)
}
test.getSimilarityMetrics_ratio_max_max_function_0<- function() {
    obs <- NA
    exp <- ChIPprofileSimilarity:::ratioMaxMax(c(0,0,0,0), c(0,0,0,0))
    checkEqualsNumeric(obs, exp, tolerance = .Machine$double.eps^0.5)
}

## Test the result of diffPosMax
test.getSimilarityMetrics_diff_pos_max_function_na_only<- function() {
    obs <- NA
    exp <- ChIPprofileSimilarity:::diffPosMax(c(NA,NA,NA,NA), c(NA,NA,NA,NA))
    checkEqualsNumeric(obs, exp, tolerance = .Machine$double.eps^0.5)
}
test.getSimilarityMetrics_diff_pos_max_function_0<- function() {
    obs <- 0
    exp <- ChIPprofileSimilarity:::diffPosMax(c(0,0,0,0), c(0,0,0,0))
    checkEqualsNumeric(obs, exp, tolerance = .Machine$double.eps^0.5)
}

## Test the result of ratioIntersect
test.getSimilarityMetrics_ratio_Intersect_function_na_only<- function() {
    obs <- NA
    exp <- ChIPprofileSimilarity:::ratioIntersect(c(NA,NA,NA,NA), c(NA,NA,NA,NA))
    checkEqualsNumeric(obs, exp, tolerance = .Machine$double.eps^0.5)
}
test.getSimilarityMetrics_ratio_Intersect_function_0<- function() {
    obs <- NA
    exp <- ChIPprofileSimilarity:::ratioIntersect(c(0,0,0,0), c(0,0,0,0))
    checkEqualsNumeric(obs, exp, tolerance = .Machine$double.eps^0.5)
}


#### Using dispersed NA

## Test the result of ratioArea
test.getSimilarityMetrics_ratio_area_function_na<- function() {
    obs <- 1.051886792
    exp <- ChIPprofileSimilarity:::ratioArea(c(NA,NA,6,24,65,34,15,4,53,22), c(NA,9,46,44,9,39,27,NA,34,4))
    checkEqualsNumeric(obs, exp, tolerance = .Machine$double.eps^0.5)
}

## Test the result of ratioMaxMax
test.getSimilarityMetrics_ratio_max_max_function_na<- function() {
    obs <- 1.413043478
    exp <- ChIPprofileSimilarity:::ratioMaxMax(c(NA,NA,6,24,65,34,15,4,53,22), c(NA,9,46,44,9,39,27,NA,34,4))
    checkEqualsNumeric(obs, exp, tolerance = .Machine$double.eps^0.5)
}

## Test the result of diffPosMax
test.getSimilarityMetrics_diff_pos_max_function_na<- function() {
    obs <- 2
    exp <- ChIPprofileSimilarity:::diffPosMax(c(NA,NA,6,24,65,34,15,4,53,22), c(NA,9,46,44,9,39,27,NA,34,4))
    checkEqualsNumeric(obs, exp, tolerance = .Machine$double.eps^0.5)
}

## Test the result of ratioIntersect
test.getSimilarityMetrics_ratio_Intersect_function_na<- function() {
    obs <- 0.40776699
    exp <- ChIPprofileSimilarity:::ratioIntersect(c(NA,NA,6,24,65,34,15,4,53,22), c(NA,9,46,44,9,39,27,NA,34,4))
    checkEqualsNumeric(obs, exp, tolerance = .Machine$double.eps^0.5)
}
