###################################################
# Created by Elsa Bernatchez
# 2014-08-14

###################################################
## Test the similarity functions
###################################################

### {{{ --- Test setup ---

if(FALSE) {
    library( "RUnit" )
    library( "similaRpeak" )
}

### }}}

## Test the result of ratioArea
test.getSimilarityMetrics_ratio_area_function<- function() {
    obs <- log(1.084291188)
    exp <- similaRpeak:::ratioArea(c(1,59,6,24,65,34,15,4,53,22), 
                                             c(15,9,46,44,9,39,27,34,34,4))
    checkEqualsNumeric(obs, exp, tolerance = .Machine$double.eps^0.5)
}

## Test the result of ratioMaxMax
test.getSimilarityMetrics_ratio_max_max_function<- function() {
    obs <- log(1.413043478)
    exp <- similaRpeak:::ratioMaxMax(c(1,59,6,24,65,34,15,4,53,22), 
                                               c(15,9,46,44,9,39,27,34,34,4))
    checkEqualsNumeric(obs, exp, tolerance = .Machine$double.eps^0.5)
}

## Test the result of diffPosMax
test.getSimilarityMetrics_diff_pos_max_function<- function() {
    obs <- 2
    exp <- similaRpeak:::diffPosMax(c(1,59,6,24,65,34,15,4,53,22), 
                                              c(15,9,46,44,9,39,27,34,34,4))
    checkEqualsNumeric(obs, exp, tolerance = .Machine$double.eps^0.5)
}

## Test the result of ratioIntersect
test.getSimilarityMetrics_ratio_Intersect_function<- function() {
    obs <- 0.346534653
    exp <- similaRpeak:::ratioIntersect(c(1,59,6,24,65,34,15,4,53,22), 
                                                  c(15,9,46,44,9,39,27,34,34,4))
    checkEqualsNumeric(obs, exp, tolerance = .Machine$double.eps^0.5)
}

######################################################
# Using only NA or zeros
######################################################

## Test the result of ratioArea when only NA in profiles
test.getSimilarityMetrics_ratio_area_function_na_only<- function() {
    obs <- NA
    exp <- similaRpeak:::ratioArea(c(NA,NA,NA,NA), c(NA,NA,NA,NA))
    checkEquals(obs, exp, msg ="The RATIO_AREA metric should return NA when only NA in profiles.")
}

## Test the result of diffPosMax when only 0 in profiles
test.getSimilarityMetrics_ratio_area_function_0<- function() {
    obs <- NA
    exp <- similaRpeak:::ratioArea(c(0,0,0,0), c(0,0,0,0))
    checkEquals(obs, exp, msg ="The RATIO_AREA metric should return NA when only zero in profiles.")
}

## Test the result of ratioMaxMax when only NA in profiles
test.getSimilarityMetrics_ratio_max_max_function_na_only<- function() {
    obs <- NA
    exp <- similaRpeak:::ratioMaxMax(c(NA,NA,NA,NA), c(NA,NA,NA,NA))
    checkEquals(obs, exp, msg ="The RATIO_MAX_MAX metric should return NA when only NA in profiles.")
}
## Test the result of ratioMaxMax when only zero in profiles
test.getSimilarityMetrics_ratio_max_max_function_0<- function() {
    obs <- NA
    exp <- similaRpeak:::ratioMaxMax(c(0,0,0,0), c(0,0,0,0))
    checkEquals(obs, exp, msg ="The RATIO_MAX_MAX metric should return NA when only 0 in profiles.")
}

## Test the result of diffPosMax when only NA in profiles
test.getSimilarityMetrics_diff_pos_max_function_na_only<- function() {
    obs <- NA
    exp <- similaRpeak:::diffPosMax(c(NA,NA,NA,NA), c(NA,NA,NA,NA))    
    checkEquals(obs, exp, msg ="The DIFF_POS_MAX metric should return NA when only NA in profiles.")
}

## Test the result of diffPosMax when only 0 in profiles
test.getSimilarityMetrics_diff_pos_max_function_0<- function() {
    obs <- NA
    exp <- similaRpeak:::diffPosMax(c(0,0,0,0), c(0,0,0,0))
    checkEquals(obs, exp, msg ="The DIFF_POS_MAX metric should return NA when only zero in profiles.")
}

## Test the result of ratioIntersect when only NA in profiles
test.getSimilarityMetrics_ratio_Intersect_function_na_only<- function() {
    obs <- NA
    exp <- similaRpeak:::ratioIntersect(c(NA,NA,NA,NA), 
                                                  c(NA,NA,NA,NA))
    checkEquals(obs, exp, msg ="The RATIO_INTERSECT metric should return NA when only NA in profiles.")
}

## Test the result of ratioIntersect when only 0 in profiles
test.getSimilarityMetrics_ratio_Intersect_function_0<- function() {
    obs <- NA
    exp <- similaRpeak:::ratioIntersect(c(0,0,0,0), c(0,0,0,0))
    checkEquals(obs, exp, msg ="The DIFF_POS_MAX metric should return NA when only zero in profiles.")
}

#### Using dispersed NA

## Test the result of ratioArea
test.getSimilarityMetrics_ratio_area_function_na<- function() {
    obs <- log(1.051886792)
    exp <- similaRpeak:::ratioArea(c(NA,NA,6,24,65,34,15,4,53,22), 
                                             c(NA,9,46,44,9,39,27,NA,34,4))
    checkEqualsNumeric(obs, exp, tolerance = .Machine$double.eps^0.5)
}

## Test the result of ratioMaxMax
test.getSimilarityMetrics_ratio_max_max_function_na<- function() {
    obs <- log(1.413043478)
    exp <- similaRpeak:::ratioMaxMax(c(NA,NA,6,24,65,34,15,4,53,22), 
                                               c(NA,9,46,44,9,39,27,NA,34,4))
    checkEqualsNumeric(obs, exp, tolerance = .Machine$double.eps^0.5)
}

## Test the result of diffPosMax
test.getSimilarityMetrics_diff_pos_max_function_na<- function() {
    obs <- 2
    exp <- similaRpeak:::diffPosMax(c(NA,NA,6,24,65,34,15,4,53,22), 
                                              c(NA,9,46,44,9,39,27,NA,34,4))
    checkEqualsNumeric(obs, exp, tolerance = .Machine$double.eps^0.5)
}

## Test the result of ratioIntersect
test.getSimilarityMetrics_ratio_Intersect_function_na<- function() {
    obs <- 0.40776699
    exp <- similaRpeak:::ratioIntersect(
        c(NA,NA,6,24,65,34,15,4,53,22), 
        c(NA,9,46,44,9,39,27,NA,34,4))
    checkEqualsNumeric(obs, exp, tolerance = .Machine$double.eps^0.5)
}
