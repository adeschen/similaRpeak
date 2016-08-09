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

## Test the result of ratioAreaMethod
test.getSimilarityMetrics_ratio_area_function<- function() {
    obs <- 1.084291188
    exp <- similaRpeak:::ratioAreaMethod(c(1,59,6,24,65,34,15,4,53,22), 
                                        c(15,9,46,44,9,39,27,34,34,4))
    checkEqualsNumeric(obs, exp, tolerance = .Machine$double.eps^0.5)
}

## Test the result of ratioMaxMaxMethod
test.getSimilarityMetrics_ratio_max_max_function<- function() {
    obs <- 1.413043478
    exp <- similaRpeak:::ratioMaxMaxMethod(c(1,59,6,24,65,34,15,4,53,22), 
                                        c(15,9,46,44,9,39,27,34,34,4))
    checkEqualsNumeric(obs, exp, tolerance = .Machine$double.eps^0.5)
}

## Test the result of diffPosMaxMethod
test.getSimilarityMetrics_diff_pos_max_function<- function() {
    obs <- 2
    exp <- similaRpeak:::diffPosMaxMethod(c(1,59,6,24,65,34,15,4,53,22), 
                                        c(15,9,46,44,9,39,27,34,34,4))
    checkEqualsNumeric(obs, exp, tolerance = .Machine$double.eps^0.5)
}

## Test the result of ratioIntersect
test.getSimilarityMetrics_ratio_Intersect_function<- function() {
    obs <- 0.346534653
    exp <- similaRpeak:::ratioIntersectMethod(c(1,59,6,24,65,34,15,4,53,22), 
                                            c(15,9,46,44,9,39,27,34,34,4))
    checkEqualsNumeric(obs, exp, tolerance = .Machine$double.eps^0.5)
}

######################################################
# Using only NA or zeros
######################################################

## Test the result of ratioAreaMethod when only NA in profiles
test.getSimilarityMetrics_ratio_area_function_na_only<- function() {
    obs <- as.numeric(NA)
    exp <- similaRpeak:::ratioAreaMethod(c(NA,NA,NA,NA), c(NA,NA,NA,NA))
    message <- paste0("The RATIO_AREA metric should return NA ", 
                        "when only NA in profiles.")
    checkEquals(obs, exp, msg = message)
}

## Test the result of diffPosMaxMethod when only 0 in profiles
test.getSimilarityMetrics_ratio_area_function_0<- function() {
    obs <- as.numeric(NA)
    exp <- similaRpeak:::ratioAreaMethod(c(0,0,0,0), c(0,0,0,0))
    message <- paste0("The RATIO_AREA metric should return NA ", 
                        "when only zero in profiles.")
    checkEquals(obs, exp, msg = message)
}

## Test the result of ratioMaxMaxMethod when only NA in profiles
test.getSimilarityMetrics_ratio_max_max_function_na_only<- function() {
    obs <- as.numeric(NA)
    exp <- similaRpeak:::ratioMaxMaxMethod(c(NA,NA,NA,NA), c(NA,NA,NA,NA))
    message <- paste0("The RATIO_MAX_MAX metric should return NA ", 
                        "when only NA in profiles.")
    checkEquals(obs, exp, msg = message)
}
## Test the result of ratioMaxMaxMethod when only zero in profiles
test.getSimilarityMetrics_ratio_max_max_function_0<- function() {
    obs <- as.numeric(NA)
    exp <- similaRpeak:::ratioMaxMaxMethod(c(0,0,0,0), c(0,0,0,0))
    message <- paste0("The RATIO_MAX_MAX metric should return NA ", 
                        "when only 0 in profiles.")
    checkEquals(obs, exp, msg = message)
}

## Test the result of diffPosMaxMethod when only NA in profiles
test.getSimilarityMetrics_diff_pos_max_function_na_only<- function() {
    obs <- as.numeric(NA)
    exp <- similaRpeak:::diffPosMaxMethod(c(NA,NA,NA,NA), c(NA,NA,NA,NA))   
    message <- paste0("The DIFF_POS_MAX metric should return NA ", 
                        "when only NA in profiles.")
    checkEquals(obs, exp, msg = message)
}

## Test the result of diffPosMaxMethod when only 0 in profiles
test.getSimilarityMetrics_diff_pos_max_function_0<- function() {
    obs <- as.numeric(NA)
    exp <- similaRpeak:::diffPosMaxMethod(c(0,0,0,0), c(0,0,0,0))
    message <- paste0("The DIFF_POS_MAX metric should return NA ", 
                        "when only zero in profiles.")
    checkEquals(obs, exp, msg = message)
}

## Test the result of ratioIntersectMethod when only NA in profiles
test.getSimilarityMetrics_ratio_Intersect_function_na_only<- function() {
    obs <- as.numeric(NA)
    exp <- similaRpeak:::ratioIntersectMethod(c(NA,NA,NA,NA), c(NA,NA,NA,NA))
    message <- paste0("The RATIO_INTERSECT metric should return NA ", 
                        "when only NA in profiles.")
    checkEquals(obs, exp, msg = message)
}

## Test the result of ratioIntersect when only 0 in profiles
test.getSimilarityMetrics_ratio_Intersect_function_0<- function() {
    obs <- as.numeric(NA)
    exp <- similaRpeak:::ratioIntersectMethod(c(0,0,0,0), c(0,0,0,0))
    message <- paste0("The DIFF_POS_MAX metric should return NA when only ", 
                        "zero in profiles.")
    checkEquals(obs, exp, msg = message)
}

## Test the result of spearmanCorrMethod when only NA in profiles
test.getSimilarityMetrics_spearman_corr_function_na_only<- function() {
    obs <- as.numeric(NA)
    exp <- similaRpeak:::spearmanCorrMethod(c(NA,NA,NA,NA), c(NA,NA,NA,NA))
    message <- paste0("The SPEARMAN_CORRELATION metric should return NA ", 
                        "when only NA in profiles.")
    checkEquals(obs, exp, msg = message) 
}

## Test the result of spearmanCorrMethod when only 0 in profiles
test.getSimilarityMetrics_spearman_corr_function_0<- function() {
    obs <- as.numeric(NA)
    exp <- similaRpeak:::spearmanCorrMethod(c(0,0,0,0), c(0,0,0,0))
    message <- paste0("The SPEARMAN_CORRELATION metric should return NA ", 
                        "when only zero in profiles.")
    checkEquals(obs, exp, msg = message)
}

#### Using dispersed NA

## Test the result of ratioAreaMethod
test.getSimilarityMetrics_ratio_area_function_na<- function() {
    obs <- 1.051886792
    exp <- similaRpeak:::ratioAreaMethod(c(NA,NA,6,24,65,34,15,4,53,22), 
                                        c(NA,9,46,44,9,39,27,NA,34,4))
    checkEqualsNumeric(obs, exp, tolerance = .Machine$double.eps^0.5)
}

## Test the result of ratioMaxMaxMethod
test.getSimilarityMetrics_ratio_max_max_function_na<- function() {
    obs <- 1.413043478
    exp <- similaRpeak:::ratioMaxMaxMethod(c(NA,NA,6,24,65,34,15,4,53,22), 
                                        c(NA,9,46,44,9,39,27,NA,34,4))
    checkEqualsNumeric(obs, exp, tolerance = .Machine$double.eps^0.5)
}

## Test the result of diffPosMaxMethod
test.getSimilarityMetrics_diff_pos_max_function_na<- function() {
    obs <- 2
    exp <- similaRpeak:::diffPosMaxMethod(c(NA,NA,6,24,65,34,15,4,53,22), 
                                    c(NA,9,46,44,9,39,27,NA,34,4))
    checkEqualsNumeric(obs, exp, tolerance = .Machine$double.eps^0.5)
}

## Test the result of ratioIntersectMethod
test.getSimilarityMetrics_ratio_intersect_function_na<- function() {
    obs <- 0.40776699
    exp <- similaRpeak:::ratioIntersectMethod(c(NA,NA,6,24,65,34,15,4,53,22), 
                                        c(NA,9,46,44,9,39,27,NA,34,4))
    checkEqualsNumeric(obs, exp, tolerance = .Machine$double.eps^0.5)
}

