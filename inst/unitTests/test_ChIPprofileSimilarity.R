

###################################################
## Test the prepareBamFiles() function
###################################################

## Zero min threshold should not be accepted as an argument
test.getSimilarityMetrics_zero_min_threshold<- function() {
    obs <- tryCatch(ChIPprofileSimilarity:::getSimilarityMetrics(c(1,3,4,5,3), c(3,3,3,3,4), 0, FALSE), error=conditionMessage)
    exp <- "The minimum threshold has to be a positive number."
    checkIdentical(obs, exp, msg="getSimilarityMetrics() - A zero minimum threshold argument did not generate an exception with expected message.")
}

## Negative min threshold should not be accepted as an argument
test.getSimilarityMetrics_negative_min_threshold<- function() {
    obs <- tryCatch(ChIPprofileSimilarity:::getSimilarityMetrics(c(1,3,4,5,3), c(3,3,3,3,4), -4, FALSE), error=conditionMessage)
    exp <- "The minimum threshold has to be a positive number."
    checkIdentical(obs, exp, msg="getSimilarityMetrics() - A negative minimum threshold argument did not generate an exception with expected message.")
}

## Two profiles without same vector length should not be accepted as an argument
test.getSimilarityMetrics_not_same_vector_length<- function() {
    obs <- tryCatch(ChIPprofileSimilarity:::getSimilarityMetrics(c(1,3,4,5,3,3), c(3,3,3,3,4), 0, FALSE), error=conditionMessage)
    exp <- "The two profiles should have exactly the same length."
    checkIdentical(obs, exp, msg="getSimilarityMetrics() - Two profiles without the same vector length did not generate an exception with expected message.")
}
