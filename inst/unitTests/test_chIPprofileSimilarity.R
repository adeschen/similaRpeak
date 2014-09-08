###################################################
## Test the similarity function
###################################################

### {{{ --- Test setup ---

if(FALSE) {
    library( "RUnit" )
    library( "ChiPprofileSimilarity" )
}

### }}}

########################################################################
# Test arguments
########################################################################

## Test the result of non-numeric profile1
test.similarity_profile1_non_numeric<- function() {
    checkException(ChIPprofileSimilarity:::similarity(profile1=c(1,59,6,24,"a",34,15,4,53,22), profile2=c(15,9,46,44,9,39,27,34,34,4)), msg ="The 'profile1' argument must be a numeric vector.")
}

## Test the result of non-vector profile1
test.similarity_profile1_non_vector<- function() {
    checkException(ChIPprofileSimilarity:::similarity(profile1=matrix(c(1,59,6,24,65,34,15,4,53,22), ncol=2), profile2=c(15,9,46,44,9,39,27,34,34,4)), msg ="The 'profile1' argument must be a numeric vector.")
}

## Test the result of negatives values profile1
test.similarity_profile1_negatives_values<- function() {
    checkException(ChIPprofileSimilarity:::similarity(profile1=c(0,0,0,-8,0,0,0,0,0,0), profile2=c(15,9,46,44,9,39,27,34,34,4)), msg ="The profile1 argument contains negatives or is made up of zeros only.")
}

## Test the result of different profiles lengths
test.similarity_different_profiles_lengths<- function() {
    checkException(ChIPprofileSimilarity:::similarity(profile1=c(1,59,6,24,65,34,15,4,53,22), profile2=c(39,27,34,34,4)), msg ="Lengths of 'profile1' and 'profile2' vectors aren't equals.")
}

## Test the result of null numeric ratioAreaThreshold
test.similarity_null_ratioAreaThreshold<- function() {
    checkException(ChIPprofileSimilarity:::similarity(profile1=c(1,59,6,24,65,34,15,4,53,22), profile2=c(15,9,46,44,9,39,27,34,34,4), ratioAreaThreshold=0), msg ="The 'ratioAreaThreshold' must be a positive numeric value.")
}

## Test the result of negative ratioAreaThreshold
test.similarity_negative_ratioAreaThreshold<- function() {
    checkException(ChIPprofileSimilarity:::similarity(profile1=c(1,59,6,24,65,34,15,4,53,22), profile2=c(15,9,46,44,9,39,27,34,34,4), ratioAreaThreshold=-5), msg ="The 'ratioAreaThreshold' must be a positive numeric value.")
}

## Test the result of non numeric ratioAreaThreshold
test.similarity_non_numeric_ratioAreaThreshold<- function() {
    checkException(ChIPprofileSimilarity:::similarity(profile1=c(1,59,6,24,65,34,15,4,53,22), profile2=c(15,9,46,44,9,39,27,34,34,4), ratioAreaThreshold="g"), msg ="The 'ratioAreaThreshold' must be a positive numeric value.")
}

## Test the result of null numeric ratioMaxMaxThreshold
test.similarity_null_ratioMaxMaxThreshold<- function() {
    checkException(ChIPprofileSimilarity:::similarity(profile1=c(1,59,6,24,65,34,15,4,53,22), profile2=c(15,9,46,44,9,39,27,34,34,4), ratioMaxMaxThreshold=0), msg ="The 'ratioMaxMaxThreshold' must be a positive numeric value.")
}

## Test the result of negative ratioMaxMaxThreshold
test.similarity_negative_ratioMaxMaxThreshold<- function() {
    checkException(ChIPprofileSimilarity:::similarity(profile1=c(1,59,6,24,65,34,15,4,53,22), profile2=c(15,9,46,44,9,39,27,34,34,4), ratioMaxMaxThreshold=-5), msg ="The 'ratioMaxMaxThreshold' must be a positive numeric value.")
}

## Test the result of non numeric ratioMaxMaxThreshold
test.similarity_non_numeric_ratioMaxMaxThreshold<- function() {
    checkException(ChIPprofileSimilarity:::similarity(profile1=c(1,59,6,24,65,34,15,4,53,22), profile2=c(15,9,46,44,9,39,27,34,34,4), ratioMaxMaxThreshold="g"), msg ="The 'ratioMaxMaxThreshold' must be a positive numeric value.")
}

## Test the result of null numeric ratioIntersectThreshold
test.similarity_null_ratioIntersectThreshold<- function() {
    checkException(ChIPprofileSimilarity:::similarity(profile1=c(1,59,6,24,65,34,15,4,53,22), profile2=c(15,9,46,44,9,39,27,34,34,4), ratioIntersectThreshold=0), msg ="The 'ratioIntersectThreshold' must be a positive numeric value.")
}

## Test the result of negative ratioIntersectThreshold
test.similarity_negative_ratioIntersectThreshold<- function() {
    checkException(ChIPprofileSimilarity:::similarity(profile1=c(1,59,6,24,65,34,15,4,53,22), profile2=c(15,9,46,44,9,39,27,34,34,4), ratioIntersectThreshold=-5), msg ="The 'ratioIntersectThreshold' must be a positive numeric value.")
}

## Test the result of non numeric ratioIntersectThreshold
test.similarity_non_numeric_ratioIntersectThreshold<- function() {
    checkException(ChIPprofileSimilarity:::similarity(profile1=c(1,59,6,24,65,34,15,4,53,22), profile2=c(15,9,46,44,9,39,27,34,34,4), ratioIntersectThreshold="g"), msg ="The 'ratioIntersectThreshold' must be a positive numeric value.")
}

## Test the result of null numeric diffPosMaxThresholdMinValue
test.similarity_null_diffPosMaxThresholdMinValue<- function() {
    checkException(ChIPprofileSimilarity:::similarity(profile1=c(1,59,6,24,65,34,15,4,53,22), profile2=c(15,9,46,44,9,39,27,34,34,4), diffPosMaxThresholdMinValue=0), msg ="The 'diffPosMaxThresholdMinValue' must be a positive numeric value.")
}

## Test the result of negative diffPosMaxThresholdMinValue
test.similarity_negative_diffPosMaxThresholdMinValue<- function() {
    checkException(ChIPprofileSimilarity:::similarity(profile1=c(1,59,6,24,65,34,15,4,53,22), profile2=c(15,9,46,44,9,39,27,34,34,4), diffPosMaxThresholdMinValue=-5), msg ="The 'diffPosMaxThresholdMinValue' must be a positive numeric value.")
}

## Test the result of non numeric diffPosMaxThresholdMinValue
test.similarity_non_numeric_diffPosMaxThresholdMinValue<- function() {
    checkException(ChIPprofileSimilarity:::similarity(profile1=c(1,59,6,24,65,34,15,4,53,22), profile2=c(15,9,46,44,9,39,27,34,34,4), diffPosMaxThresholdMinValue="g"), msg ="The 'diffPosMaxThresholdMinValue' must be a positive numeric value.")
}

## Test the result of null numeric diffPosMaxThresholdMaxDiff
test.similarity_null_diffPosMaxThresholdMaxDiff<- function() {
    checkException(ChIPprofileSimilarity:::similarity(profile1=c(1,59,6,24,65,34,15,4,53,22), profile2=c(15,9,46,44,9,39,27,34,34,4), diffPosMaxThresholdMaxDiff=0), msg ="The 'diffPosMaxThresholdMaxDiff' must be a positive numeric value.")
}

## Test the result of negative diffPosMaxThresholdMaxDiff
test.similarity_negative_diffPosMaxThresholdMaxDiff<- function() {
    checkException(ChIPprofileSimilarity:::similarity(profile1=c(1,59,6,24,65,34,15,4,53,22), profile2=c(15,9,46,44,9,39,27,34,34,4), diffPosMaxThresholdMaxDiff=-5), msg ="The 'diffPosMaxThresholdMaxDiff' must be a positive numeric value.")
}

## Test the result of non numeric diffPosMaxThresholdMaxDiff
test.similarity_non_numeric_diffPosMaxThresholdMaxDiff<- function() {
    checkException(ChIPprofileSimilarity:::similarity(profile1=c(1,59,6,24,65,34,15,4,53,22), profile2=c(15,9,46,44,9,39,27,34,34,4), diffPosMaxThresholdMaxDiff="g"), msg ="The 'diffPosMaxThresholdMaxDiff' must be a positive numeric value.")
}

## Test the result of null numeric diffPosMaxThresholdMaxDiff
test.similarity_null_diffPosMaxThresholdMaxDiff<- function() {
    checkException(ChIPprofileSimilarity:::similarity(profile1=c(1,59,6,24,65,34,15,4,53,22), profile2=c(15,9,46,44,9,39,27,34,34,4), diffPosMaxThresholdMaxDiff=0), msg ="The 'diffPosMaxThresholdMaxDiff' must be a positive numeric value.")
}

## Test the result of negative diffPosMaxThresholdMaxDiff
test.similarity_negative_diffPosMaxThresholdMaxDiff<- function() {
    checkException(ChIPprofileSimilarity:::similarity(profile1=c(1,59,6,24,65,34,15,4,53,22), profile2=c(15,9,46,44,9,39,27,34,34,4), diffPosMaxThresholdMaxDiff=-5), msg ="The 'diffPosMaxThresholdMaxDiff' must be a positive numeric value.")
}

## Test the result of non numeric diffPosMaxThresholdMaxDiff
test.similarity_non_numeric_diffPosMaxThresholdMaxDiff<- function() {
    checkException(ChIPprofileSimilarity:::similarity(profile1=c(1,59,6,24,65,34,15,4,53,22), profile2=c(15,9,46,44,9,39,27,34,34,4), diffPosMaxThresholdMaxDiff="g"), msg ="The 'diffPosMaxThresholdMaxDiff' must be a positive numeric value.")
}

## Test the result of null numeric diffPosMaxTolerancePercent
test.similarity_null_diffPosMaxTolerancePercent<- function() {
    checkException(ChIPprofileSimilarity:::similarity(profile1=c(1,59,6,24,65,34,15,4,53,22), profile2=c(15,9,46,44,9,39,27,34,34,4), diffPosMaxTolerancePercent=0), msg ="The 'diffPosMaxTolerancePercent' must be a positive numeric value.")
}

## Test the result of negative diffPosMaxTolerancePercent
test.similarity_negative_diffPosMaxTolerancePercent<- function() {
    checkException(ChIPprofileSimilarity:::similarity(profile1=c(1,59,6,24,65,34,15,4,53,22), profile2=c(15,9,46,44,9,39,27,34,34,4), diffPosMaxTolerancePercent=-5), msg ="The 'diffPosMaxTolerancePercent' must be a positive numeric value.")
}

## Test the result of non numeric diffPosMaxTolerancePercent
test.similarity_non_numeric_diffPosMaxTolerancePercent<- function() {
    checkException(ChIPprofileSimilarity:::similarity(profile1=c(1,59,6,24,65,34,15,4,53,22), profile2=c(15,9,46,44,9,39,27,34,34,4), diffPosMaxTolerancePercent="g"), msg ="The 'diffPosMaxTolerancePercent' must be a positive numeric value.")
}

########################################################################
# Test arguments for specifics metrics
########################################################################

## Test the result of zeros vector profile1 for the RATIO_MAX_MAX value
test.similarity_profile1_zeros_RATIO_MAX_MAX<- function() {
    obs = ChIPprofileSimilarity:::similarity(profile1=c(0,0,0,0,0,0,0,0,0,0), profile2=c(15,9,46,44,9,39,27,34,34,4))
    exp = NA
    checkEquals(obs$metrics$RATIO_MAX_MAX, exp , msg="A profile with only zero values did not generate NA for the RATIO_MAX_MAX")
}

## Test the result of zeros vector profile1 for the RATIO_AREA value
test.similarity_profile1_zeros_RATIO_AREA<- function() {
    obs = ChIPprofileSimilarity:::similarity(profile1=c(0,0,0,0,0,0,0,0,0,0), profile2=c(15,9,46,44,9,39,27,34,34,4))
    exp = NA
    checkEquals(obs$metrics$RATIO_AREA, exp , msg="A profile with only zero values did not generate NA for the RATIO_MAX_MAX")
}

## Test the result of zeros vector profile1 for the RATIO_INTERSECT value
test.similarity_profile1_zeros_RATIO_INTERSECT<- function() {
    obs = ChIPprofileSimilarity:::similarity(profile1=c(0,0,0,0,0,0,0,0,0,0), profile2=c(15,9,46,44,9,39,27,34,34,4))
    exp = 0
    checkEquals(obs$metrics$RATIO_INTERSECT, exp , msg="A profile with only zero values did not generate 0 for the RATIO_MAX_MAX")
}

## Test the result of zeros vector profile1 and profile2 for the DIFF_POS_MAX value
test.similarity_profile1_zeros_DIFF_POS_MAX<- function() {
    obs = ChIPprofileSimilarity:::similarity(profile1=c(0,0,0,0,0,0,0,0,0,0), profile2=c(0,0,0,0,0,0,0,0,0,0))
    exp = 0
    checkEquals(obs$metrics$DIFF_POS_MAX, exp , msg="A profile with only zero values did not generate 0 for the RATIO_MAX_MAX")
}

## Test the result of zeros vector profile1 and profile2 for the RATIO_INTERSECT value
test.similarity_profile1_and_profile2_zeros_RATIO_INTERSECT<- function() {
    obs = ChIPprofileSimilarity:::similarity(profile1=c(0,0,0,0,0,0,0,0,0,0), profile2=c(0,0,0,0,0,0,0,0,0,0))
    exp = NA
    checkEquals(obs$metrics$RATIO_INTERSECT, exp , msg="Two profiles with only zero values did not generate NA for the RATIO_MAX_MAX")
}

########################################################################
# Test expected results for specifics metrics
########################################################################

## Test the metadata results
test.similarity_metadata<- function() {
    obs <- list("nbrPosition"=10,"areaProfile1"=283,"areaProfile2"=261,"maxProfile1"=65,"maxProfile2"=46,"maxPositionProfile1"=5,
                "maxPositionProfile2"=3)
    exp <- ChIPprofileSimilarity:::similarity(c(1,59,6,24,65,34,15,4,53,22), c(15,9,46,44,9,39,27,34,34,4))[1:7]
    checkEquals(obs, exp, tolerance = .Machine$double.eps^0.5)
}

## Test the metadata results with some NA in profiles
test.similarity_na_profile+metadata<- function() {
    obs <- list("nbrPosition"=10,"areaProfile1"=223,"areaProfile2"=212,"maxProfile1"=65,"maxProfile2"=46,"maxPositionProfile1"=5,
                "maxPositionProfile2"=3)
    exp <- ChIPprofileSimilarity:::similarity(c(NA,NA,6,24,65,34,15,4,53,22), c(NA,9,46,44,9,39,27,NA,34,4))[1:7]
    checkEquals(obs, exp, tolerance = .Machine$double.eps^0.5)
}

## Test the result of metric RATIO_AREA
test.similarity_ratio_area<- function() {
    obs <- 1.084291
    exp <- ChIPprofileSimilarity:::similarity(c(1,59,6,24,65,34,15,4,53,22), c(15,9,46,44,9,39,27,34,34,4))$metrics$RATIO_AREA
    checkEquals(obs, exp, tolerance = .Machine$double.eps^0.5)
}

## Test the result of metric DIFF_POS_MAX
test.similarity_diff_pos_max<- function() {
    obs <- 2
    exp <- ChIPprofileSimilarity:::similarity(c(1,59,6,24,65,34,15,4,53,22), c(15,9,46,44,9,39,27,34,34,4))$metrics$DIFF_POS_MAX
    checkEquals(obs, exp, tolerance = .Machine$double.eps^0.5)
}

## Test the result of metric RATIO_MAX_MAX
test.similarity_ratio_max_max<- function() {
    obs <- 1.477273
    exp <- ChIPprofileSimilarity:::similarity(c(1,59,6,24,65,34,15,4,53,22), c(15,9,46,44,9,39,27,34,34,4))$metrics$RATIO_MAX_MAX
    checkEquals(obs, exp, tolerance = .Machine$double.eps^0.5)
}

## Test the result of metric RATIO_INTERSECT
test.similarity_ratio_intersect<- function() {
    obs <- 0.4852941
    exp <- ChIPprofileSimilarity:::similarity(c(1,59,6,24,65,34,15,4,53,22), c(15,9,46,44,9,39,27,34,34,4))$metrics$RATIO_INTERSECT
    checkEquals(obs, exp, tolerance = .Machine$double.eps^0.5)
}


