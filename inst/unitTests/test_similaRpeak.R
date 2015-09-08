###################################################
# Created by Astrid Deschenes
# 2014-08-08

###################################################
## Test the similarity function
###################################################

### {{{ --- Test setup ---

if(FALSE) {
    library( "RUnit" )
    library( "similaRpeak" )
}

### }}}

########################################################################
# Test arguments that should generate an exception
########################################################################

## Test the result of non-numeric profile1
test.similarity_profile1_non_numeric <- function() {
    exp <- "The 'profile1' argument must be a numeric vector."
    obs <- tryCatch(similarity(profile1 = c(1,59,6,24,2,"a",15,4,53,22),
                                profile2 = c(15,9,46,44,9,39,22,34,34,4)), 
                    error = conditionMessage)
    message <- paste0("test.similarity_profile1_non_numeric() ",
                      "- A non numeric profile1 did not generated ",
                      "expected error.")
    checkEquals(obs, exp, msg = message)
}

## Test the result of non-vector profile1
test.similarity_profile1_non_vector <- function() {
    exp <- "The 'profile1' argument must be a numeric vector."
    obs <- tryCatch(similarity(profile1 = matrix(c(15,9,46,44,9,39,27,34,34,4), 
                                                ncol=2), 
                                profile2 = c(15,9,46,44,9,39,27,34,34,4)),
                    error = conditionMessage)
    message <- paste0("test.similarity_profile1_non_vector() ",
                      "- A non-vector profile1 did not generated ",
                      "expected error.")
    checkEquals(obs, exp, msg = message)
}

## Test the result of negatives values profile1
test.similarity_profile1_negatives_values <- function() {
    exp <- "The 'profile1' argument contains at least one negative number."
    obs <- tryCatch(similarity(profile1 = c(0,0,0,-8,0,0,0,0,0,0),
                               profile2 = c(15,9,46,44,9,39,27,34,34,4)), 
                    error = conditionMessage)
    message <- paste0("test.similarity_profile1_negatives_values() ",
                      "- A profile1 with negative value did not generated ",
                      "expected error.")
    checkEquals(obs, exp, msg = message)
}

## Test the result of non-numeric profile2
test.similarity_profile2_non_numeric <- function() {
    exp <- "The 'profile2' argument must be a numeric vector."
    obs <- tryCatch(similarity(profile1 = c(1,59,6,24,2,34,15,4,53,22),
                                profile2 = c(15,9,46,44,9,39,"e",34,34,4)), 
                    error = conditionMessage)
    message <- paste0("test.similarity_profile2_non_numeric() ",
                        "- A non numeric profile2 did not generated ",
                        "expected error.")
    checkEquals(obs, exp, msg = message)
}

## Test the result of non-vector profile2
test.similarity_profile2_non_vector <- function() {
    exp <- "The 'profile2' argument must be a numeric vector."
    obs <- tryCatch(similarity(profile1 = c(1,59,6,24,65,34,15,4,53,22),
                                profile2 = matrix(c(15,9,46,44,9,39,27,34,34,4), 
                                                ncol=2)), 
                    error = conditionMessage)
    message <- paste0("test.similarity_profile2_non_vector() ",
                        "- A non-vector profile2 did not generated ",
                        "expected error.")
    checkEquals(obs, exp, msg = message)
}

## Test the result of negatives values profile2
test.similarity_profil2_negatives_values <- function() {
    exp <- "The 'profile2' argument contains at least one negative number."
    obs <- tryCatch(similarity(profile1 = c(0,0,0,8,0,0,0,0,0,0),
                               profile2 = c(15,9,46,-1,9,39,27,34,34,4)), 
                    error = conditionMessage)
    message <- paste0("test.similarity_profil2_negatives_values() ",
                          "- A profile2 with negative value did not generated ",
                          "expected error.")
    checkEquals(obs, exp, msg = message)
}


## Test the result of different profiles lengths
test.similarity_different_profiles_lengths <- function() {
    exp <- "Lengths of 'profile1' and 'profile2' vectors aren't equals."
    obs <- tryCatch(similarity(profile1 = c(1,59,6,24,65,34,15,4,53,22),
                                    profile2 = c(39,27,34,34,4)), 
                                    error = conditionMessage)
    message <- paste0("test.similarity_different_profiles_lengths() ",
                      "- A profile1 and profile2 with different lenghts ",
                      "did not generated expected error.")
    checkEquals(obs, exp, msg = message)
}

## Test the result of negative ratioAreaThreshold
test.similarity_negative_ratioAreaThreshold <- function() {
    exp <- "The 'ratioAreaThreshold' must be a positive numeric value."
    obs <- tryCatch(similarity(profile1 = c(1,59,6,24,65,34,15,4,53,22),
                                profile2 = c(15,9,46,44,9,39,27,34,34,4),
                                ratioAreaThreshold= -4), 
                                error = conditionMessage)
    message <- paste0("test.similarity_negative_ratioAreaThreshold() ",
                      "- A ratioAreaThreshold with negative value did not generated ",
                      "expected error.")
    checkEquals(obs, exp, msg = message)
}

## Test the result of non numeric ratioAreaThreshold
test.similarity_non_numeric_ratioAreaThreshold <- function() {
    exp <- "The 'ratioAreaThreshold' must be a positive numeric value."
    obs <- tryCatch(similarity(profile1 = c(1,59,6,24,65,34,15,4,53,22),
                               profile2 = c(15,9,46,44,9,39,27,34,34,4),
                               ratioAreaThreshold= "g"), 
                    error = conditionMessage)
    message <- paste0("test.similarity_non_numeric_ratioAreaThreshold() ",
                      "- A non numeric ratioAreaThreshold ",
                      "did not generated expected error.")
    checkEquals(obs, exp, msg = message)
}

## Test the result of zero numeric ratioMaxMaxThreshold
test.similarity_zero_ratioMaxMaxThreshold <- function() {
    exp <- "The 'ratioMaxMaxThreshold' must be a positive numeric value."
    obs <- tryCatch(similarity(profile1 = c(1,59,6,24,65,34,15,4,53,22),
                               profile2 = c(15,9,46,44,9,39,27,34,34,4),
                               ratioMaxMaxThreshold= 0), 
                    error = conditionMessage)
    message <- paste0("test.similarity_null_ratioMaxMaxThreshold() ",
                      "- A ratioMaxMaxThreshold with zero value ",
                      "did not generated expected error.")
    checkEquals(obs, exp, msg = message)
}

## Test the result of negative ratioMaxMaxThreshold
test.similarity_negative_ratioMaxMaxThreshold <- function() {
    exp <- "The 'ratioMaxMaxThreshold' must be a positive numeric value."
    obs <- tryCatch(similarity(profile1 = c(1,59,6,24,65,34,15,4,53,22),
                               profile2 = c(15,9,46,44,9,39,27,34,34,4),
                               ratioMaxMaxThreshold= -5), 
                    error = conditionMessage)
    message <- paste0("test.similarity_negative_ratioMaxMaxThreshold() ",
                      "- A ratioMaxMaxThreshold with negative value ",
                      "did not generated expected error.")
    checkEquals(obs, exp, msg = message)
}

## Test the result of non numeric ratioMaxMaxThreshold
test.similarity_non_numeric_ratioMaxMaxThreshold <- function() {
    exp <- "The 'ratioMaxMaxThreshold' must be a positive numeric value."
    obs <- tryCatch(similarity(profile1 = c(1,59,6,24,65,34,15,4,53,22),
                               profile2 = c(15,9,46,44,9,39,27,34,34,4),
                               ratioMaxMaxThreshold= "g"), 
                    error = conditionMessage)
    message <- paste0("test.similarity_non_numeric_ratioMaxMaxThreshold() ",
                      "- A ratioMaxMaxThreshold with non numeric value ",
                      "did not generated expected error.")
    checkEquals(obs, exp, msg = message)
}

## Test the result of zero numeric ratioIntersectThreshold
test.similarity_zero_ratioIntersectThreshold <- function() {
    exp <- "The 'ratioIntersectThreshold' must be a positive numeric value."
    obs <- tryCatch(similarity(profile1 = c(1,59,6,24,65,34,15,4,53,22),
                               profile2 = c(15,9,46,44,9,39,27,34,34,4),
                               ratioIntersectThreshold= 0), 
                    error = conditionMessage)
    message <- paste0("test.similarity_zero_ratioIntersectThreshold() ",
                      "- A ratioIntersectThreshold with zero value ",
                      "did not generated expected error.")
    checkEquals(obs, exp, msg = message)
}

## Test the result of negative ratioIntersectThreshold
test.similarity_negative_ratioIntersectThreshold <- function() {
    exp <- "The 'ratioIntersectThreshold' must be a positive numeric value."
    obs <- tryCatch(similarity(profile1 = c(1,59,6,24,65,34,15,4,53,22),
                               profile2 = c(15,9,46,44,9,39,27,34,34,4),
                               ratioIntersectThreshold= -3), 
                    error = conditionMessage)
    message <- paste0("test.similarity_negative_ratioIntersectThreshold() ",
                      "- A ratioIntersectThreshold with negative value ",
                      "did not generated expected error.")
    checkEquals(obs, exp, msg = message)
}

## Test the result of non numeric ratioIntersectThreshold
test.similarity_non_numeric_ratioIntersectThreshold <- function() {
    exp <- "The 'ratioIntersectThreshold' must be a positive numeric value."
    obs <- tryCatch(similarity(profile1 = c(1,59,6,24,65,34,15,4,53,22),
                               profile2 = c(15,9,46,44,9,39,27,34,34,4),
                               ratioIntersectThreshold= "g"), 
                    error = conditionMessage)
    message <- paste0("test.similarity_negative_ratioIntersectThreshold() ",
                      "- A ratioIntersectThreshold with non numeric value ",
                      "did not generated expected error.")
    checkEquals(obs, exp, msg = message)
}

## Test the result of zero numeric diffPosMaxThresholdMinValue
test.similarity_zero_diffPosMaxThresholdMinValue <- function() {
    exp <- "The 'diffPosMaxThresholdMinValue' must be a positive numeric value."
    obs <- tryCatch(similarity(profile1 = c(1,59,6,24,65,34,15,4,53,22),
                               profile2 = c(15,9,46,44,9,39,27,34,34,4),
                               diffPosMaxThresholdMinValue= 0), 
                    error = conditionMessage)
    message <- paste0("test.similarity_zero_diffPosMaxThresholdMinValue() ",
                      "- A diffPosMaxThresholdMinValue with zero value ",
                      "did not generated expected error.")
    checkEquals(obs, exp, msg = message)
}

## Test the result of negative diffPosMaxThresholdMinValue
test.similarity_negative_diffPosMaxThresholdMinValue <- function() {
    exp <- "The 'diffPosMaxThresholdMinValue' must be a positive numeric value."
    obs <- tryCatch(similarity(profile1 = c(1,59,6,24,65,34,15,4,53,22),
                               profile2 = c(15,9,46,44,9,39,27,34,34,4),
                               diffPosMaxThresholdMinValue= -5), 
                    error = conditionMessage)
    message <- paste0("test.similarity_negative_diffPosMaxThresholdMinValue() ",
                      "- A diffPosMaxThresholdMinValue with negative value ",
                      "did not generated expected error.")
    checkEquals(obs, exp, msg = message)
}

## Test the result of non numeric diffPosMaxThresholdMinValue
test.similarity_non_numeric_diffPosMaxThresholdMinValue <- function() {
    exp <- "The 'diffPosMaxThresholdMinValue' must be a positive numeric value."
    obs <- tryCatch(similarity(profile1 = c(1,59,6,24,65,34,15,4,53,22),
                               profile2 = c(15,9,46,44,9,39,27,34,34,4),
                               diffPosMaxThresholdMinValue= "t"), 
                    error = conditionMessage)
    message <- paste0("test.similarity_non_numeric_diffPosMaxThresholdMinValue ",
                      "- A diffPosMaxThresholdMinValue with non numeric value ",
                      "did not generated expected error.")
    checkEquals(obs, exp, msg = message)
}

## Test the result of negative diffPosMaxThresholdMaxDiff
test.similarity_negative_diffPosMaxThresholdMaxDiff <- function() {
    exp <- "The 'diffPosMaxThresholdMaxDiff' must be a positive numeric value."
    obs <- tryCatch(similarity(profile1 = c(1,59,6,24,65,34,15,4,53,22),
                               profile2 = c(15,9,46,44,9,39,27,34,34,4),
                               diffPosMaxThresholdMaxDiff= -4), 
                    error = conditionMessage)
    message <- paste0("test.similarity_negative_diffPosMaxThresholdMaxDiff() ",
                      "- A diffPosMaxThresholdMaxDiff with negative value ",
                      "did not generated expected error.")
    checkEquals(obs, exp, msg = message)
}

## Test the result of non numeric diffPosMaxThresholdMaxDiff
test.similarity_non_numeric_diffPosMaxThresholdMaxDiff <- function() {
    obs <- tryCatch(similarity(
        profile1 = c(1,59,6,24,65,34,15,4,53,22), 
        profile2 = c(15,9,46,44,9,39,27,34,34,4), 
        diffPosMaxThresholdMaxDiff="g"), error = conditionMessage)
    exp <- "The 'diffPosMaxThresholdMaxDiff' must be a positive numeric value."
    checkIdentical(obs, exp, 
            msg=paste("similarity_non_numeric_diffPosMaxThresholdMaxDiff() ",
                    "- A non numeric diffPosMaxThresholdMaxDiff did not ", 
                    "generate an exception with expected message.", sep = ""))
}

## Test the result of negative diffPosMaxThresholdMaxDiff
test.similarity_negative_diffPosMaxThresholdMaxDiff <- function() {
    obs <- tryCatch(similarity(
        profile1 = c(1,59,6,24,65,34,15,4,53,22), 
        profile2 = c(15,9,46,44,9,39,27,34,34,4), 
        diffPosMaxThresholdMaxDiff = -0.2), 
        error = conditionMessage)
    exp <- "The 'diffPosMaxThresholdMaxDiff' must be a positive numeric value."
    checkIdentical(obs, 
                    exp, 
                    msg=paste0("similarity_negative_diffPosMaxThresholdMax",
                            "Diff() - A negative diffPosMaxThresholdMaxDiff ",
                            "did not generate an exception with expected ",
                            "message."))
}

## Test the result of non numeric diffPosMaxTolerance
test.similarity_non_numeric_diffPosMaxThresholdMaxDiff <- function() {
    obs <- tryCatch(similarity(
        profile1 = c(1,59,6,24,65,34,15,4,53,22), 
        profile2 = c(15,9,46,44,9,39,27,34,34,4), 
        diffPosMaxTolerance = "g"), error = conditionMessage)
    exp <- paste("The 'diffPosMaxTolerance' must be a positive numeric ",
                    "value between 0 and 1 included.", sep = "")
    checkIdentical(obs, exp, 
                    msg=paste("similarity_non_numeric_",
                    "diffPosMaxThresholdMaxDiff() - A non numerical ",
                    "diffPosMaxThresholdMaxDiff did not generate an ",
                    "exception with expected message.", sep = ""))
}

## Test the result of negative diffPosMaxTolerance
test.similarity_negative_diffPosMaxTolerance <- function() {
    obs <- tryCatch(similarity(
        profile1 = c(1,59,6,24,65,34,15,4,53,22), 
        profile2 = c(15,9,46,44,9,39,27,34,34,4), 
        diffPosMaxTolerance = -0.5), error = conditionMessage)
    exp <- paste("The 'diffPosMaxTolerance' must be a positive numeric ", 
                 "value between 0 and 1 included.", sep = "")
    checkIdentical(obs, exp, 
                    msg = paste("similarity_negative_diffPosMaxTolerance() - ",
                            "A negative diffPosMaxTolerance did not generate ",
                            "an exception with expected message.", sep = ""))
}

## Test the result of non numeric diffPosMaxTolerance
test.similarity_non_numeric_diffPosMaxTolerance <- function() {
    obs <- tryCatch(similarity(
        profile1=c(1,59,6,24,65,34,15,4,53,22), 
        profile2=c(15,9,46,44,9,39,27,34,34,4), 
        diffPosMaxTolerance="g"), 
        error=conditionMessage)
    exp <- paste("The 'diffPosMaxTolerance' must be a positive numeric ",
                    "value between 0 and 1 included.", sep = "")
    checkIdentical(obs, exp, 
                    msg=paste("similarity_non_numeric_diffPosMaxTolerance() - ",
                        "A non numeric diffPosMaxTolerance did not generate ",
                        "an,exception with expected message.", sep = ""))
}

## Test the result of value superior to 1 for diffPosMaxTolerance
test.similarity_superior_1_diffPosMaxTolerance <- function() {
    obs <- tryCatch(similarity(
        profile1=c(1,59,6,24,65,34,15,4,53,22), 
        profile2=c(15,9,46,44,9,39,27,34,34,4), 
        diffPosMaxTolerance=1.01), error = conditionMessage)
    exp <- paste("The 'diffPosMaxTolerance' must be a positive numeric value ",
                    "between 0 and 1 included.", sep = "")
    checkIdentical(obs, 
                    exp, 
                    msg = paste("similarity_superior_1_diffPosMaxTolerance() - ",
                            "A diffPosMaxTolerance superior to 1 did not ",
                            "generate an exception with expected message.",
                            sep = ""))
}

## Test the result of zero numeric diffPosMaxThresholdMaxDiff
test.similarity_zero_diffPosMaxThresholdMaxDiff <- function() {
    obs <- tryCatch(similarity(
        profile1=c(1,59,6,24,65,34,15,4,53,22), 
        profile2=c(15,9,46,44,9,39,27,34,34,4), 
        diffPosMaxThresholdMaxDiff=0), error=conditionMessage)
    exp <- "The 'diffPosMaxThresholdMaxDiff' must be a positive numeric value."
    checkEquals(obs, 
                exp, 
                msg=paste("similarity_zero_diffPosMaxThresholdMaxDiff() - ",
                        "A zero value diffPosMaxThresholdMaxDiff dit not ",
                        "generate the expected exception.", sep = ""))
}

## Test the result of zero numeric ratioAreaThreshold
test.similarity_zero_ratioAreaThreshold <- function() {
    obs <- tryCatch(similarity(
        profile1=c(1,59,6,24,65,34,53,4,53,22), 
        profile2=c(15,9,46,44,9,39,27,34,34,4), 
        ratioAreaThreshold = 0), error=conditionMessage)
    exp <- "The 'ratioAreaThreshold' must be a positive numeric value."
    checkEquals(obs, 
                exp, 
                msg=paste("similarity_zero_ratioAreaThreshold() - A zero ",
                        "value ratioAreaThreshold dit not generate the ",
                        "expected exception.", sep = ""))
}

## Test the result of zero numeric ratioNormalizedIntersectThreshold 
test.similarity_zero_ratioNormalizedIntersectThreshold <- function() {
    obs <- tryCatch(similarity(
                profile1=c(1,59,6,24,65,34,53,4,53,22), 
                profile2=c(15,9,46,44,9,39,27,34,34,4), 
                ratioNormalizedIntersectThreshold  = 0), 
                error=conditionMessage)
    exp <- paste0("The 'ratioNormalizedIntersectThreshold' must be ", 
                    "a positive numeric value.")
    checkEquals(obs, 
                exp, 
                msg=paste("similarity_zero_ratioNormalizedIntersectThreshold()",
                    " - A zero value ratioAreaThreshold dit not generate the ",
                    "expected exception.", sep = ""))
}

## Test the result of non numeric ratioNormalizedIntersectThreshold
test.similarity_non_numeric_ratioNormalizedIntersectThreshold <- function() {
    obs <- tryCatch(similarity(
        profile1 = c(1,59,6,24,65,34,15,4,53,22), 
        profile2 = c(15,9,46,44,9,39,27,34,34,4), 
        ratioNormalizedIntersectThreshold = "g"), error = conditionMessage)
    exp <- paste0("The 'ratioNormalizedIntersectThreshold' must be ", 
                  "a positive numeric value.")
    checkIdentical(obs, exp, 
                   msg=paste("similarity_non_numeric_",
                             "ratioNormalizedIntersectThreshold() - A non numerical ",
                             "ratioNormalizedIntersectThreshold did not generate an ",
                             "exception with expected message.", sep = ""))
}

## Test the result of zero numeric spearmanCorrSDThreashold 
test.similarity_zero_spearmanCorrSDThreashold <- function() {
    obs <- tryCatch(similarity(
        profile1=c(1,59,6,24,65,34,53,4,53,22), 
        profile2=c(15,9,46,44,9,39,27,34,34,4), 
        spearmanCorrSDThreashold  = 0), 
        error=conditionMessage)
    exp <- paste0("The 'spearmanCorrSDThreashold' must be ", 
                  "a positive numeric value.")
    checkEquals(obs, 
                exp, 
                msg=paste("similarity_zero_spearmanCorrSDThreashold()",
                          " - A zero value ratioAreaThreshold dit not generate the ",
                          "expected exception.", sep = ""))
}

## Test the result of non numeric spearmanCorrSDThreashold
test.similarity_non_numeric_spearmanCorrSDThreashold <- function() {
    obs <- tryCatch(similarity(
        profile1 = c(1,59,6,24,65,34,15,4,53,22), 
        profile2 = c(15,9,46,44,9,39,27,34,34,4), 
        spearmanCorrSDThreashold = "g"), error = conditionMessage)
    exp <- paste0("The 'spearmanCorrSDThreashold' must be ", 
                  "a positive numeric value.")
    checkIdentical(obs, exp, 
                   msg=paste("similarity_non_numeric_",
                             "spearmanCorrSDThreashold() - A non numerical ",
                             "spearmanCorrSDThreashold did not generate an ",
                             "exception with expected message.", sep = ""))
}

########################################################################
# Test results for specifics metrics
########################################################################

## Test the result of zeros vector profile1 for the RATIO_MAX_MAX value
test.similarity_profile1_zeros_RATIO_MAX_MAX <- function() {
    obs <- similarity(
        profile1 = c(0,0,0,0,0,0,0,0,0,0), 
        profile2 = c(15,9,46,44,9,39,27,34,34,4))
    exp <- as.numeric(NA)
    checkEquals(obs$metrics$RATIO_MAX_MAX, exp , 
                msg = paste("A profile with only zero values did not generate",
                        " NA for the RATIO_MAX_MAX", sep = ""))
}

## Test the result of zeros vector profile1 for the RATIO_AREA value
test.similarity_profile1_zeros_RATIO_AREA <- function() {
    obs <- similarity(
        profile1 = c(0,0,0,0,0,0,0,0,0,0), 
        profile2 = c(15,9,46,44,9,39,27,34,34,4))
    exp <- as.numeric(NA)
    checkEquals(obs$metrics$RATIO_AREA, exp, 
                msg = paste0("A profile with only zero values did not generate ",
                            "NA for the RATIO_MAX_MAX"))
}

## Test the result of zeros vector profile1 for the RATIO_INTERSECT value
test.similarity_profile1_zeros_RATIO_INTERSECT <- function() {
    obs <- similarity(
        profile1 = c(0,0,0,0,0,0,0,0,0,0), 
        profile2 = c(15,9,46,44,9,39,27,34,34,4))
    exp <- 0
    checkEquals(obs$metrics$RATIO_INTERSECT, exp, 
            msg = paste0("A profile with only zero values did not generate ",
                            "0 for the RATIO_MAX_MAX"))
}

## Test the result of zeros vector profile1 and profile2 for 
## the DIFF_POS_MAX value
test.similarity_profile1_zeros_DIFF_POS_MAX <- function() {
    obs <- similarity(
        profile1 = c(0,0,0,0,0,0,0,0,0,0), 
        profile2 = c(0,0,0,0,0,0,0,0,0,0))
    exp <- as.numeric(NA)
    checkEquals(obs$metrics$DIFF_POS_MAX, exp, 
                msg = paste0("A profile with only zero values did not ",
                        "generate 0 for the RATIO_MAX_MAX"))
}

## Test the result of zeros vector profile1 and profile2 
## for the RATIO_INTERSECT value
test.similarity_profile1_and_profile2_zeros_RATIO_INTERSECT <- function() {
    obs <- similarity(
        profile1 = c(0,0,0,0,0,0,0,0,0,0), profile2 = c(0,0,0,0,0,0,0,0,0,0))
    exp <- as.numeric(NA)
    checkEquals(obs$metrics$RATIO_INTERSECT, exp, 
                msg = paste0("Two profiles with only zero values did not ",
                        "generate NA for the RATIO_MAX_MAX"))
}

## Test the result of zero numeric diffPosMaxTolerance
test.similarity_zero_diffPosMaxTolerance <- function() {
    obs <- similarity(
        c(1,59,6,24,65,34,15,4,53,22), 
        c(15,99,46,44,5,39,27,34,34,4), 
        diffPosMaxTolerance = 0)$metrics$DIFF_POS_MAX
    exp <- 3
    checkEquals(obs, exp, 
                tolerance = .Machine$double.eps^0.5, 
                msg = paste0("similarity_zero_diffPosMaxTolerance() - A zero ",
                        "value diffPosMaxTolerance dit not generate the ",
                        "expected DIFF_POS_MAX value"))
}


########################################################################
# Test expected results for specifics metrics
########################################################################

## Test the metadata results
test.similarity_metadata <- function() {
    exp <- list("nbrPosition" = 10, "areaProfile1" = 283, "areaProfile2" = 261,
                    "maxProfile1" = 65, "maxProfile2" = 46, 
                    "maxPositionProfile1" = 5, "maxPositionProfile2" = 3)
    obs <- similarity(c(1,59,6,24,65,34,15,4,53,22), 
                                        c(15,9,46,44,9,39,27,34,34,4))[1:7]
    message <- paste0("test.similarity_metadata() - ",
                        "profiles with only positives values did not ",
                        "generated all expected values")
    checkEquals(obs, exp, tolerance = .Machine$double.eps^0.5, msg = message)
}

## Test the metadata results with some NA in profiles
test.similarity_na_profile_metadata <- function() {
    exp <- list("nbrPosition" = 10,"areaProfile1" = 223,"areaProfile2" = 212,
                "maxProfile1" = 65,"maxProfile2" = 46,"maxPositionProfile1" = 5,
                "maxPositionProfile2" = 3)
    obs <- similarity(c(NA,NA,6,24,65,34,15,4,53,22), 
                                    c(NA,9,46,44,9,39,27,NA,34,4))[1:7]
    message <- paste0("test.similarity_na_profile_metadata() - ",
                        "profiles with some NA did not generated all ",
                        "expected values")
    checkEquals(obs, exp, tolerance = .Machine$double.eps^0.5, msg = message)
}

## Test the result of metric RATIO_AREA
test.similarity_ratio_area <- function() {
    exp <- 1.08429118773946
    obs <- similarity(c(1,59,6,24,65,34,15,4,53,22), 
                            c(15,9,46,44,9,39,27,34,34,4))$metrics$RATIO_AREA
    message <- paste0("test.similarity_ratio_area() -",
                      " metric RATIO_AREA did not ", 
                      "generated the expected value")
    checkEquals(obs, exp, tolerance = .Machine$double.eps^0.5)
}

## Test the result of metric DIFF_POS_MAX
test.similarity_diff_pos_max <- function() {
    exp <- 2
    obs <- similarity(c(1,59,6,24,65,34,15,4,53,22), 
                            c(15,9,46,44,9,39,27,34,34,4))$metrics$DIFF_POS_MAX
    message <- paste0("test.similarity_diff_pos_max() -",
                        " metric DIFF_POS_MAX did not ", 
                        "generated the expected value")
    checkEquals(obs, exp, tolerance = .Machine$double.eps^0.5, msg = message)
}

## Test the result of metric DIFF_POS_MAX with more than 1 position as maximum
test.similarity_diff_pos_max_with_more_than_one_maxium_position <- function() {
    obs <- 2
    exp <- similarity(c(1,59,59,65,65,65,15,4,53,53,53,22), 
                      c(15,9,46,44,9,39,27,34,34,3,3,4))$metrics$DIFF_POS_MAX
    message <- paste0("test.similarity_diff_pos_max_with_more_than_one_maxium_position() -",
                      " metric DIFF_POS_MAX with profile with more than 1 maximum",
                      " position did not generated the expected value")
    checkEquals(obs, exp, tolerance = .Machine$double.eps^0.5, msg = message)
}

## Test the result of metric DIFF_POS_MAX with more than 1 position as maximum and small diffPosMaxThresholdMaxDiff
test.similarity_diff_pos_max_with_more_than_one_maxium_position_and_small_diffPosMaxThresholdMaxDiff <- function() {
    obs <- similarity(c(1,59,6,24,65,34,15,4,53,51,65,22), 
                      c(15,9,46,44,9,39,27,34,34,3,3,4), diffPosMaxThresholdMaxDiff=3)$metrics$DIFF_POS_MAX
    message <- paste0("test.similarity_diff_pos_max_with_more_than_one_maxium_position() -",
                      " metric DIFF_POS_MAX with profile with more than 1 maximum",
                      " position and small diffPosMaxThresholdMaxDiff value did not ", 
                      "generated the expected value")
    checkTrue(is.na(obs), msg = message)
}

## Test the result of metric RATIO_MAX_MAX
test.similarity_ratio_max_max <- function() {
    exp <- 1.41304347826
    obs <- similarity(c(1,59,6,24,65,34,15,4,53,22), 
                        c(15,9,46,44,9,39,27,34,34,4))$metrics$RATIO_MAX_MAX
    message <- paste0("test.similarity_ratio_max_max() -",
                      " metric RATIO_MAX_MAX did not ", 
                      "generated the expected value")
    checkEquals(obs, exp, tolerance = .Machine$double.eps^0.5, msg = message)
}

## Test the result of metric RATIO_INTERSECT
test.similarity_ratio_intersect <- function() {
    exp <- 0.346534653465
    obs <- similarity(c(1,59,6,24,65,34,15,4,53,22), 
                        c(15,9,46,44,9,39,27,34,34,4))$metrics$RATIO_INTERSECT
    message <- paste0("test.similarity_ratio_intersect() -",
                      " metric RATIO_INTERSECT did not ", 
                      "generated the expected value")
    checkEquals(obs, exp, tolerance = .Machine$double.eps^0.5, msg = message)
}

## Test the result of metric RATIO_NORMALIZED_INTERSECT
test.similarity_ratio_intersect <- function() {
    exp <- 0.343525474289
    obs <- similarity(c(1,59,6,24,65,34,15,4,53,22), 
                      c(15,9,46,44,9,39,27,34,34,4))$metrics$RATIO_NORMALIZED_INTERSECT
    message <- paste0("test.similarity_ratio_intersect() -",
                      " metric RATIO_NORMALIZED_INTERSECT did not ", 
                      "generated the expected value")
    checkEquals(obs, exp, tolerance = .Machine$double.eps^0.5, msg = message)
}


