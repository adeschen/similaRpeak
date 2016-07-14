###################################################
# Created by Elsa Bernatchez
# 2014-08-14

###################################################

###################################################
## Test the MetricFactory R6 classes
###################################################

### {{{ --- Test setup ---

if(FALSE) {
    library( "RUnit" )
    library( "similaRpeak" )
}

### }}}

###################################################
# Test Factory class
###################################################

factory <- MetricFactory$new()

## Test the result of using "ALL"
test.metrics_metricfactory_all <- function() {
    exp <- list("RATIO_AREA" = 1.084291188, "DIFF_POS_MAX" = 2,
                "RATIO_MAX_MAX" = 1.413043478, 
                "RATIO_INTERSECT" = 0.346534653,
                "RATIO_NORMALIZED_INTERSECT" = 0.343525474,
                "SPEARMAN_CORRELATION" = -0.256102322)
    obs <- factory$createMetric("ALL", c(1,59,6,24,65,34,15,4,53,22), 
                                c(15,9,46,44,9,39,27,34,34,4))
    checkEquals(obs, exp, tolerance = .Machine$double.eps^0.5)
}

###################################################
# Test good result for each metric
###################################################

## Test the result of using "RATIO_AREA"
test.metrics_metricfactory_ratio_area <- function() {
    exp <- list("RATIO_AREA" = 1.084291188)
    obs <- factory$createMetric("RATIO_AREA", c(1,59,6,24,65,34,15,4,53,22), 
                                c(15,9,46,44,9,39,27,34,34,4))
    checkEquals(obs, exp, tolerance = .Machine$double.eps^0.5)
}

## Test the result of using "DIFF_POS_MAX"
test.metrics_metricfactory_diff_pos_max <- function() {
    exp <- list("DIFF_POS_MAX" = 2)
    obs <- factory$createMetric("DIFF_POS_MAX", c(1,59,6,24,65,34,15,4,53,22), 
                                c(15,9,46,44,9,39,27,34,34,4))
    checkEquals(obs, exp, tolerance = .Machine$double.eps^0.5)
}

## Test the result of using "RATIO_MAX_MAX"
test.metrics_metricfactory_ratio_max_max <- function() {
    exp <- list("RATIO_MAX_MAX" = 1.413043478)
    obs <- factory$createMetric("RATIO_MAX_MAX", c(1,59,6,24,65,34,15,4,53,22), 
                                c(15,9,46,44,9,39,27,34,34,4))
    checkEquals(obs, exp, tolerance = .Machine$double.eps^0.5)
}

## Test the result of using "RATIO_INTERSECT"
test.metrics_metricfactory_ratio_intersect <- function() {
    exp <- list("RATIO_INTERSECT" = 0.346534653)
    obs <- factory$createMetric("RATIO_INTERSECT", 
                                c(1,59,6,24,65,34,15,4,53,22), 
                                c(15,9,46,44,9,39,27,34,34,4))
    checkEquals(obs, exp, tolerance = .Machine$double.eps^0.5)
}

## Test the result of using "RATIO_NORMALIZED_INTERSECT"
test.metrics_metricfactory_ratio_intersect <- function() {
    exp <- list("RATIO_NORMALIZED_INTERSECT" = 0.343525474289)
    obs <- factory$createMetric("RATIO_NORMALIZED_INTERSECT", 
                                c(1,59,6,24,65,34,15,4,53,22), 
                                c(15,9,46,44,9,39,27,34,34,4))
    checkEquals(obs, exp, tolerance = .Machine$double.eps^0.5)
}

#####################################################
# Test results for each metric when NA in profiles
#####################################################

## Test the result of using "ALL" and NA in first profile
test.metrics_metricfactory_all_na_only <- function() {
    checkException(factory$createMetric("ALL", c(NA,NA,NA,NA), c(3, 3, 3, 4)),
                    msg = "The 'profile1' argument must be a numeric vector.")
}

## Test the result of using "ALL" and NA in second profile
test.metrics_metricfactory_all_na_only <- function() {
    checkException(factory$createMetric("ALL", c(3, 3, 3, 3), c(NA,NA,NA,NA)),
                    msg = "The 'profile2' argument must be a numeric vector.")
}

## Test the result of DIFF_POS_MAX when using profiles with only zero values
test.metrics_metricfactory_all_0_diff_pos_max <- function() {
    naValue <- as.numeric(NA)
    exp <- list("RATIO_AREA" = naValue,"DIFF_POS_MAX" = naValue,
                "RATIO_MAX_MAX" = naValue, "RATIO_INTERSECT" = naValue, 
                "RATIO_NORMALIZED_INTERSECT" = naValue)
    obs <- factory$createMetric("ALL", c(0,0,0,0), c(0,0,0,0))
    checkEquals(obs$DIFF_POS_MAX, exp$DIFF_POS_MAX, 
                msg = paste("The DIFF_POS_MAX is not expected value when only ", 
                            "zero in profiles."), sep = "")
}

## Test the result of RATIO_MAX_MAX when using profiles with only zero values
test.metrics_metricfactory_all_0_ratio_max_max <- function() {
    naValue <- as.numeric(NA)
    exp <- list("RATIO_AREA"= naValue,"DIFF_POS_MAX"= naValue,
                "RATIO_MAX_MAX"= naValue, "RATIO_INTERSECT"= naValue, 
                "RATIO_NORMALIZED_INTERSECT" =  naValue)
    obs <- factory$createMetric("ALL", c(0,0,0,0), c(0,0,0,0))
    message <- paste0("The RATIO_MAX_MAX is not expected value when only ",
                        "zero in profiles.")
    checkEquals(obs$RATIO_MAX_MAX, exp$RATIO_MAX_MAX, msg = message)
}

## Test the result of RATIO_INTERSECT when using profiles with only zero values
test.metrics_metricfactory_all_0_ratio_intersect <- function() {
    naValue <- as.numeric(NA)
    exp <- list("RATIO_AREA"= naValue, "DIFF_POS_MAX"= naValue,
                "RATIO_MAX_MAX"= naValue, "RATIO_INTERSECT"= naValue, 
                "RATIO_NORMALIZED_INTERSECT" = naValue)
    obs <- factory$createMetric("ALL", c(0,0,0,0), c(0,0,0,0))
    checkEquals(obs$RATIO_INTERSECT, exp$RATIO_INTERSECT, 
                msg = paste("The RATIO_INTERSECT is not expected value when ",
                            "only zero in profiles.", sep=""))
}

## Test the result of RATIO_NORMALIZED_INTERSECT when using profiles with 
## only zero values
test.metrics_metricfactory_all_0_ratio_normalized_intersect <- function() {
    naValue <- as.numeric(NA)
    exp <- list("RATIO_AREA" = naValue, "DIFF_POS_MAX"= naValue,
                "RATIO_MAX_MAX" = naValue, "RATIO_INTERSECT"= naValue,
                "RATIO_NORMALIZED_INTERSECT" = naValue)
    obs <- factory$createMetric("ALL", c(0,0,0,0), c(0,0,0,0))
    checkEquals(obs$RATIO_NORMALIZED_INTERSECT, exp$RATIO_INTERSECT, 
                msg = paste("The RATIO_NORMALIZED_INTERSECT is not expected ",
                            "value when only zero in profiles.", sep=""))
}

## Test the result of RATIO_AREA when using a profiles with only zero values
test.metrics_metricfactory_all_0_ratio_area <- function() {
    naValue <- as.numeric(NA)
    obs <- list("RATIO_AREA"= naValue,"DIFF_POS_MAX"= naValue,
                "RATIO_MAX_MAX"= naValue, "RATIO_INTERSECT"= naValue, 
                "RATIO_NORMALIZED_INTERSECT" = naValue)
    exp <- factory$createMetric("ALL", c(0,0,0,0), c(0,0,0,0))
    checkEquals(obs$RATIO_AREA, exp$RATIO_AREA, 
                msg = paste("The RATIO_AREA is not expected value when only ",
                            "zero in profiles.", sep=""))
}

## Test the result of using "ALL" with some NA in profiles
test.metrics_metricfactory_all_with_some_na <- function() {
    exp <- list("RATIO_AREA"=1.05188679245,"DIFF_POS_MAX"=2,
                "RATIO_MAX_MAX"=1.41304347826,"RATIO_INTERSECT"=0.40776699,
                "RATIO_NORMALIZED_INTERSECT" = 0.40445316,
                "SPEARMAN_CORRELATION" = -0.32142857)
    obs <- factory$createMetric("ALL", c(NA,NA,6,24,65,34,15,4,53,22), 
                                c(NA,9,46,44,9,39,27,NA,34,4))
    checkEquals(obs, exp, tolerance = .Machine$double.eps^0.5)
}

## Test the result of using "ALL" with no complete element pairs in profiles
test.metrics_metricfactory_all_with_no_complete_element_pairs <- function() {
    exp <- list("RATIO_AREA"=1.69565217391,"DIFF_POS_MAX"=2,
                "RATIO_MAX_MAX"=1.41304347826,"RATIO_INTERSECT"=0.0839160839161,
                "RATIO_NORMALIZED_INTERSECT" = 0.0655737704918,
                "SPEARMAN_CORRELATION" = as.numeric(NA))
    obs <- factory$createMetric("ALL", c(NA,NA,NA,24,65,34,15,4,53,NA), 
                                c(12,9,46,44,NA,NA,NA,NA,NA,4))
    checkEquals(obs, exp, tolerance = .Machine$double.eps^0.5)
}

## Test the result of using a wrong metricVector name
test.metrics_metricfactory_wrong_metric_name <- function() {
    obs <- tryCatch(factory$createMetric(metricType="TOTO", 
                                        c(1,59,6,24,65,34,15,4,53,22), 
                                        c(15,9,46,44,9,39,27,34,34,4)), 
                    error=conditionMessage)
    exp <- paste("The metricType must be one of those choices: ALL, ",
                "RATIO_AREA, DIFF_POS_MAX, RATIO_MAX_MAX, RATIO_INTERSECT, ", 
                "RATIO_NORMALIZED_INTERSECT, SPEARMAN_CORRELATION", sep="")
    checkEquals(obs, exp, 
                msg = paste("metrics_metricfactory_wrong_metric_name() - ",
                            "A wrong metricType name did not generated the ",
                            "expected exception.", sep=""))
}

## Test the result of missing metricType
test.metrics_metricfactory_missing_metric_type <- function() {
    obs <- tryCatch(factory$createMetric(metricType=, 
                                        profile1=c(1,59,6,24,65,34,15,4,53,22), 
                                        profile2=c(15,9,46,44,9,39,27,34,34,4)), 
                    error=conditionMessage)
    exp <- "The 'metricType' argument is mandatory."
    checkEquals(obs, exp, 
                msg = paste0("metrics_metricfactory_missing_metric_type() - ",
                        "The missing 'metricType' argument did not generated ",
                        "the expected excpetion."))
}

####################################################################
## Test result for each metric when missing profile
####################################################################

## Test the result of missing profile1
test.metrics_metricfactory_ALL_missing_profile1 <- function() {
    obs <- tryCatch(factory$createMetric(metricType="ALL", 
                        profile2=c(15,9,46,44,9,39,27,34,34,4)), 
                        error=conditionMessage)
    exp <- "The 'profile1' argument is mandatory."
    message <- paste0("metrics_metricfactory_missing_profile1() ",
                        "- The missing 'profile1' argument did not ",
                        "generated the expected exception.")
    checkEquals(obs, exp, msg = message)
}

## Test the result of missing profile2
test.metrics_metricfactory_ALL_missing_profile2 <- function() {
    obs <- tryCatch(factory$createMetric(metricType="ALL", 
                            profile1=c(15,9,46,44,9,39,27,34,34,4)), 
                            error=conditionMessage)
    exp <- "The 'profile2' argument is mandatory."
    message <- paste0("metrics_metricfactory_missing_profile2() ",
                      "- The 'profile2' argument is mandatory.")
    checkEquals(obs, exp, msg = message)
}

## Test the result of missing profile1 for RATIO_AREA
test.metrics_metricfactory_RATIO_AREA_missing_profile1 <- function() {
    obs <- tryCatch(factory$createMetric(metricType="RATIO_AREA", 
                                         profile2=c(15,9,46,44,9,39,27,34,34,4)), 
                    error=conditionMessage)
    exp <- "The 'profile1' argument is mandatory."
    message <- paste0("metrics_metricfactory_RATIO_AREA_missing_profile1() ",
                      "- The missing 'profile1' argument did not ",
                      "generated the expected exception.")
    checkEquals(obs, exp, msg = message)
}

## Test the result of missing profile2 for RATIO_AREA
test.metrics_metricfactory_RATIO_AREA_missing_profile2 <- function() {
    obs <- tryCatch(factory$createMetric(metricType="RATIO_AREA", 
                                         profile1=c(15,9,46,44,9,39,27,34,34,4)), 
                    error=conditionMessage)
    exp <- "The 'profile2' argument is mandatory."
    message <- paste0("metrics_metricfactory_RATIO_AREA_missing_profile2() ",
                      "- The 'profile2' argument is mandatory.")
    checkEquals(obs, exp, msg = message)
}

## Test the result of RATIO_MAX_MAX when using profile 1 with only NA values
test.metrics_metricfactory_all_RATIO_MAX_MAX_profile_1_ratio_max_max <- function() {
    obs <- tryCatch(factory$createMetric(metricType="RATIO_MAX_MAX", 
                                         profile1=c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),
                                         profile2=c(15,9,46,44,9,39,27,34,34,4)), 
                    error=conditionMessage)
    exp <- "The 'profile1' argument must be a numeric vector."
    message <- paste0("test.metrics_metricfactory_all_RATIO_MAX_MAX_profile_1_ratio_max_max() ",
                      "- The 'profile2' with NA did not raise the exepected error.")
    checkEquals(obs, exp, msg = message)
}

## Test the result of RATIO_MAX_MAX when using profile 2 with only NA values
test.metrics_metricfactory_all_RATIO_MAX_MAX_profile_2_ratio_max_max <- function() {
    obs <- tryCatch(factory$createMetric(metricType="RATIO_MAX_MAX", 
                                         profile1=c(15,9,46,44,9,39,27,34,34,4), 
                                         profile2=c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)),
                    error=conditionMessage)
    exp <- "The 'profile2' argument must be a numeric vector."
    message <- paste0("test.metrics_metricfactory_all_RATIO_MAX_MAX_profile_2_ratio_max_max() ",
                      "- The 'profile2' with NA did not raise the exepected error.")
    checkEquals(obs, exp, msg = message)
}


###########################################################################
## Test result for each metric when profile 1 and 2 having different length
###########################################################################

## Test the result of RATIO_MAX_MAX when using profile 1 and 2 with different length
test.metrics_metricfactory_all_RATIO_MAX_MAX_profile_1_and_2_diff_length <- function() {
    obs <- tryCatch(factory$createMetric(metricType="RATIO_MAX_MAX", 
                    profile1=c(15,9,46,44,9,39,27,34,34,4,2), 
                    profile2=c(44,9,39,27,34,34,4,2)),
                    error=conditionMessage)
    exp <- "Lengths of 'profile1' and 'profile2' vectors aren't equals."
    message <- paste0("test.metrics_metricfactory_all_RATIO_MAX_MAX_profile_1_and_2_diff_length() ",
                      "- The 'profile1' and 'profile2' having different length did not raise the exepected error.")
    checkEquals(obs, exp, msg = message)
}

###############################################
# Test Metric class
###############################################

## Test the creation of a metric object
test.metric_class <- function() {
    obs <- similaRpeak:::Metric$new()

    exp <- "The 'profile2' argument is mandatory."
    message <- paste0("test.metric_class() ",
                      "- The metric object does not correspond to the expected object.")

    checkEquals(obs$getType(), NA, msg = message)
    checkEquals(obs$calculateMetric(), NULL, msg = message)
    checkEquals(obs$getInfo(), NULL, msg = message)
}


