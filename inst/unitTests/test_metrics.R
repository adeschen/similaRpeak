###################################################
## Test the MetricFactory R6 classes
###################################################

factory = ChIPprofileSimilarity:::MetricFactory$new()

## Test the result of using "ALL"
test.metrics_metricfactory_all<- function() {
    exp <- list("RATIO_AREA"=1.084291188,"DIFF_POS_MAX"=2,
                "RATIO_MAX_MAX"=1.413043478,"RATIO_INTERSECT"=0.346534653)
    obs <- factory$createMetric("ALL", c(1,59,6,24,65,34,15,4,53,22), 
                                c(15,9,46,44,9,39,27,34,34,4))
    checkEquals(obs, exp, tolerance = .Machine$double.eps^0.5)
}

## Test the result of using "RATIO_AREA"
test.metrics_metricfactory_ratio_area<- function() {
    exp <- list("RATIO_AREA"=1.084291188)
    obs <- factory$createMetric("RATIO_AREA", c(1,59,6,24,65,34,15,4,53,22), 
                                c(15,9,46,44,9,39,27,34,34,4))
    checkEquals(obs, exp, tolerance = .Machine$double.eps^0.5)
}

## Test the result of using "DIFF_POS_MAX"
test.metrics_metricfactory_diff_pos_max<- function() {
    exp <- list("DIFF_POS_MAX"=2)
    obs <- factory$createMetric("DIFF_POS_MAX", c(1,59,6,24,65,34,15,4,53,22), 
                                c(15,9,46,44,9,39,27,34,34,4))
    checkEquals(obs, exp, tolerance = .Machine$double.eps^0.5)
}

## Test the result of using "RATIO_MAX_MAX"
test.metrics_metricfactory_ratio_max_max<- function() {
    exp <- list("RATIO_MAX_MAX"=1.413043478)
    obs <- factory$createMetric("RATIO_MAX_MAX", c(1,59,6,24,65,34,15,4,53,22), 
                                c(15,9,46,44,9,39,27,34,34,4))
    checkEquals(obs, exp, tolerance = .Machine$double.eps^0.5)
}

## Test the result of using "RATIO_INTERSECT"
test.metrics_metricfactory_ratio_intersect<- function() {
    exp <- list("RATIO_INTERSECT"=0.346534653)
    obs <- factory$createMetric("RATIO_INTERSECT", 
                                c(1,59,6,24,65,34,15,4,53,22), 
                                c(15,9,46,44,9,39,27,34,34,4))
    checkEquals(obs, exp, tolerance = .Machine$double.eps^0.5)
}

## Test the result of using "ALL" and NA or zeros
test.metrics_metricfactory_all_na_only<- function() {
    
    checkException(factory$createMetric("ALL", c(NA,NA,NA,NA), c(NA,NA,NA,NA)),
                   msg ="The 'profile1' argument must be a numeric vector.")
}

## Test the result of DIFF_POS_MAX when using profiles with only zero values
test.metrics_metricfactory_all_0_diff_pos_max<- function() {
    exp <- list("RATIO_AREA"=NA,"DIFF_POS_MAX"=NA,"RATIO_MAX_MAX"=NA,
                "RATIO_INTERSECT"=NA)
    obs <- factory$createMetric("ALL", c(0,0,0,0), c(0,0,0,0))
    checkEquals(obs$DIFF_POS_MAX, exp$DIFF_POS_MAX, 
                msg=paste("The DIFF_POS_MAX is not expected value when only ", 
                            "zero in profiles."), sep="")
}

## Test the result of RATIO_MAX_MAX when using profiles with only zero values
test.metrics_metricfactory_all_0_ratio_max_max<- function() {
    exp <- list("RATIO_AREA"=NA,"DIFF_POS_MAX"=NA,"RATIO_MAX_MAX"=NA,
                "RATIO_INTERSECT"=NA)
    obs <- factory$createMetric("ALL", c(0,0,0,0), c(0,0,0,0))
    checkEquals(obs$RATIO_MAX_MAX, exp$RATIO_MAX_MAX, 
                msg=paste("The RATIO_MAX_MAX is not expected value when only ",
                            "zero in profiles.", sep=""))
}

## Test the result of RATIO_INTERSECT when using profiles with only zero values
test.metrics_metricfactory_all_0_ratio_intersect<- function() {
    exp <- list("RATIO_AREA"=NA,"DIFF_POS_MAX"=NA,"RATIO_MAX_MAX"=NA,
                "RATIO_INTERSECT"=NA)
    obs <- factory$createMetric("ALL", c(0,0,0,0), c(0,0,0,0))
    checkEquals(obs$RATIO_INTERSECT, exp$RATIO_INTERSECT, 
                msg=paste("The RATIO_INTERSECT is not expected value when ",
                            "only zero in profiles.", sep=""))
}

## Test the result of RATIO_AREA when using a profiles with only zero values
test.metrics_metricfactory_all_0_ratio_area<- function() {
    obs <- list("RATIO_AREA"=NA,"DIFF_POS_MAX"=NA,"RATIO_MAX_MAX"=NA,
                "RATIO_INTERSECT"=NA)
    exp <- factory$createMetric("ALL", c(0,0,0,0), c(0,0,0,0))
    checkEquals(obs$RATIO_AREA, exp$RATIO_AREA, 
                msg=paste("The RATIO_AREA is not expected value when only ",
                            "zero in profiles.", sep=""))
}

## Test the result of using "ALL" with some NA in profiles
test.metrics_metricfactory_all_with_some_na<- function() {
    exp <- list("RATIO_AREA"=1.05188679245,"DIFF_POS_MAX"=2,
                "RATIO_MAX_MAX"=1.41304347826,"RATIO_INTERSECT"=0.40776699)
    obs <- factory$createMetric("ALL", c(NA,NA,6,24,65,34,15,4,53,22), 
                                c(NA,9,46,44,9,39,27,NA,34,4))
    checkEquals(obs, exp, tolerance = .Machine$double.eps^0.5)
}

## Test the result of using a wrong metricVector name
test.metrics_metricfactory_wrong_metric_name<- function() {
    obs <- tryCatch(factory$createMetric(metricType="TOTO", 
                                        c(1,59,6,24,65,34,15,4,53,22), 
                                        c(15,9,46,44,9,39,27,34,34,4)), 
                    error=conditionMessage)
    exp <- paste("The metricType must be one of those choices: ALL, ",
                "RATIO_AREA, DIFF_POS_MAX, RATIO_MAX_MAX, RATIO_INTERSECT"
                , sep="")
    checkEquals(obs, 
                exp, 
                msg=paste("metrics_metricfactory_wrong_metric_name() - ",
                            "A wrong metricType name did not generated the ",
                            "expected exception.", sep=""))
}

## Test the result of missing metricType
test.metrics_metricfactory_missing_metric_type<- function() {
    obs <- tryCatch(factory$createMetric(metricType=, 
                                        profile1=c(1,59,6,24,65,34,15,4,53,22), 
                                        profile2=c(15,9,46,44,9,39,27,34,34,4)), 
                    error=conditionMessage)
    exp <- "The 'metricType' argument is mandatory."
    checkEquals(obs, 
                exp, 
                msg = paste("metrics_metricfactory_missing_metric_type() - ",
                        "The missing 'metricType' argument did not generated ",
                        "the expected excpetion.", sep=""))
}

## Test the result of missing profile1
test.metrics_metricfactory_missing_profile1<- function() {
    obs <- tryCatch(factory$createMetric(metricType="ALL", 
                                    profile2=c(15,9,46,44,9,39,27,34,34,4)), 
                    error=conditionMessage)
    exp <- "The 'profile1' argument is mandatory."
    checkEquals(obs, 
                exp, 
                msg = paste("metrics_metricfactory_missing_profile1() ",
                        "- The missing 'profile1' argument did not ",
                        "generated the expected exception.", sep=""))
}

## Test the result of missing profile2
test.metrics_metricfactory_missing_profile2<- function() {
    obs <- tryCatch(factory$createMetric(metricType="ALL", 
                            profile1=c(15,9,46,44,9,39,27,34,34,4)), 
                            error=conditionMessage)
    exp <- "The 'profile2' argument is mandatory."
    checkEquals(obs, 
                exp, 
                msg =paste("metrics_metricfactory_missing_profile2() ",
                    "- The 'profile2' argument is mandatory.", sep=""))
}






