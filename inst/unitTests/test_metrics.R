###################################################
## Test the MetricFactory R6 classes
###################################################

factory = MetricFactory$new()

## Test the result of using "ALL"
test.metrics_metricfactory_all<- function() {
    obs <- list("RATIO_AREA"=1.084291188,"DIFF_POS_MAX"=2,"RATIO_MAX_MAX"=1.413043478,"RATIO_INTERSECT"=0.346534653)
    exp <- factory$createMetric("ALL", c(1,59,6,24,65,34,15,4,53,22), c(15,9,46,44,9,39,27,34,34,4))
    checkEquals(obs, exp, tolerance = .Machine$double.eps^0.5)
}

## Test the result of using "RATIO_AREA"
test.metrics_metricfactory_ratio_area<- function() {
    obs <- list("RATIO_AREA"=1.084291188)
    exp <- factory$createMetric("RATIO_AREA", c(1,59,6,24,65,34,15,4,53,22), c(15,9,46,44,9,39,27,34,34,4))
    checkEquals(obs, exp, tolerance = .Machine$double.eps^0.5)
}

## Test the result of using "DIFF_POS_MAX"
test.metrics_metricfactory_diff_pos_max<- function() {
    obs <- list("DIFF_POS_MAX"=2)
    exp <- factory$createMetric("DIFF_POS_MAX", c(1,59,6,24,65,34,15,4,53,22), c(15,9,46,44,9,39,27,34,34,4))
    checkEquals(obs, exp, tolerance = .Machine$double.eps^0.5)
}

## Test the result of using "RATIO_MAX_MAX"
test.metrics_metricfactory_ratio_max_max<- function() {
    obs <- list("RATIO_MAX_MAX"=1.413043478)
    exp <- factory$createMetric("RATIO_MAX_MAX", c(1,59,6,24,65,34,15,4,53,22), c(15,9,46,44,9,39,27,34,34,4))
    checkEquals(obs, exp, tolerance = .Machine$double.eps^0.5)
}

## Test the result of using "RATIO_INTERSECT"
test.metrics_metricfactory_ratio_intersect<- function() {
    obs <- list("RATIO_INTERSECT"=0.346534653)
    exp <- factory$createMetric("RATIO_INTERSECT", c(1,59,6,24,65,34,15,4,53,22), c(15,9,46,44,9,39,27,34,34,4))
    checkEquals(obs, exp, tolerance = .Machine$double.eps^0.5)
}

## Test the result of using "ALL" and NA or zeros
test.metrics_metricfactory_all_na_only<- function() {
    checkException(factory$createMetric("ALL", c(NA,NA,NA,NA), c(NA,NA,NA,NA)), msg ="The 'profile1' argument must be a numeric vector.")
}
test.metrics_metricfactory_all_0<- function() {
    obs <- list("RATIO_AREA"=NA,"DIFF_POS_MAX"=0,"RATIO_MAX_MAX"=NA,"RATIO_INTERSECT"=NA)
    exp <- factory$createMetric("ALL", c(0,0,0,0), c(0,0,0,0))
    checkEquals(obs, exp, tolerance = .Machine$double.eps^0.5)
}
test.metrics_metricfactory_all_na<- function() {
    obs <- list("RATIO_AREA"=1.051886792,"DIFF_POS_MAX"=2,"RATIO_MAX_MAX"=1.413043478,"RATIO_INTERSECT"=0.40776699)
    exp <- factory$createMetric("ALL", c(NA,NA,6,24,65,34,15,4,53,22), c(NA,9,46,44,9,39,27,NA,34,4))
    checkEquals(obs, exp, tolerance = .Machine$double.eps^0.5)
}


## Test the result of using a wrong metricVector name
test.metrics_metricfactory_wrong_metric_name<- function() {
    checkException(factory$createMetric("TOTO", c(1,59,6,24,65,34,15,4,53,22), c(15,9,46,44,9,39,27,34,34,4)), msg ="The 'profile1' argument must be a numeric vector.")
}



# essayer de faire une erreur dans le nom des fonctions a passer




















