# Created by Astrid Deschenes
# 2014-08-13

# Class representing a abstract Metric which is used to quantify the similarity 
# between two ChIP profiles covering the same range.
#
Metric <- R6Class("Metric",
    public = list(
        initialize = function() {  
        },
        getType = function() {
            return(private$type)
        },
        getMetric = function() {
            return(private$metric)
        },
        calculateMetric = function(profile1, profile2, threshold = NULL) {
        },
        getInfo = function() {
            cat(paste0("Metric type: ", private$type, ". Metric value: ", 
                    private$metric, "\n"))
        }
    ),
    private = list(
        metric = NA,
        type = NA,
        setMetric = function(val){
            private$metric <<- val
        },
        setType = function(val){
            private$type <<- val
        }
    )
)


#' @title RatioMaxMax class
#' 
#' @description An object which is a interface to calculate the ratio between 
#' the peaks values between two profiles.
#' 
#' The \code{RatioMaxMax} object is needed to 
#' calculate the ratio between 
#' the peaks values between two profiles.
#' A threshold and the two profiles are set during the \code{RatioMaxMax} 
#' object creation. If different threshold or 
#' profiles are needed, the \code{calculateMetric} function should be used, 
#' with the new profiles and threshold passed as arguments to update those
#' values inside the \code{RatioMaxMax} object.
#' 
#' @return The \code{RatioMaxMax$new} function returns a \code{RatioMaxMax} 
#' object which contains the information about the two profiles and the 
#' threshold used to calculate the metric. It can be used, as many times 
#' needed, to calculate the specified metric. 
#' 
#' @section Constructor:
#' Create a \code{RatioMaxMax} object.
#' 
#' @section Constructor:
#' Create a \code{RatioMaxMax} object.
#' 
#' \code{RatioMaxMax$new(profile1, profile2, threshold = 1)}
#'     
#' \itemize{
#' \item \code{getMetric} { A function that returns the value of the
#' calculated metric }
#' \item \code{getInfo} { A function that returns a description of the metric
#' with the metric value.}
#' \item \code{getType} { A function that returns the unique name associated
#' to this metric }
#' \item \code{calculateMetric} { A function that modifies the values of the
#' two profiles and the threshold. The new values (profile1, profile2, 
#' threshold) are passed as arguments.}
#' }
#' 
#' @seealso
#' \itemize{
#' \item \code{\link{MetricFactory}} {for using a interface to calculate all 
#' available metrics separately or togheter.}
#' }
#' 
#' @import R6
#' @author Astrid Deschenes, Elsa Bernatchez
RatioMaxMax <- R6Class("RatioMaxMax",
    inherit = Metric,
    public = list(
        initialize = function(profile1, profile2, threshold = 1) {

            # Fix the type of metric
            super$setType("RATIO_MAX_MAX")

            if (!missing(profile1) && !missing(profile2)) {
                self$calculateMetric(profile1, profile2, threshold)   
            }
        },
        calculateMetric = function(profile1, profile2, threshold = 1) {

            # Reset metric value to NA
            super$setMetric(as.numeric(NA))

            # Profile1 and profile2 are mandatory
            if (missing(profile1)) {
                stop(paste0("The 'profile1' argument is mandatory. ", 
                    "The metric value has been reset to NA."))
            }
            if (missing(profile2)) {
                stop(paste0("The 'profile2' argument is mandatory. ", 
                    "The metric value has been reset to NA."))
            }

            # The profile1 and profile2 arguments are 
            # numeric vectors where at least one element is greater than zero
            if (!is.vector(profile1) || !is.numeric(profile1)) {
                stop(paste0("The 'profile1' argument must be a numeric ",
                    "vector. The metric value has been reset to NA."))
            } 
            if (!is.vector(profile2) || !is.numeric(profile2)) {
                stop(paste0("The 'profile2' argument must be a numeric ",
                    "vector. The metric value has been reset to NA."))
            }

            # The length of profile1 is equal to the length of profile2
            if (length(profile1) != length(profile2)) {
                stop(paste0("Lengths of 'profile1' and 'profile2' vectors ", 
                    "aren't equals. The metric value has been reset to NA."))
            }

            # Calculate and assign the new max max ratio
            super$setMetric(ratioMaxMax(profile1, profile2, threshold))
        }
    )
)


# Class representing an Area Ratio metric which is the ratio of profiles
# total areas between two ChIP profiles covering the same range.
#
RatioArea <- R6Class("RatioArea",
    inherit = Metric,
    public = list(
        initialize = function(profile1, profile2, threshold = 1) {

            # Fix the type of metric
            super$setType("RATIO_AREA")
                               
            if (!missing(profile1) && !missing(profile2)) {
                self$calculateMetric(profile1, profile2, threshold)   
            }
        },
        calculateMetric = function(profile1, profile2, threshold = 1) {

            # Reset metric value to NA
            super$setMetric(as.numeric(NA))

            # Profile1 and profile2 are mandatory
            if (missing(profile1)) {
                stop(paste0("The 'profile1' argument is mandatory. The metric ",
                            "value has been reset to NA."))
            }
            if (missing(profile2)) {
                stop(paste0("The 'profile2' argument is mandatory. The metric ",
                            "value has been reset to NA."))
            }

            # The profile1 and profile2 arguments are numeric vectors where at 
            # least one element is greater than zero
            if (!is.vector(profile1) || !is.numeric(profile1)) {
                stop(paste0("The 'profile1' argument must be a numeric ", 
                            "vector. The metric value has been reset to NA."))
            }
            if (!is.vector(profile2) || !is.numeric(profile2)) {
                stop(paste0("The 'profile2' argument must be a numeric ", 
                            "vector. The metric value has been reset to NA."))
            }

            # The length of profile1 is equal to the length  of profile2
            if (length(profile1) != length(profile2)) {
                stop(paste0("Lengths of 'profile1' and 'profile2' vectors ", 
                            "aren't equals. The metric value has been reset ",
                            "to NA."))
            }

            # Calculate and assign the new max max ratio
            super$setMetric(ratioArea(profile1, profile2, threshold))
        }
    )
)  


# Class representing a Positions Difference metric which is the difference 
# of profiles maximal peaks positions 
# between two ChIP profiles covering the same range.
#
DiffPosMax <- R6Class("DiffPosMax",
    inherit = Metric,
    public = list(
        initialize = function(profile1, profile2, threshold = 1, 
                            thresholdDiff = 100, tolerance = 0.01) {

            # Fix the type of metric
            super$setType("DIFF_POS_MAX")

            if (!missing(profile1) && !missing(profile2)) {
                self$calculateMetric(profile1, profile2, threshold, 
                                        thresholdDiff, tolerance)   
            }
        },
        calculateMetric = function(profile1, profile2, threshold = 1, 
                            thresholdDiff = 100, tolerance = 0.01) {

            # Reset metric value to NA
            super$setMetric(NA)

            # Profile1 and profile2 are mandatory
            if (missing(profile1)) {
                stop(paste0("The 'profile1' argument is mandatory. ", 
                       "The metric value has been reset to NA."))
            }

            if (missing(profile2)) {
                stop(paste0("The 'profile2' argument is mandatory. ", 
                        "The metric value has been reset to NA."))
            }

            # The profile1 and profile2 arguments are numeric 
            # vectors where at least one element is greater than zero
            if (!is.vector(profile1) || !is.numeric(profile1)) {
                stop(paste0("The 'profile1' argument must be a numeric ",
                    "vector. The metric value has been reset to NA."))
            }

            if (!is.vector(profile2) || !is.numeric(profile2)) {
                stop(paste0("The 'profile2' argument must be a numeric ",
                    "vector. The metric value has been reset to NA."))
            }

            # The length of profile1 is equal to the length of profile2
            if (length(profile1) != length(profile2)) {
                stop(paste0("Lengths of 'profile1' and 'profile2' vectors ", 
                    "aren't equals. The metric value has been reset to NA."))
            }

            # Calculate and assign the new difference position maximum
            super$setMetric(diffPosMax(profile1, profile2, threshold, 
                            thresholdDiff, tolerance))
        }
    )
)


# Class representing an Intersect Ratio metric which is the ratio of profiles 
# intersection area between two ChIP profiles covering the same range and 
# those profiles total areas.
#
RatioIntersect <- R6Class("RatioIntersect",
    inherit = Metric,
    public = list(
        initialize = function(profile1,profile2, threshold = 1) {
                             
            # Fix the type of metric
            super$setType("RATIO_INTERSECT")

            if (!missing(profile1) && !missing(profile2)) {
                self$calculateMetric(profile1, profile2, threshold)   
            }
        },
        calculateMetric = function(profile1, profile2, threshold = 1) {

            # Reset metric value to NA
            super$setMetric(as.numeric(NA))

            # Profile1 and profile2 are mandatory
            if (missing(profile1)) {
                stop(paste0("The 'profile1' argument is mandatory. The ",
                    "metric value has been reset to NA."))
            }
            if (missing(profile2)) {
                stop(paste0("The 'profile2' argument is mandatory. The ", 
                    "metric value has been reset to NA."))
            }

            # The profile1 and profile2 arguments are numeric 
            # vectors where at least one element is greater than zero
            if (!is.vector(profile1) || !is.numeric(profile1)) {
                stop(paste0("The 'profile1' argument must be a numeric ",
                    "vector. The metric value has been reset to NA."))
            }
            if (!is.vector(profile2) || !is.numeric(profile2)) {
                stop(paste0("The 'profile2' argument must be a numeric ",
                    "vector. The metric value has been reset to NA."))
            }

            # The length of profile1 is equal to the length of profile2
            if (length(profile1) != length(profile2)) {
                stop(paste0("Lengths of 'profile1' and 'profile2' vectors ", 
                    "aren't equals. The metric value has been reset to NA."))
            }

            # Calculate and assign the new intersect ratio
            super$setMetric(ratioIntersect(profile1, profile2, threshold))
        }
    )
)


# Class representing a Normalized Intersect Ratio metric which is the ratio of 
# profiles intersection area between two normalized ChIP profiles covering 
# the same range and those profiles total areas. The ChIP profiles are 
# normalized by multiplying the profile values with the length 
# of the profile and dividind it by the area of the profile.
# normalized values = profile values * length(profile)/area(profile)
#
RatioNormalizedIntersect <- R6Class("RatioNormalizedIntersect",
    inherit = Metric,
    public = list(
        initialize = function(profile1, profile2, threshold = 1) {

            # Fix the type of metric
            super$setType("RATIO_NORMALIZED_INTERSECT")

            if (!missing(profile1) && !missing(profile2)) {
                    self$calculateMetric(profile1, profile2, threshold)
            }
        },
        calculateMetric = function(profile1, profile2, threshold = 1) {

            # Reset metric value to NA
            super$setMetric(as.numeric(NA))

            # Profile1 and profile2 are mandatory
            if (missing(profile1)) {
                stop(paste0("The 'profile1' argument is mandatory. The ",
                    "metric value has been reset to NA."))
            }
            if (missing(profile2)) {
                stop(paste0("The 'profile2' argument is mandatory. The ", 
                    "metric value has been reset to NA."))
            }

            # The profile1 and profile2 arguments are numeric 
            # vectors where at least one element is greater than zero
            if (!is.vector(profile1) || !is.numeric(profile1)) {
                stop(paste0("The 'profile1' argument must be a numeric ", 
                    "vector. The metric value has been reset to NA."))
            }
            if (!is.vector(profile2) || !is.numeric(profile2)) {
                stop(paste0("The 'profile2' argument must be a numeric ",
                    "vector. The metric value has been reset to NA."))
            }

            # The length of profile1 is equal to the length of profile2
            if (length(profile1) != length(profile2)) {
                stop(paste0("Lengths of 'profile1' and 'profile2' vectors ", 
                    "aren't equals. The metric value has been reset to NA."))
            }

            # Normalized profiles values
            normProfile1 <- profile1*(length(profile1)/sum(profile1, 
                                            na.rm=TRUE))
            normProfile2 <- profile2*(length(profile2)/sum(profile2, 
                                            na.rm=TRUE))

            # Calculate and assign the normalized intersect ratio
            super$setMetric(ratioIntersect(normProfile1, normProfile2, 
                                            threshold))
        }
    )
)

# Class representing a Spearman correlation metric which is the Spearman's  
# rank correlation coefficient of the two profiles.
#
SpearmanCorrelation <- R6Class("SpearmanCorrelation",
    inherit = Metric,
    public = list(
        initialize = function(profile1, profile2, threshold = NULL) {

            # Fix the type of metric
            super$setType("SPEARMAN_CORRELATION")

            if (!missing(profile1) && !missing(profile2)) {
                self$calculateMetric(profile1, profile2, threshold)   
            }
        },
        calculateMetric = function(profile1, profile2, threshold = 1e-8) {

            # Reset metric value to NA
            super$setMetric(as.numeric(NA))

            # Profile1 and profile2 are mandatory
            if (missing(profile1)) {
                stop(paste0("The 'profile1' argument is mandatory. ", 
                    "The metric value has been reset to NA."))
            }
            if (missing(profile2)) {
                stop(paste0("The 'profile2' argument is mandatory. The ",
                    "metric value has been reset to NA."))
            }

            # The profile1 and profile2 arguments are numeric 
            # vectors where at least one element is greater than zero
            if (!is.vector(profile1) || !is.numeric(profile1)) {
                stop(paste0("The 'profile1' argument must be a numeric ", 
                            "vector. The metric value has been reset to NA."))
            }
            if (!is.vector(profile2) || !is.numeric(profile2)) {
                stop(paste0("The 'profile2' argument must be a numeric ", 
                            "vector. The metric value has been reset to NA."))
            }

            # The length of profile1 is equal to the length of profile2
            if (length(profile1) != length(profile2)) {
                stop(paste0("Lengths of 'profile1' and 'profile2' vectors ", 
                    "aren't equals. The metric value has been reset to NA."))
            }

            # Calculate and assign the spearman correlation
            super$setMetric(spearmanCorr(profile1, profile2, threshold))
        }
    )
)

#' @title MetricFactory object
#' 
#' @description An object which is a interface to calculate all available 
#' metrics separately.
#' 
#' The \code{MetricFactory} object is inspired from the factory design 
#' pattern. Only one instance of \code{MetricFactory} object is necessary to 
#' calculate all available metrics for different profiles, as long as the 
#' thresholds set in the \code{MetricFactory} instance are appropriate for 
#' the calculation. The thresholds are set during the \code{MetricFactory} 
#' object creation and cannot be changed afterwards. If different thresholds 
#' are needed, a new \code{MetricFactory} object, with the new thresholds, 
#' must be instantiated.
#' 
#' @return The \code{MetricFactory$new} function returns a \code{MetricFactory} 
#' object which contains the information about the thresholds used to calculate 
#' each metric. It can be used, as many times needed, to calculate the 
#' specified metrics. 
#' 
#' @section Constructor:
#' Create a \code{MetricFactory} object.
#' 
#' \code{MetricFactory$new(ratioAreaThreshold=1, 
#'     ratioMaxMaxThreshold=1, 
#'     ratioIntersectThreshold=1,
#'     ratioNormalizedIntersectThreshold=1,
#'     diffPosMaxThresholdMinValue=1, 
#'     diffPosMaxThresholdMaxDiff=100, 
#'     diffPosMaxTolerance=0.01, 
#'     spearmanCorrSDThreashold=1e-8)}
#'     
#' \itemize{
#' \item \code{ratioAreaThreshold} { The minimum denominator accepted to 
#' calculate the ratio of the area between both profiles. Default = 1.}
#' \item \code{ratioMaxMaxThreshold} { The minimum denominator accepted to 
#' calculate the ratio of the maximum values between both profiles. Default = 1.}
#' \item \code{ratioIntersectThreshold} { The minimum denominator accepted to 
#' calculate the ratio of the intersection area of both profiles and the 
#' total area. Default = 1.}
#' \item \code{ratioIntersectThreshold} { The minimum denominator accepted to 
#' calculate the ratio of the intersection area of both profiles and the 
#' total area for normalized profiles. Default = 1.}
#' \item \code{diffPosMaxThresholdMinValue} { The minimum peak accepted to 
#' calculate the metric. Default = 1.}
#' \item \code{diffPosMaxThresholdMaxDiff} { The maximum distance accepted 
#' between 2 peaks positions in one profile to calculate the metric. 
#' Default=100.}
#' \item \code{diffPosMaxTolerance} {The maximum variation accepted on the 
#' maximum value to consider a position as a peak position. Default=0.01.}
#' \item\code{spearmanCorrSDThreashold} {The minimum standard deviation 
#' accepted on both profiles to consider to calculate the metric. Default=1e-8.}
#' }
#' 
#' @examples
#' 
#' ## Initialized the factory object
#' factory = MetricFactory$new(ratioAreaThreshold=100,
#'     ratioIntersectThreshold=20,
#'     diffPosMaxTolerance=0.04)
#'     
#' ## Define 2 ChIP-Seq profiles
#' profile1 <- c(1,59,6,24,65,34,15,4,53,22)
#' profile2 <- c(15,9,46,44,9,39,27,34,34,4)
#' 
#' ## Use the factory object to calculate each metric separatly
#' ratio_max_max <- factory$createMetric(metricType="RATIO_MAX_MAX", 
#'     profile1, profile2)
#' ratio_max_max
#' 
#' diff_pos_max <- factory$createMetric(metric="DIFF_POS_MAX", profile1, 
#'     profile2)
#' diff_pos_max
#'     
#' ## Example using ChIP-Seq profiles of H3K27ac (DCC accession: ENCFF000ASG) 
#' ## and H3K4me1 (DCC accession: ENCFF000ARY) from the Encyclopedia of DNA  
#' ## Elements (ENCODE) for the region 
#' data(demoProfiles)
#' 
#' ## Visualize ChIP-Seq profiles 
#' plot(demoProfiles$chr3.73159773.73160145$H3K27ac, type="l", col="blue",
#'     xlab="", ylab="", ylim=c(0, 125), main="chr3:73159773-73160145")
#' par(new=TRUE)
#' plot(demoProfiles$chr3.73159773.73160145$H3K4me1, type="l", col="darkgreen", 
#'     xlab="Position", ylab="Coverage in reads per million (RPM)",  
#'     ylim=c(0, 125))
#' legend("topright", c("H3K27ac","H3K4me1"), cex=1.2, 
#'     col=c("blue","darkgreen"), lty=1)
#'     
#' ## Calculate metrics using factory object 
#' ratio_norm_intersect <- factory$createMetric(metricType = 
#'     "RATIO_NORMALIZED_INTERSECT", 
#'     profile1=demoProfiles$chr3.73159773.73160145$H3K4me1, 
#'     profile2=demoProfiles$chr3.73159773.73160145$H3K27ac)
#' ratio_norm_intersect
#' 
#' ratio_area <- factory$createMetric(metricType="RATIO_AREA",
#'     profile1=demoProfiles$chr3.73159773.73160145$H3K4me1, 
#'     profile2=demoProfiles$chr3.73159773.73160145$H3K27ac)
#' ratio_area
#' 
#' @seealso 
#' \itemize{
#' \item \code{\link{similarity}} {for calculating all available metrics 
#' between two ChIP-Seq profiles.}
#' \item \code{\link{demoProfiles}} {for more informations about ChIP-Seq
#' profiles present in the demoProfiles data.}
#' }
#'
#' @author  Astrid Deschenes
#' @import R6
#' @export
MetricFactory <- R6Class("MetricFactory",
    public = list(
        initialize = function(ratioAreaThreshold = 1, 
                                ratioMaxMaxThreshold = 1, 
                                ratioIntersectThreshold = 1,
                                ratioNormalizedIntersectThreshold = 1,
                                diffPosMaxThresholdMinValue = 1, 
                                diffPosMaxThresholdMaxDiff = 100, 
                                diffPosMaxTolerance = 0.01,
                                spearmanCorrSDThreashold = 1e-8) {

            private$ratioAreaThreshold <<- ratioAreaThreshold
            private$ratioMaxMaxThreshold <<- ratioMaxMaxThreshold
            private$ratioIntersectThreshold <<- ratioIntersectThreshold
            private$ratioNormalizedIntersectThreshold <<- 
                ratioNormalizedIntersectThreshold
            private$diffPosMaxThresholdMinValue <<- diffPosMaxThresholdMinValue
            private$diffPosMaxThresholdMaxDiff <<- diffPosMaxThresholdMaxDiff
            private$diffPosMaxTolerance <<- diffPosMaxTolerance
            private$spearmanCorrSDThreashold <<- spearmanCorrSDThreashold
        },
        createMetric = function(metricType, profile1, profile2) {

            # Metric, profile1 and profile2 are mandatory
            if (missing(metricType)) {
                stop(paste0("The 'metricType' argument is mandatory."))
            }

            if (missing(profile1)) {
                stop(paste0("The 'profile1' argument is mandatory."))
            }

            if (missing(profile2)) {
                stop(paste0("The 'profile2' argument is mandatory."))
            }

            # The profile1 and profile2 arguments are numeric vectors 
            if (!is.vector(profile1) || !is.numeric(profile1)) {
                stop(paste0("The 'profile1' argument must be a numeric ", 
                            "vector."))
            }

            if (!is.vector(profile2) || !is.numeric(profile2)) {
                stop(paste0("The 'profile2' argument must be a numeric ", 
                            "vector."))
            }

            # The length of profile1 is equal to the length of profile2
            if (length(profile1) != length(profile2)) {
                stop(paste0("Lengths of 'profile1' and 'profile2' vectors ", 
                    "aren't equals."))
            }

            # Metric type must exist
            if (!metricType %in% private$metricVector) {
                stop(paste0("The metricType must be one of those choices: ", 
                    paste(private$metricVector, collapse = ", "))) 
            }

            result_name = list()
            result =list()

            if (metricType == "ALL" ||  metricType == "RATIO_AREA") {
                metric <- RatioArea$new(profile1, profile2, 
                                    private$ratioAreaThreshold)

                result_name <- c(result_name, metric$getType())
                result <- c(result, metric$getMetric())
            }

            if (metricType == "ALL" || metricType == "DIFF_POS_MAX") {
                metric <- DiffPosMax$new(profile1,  profile2, 
                                    private$diffPosMaxThresholdMinValue, 
                                    private$diffPosMaxThresholdMaxDiff, 
                                    private$diffPosMaxTolerance)

                result_name <- c(result_name, metric$getType())
                result <- c(result, metric$getMetric())
            }

            if (metricType == "ALL" || metricType == "RATIO_MAX_MAX") {
                metric <- RatioMaxMax$new(profile1, profile2, 
                                    private$ratioMaxMaxThreshold)
                
                result_name <- c(result_name, metric$getType())
                result <- c(result, metric$getMetric())
            }

            if (metricType == "ALL" || metricType == "RATIO_INTERSECT") {
                metric <- RatioIntersect$new(profile1, profile2, 
                                    private$ratioIntersectThreshold)

                result_name <- c(result_name, metric$getType())
                result <- c(result, metric$getMetric())
            }

            if (metricType == "ALL" || 
                    metricType == "RATIO_NORMALIZED_INTERSECT") {
                metric <- RatioNormalizedIntersect$new(profile1, profile2, 
                                    private$ratioNormalizedIntersectThreshold)

                result_name <- c(result_name,metric$getType())
                result <- c(result, metric$getMetric())
            }

            if (metricType == "ALL" || 
                    metricType == "SPEARMAN_CORRELATION") {
                metric <- SpearmanCorrelation$new(profile1, profile2, 
                                            private$spearmanCorrSDThreashold)

                result_name <- c(result_name,metric$getType())
                result <- c(result, metric$getMetric())
            }

            names(result) <- result_name

            return(result)

        }
    ), private = list(
        # Vector of all existing types of metrics
        metricVector = c("ALL",
                            "RATIO_AREA", 
                            "DIFF_POS_MAX", 
                            "RATIO_MAX_MAX", 
                            "RATIO_INTERSECT",
                            "RATIO_NORMALIZED_INTERSECT",
                            "SPEARMAN_CORRELATION"),
            # Threshold values
            ratioAreaThreshold = NA,
            ratioMaxMaxThreshold = NA,
            ratioIntersectThreshold = NA,
            ratioNormalizedIntersectThreshold = NA,
            diffPosMaxThresholdMinValue = NA,
            diffPosMaxThresholdMaxDiff = NA,
            diffPosMaxTolerance = NA,
            spearmanCorrSDThreashold = NA
    )
)
