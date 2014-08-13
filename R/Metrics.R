# Created by Astrid Louise Deschenes
# 2014-08-13

library(R6)

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
                      calculateMetric = function(profile1, profile2, threshold=NULL) {
                      },
                      getInfo = function() {
                          cat(paste0("Metric type: ", type, ". Metric value: ", metric, "\n"))
                      }
                   ),  
                   private = list(
                      metric = NA,
                      type = NA,
                      setMetric = function(val){
                          metric <<- val
                      },
                      setType = function(val){
                          type <<- val
                      }
                   )
              )


# Class representing a Max Max Ratio metric which is the ratio of profiles maximal peaks 
# between two ChIP profiles covering the same range
#
RatioMaxMax <- R6Class("RatioMaxMax",
                        inherit = Metric,
                        public = list(
                            initialize = function(profile1, profile2, threshold) {
                                
                                # Fix the type of metric
                                super$setType("RATIO_MAX_MAX")
                                
                                if (!missing(profile1) && !missing(profile2)) {
                                    calculateMetric(profile1, profile2, threshold)   
                                }
                            },
                            calculateMetric = function(profile1, profile2, threshold=1) {
                                
                                # Reset metric value to NA
                                super$setMetric(NA)
                                
                                # Profile1 and profile2 are mandatory
                                if (missing(profile1)) {
                                    stop("The 'profile1' argument is mandatory. The metric value has been reset to NA.")
                                }
                                if (missing(profile2)) {
                                    stop("The 'profile2' argument is mandatory.The metric value has been reset to NA.")
                                }
                                
                                # The profile1 and profile2 arguments are numeric vectors where at 
                                # least one element is greater  than zero
                                if (!is.vector(profile1) | !is.numeric(profile1)) {
                                    stop("The 'profile1' argument must be a numeric vector. The metric value has been reset to NA.")
                                }
                                if (!is.vector(profile2) | !is.numeric(profile2)) {
                                    stop("The 'profile2' argument must be a numeric vector. The metric value has been reset to NA.")
                                }
                                
                                # The length of profile1 is equal to the length of profile2
                                if (length(profile1) != length(profile2)) {
                                    stop("Lengths of 'profile1' and 'profile2' vectors aren't equals. The metric value has been reset to NA.")
                                }
                                
                                # Calculate and assign the new max max ratio
                                super$setMetric(ratioMaxMax(profile1, profile2, threshold))
                            }
                        )
                    )
                       
                       