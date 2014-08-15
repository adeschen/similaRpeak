###################################################
## Test the similarity function
###################################################

## Test the result of non-numeric profile1
test.similarity_profile1_non_numeric<- function() {
    checkException(similarity(profile1=c(1,59,6,24,"a",34,15,4,53,22), profile2=c(15,9,46,44,9,39,27,34,34,4)), msg ="The 'profile1' argument must be a numeric vector.")
}

## Test the result of non-vector profile1
test.similarity_profile1_non_vector<- function() {
    checkException(similarity(profile1=matrix(c(1,59,6,24,65,34,15,4,53,22), ncol=2), profile2=c(15,9,46,44,9,39,27,34,34,4)), msg ="The 'profile1' argument must be a numeric vector.")
}

## Test the result of zeros vector profile1
test.similarity_profile1_zeros<- function() {
    checkException(similarity(profile1=c(0,0,0,0,0,0,0,0,0,0), profile2=c(15,9,46,44,9,39,27,34,34,4)), msg ="The profile1 argument contains negatives or is made up of zeros only.")
}

## Test the result of negatives values profile1
test.similarity_profile1_negatives_values<- function() {
    checkException(similarity(profile1=c(0,0,0,-8,0,0,0,0,0,0), profile2=c(15,9,46,44,9,39,27,34,34,4)), msg ="The profile1 argument contains negatives or is made up of zeros only.")
}

## Test the result of different profiles lengths
test.similarity_different_profiles_lengths<- function() {
    checkException(similarity(profile1=c(1,59,6,24,65,34,15,4,53,22), profile2=c(39,27,34,34,4)), msg ="Lengths of 'profile1' and 'profile2' vectors aren't equals.")
}

## Test the result of nul numeric ratioAreaThreshold
test.similarity_nul_ratioAreaThreshold<- function() {
    checkException(similarity(profile1=c(1,59,6,24,65,34,15,4,53,22), profile2=c(15,9,46,44,9,39,27,34,34,4), ratioAreaThreshold=0), msg ="The 'ratioAreaThreshold' must be a positive numeric value.")
}

## Test the result of negative ratioAreaThreshold
test.similarity_negative_ratioAreaThreshold<- function() {
    checkException(similarity(profile1=c(1,59,6,24,65,34,15,4,53,22), profile2=c(15,9,46,44,9,39,27,34,34,4), ratioAreaThreshold=-5), msg ="The 'ratioAreaThreshold' must be a positive numeric value.")
}

## Test the result of non numeric ratioAreaThreshold
test.similarity_non_numeric_ratioAreaThreshold<- function() {
    checkException(similarity(profile1=c(1,59,6,24,65,34,15,4,53,22), profile2=c(15,9,46,44,9,39,27,34,34,4), ratioAreaThreshold="g"), msg ="The 'ratioAreaThreshold' must be a positive numeric value.")
}


#### Using regular values

## Test the result of correct ratioArea
test.similarity<- function() {
    obs <- list("nbrPosition"=10,"areaProfile1"=283,"areaProfile2"=261,"maxProfile1"=65,"maxProfile2"=46,"maxPositionProfile1"=5,
                "maxPositionProfile2"=3)
    exp <- similarity(c(1,59,6,24,65,34,15,4,53,22), c(15,9,46,44,9,39,27,34,34,4))[1:7]
    checkEquals(obs, exp, tolerance = .Machine$double.eps^0.5)
}

## Test the result of ratioArea with some NA
test.similarity_na<- function() {
    obs <- list("nbrPosition"=10,"areaProfile1"=223,"areaProfile2"=212,"maxProfile1"=65,"maxProfile2"=46,"maxPositionProfile1"=5,
                "maxPositionProfile2"=3)
    exp <- similarity(c(NA,NA,6,24,65,34,15,4,53,22), c(NA,9,46,44,9,39,27,NA,34,4))[1:7]
    checkEquals(obs, exp, tolerance = .Machine$double.eps^0.5)
}