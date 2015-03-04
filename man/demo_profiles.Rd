\name{demo_profiles}
\alias{demo_profiles}
\docType{data}
\title{
Selected ChIP-seq profiles of two histone post-transcriptional modifications 
linked to highly active enhancers H3K27ac and H3K4me1 (for demonstration 
purpose)
}
\description{
ChIP-seq profiles of two histone post-transcriptional modifications 
linked to highly active enhancers H3K27ac (DCC accession: ENCFF000ASG) and 
H3K4me1 (DCC accession: ENCFF000ARY) from the Encyclopedia of DNA Elements 
(ENCODE) data (Dunham I et al. 2012).
}
\usage{data("demo_profiles")}
\format{
  A data frame with 0 observations on the following 2 variables.
  \describe{
    \item{\code{x}}{a numeric vector}
    \item{\code{y}}{a numeric vector}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
Dunham I, Kundaje A, Aldred SF, et al. An integrated encyclopedia of DNA 
elements in the human genome. Nature. 2012 Sep 6;489(7414):57-74.
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(demo_profiles)

par(mar=c(2,4,2,2))
par(mfrow=c(2,2)) 

plot(H3K27ac_chr2_profile,
        type="l", col="blue", xlab="", ylab="", ylim=c(0, 25),
        main="chr2:70360770-70361098")
par(new=TRUE)
plot(H3K4me1_chr2_profile,
        type="l", col="darkgreen", xlab="Position", 
        ylab="Coverage",  ylim=c(0, 25))
legend("topright", c("H3K27ac","H3K4me1"), cex=1.2, 
        col=c("blue","darkgreen"), lty=1)

plot(H3K27ac_chr8_profile,
        type="l", col="blue", xlab="", ylab="", ylim=c(0, 1500),
        main="chr8:43092918-43093442")
par(new=TRUE)
plot(H3K4me1_chr8_profile,
        type="l", col="darkgreen", xlab="Position", 
        ylab="Coverage",  ylim=c(0, 1500))
legend("topright", c("H3K27ac","H3K4me1"), cex=1.2, 
        col=c("blue","darkgreen"), lty=1)

plot(H3K27ac_chr3_profile,
        type="l", col="blue", xlab="", ylab="", ylim=c(0, 125),
        main="chr3:73159773-73160145")
par(new=TRUE)
plot(H3K4me1_chr3_profile,
        type="l", col="darkgreen", xlab="Position", 
        ylab="Coverage",  ylim=c(0, 125))
legend("topright", c("H3K27ac","H3K4me1"), cex=1.2, 
        col=c("blue","darkgreen"), lty=1)

plot(H3K27ac_chr19_profile,
        type="l", col="blue", xlab="", ylab="", ylim=c(0, 350),
        main="chr19:27739373-27739767")
par(new=TRUE)
plot(H3K4me1_chr19_profile,
        type="l", col="darkgreen", xlab="Position", 
        ylab="Coverage",  ylim=c(0, 350))
legend("topright", c("H3K27ac","H3K4me1"), cex=1.2, 
        col=c("blue","darkgreen"), lty=1)
}
\keyword{datasets}
