\name{AffysTissue}
\alias{AffysTissue}
\alias{AffysTissueMC}
\alias{AffysTissue.noBl}
\docType{data}
\title{Affymetrix' CDF probe set pairs}
\description{Affymetrix' CDF probe set pairs}
\usage{
data(AffysTissue)
data(AffysTissueMC)
data(AffysTissue.noBl)
}
\format{

These three data.frames contain data for Affymetrix' CDF's probe set pairs with Q75 >= 55.
The \code{pr} and \code{target} columns hold the names of probe set X and Y, respectively. 
Column \code{alSum} contains the Q75 value of the alignment scores of X's 
reporters to the transcript of Y. 
The \code{meant2hit} and \code{PSofTarget} columns contain respectively the gene locus 
the probe set was designed to target and the probe set ID of the off-target Y, 
according to Affymetrix (extracted from ATH1-121501\_annot.csv).
The \code{peCC} column holds the pairs' Pearson correlation coefficient calculated 
on their expression intensities in the Tissue dataset (data originally from AtgenExpress Project).

\code{AffysTissue} is data.frame that contains all Affymetrix' CDF's probe set pairs 
with Q75 >= 55.
\code{AffysTissueMC} holds a subset of the pairs in \code{AffysTissue}, namely those
whose metacorrelation coefficient is not NA are included. 
For \code{AffysTissue.noBl}, the pairs of \code{AffysTissue} that align to each 
other with BLAST in at least one direction with an E-value smaller than
\eqn{10^{-10}} were omitted.
}


\references{
Casneuf, Van de Peer and Huber, 
AtgenExpress data used: Schmid, M., Davison, T. S., Henz, S. R., Pape, U. J., Demar, M., Vingron, M. Schoelkopf, B. Weigel, D., and Lohmann, J. (2005) A gene expression map of Arabidopsis development. \emph{Nature Genetics}, 37, 501-506.
}

\seealso{\code{\link[Xhyb:CustomsTissue]{CustomsTissue}}}
\examples{
data(AffysTissue)
data(AffysTissueMC)
data(AffysTissue.noBl)

## see also the vignette
}

\author{Tineke Casneuf \email{tine@ebi.ac.uk}}
\keyword{datasets}
