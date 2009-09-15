\name{CustomsTissue}
\alias{CustomsTissue}
\alias{CustomsTissueMC}
\alias{CustomsTissue.noBl}
\docType{data}
\title{Custom-made CDF's probe set pairs}
\description{Custom-made CDF's probe set pairs}
\usage{
data(CustomsTissue)
data(CustomsTissueMC)
data(CustomsTissue.noBl)
}
\format{

These three data.frames contain data for the custom-made CDF's probe set pairs with Q75 >= 55.
The \code{pr} and \code{target} columns hold the names of probe set X and Y, respectively. 
Column \code{alSum} contains the Q75 value of the alignment scores of X's 
reporters to the transcript of Y. 
The \code{peCC} column holds the pairs' Pearson correlation coefficient calculated 
on their expression intensities in the Tissue dataset (data originally from AtgenExpress Project).

\code{CustomsTissue} is data.frame that contains all custom-made CDF's probe set pairs 
with Q75 >= 55.
\code{CustomsTissueMC} holds a subset of the pairs in \code{CustomsTissue}, namely those
whose metacorrelation coefficient is not NA are included. 
For \code{CustomsTissue.noBl}, the pairs of \code{CustomsTissue} that align to each 
other with BLAST in at least one direction with an E-value smaller than
\eqn{10^{-10}} were omitted.
}


\references{
Casneuf, Van de Peer and Huber,
AtgenExpress data used: Schmid, M., Davison, T. S., Henz, S. R., Pape, U. J., Demar, M., Vingron, M. Schoelkopf, B. Weigel, D., and Lohmann, J. (2005) A gene expression map of Arabidopsis development. \emph{Nature Genetics}, 37, 501-506.
}

\seealso{\code{\link{AffysTissue}}}
\examples{
data(CustomsTissue)
data(CustomsTissueMC)
data(CustomsTissue.noBl)

## see also the vignette
}

\author{Tineke Casneuf \email{tine@ebi.c.uk}}
\keyword{datasets}
