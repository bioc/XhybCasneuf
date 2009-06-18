\name{XhybExamples-class}
\docType{class}
\alias{XhybExamples-class}
\alias{plotExample}
\alias{plotExample,XhybExamples-method}

\title{Class "XhybExamples"}
\description{Class containing information of cross-hybridisation examples}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("XhybExamples", ...)}.
}
\section{Slots}{
	 \describe{
    \item{\code{X}:}{Object of class \code{"character"} 
    contains the ID of probe set X}
    \item{\code{Y}:}{Object of class \code{"character"} 
    contains the ID of probe set Y}
    \item{\code{IVX}:}{Object of class \code{"numeric"} 
    contains the expression intensities of X in the Tissue dataset }
    \item{\code{IVY}:}{Object of class \code{"numeric"} 
    contains the expression intensities of Y in the Tissue dataset  }
    \item{\code{IVXi}:}{Object of class \code{"matrix"} 
    contains the expression intensities of X's reporters in the Tissue dataset }
    \item{\code{ai}:}{Object of class \code{"numeric"} 
    contains the alignment scores of X's reporters to Y's transcript sequence}
  }
}
\section{Methods}{
  \describe{
    \item{plotExample}{\code{signature(ex = "XhybExamples")}: ... }
	 }
}
\references{ Casneuf, Van de Peer and Huber (submitted); 
AtgenExpress data used: Schmid, M., Davison, T. S., Henz, S. R., Pape, U. J., Demar, M., Vingron, M. Schoelkopf, B. Weigel, D., and Lohmann, J. (2005) A gene expression map of Arabidopsis development. Nature Genetics, 37, 501-506.
}
\author{Tineke Casneuf \email{tine@ebi.ac.uk}}


\examples{
data(ex1)
data(ex2)
data(ex3)
plotExample(ex1)
plotExample(ex2)
plotExample(ex3)
}
\keyword{classes}
