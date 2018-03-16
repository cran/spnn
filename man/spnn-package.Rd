\name{spnn-package}
\alias{spnn-package}
\alias{spnn}
\docType{package}
\title{
\packageTitle{spnn}
}
\description{
\packageDescription{spnn}
}
\details{
The package exports 2 main functions:

\itemize{
\item{\link{pnn.learn}}{ Create or update a Scale Invariant Probabilistic Neural Network.}
\item{\link{pnn.guess}}{ Estimates the category probabilities of a new observation.}
}
}
\author{
\packageMaintainer{spnn}
}
\references{
[1] Specht, Donald F. "Probabilistic neural networks." Neural networks 3.1 (1990): 109-118.

[2] Specht, Donald F. "Enhancements to probabilistic neural networks." Neural Networks, 1992.IJCNN., International Joint Conference on. Vol. 1. IEEE, 1992.

[3] \url{http://texasquant.blogspot.com/2018/01/scale-invariant-probabilistic-neural.html}

[4] Ebrahimi, Romin "Scale Invariant Probabilistic Neural Networks." Working Paper, 2018
}
\keyword{spnn}
\keyword{neural}
\keyword{network}
\keyword{probability}
\keyword{pnn}
\seealso{
\code{\link{pnn.learn}}, \code{\link{pnn.guess}}, \code{\link{norms}}
}
\examples{
library(spnn)
data(norms)

pnn <- pnn.learn(norms)
pnn.guess(pnn, c(1,1))
pnn.guess(pnn, c(2,1))
pnn.guess(pnn, c(1.5,1))
}
