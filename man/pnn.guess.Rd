\name{pnn.guess}
\alias{pnn.guess}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
pnn.guess
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Estimates the category probabilities of a new observation.
}
\usage{
pnn.guess(nn, X)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nn}{
%%     ~~Describe \code{nn} here~~
A trained Scaled Invariant Probabilistic Neural Network.
}
  \item{X}{
%%     ~~Describe \code{X} here~~
A vector describing a new observation.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
Given a trained Scale Invariant Probabilistic Neural Network, the function pnn.guess returns the category with the highest probability and the probabilities of each category.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
A list of the guessed category and the probabilities of each category.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{spnn-package}}, \code{\link{pnn.learn}}, \code{\link{norms}}
}
\examples{
library(spnn)
data(norms)

pnn <- pnn.learn(norms)
pnn.guess(nn = pnn, X = c(1,2))
}
