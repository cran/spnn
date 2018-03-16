\name{pnn.learn}
\alias{pnn.learn}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
pnn.learn
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Create or update a Scale Invariant Probabilistic Neural Network.
}
\usage{
pnn.learn(set, nn, sigma, alpha, category.column = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{set}{
%%     ~~Describe \code{set} here~~
data.frame representing the training set. The first column (default category.column = 1) is used to define the category or class of each observation.
}
  \item{nn}{
%%     ~~Describe \code{nn} here~~
(optional) A Scale Invariant Probabilistic Neural Network object. If provided, the training data set input is concatenated to the current training data set of the neural network. If not provided, a new SPNN object is created.
}
  \item{sigma}{
%%     ~~Describe \code{sigma} here~~
An n by n square matrix of smoothing parameters where n is the number of input factors. Defaults to using the covariance matrix of the training data set excluding the category.column.
}
  \item{alpha}{
%%     ~~Describe \code{alpha} here~~
An n by n square matrix of alpha smoothing multipliers where n is the number of input factors. This is meant to be utilized if a custom optimization method is used for selecting elements of alpha. In this special case, the smoothing parameter matrix sigma is decomposed to the kronecker product of the alpha smoothing matrix and the default covariance matrix.
}
  \item{category.column}{
%%     ~~Describe \code{category.column} here~~
The column number of category data. Default is 1.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
The function pnn.learn creates a new Scale Invariant Probabilistic Neural Network with a given training data set or updates the training data of an existing SPNN. It sets the parameters: model, set, category.column, sigma, alpha, k, and n of the SPNN.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
A trained Scale Invariant Probabilistic Neural Network (SPNN)
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{spnn-package}}, \code{\link{pnn.guess}}, \code{\link{norms}}
}
\examples{
library(spnn)
data(norms)

pnn <- pnn.learn(norms)
}
