\name{norms}
\alias{norms}
\docType{data}
\title{
%%   ~~ data name/kind ... ~~
norms
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
Gaussian random sample dataset.
}
\usage{data("norms")}
\format{
  A data frame with 400 observations and 3 columns of the following 3 variables.
  \describe{
    \item{\code{c}}{a category factor with levels A or B}
    \item{\code{x}}{a numeric vector coordinate x}
    \item{\code{y}}{a numeric vector coordinate y}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
The dataset represents gaussian random points with mean equal to 1 and standard deviation equal to 0.25. Each point has a category attribute A or B. They are centered at (1,1) and (2,2) for category A; centered at (1,2) and (2,1) for category B.
}
\examples{
library(spnn)
data(norms)

norms[1:10,]
}
\keyword{datasets}
