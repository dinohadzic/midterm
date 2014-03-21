\name{summaryBMA}
\alias{ANY-method}
\alias{summaryBMA}
\alias{summaryBMA,}
\title{Provides descriptive summary statistics for either the posterior expected values or the posterior probability that coefficients are non-zero}
\usage{
summaryBMA(M, Y, gprior, whichsumm)
}
\arguments{
  \item{M}{a matrix of observations for the covariates.}

  \item{Y}{a numeric vector whose length is equal to the
  number of rows of \code{M}.}

  \item{gprior}{a user specified prior.}

  \item{whichsumm}{a user specifed argument for which
  summary to produce, expected values ("Expected") or
  non-zero probability ("Non-Zero").}
}
\value{
An object of class list with elements
\item{PosteriorValues}{If whichsumm specified as
"Expected", posterior expected value of each coefficient.}
\item{SummaryPosteriorValues}{If whichsumm specified as
"Expected", summary statistics for expected values.}
\item{PosteriorNonZero}{If whichsumm specified as
"Non-Zero", posterior probability that the coefficient is
non-zero.} \item{SummaryPosteriorNonZero}{If whichsumm
specified as "Non-Zero", summary statistics for posterior
non-zero probabilities.}
}
\description{
Provides summary statistics for posterior expected values
or posterior probability that coefficients are non-zero
}
\examples{
M <- matrix(rnorm(30), ncol=3)
Y <- c(rnorm(10))
summaryBMA(M, Y, gprior=3, whichsumm="Expected")
}
\author{
Dino Hadzic \email{dino.hadzic@wustl.edu}
}
\seealso{
\code{\link{fitBMA}}
}

