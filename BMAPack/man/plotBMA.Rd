\name{plotBMA}
\alias{ANY-method}
\alias{plotBMA}
\alias{plotBMA,}
\title{Plots the posterior expected value of each coefficient or the posterior probability that the coefficient is non-zero}
\usage{
plotBMA(M, Y, gprior, whichplot)
}
\arguments{
  \item{M}{a matrix of observations for the covariates.}

  \item{Y}{a numeric vector whose length is equal to the
  number of rows of \code{M}.}

  \item{gprior}{a user specified prior.}

  \item{whichplot}{a user specificed argument for which
  plot to run, expected values ("Expected") or non-zero
  probability ("Non-Zero").}
}
\value{
A plot that graphs either a) the posterior expected value
of each coefficient, or b) the posterior probability that
the coefficient is non-zero.
}
\description{
Plots the expected value of each coefficient or the
posterior probability that the coefficient is non-zero.
}
\examples{
M <- matrix(rnorm(30), ncol=3)
 Y <- c(rnorm(10))
 plotBMA(M, Y, gprior=3, whichplot="Expected")
}
\author{
Dino Hadzic \email{dino.hadzic@wustl.edu}
}
\seealso{
\code{\link{summaryBMA}}
}

