#' Provides descriptive summary statistics for either the posterior expected values or the posterior probability that coefficients are non-zero
#' 
#' Provides summary statistics for posterior expected values or posterior probability that coefficients are non-zero
#' 
#' @param M a matrix of observations for the covariates.
#' @param Y a numeric vector whose length is equal to the number of rows of \code{M}.
#' @param gprior a user specified prior.
#' @param whichsumm a user specifed argument for which summary to produce, expected values ("Expected") or non-zero probability ("Non-Zero").
#' 
#' @return An object of class list with elements
#' \item{PosteriorValues}{If whichsumm specified as "Expected", posterior expected value of each coefficient.}
#' \item{SummaryPosteriorValues}{If whichsumm specified as "Expected", summary statistics for expected values.}
#' \item{PosteriorNonZero}{If whichsumm specified as "Non-Zero", posterior probability that the coefficient is non-zero.}
#' \item{SummaryPosteriorNonZero}{If whichsumm specified as "Non-Zero", summary statistics for posterior non-zero probabilities.}
#' @author Dino Hadzic \email{dino.hadzic@@wustl.edu}
#' @examples
#' 
#' M <- matrix(rnorm(30), ncol=3)
#' Y <- c(rnorm(10))
#' summaryBMA(M, Y, gprior=3, whichsumm="Expected")
#' @rdname summaryBMA
#' @aliases summaryBMA, ANY-method
#' @export
summaryBMA <- function(M, Y, gprior, whichsumm){ #Function takes as arguments matrix of observations M, vector of 
#outcomes Y, user specified gprior, and whichsumm. The argument whichsumm can be specified as either "Expected",
#in which case it will provide summary statistics for posterior expected values for the coefficients, or "Non-Zero",
#in which case it will provide summary statistics for posterior non-zero probabilities for the coefficients.
  
  BMAOutput <- fitBMA(M, Y, gprior) #First run fitBMA, and store output as BMAOutput.
  
#The if statement below states that when whichsumm is specified as "Expected", the function will output summary 
#satistics for posterior expected values for the coefficients. The output is a list.
  if(whichsumm == "Expected"){
  PosteriorValues <- BMAOutput$PosteriorValues
  SummaryPosteriorValues <- summary(BMAOutput$PosteriorValues)
  return(list("PosteriorValues"=PosteriorValues, "SummaryPosteriorValues"=SummaryPosteriorValues))
  }
  
#The if statement below states that when whichsumm is specified as "Non-Zero", the function will output summary 
#satistics for posterior probabilities that coefficients are non-zero. The output is a list.
  if(whichsumm == "Non-Zero"){
  PosteriorNonZero <- BMAOutput$PosteriorNonZero
  SummaryPosteriorNonZero <- summary(BMAOutput$PosteriorNonZero)
  return(list("PosteriorNonZero"=PosteriorNonZero, "SummaryPosteriorNonZero"=SummaryPosteriorNonZero))
  }
}

  



