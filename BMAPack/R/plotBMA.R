#' Plots the posterior expected value of each coefficient or the posterior probability that the coefficient is non-zero
#' 
#' Plots the expected value of each coefficient or the posterior probability that the coefficient is non-zero.
#' 
#' @param M a matrix of observations for the covariates.
#' @param Y a numeric vector whose length is equal to the number of rows of \code{M}.
#' @param gprior a user specified prior.
#' @param whichplot a user specificed argument for which plot to run, expected values ("Expected") or non-zero probability ("Non-Zero").
#' 
#' @return A plot that graphs either a) the posterior expected value of each coefficient, or b) the posterior probability that the coefficient is non-zero.
#' @author Dino Hadzic \email{dino.hadzic@@wustl.edu}
#' @examples
#' 
#'  M <- matrix(rnorm(30), ncol=3)
#'  Y <- c(rnorm(10))
#'  plotBMA(M, Y, gprior=3, whichplot="Expected")
#'  @seealso \code{\link{summaryBMA}}
#' @rdname plotBMA
#' @aliases plotBMA, ANY-method
#' @export
plotBMA <- function(M, Y, gprior, whichplot){ #Function takes as arguments the matrix of observations M, the vector
  #of outcomes Y, the user specified gprior, and whichplot. The argument whichplot can be specified as either "Expected",
  #in which case it will plot posterior expected values for the coefficients, or "Non-Zero", in which case it will plot
  #the posterior probabilities that the coefficient is non-zero.
  
  BMAOutput <- fitBMA(M, Y, gprior) #First run fitBMA, and store output as BMAOutput.
  
  #The if statement below states that when whichplot is specified as "Expected", the function will plot the
  #posterior expected values for the coefficients. The "axis" line code ensures that the tick marks are only 
  #whole numbers that correspond to the coefficient of interest.
  if(whichplot == "Expected"){
    plot(1:length(BMAOutput$PosteriorValues), BMAOutput$PosteriorValues, xlab="Covariate", 
         ylab="Posterior Expected Value", main="Posterior Expected Value \n of Each Coefficient",
         pch=16, xaxt="n")
    axis(1, at=seq(1,length(BMAOutput$PosteriorValues), by=1))
  }
  
  #The if statement below states that when whichplot is specified as "Non-Zero", the function will plot the
  #posterior probability that the coefficient is non-zero. The "axis" line code ensures that the tick marks are only 
  #whole numbers that correspond to the coefficient of interest.
  if(whichplot == "Non-Zero"){
    plot(1:length(BMAOutput$PosteriorNonZero), BMAOutput$PosteriorNonZero, xlab="Covariate", 
         ylab="Posterior Non-Zero Probability", main="Posterior Probability \n Coefficient is Non-Zero",
         pch=16, xaxt="n")
    axis(1, at=seq(1,length(BMAOutput$PosteriorNonZero), by=1))
  }
}
