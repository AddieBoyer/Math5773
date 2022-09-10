#' Binomial Application to Overbooking
#'
#' @param N Number of seats on a flight
#' @param gamma Probability that more people show up than there are seats on the plane
#' @param p Probability a ticketholder shows up for the flight
#' @importFrom graphics abline curve title
#' @importFrom stats optimize pbinom pnorm
#' @importFrom grDevices pdf
#'
#' @description Calculates the number of tickets to be sold for a flight using the binomial distribution and the normal approximation of the binomial distribution. Creates plots of the number of tickets sold against the objective function to find the optimal ticket sales. Prints optimal ticket values.
#'
#' @return The optimal number of tickets sold based on the discrete binomial distribution, denoted by nd, and the continuous normal approximation, denoted by nc.
#'
#' @export
#'
#' @examples ntickets(400, 0.02, 0.95)
#'
#'
ntickets <- function(N,gamma,p){
  #Discrete distribution
  x_1 = seq(N,N*1.1,1)
  y_1 = 1-gamma - pbinom(N+.05,x_1,p)
  plot(x_1, y_1, type = 'p', xlab="n", ylab="Objective",)
  nd <-x_1[which.min(abs(y_1))]
  abline(v=nd, col="blue")
  abline(h=0, col="blue")
  title(cex.main=.75,paste("Objective Vs n to find optimal tickets sold \n
              nd=", nd, "\n gamma=", gamma, "N=", N, "discrete"))

  #Continuous Approximation

  y_abscont = function(x) abs(1-gamma-pnorm(N+.5,x*p,sqrt(x*p*(1-p))))
  y_cont = function(x) 1-gamma-pnorm(N+.5,x*p,sqrt(x*p*(1-p)))


  roots <- optimize(y_abscont, c(N, N*1.1))
  nc = roots$minimum

  curve(y_cont, from=N, to=N*1.1, xlab="n", ylab="Objective")
  abline(h=0, col="red")
  abline(v=nc, col="red")
  title(cex.main=.75,paste("Objective Vs n to find optimal tickets sold \n
              nc=", nc, "\n gamma=", gamma, "N=", N, "continuous"))
  ret_list <- list(nd, nc, N, p, gamma)
  names(ret_list) <- c("nd", "nc", "N", "p", "gamma")

  return(ret_list)

}

ntickets(400,.02,.95)


