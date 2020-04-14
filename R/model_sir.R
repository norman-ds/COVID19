#' SIR model
#'
#' These function returns the delta of the classic [SIR model](https://www.idmod.org/docs/hiv/model-sir.html#sir-model) without births or deaths.
###' @param t vector of time steps
#' @param y vector of model variables S,I,R.
#' @param parms vector of model parameters R0, DoI, N.
#' @return equation list

SIR <- function(t, y, parms) {
  
  if (!all(c("R0", "DoI", "N") %in% names(parms))) {
    stop("The SIR model requires R0 (recoverd population at time zero), DoI (duration, D, of infection) and N (total population).")
  }
  
  # constant rates
  gamma <- 1/parms["DoI"]
  beta <- parms["R0"] * gamma / parms["N"]
  
  # Delta Susceptibles S
  dS <- - beta * y["S"] * y["I"]
  
  # Delta Infecteds I
  dI <- beta * y["S"] * y["I"] -
    gamma * y["I"]
  
  # Delta Recovereds R
  dR <- gamma * y["I"]
  
  dy <- c(dS, dI, dR)
  names(dy) <- c("dS","dI","dR")
  return(list(dy))
  
}


# initialization 
t <- seq(0, 180, by=1) # time points
DoI <- 20 # average duration of infection
N <- 8.57e6 # population Switzerland
I0 <- 100 # infectious population at time zero
R0 <- 10 # recoverd population at time zero
S0 <- N-I0-R0 # susceptible population at time zero

y0 <- c(S=S0, I=I0, R=R0)
parms <- c(R0=R0, N=N, DoI=DoI)
(dy <- SIR(t, y0, parms))

y.names <- c("susceptible ", "infectious", "recoverd")
y.mat <- deSolve::lsoda(y = y0,
                       times = t,
                       func = SIR,
                       parms = parms)

layout(matrix(1:2, nrow=1))
matplot(t, y.mat[,2:4], type = "l", ylab = "N", col=1)
legend("right", y.names , lty=5:1, col=1, bty="n", title = "population")
matplot(t, y.mat[,2:4], type = "l", ylab = "N", log = "y", col=1)



