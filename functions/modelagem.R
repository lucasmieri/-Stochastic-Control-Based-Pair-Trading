library(tidyquant)
library(zoo)
library(dplyr)
library(lubridate)
library(Rcpp)
library(rootSolve)
script_dir_cpp <- dirname(rstudioapi::getSourceEditorContext()$path)
base_dir_cpp <- file.path(script_dir_cpp, "functions/s_1F1.cpp")
sourceCpp(base_dir_cpp)
s_1F1_vec<-Vectorize(s_1F1)


phi_1 <- function(x, params) {
  theta <- params$theta
  mu <- params$mu
  sigma <- params$sigma
  rho <- params$rho
  k <- params$k
  m <- params$M
  
  term1 <- gamma(rho/(2*theta)) * s_1F1((k^2 * (x-mu)^2)/2, rho/(2*theta),1/2)
  term2 <- sqrt(2) * k * (x-mu) * gamma((theta+rho)/(2*theta)) * s_1F1((k^2 * (x-mu)^2)/2, (theta+rho)/(2*theta) , 3/2)
  #verificar aqui
  return(2^(-1+rho/(2*theta)) * (term1 + term2))
}

#--------------------------------------------------------------------------------------------------------------------------------------------
#Integrate[t^((rho/theta)-1)*e^(-t^2/2)*e^(k*(mu-x)*t),{t,0,∞}]
phi_2 <- function(x, params) {
  theta <- params$theta
  mu <- params$mu
  sigma <- params$sigma
  rho <- params$rho
  k <- params$k
  m <- params$M
  
  term1 <- gamma(rho/(2*theta)) * s_1F1((k^2 * (x-mu)^2)/2, rho/(2*theta), 1/2)
  term2 <- k * sqrt(2) * (mu - x) * gamma((theta+rho)/(2*theta)) * s_1F1((k^2 * (x-mu)^2)/2,(theta+rho)/(2*theta), 3/2)
  
  return(2^(-1 + rho/(2*theta)) * (term1 + term2))
}
#--------------------------------------------------------------------------------------------------------------------------------------------
#t^((rho/theta)-1)*e^(-t^2/2)*e^(-k*(mu-x)*t)
#Integrate[(k t^(ρ/θ))/E^((t (t + 2 k (-x + μ)))/2),{t,0,∞}]
phi1_derivate<-function(x,params){
  theta <- params$theta
  mu <- params$mu
  sigma <- params$sigma
  rho <- params$rho
  k <- params$k
  
  term1 <- (gamma((theta + rho)/(2*theta)) * s_1F1((k^2 * (x - mu)^2)/2, (theta + rho)/(2*theta),1/2)/sqrt(2))
  term2 <- k * (x - mu) * gamma(1 + rho/(2*theta)) * s_1F1((k^2 * (x - mu)^2)/2, 1 + rho/(2*theta),  3/2)
  
  return(2^(rho/(2*theta)) * k * (term1 + term2))
}
#--------------------------------------------------------------------------------------------------------------------------------------------
#t^((rho/theta)-1)*e^(-t^2/2)*e^(k*(mu-x)*t)
#Integrate[-((k t^(ρ/θ))/E^((t (t + 2 k (x - μ)))/2)),{t,0,∞}]
phi2_derivate <- function(x, params) {
  theta <- params$theta
  mu <- params$mu
  sigma <- params$sigma
  rho <- params$rho
  k <- params$k
  
  term1 <- (gamma((theta + rho)/(2*theta)) * s_1F1((k^2 * (x - mu)^2)/2,(theta + rho)/(2*theta), 1/2) / sqrt(2))
  term2 <- k * (-x + mu) * gamma(1 + rho/(2*theta)) * s_1F1((k^2 * (x - mu)^2)/2, 1 + rho/(2*theta), 3/2)
  
  return(-(2^(rho/(2*theta)) * k * (term1 + term2)))
}
#--------------------------------------------------------------------------------------------------------------------------------------------

phi_matrix<-function(x,params){
  return(rbind(
    c(phi_1(x, params), phi_2(x, params)),
    c(phi1_derivate(x, params), phi2_derivate(x, params))))
}

phi_matrix_inverse<-function(x,params){
  return(solve(phi_matrix(x,params)))
}
#--------------------------------------------------------------------------------------------------------------------------------------------

P1<-function(x,params){
  k <- params$K0
  phi_inv<-phi_matrix_inverse(x,params)
  term1<-rbind(c(x+k),c(1))
  return (phi_inv%*%term1)
}

#--------------------------------------------------------------------------------------------------------------------------------------------

P2<-function(x,params){
  k <- params$K0
  phi_inv<-phi_matrix_inverse(x,params)
  term1<-rbind(c(x-k),c(1))
  return (phi_inv%*%term1)
}
#--------------------------------------------------------------------------------------------------------------------------------------------
R<-function(x,params){
  k <- params$K0
  phi_inv<-phi_matrix_inverse(x,params)
  term1<-rbind(phi_1(x,params),phi1_derivate(x,params))
  return (phi_inv%*%term1)
}

#--------------------------------------------------------------------------------------------------------------------------------------------

#Equation 17
#(φ1(M), φ2(M))P1(x0) = M − K.
eqn<-function(x,params){
  M<-params$M
  K0<-params$K0
  phi_vec<-rbind(c(phi_1(M,params),phi_2(M,params)))
  #return (rbind(c(phi_1(x, params),(phi_2(x, params))))%*%P1(x,params)-(M-K0))
  return (
    phi_vec%*%P1(x,params)-(M-K0)
  )
}
#--------------------------------------------------------------------------------------------------------------------------------------------


#--------------------------------------------------------------------------------------------------------------------------------------------
#Equation 16
eqn_16<-function(params){
  M <- params$M
  phi1_m <- phi_1(M, params)
  phi2_m <-phi_2(M,params)
  
  return(solve())
}

#--------------------------------------------------------------------------------------------------------------------------------------------
#Equation 18
#2 raizes 
#A2(R(x1) − R(x2)) = P2(x2) − P1(x1).
eqn2 <- function(x, params){
  A2 <- x
  P1_x1 <- P1(params$x1, params)
  P2_x2 <- P2(params$x2, params)
  R_x1 <- R(params$x1, params)
  R_x2 <- R(params$x2, params)
  # Compute the left-hand side of the equation
  lhs <- A2 * (R_x1 - R_x2)
  # Compute the right-hand side of the equation
  rhs <- P2_x2 - P1_x1
  # Return the difference
  return(solve(lhs - rhs))
}
#root <- uniroot(eqn2, interval = c(-1, 1), params = params)$root


#--------------------------------------------------------------------------------------------------------------------------------------------
#Equation 21
# Define the function
#x1 e x2 como variaveis
equation_21 <- function(x,params){
  M <- params$M
  K <- params$K0
  x1 = x[1]
  x2 = x[2]
  P1_x1 <- P1(x1, params)
  P2_x2 <- P2(x2, params)
  R_x1 <- R(x1, params)
  R_x2 <- R(x2, params)
  
  phi1_m <- phi_1(M, params)
  phi2_m <-phi_2(M,params)
  phi_vec <-rbind(c(phi1_m, phi2_m))
  
  # Compute the denominator
  denominator <- (phi_vec%*% R_x2)
  
  # Compute the left-hand side of the equation
  numerator<-(M-K -(phi_vec%*% P2_x2))
  lhs <- (R_x1 - R_x2)%*%(numerator/ denominator)
  
  # Compute the right-hand side of the equation
  rhs <- P2_x2 - P1_x1
  
  # Return the difference
  return(lhs - rhs)
}

modelagem <- function(params) {
  # Calcular k
  params$k <- sqrt(2 * params$theta) / params$sigma
  
  # Calcular x1 e x2
  params$x1_limit<-(params$theta*params$mu-params$rho*params$K0)/(params$rho+params$theta)
  params$x2_limit<-(params$theta*params$mu+params$rho*params$K0)/(params$rho+params$theta)
  
  # Encontrar x0
  params$x0 <- uniroot(eqn, interval = c(params$M, 0), extendInt='yes', params = params)$root
  
  # Resolver equação 21 para encontrar x1 e x2
  x_guess <- c(params$x0, params$mu + params$sigma)
  root <- multiroot(f = function(x) equation_21(x, params), rtol = 1e-11, start = x_guess, maxiter = 1000)$root
  params$x1 <- root[1]
  params$x2 <- root[2]
  
  return(params)
}



