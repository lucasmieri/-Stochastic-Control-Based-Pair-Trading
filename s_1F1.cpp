#include <Rcpp.h>
#include <math.h>
using namespace Rcpp;

// Confluent Hypergeometric Function of the First
// kind.
// [[Rcpp::export]]
double s_1F1(double x, double a, double b,
    double tol = 1e-9, bool verbose = false,
    unsigned int max_iter = 10000)
{
  double t = 1, s = 1;
  unsigned int k = 0;
  double m = (a/b)*(x);
  while( m > 1 || fabs(t/s)*(1/(1-m)-1) > tol )
  {
    t = t*m;
    s = s + t;
    k = k + 1;
    m = ((a+k)/(b+k))*(x/(k+1));
    if(k > max_iter) break;
  }
  if(verbose) Rprintf("iterations: %u\n",k);
  return(s);
}
