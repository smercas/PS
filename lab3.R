#I
density_exponential = function(lambda, delta, a) {
  x = seq(0, a, delta);
  y = dexp(x, lambda);
  plot(x, y, type = "l");
}
density_exponential(3, 0.01, 10)

#I.1
#a
density_gamma = function(alpha, lambda, delta, a) {
  x = seq(0, a, delta);
  y = dgamma(x, alpha, lambda);
  plot(x, y, type = "l");
}
density_gamma(3/2, 3, 0.01, 10)

#b
density_Student = function(df, delta, a) {
  x = seq(-a, a, delta);
  y = dt(x, df);
  plot(x, y, type = "l");
}
density_Student(0.01, 0.01, 10)

#c
density_normal = function(lambda, delta, a) {
  x = seq(-a, a, delta);
  y = dnorm(lambda, delta);
  plot(x, y, type = "l");
}
density_exponential(3, 0.01, 10)

#II
LLN_Poisson = function(lambda, n) {
  return(mean(rpois(n, lambda)));
}
LLN_Poisson(3, 1)

LLN_Gamma = function(alpha, lambda, n) {
  return(mean(rgamma(n, alpha, lambda)));
}
LLN_Gamma(6, 3, 1000000)

#II.1
#a
LLN_Exp = function(lambda, n) {
  return(mean(rexp(n, lambda)));
}
LLN_Exp(3, 1000000);

#b
LLN_Binom = function(m, p, n) {
  return(mean(rbinom(n, m, p)));
}
LLN_Binom(10, 0.1, 10000000);

#c
LLN_Student = function(r, n) {
  return(mean(rt(n, r)));
}
LLN_Student(2, 1000);
LLN_Student(3, 1000);
LLN_Student(4, 1000);
LLN_Student(5, 1000);
LLN_Student(2, 10000);
LLN_Student(3, 10000);
LLN_Student(4, 10000);
LLN_Student(5, 10000);
LLN_Student(2, 100000);
LLN_Student(3, 100000);
LLN_Student(4, 100000);
LLN_Student(5, 100000);
LLN_Student(2, 1000000);
LLN_Student(3, 1000000);
LLN_Student(4, 1000000);
LLN_Student(5, 1000000);


#III
CLT_Poisson = function(lambda, n, N, z) {
  expectation = lambda;
  st_dev = sqrt(lambda);
  upper_bound = z * st_dev/sqrt(n) + expectation;
  sum = 0;
  for(i in 1:N) {
    x_n = mean(rpois(n, lambda));
    if(x_n <= upper_bound) {
      sum = sum + 1;
    }
  }
  return(c(sum/N, pnorm(z)));
}
CLT_Poisson(2, 1000, 10000, 0)

#III.1
CLT_Exp = function(lambda, n, N, z) {
  expectation = 1/lambda;
  st_dev = 1/lambda;
  upper_bound = z * st_dev/sqrt(n) + expectation;
  sum = 0;
  for(i in 1:N) {
    x_n = mean(rexp(n, lambda));
    if(x_n <= upper_bound) {
      sum = sum + 1;
    }
  }
  return(c(sum/N, pnorm(z)));
}
CLT_Exp(5, 30, 10000, 0)

#III.2
CLT_Gamma = function(alpha, lambda, n, N, z) {
  expectation = alpha/lambda;
  st_dev = alpha/(lambda*lambda);
  upper_bound = z * st_dev/sqrt(n) + expectation;
  sum = 0;
  for(i in 1:N) {
    x_n = mean(rgamma(n, lambda));
    if(x_n <= upper_bound) {
      sum = sum + 1;
    }
  }
  return(c(sum/N, pnorm(z)));
}
CLT_Exp(5, 30, 10000, 0)

#IV
binomial_probability = function(n, p, k) {
  expectation = n*p;
  variance = n*p*(1 - p);
  standard_deviation = sqrt(variance);
  q = (k + 0.5 - expectation)/standard_deviation;
  return(1 - pnorm(q));
}
binomial_probability(50, 0.3, 10)
