#1
LLN_Geom = function(n, p) {
  return (c(mean(rgeom(n, p)), 1 / p));
}
LLN_Geom(5000, 0.2)
LLN_Geom(10000, 0.6)
LLN_Geom(100000, 0.6)
LLN_Geom(500000, 0.8)

#2
CLT_Student = function(r, n, N, z) {
  expectation = 0
  st_dev = sqrt(r / (r - 2))
  upper_bound = z * st_dev / sqrt(n) + expectation
  sum = 0
  for (i in 1:N) {
    x_n = mean(rt(n, r))
    if (x_n <= upper_bound) {
      sum = sum + 1
    }
  }
  return (sum / N)
}
estimated_prob = CLT_Student(3, 50, 5000, -1.5)
true_prob = pnorm(-1.5)
relative_error = abs(estimated_prob - true_prob) / true_prob
relative_error
estimated_prob = CLT_Student(4, 50, 10000, 0)
true_prob = pnorm(0)
relative_error = abs(estimated_prob - true_prob) / true_prob
relative_error
estimated_prob = CLT_Student(5, 50, 20000, 1.5)
true_prob = pnorm(1.5)
relative_error = abs(estimated_prob - true_prob) / true_prob
relative_error

#3
ML_Binom = function(n, p, h, k) {
  expectation = n * p
  standard_deviation = sqrt(n * p * (1 - p))
  q1 = (h - expectation) / standard_deviation
  q2 = (k - expectation) / standard_deviation
  approx_prob = pnorm(q2) - pnorm(q1)
  return (approx_prob)
}
ML_Binom(100, 0.3, 20, 40)
#P(X<40)
pbinom(40, 100, 0.3) - factorial(100) / (factorial(40) * factorial(60)) * 0.3 ^ 40 * (1 - 0.3) ^ 60
pbinom(39, 100, 0.3)
#P(X<20)
pbinom(20, 100, 0.3) - factorial(100) / (factorial(20) * factorial(80)) * 0.3 ^ 20 * (1 - 0.3) ^ 80
pbinom(19, 100, 0.3)
cat(pbinom(39, 100, 0.3) - pbinom(19, 100, 0.3))
