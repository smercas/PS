#I
zconfidence_interval = function(alpha, n, sample_mean, sigma) {
  critical_z = qnorm(1 - alpha / 2, 0, 1)
  a = sample_mean - critical_z * sigma / sqrt(n)
  b = sample_mean + critical_z * sigma / sqrt(n)
  interval = c(a, b)
  return (interval)
}
zconfidence_interval(1 - 0.9, 8, 138, 11)
zconfidence_interval(1 - 0.95, 8, 138, 11)
zconfidence_interval(1 - 0.99, 8, 138, 11)

#II
zconfidence_interval(1 - 0.95, 256, 18, sqrt(1.44))

#III
test_proportion = function(alpha, n, successes, p0, hyp) {
  p_prim = successes / n
  z_score = (p_prim - p0) / sqrt(p0 * (1 - p0) / n)
  cat(z_score, "\n")
  if (hyp == "r") {
    critical_z = qnorm(1 - alpha, 0, 1)
    cat(critical_z, "\n")
    if (z_score > critical_z) {
      cat("we reject H0\n")
    } else {
      cat("we don't have enough evidence to reject H0\n")
    }
  } else if (hyp == "l") {
    critical_z = qnorm(1 - alpha, 0, 1)
    cat(critical_z, "\n")
    if (z_score < critical_z) {
      cat("we reject H0\n")
    } else {
      cat("we don't have enough evidence to reject H0\n")
    }
  } else {
    critical_z = qnorm(1 - alpha / 2, 0, 1)
    cat(critical_z, "\n")
    if (abs(z_score) > critical_z) {
      cat("we reject H0\n")
    } else {
      cat("we don't have enough evidence to reject H0\n")
    }
  }
}
test_proportion(0.01, 153, 17, 0.12, "l")
test_proportion(0.05, 153, 17, 0.12, "l")
