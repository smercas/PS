#I.1
selection_mean = function(filename) {
  x = scan(filename);
  m = mean(x)
  return (m)
}
selection_mean("history.txt")

#II
alpha = 1 - 0.9
sample_mean = 20
n = 100
sigma = sqrt(9)
critical_z = qnorm(1 - alpha / 2, 0, 1)
a = sample_mean - critical_z * sigma / sqrt(n)
b = sample_mean + critical_z * sigma / sqrt(n)
interval = c(a, b)
interval

#II.1
zconfidence_interval = function(alpha, n, sample_mean, sigma) {
  critical_z = qnorm(1 - alpha / 2, 0, 1)
  a = sample_mean - critical_z * sigma / sqrt(n)
  b = sample_mean + critical_z * sigma / sqrt(n)
  interval = c(a, b)
  return (interval)
}
zconfidence_interval(1 - 0.9, 100, 20, sqrt(9))

#II.2
zconfidence_interval(1 - 0.9, 25, 67.53, sqrt(100))

#II.3
zconfidence_interval(1 - 0.95, 50, 5, 0.5)

#II.4
zconfidence_interval(1 - 0.99, 100, 1280, 140)

#II.5
zconfidence_interval(1 - 0.9, 35, 60, 5)
zconfidence_interval(1 - 0.95, 35, 60, 5)
zconfidence_interval(1 - 0.99, 35, 60, 5)

#II.6
zconfidence_interval_file = function(alpha, sigma, filename) {
  x = scan(filename)
  n = length(x)
  sample_mean = mean(x)
  critical_z = qnorm(1 - alpha / 2, 0, 1)
  a = sample_mean - critical_z * sigma / sqrt(n)
  b = sample_mean + critical_z * sigma / sqrt(n)
  interval = c(a, b)
  return (interval)
}
zconfidence_interval_file(1 - 0.95, 5, "history.txt")

#III
alpha = 1 - 0.95
sample_mean = 3.3
n = 60
s = 0.4
se = s / sqrt(n)
critical_t = qt(1 - alpha / 2, n - 1)
a = sample_mean - critical_t * se
b = sample_mean + critical_t * se
interval = c(a, b)
interval

#III.1
t_conf_interval = function(alpha, n, sample_mean, s) {
  se = s / sqrt(n)
  critical_t = qt(1 - alpha / 2, n - 1)
  a = sample_mean - critical_t * se
  b = sample_mean + critical_t * se
  interval = c(a, b)
  return (interval)
}

#III.2
t_conf_interval(1 - 0.99, 196, 44.65, sqrt(2.25))

#III.3
#a
t_conf_interval(1 - 0.99, 49, 12, 1.75)
t_conf_interval(1 - 0.95, 49, 12, 1.75)
#b
t_conf_interval(1 - 0.95, 49, 13.5, 1.25)

#III.4
t_conf_interval_file = function(alpha, filename) {
  x = scan(filename)
  n = length(x)
  sample_mean = mean(x)
  s = sd(x)
  se = s / sqrt(n)
  critical_t = qt(1 - alpha / 2, n - 1)
  a = sample_mean - critical_t * se
  b = sample_mean + critical_t * se
  interval = c(a, b)
  return (interval)
}

#III.5
t_conf_interval_file(1 - 0.9, "sample.txt")
t_conf_interval_file(1 - 0.95, "sample.txt")
t_conf_interval_file(1 - 0.99, "sample.txt")

#IV
alpha = 0.01
n = 100
successes = 63
p_prim = successes / n
p0 = 0.6
z_score = (p_prim - p0) / sqrt(p0 * (1 - p0) / n)
critical_z = qnorm(1 - alpha, 0, 1)
z_score
critical_z

#IV.1
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
test_proportion(0.01, 100, 63, 0.6, "r")

#IV.2
test_proportion(0.05, 150, 20, 0.10, "s")
