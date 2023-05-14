#I
#circle_area = pi * r ^ 2
#square_area = (2 * r) ^ 2 = 4 * r ^ 2
#pi = 4 * circle_area / square_area
circle_area = function(N, r) {
  N_C = 0
  for (i in 1:N) {
    x = runif(1, -r, r)
    y = runif(1, -r, r)
    if (x * x + y * y <= r * r) {
      N_C = N_C + 1
    }
  }
  return((2 * r) ^ 2 * N_C / N)
}
circle_area(100000, 1)
MC_area = circle_area(100000, 1)
area = pi
abs_err = abs(MC_area - area)
rel_err = abs_err/area
cat("estimated area is", MC_area)
cat("absolute error is", abs_err)
cat("relative error is", rel_err)

#I.1
#sphere_volume = 4 / 3 * pi * r ^ 3
#cube_volume = (2 * r) ^ 3 = 8 * r ^ 3
sphere_volume = function(N, r) {
  N_C = 0
  for (i in 1:N) {
    x = runif(1, -r, r)
    y = runif(1, -r, r)
    z = runif(1, -r, r)
    if (x * x + y * y + z * z <= r * r) {
      N_C = N_C + 1
    }
  }
  return ((2 * r) ^ 3 * N_C / N)
}
sphere_volume(10000, 1)
MC_volume = sphere_volume(100000, 1)
volume = 4 / 3 * pi
abs_err = abs(MC_volume - volume)
rel_err = abs_err/volume
cat("estimated area is", MC_volume)
cat("absolute error is", abs_err)
cat("relative error is", rel_err)

#I.2
parabola_area <- function(N) {
  N_C <- 0
  for (i in 1:N) {
    x <- runif(1, 0, 2)
    y <- runif(1, 0, 2)
    if (y <= -2 * x ^ 2 + 5 * x - 2)
      N_C <- N_C + 1
  }
  return((2 - 0) ^ 2 * N_C / N)
}
parabola_area(100000)
area = integrate(function(x) -2 * x ^ 2 + 5 * x - 2, 0.5, 2)$value
var = parabola_area(10000)
relative_error = abs(var - area) / abs(area)
percentage_error = relative_error * 100


#II
MC_integration = function(N, a, b) {
  sum = 0
  for (i in 1:N) {
    u = runif(1, a, b)
    sum = sum + exp(-u * u / 2)
  }
  return ((b - a) * sum / N)
}
MC_integration(100000, 0, 10)

MC_integration_average = function(k, N) {
  estimates = vector(mode = "numeric", length = k)
  for (i in 1:k) {
    estimates[i] = MC_integration(N, 0, 10)
  }
  print(mean(estimates))
  print(sd(estimates))
}
MC_integration_average(30, 100000)

MC_improved_integration = function(N) {
  sum = 0
  for (i in 1:N) {
    u = rexp(1, 1)
    sum = sum + exp(-u * u) / exp(-u)
  }
  return (sum / N)
}

MC_improved_integration_average = function(k, N) {
  estimates = vector(mode = "numeric", length = k)
  for (i in 1:N) {
    estimates[i] = MC_improved_integration(N)
  }
  print(mean(estimates))
  print(sd(estimates))
}
MC_improved_integration_average(30, 100000)

#II.1.a
MC_integration = function(N, a, b) {
  sum = 0
  for (i in 1:N) {
    u = runif(1, a, b)
    sum = sum + sin(u) ^ 2
  }
  return ((b - a) * sum / N)
}
MC_integration(10000, 0, pi)

MC_integral = MC_integration(10000, 0, pi)
integral = integrate(function(x) sin(x) ^ 2, 0, pi)$value
abs_err = abs(MC_integral - integral)
rel_err = abs_err / integral
cat("estimated area is", MC_integral)
cat("absolute error is", abs_err)
cat("relative error is", rel_err)

#II.1.b
MC_integration = function(N, a, b) {
  sum = 0
  for (i in 1:N) {
    u = runif(1, a, b)
    sum = sum + exp(u)
  }
  return ((b - a) * sum / N)
}
MC_integration(10000, 1, 4)

MC_integral = MC_integration(10000, 1, 4)
integral = integrate(function(x) exp(x), 1, 4)$value
abs_err = abs(MC_integral - integral)
rel_err = abs_err / integral
cat("estimated area is", MC_integral)
cat("absolute error is", abs_err)
cat("relative error is", rel_err)

#II.1.c
MC_integration = function(N, a, b) {
  sum = 0
  for (i in 1:N) {
    u = runif(1, a, b)
    sum = sum + 1 / sqrt(1 - u * u)
  }
  return ((b - a) * sum / N)
}
MC_integration(10000, 0, 1)

MC_integral = MC_integration(10000, 0, 1)
integral = integrate(function(x) 1 / sqrt(1 - x ^ 2), 0, 1)$value
abs_err = abs(MC_integral - integral)
rel_err = abs_err / integral
cat("estimated area is", MC_integral)
cat("absolute error is", abs_err)
cat("relative error is", rel_err)

#II.1.d
MC_improved_integration = function(N, sampling_function) {
  sum = 0
  for (i in 1:N) {
    u = runif(1, 0, 1)
    sum = sum + 1 / (4 * sampling_func(u) ^ 2 - 1) / sampling_func(u)
  }
  return (sum / N)
}
sampling_func = function(u) {
  return (1 / u ^ (1/16.9))
}
MC_improved_integration(100000, sampling_func)

MC_integral = MC_improved_integration(100000, sampling_func)
integral = integrate(function(x) 1 / (4 * x ^ 2 - 1), 1, Inf)$value
abs_err = abs(MC_integral - integral)
rel_err = abs_err / integral
cat("estimated area is", MC_integral)
cat("absolute error is", abs_err)
cat("relative error is", rel_err)

#II.2
MC_improved_integration = function(lambda, N) {
  sum = 0
  for (i in 1:N) {
    u = rexp(1, lambda)
    sum = sum + exp(-2 * u ^ 2) / (lambda * exp(-lambda * u))
  }
  return (sum / N)
}
MC_improved_integration(3, 100000)

MC_integral = MC_improved_integration(3, 50000)
integral = integrate(function(x) exp(-2 * x ^ 2), 0, Inf)$value
abs_err = abs(MC_integral - integral)
rel_err = abs_err / integral
cat("estimated area is", MC_integral)
cat("absolute error is", abs_err)
cat("relative error is", rel_err)

MC_improved_integration_average = function(k, lambda, N) {
  estimates = vector(mode = "numeric", length = k)
  for (i in 1:N) {
    estimates[i] = MC_improved_integration(lambda, N)
  }
  print(mean(estimates))
  print(sd(estimates))
}
MC_improved_integration_average(30, 3, 500)

#III
Nr_days = function() {
  nr_days = 1
  last_errors = c(27, 31)
  nr_errors = 27
  while (nr_errors > 0) {
    lambda = min(last_errors)
    nr_errors = rpois(1, lambda)
    last_errors = c(nr_errors, last_errors[1])
    nr_days = nr_days + 1
  }
  return (nr_days)
}
MC_nr_days = function(N) {
  s = 0
  for (i in 1:N) {
    s = s + Nr_days()
  }
  return (s / N)
}
MC_nr_days(10)

#III.1
Nr_days = function() {
  nr_days = 2
  last_errors = c(13, 15, 9)
  nr_errors = 13
  while (nr_errors > 0) {
    lambda = mean(last_errors)
    nr_errors = rpois(1, lambda)
    last_errors = c(nr_errors, last_errors[1:2])
    nr_days = nr_days + 1
  }
  return (nr_days)
}
MC_nr_days = function(N) {
  s = 0
  for (i in 1:N) {
    s = s + Nr_days()
  }
  return (s / N)
}
MC_nr_days(10)

#III.2
Nr_hours = function() {
  u = runif(1, 0, 1)
  chance = 3 / 4
  if (u < chance) {
    return (rexp(1, 12))
  } else {
    return (rexp(1, 4))
  }
}
MC_Nr_hours = function(N) {
  s = 0
  for (i in 1:N) {
    s = s + Nr_hours()
  }
  return (s / N)
}
MC_Nr_hours(10000)

#IV
Nr_days = function() {
  nr_days = 2
  last_errors = c(18, 22, 28)
  nr_errors = 18
  while (nr_errors > 0) {
    lambda = min(last_errors)
    nr_errors = rpois(1, lambda)
    last_errors = c(nr_errors, last_errors[1:2])
    nr_days = nr_days + 1
  }
  return (nr_days)
}
MC_nr_days_21 = function(N) {
  s = 0
  for (i in 1:N) {
    if (Nr_days() > 21) {
      s = s + 1
    }
  }
  return (s / N)
}
MC_nr_days_21(5000)

alfa = 1 - 0.95
z = qnorm(alfa / 2)
epsilon = 0.01
p = 0.5
N_min = p * (1 - p) * (z / epsilon) ^ 2
N_min
MC_nr_days_21(N_min + 1)

#IV.1
MC_prob = function(N) {
  s = 0
  for (i in 1:N) {
    x = rgeom(1, 0.3)
    y = rgeom(1, 0.5)
    if (x > y ^ 2) {
      s = s + 1
    }
  }
  return (s / N)
}

alfa = 1 - 0.95
z = qnorm(alfa / 2)
epsilon = 0.005
p = 0.5
N_min = p * (1 - p) * (z / epsilon) ^ 2
N_min
MC_prob(N_min + 1)

#IV.2
MC_infection = function(num_of_computers, inf_prob, k, expected_num) {
  infected = vector(mode = "logical", length = num_of_computers)
  infected[1] = TRUE
  infected[2:num_of_computers] = FALSE
  num_infected = 1
  while (num_infected < expected_num && num_infected > 0) {
    for (i in 1:num_of_computers) {
      if (infected[i] == FALSE) {
        infected[i] = sum(runif(num_infected, 0, 1) < inf_prob) != 0
      }
    }
    num_infected = sum(infected)
    if (num_infected < expected_num && num_infected  > 0) {
      index = ceiling(runif(1, 0, 4))
      for (i in 1:min(k[index], num_infected)) {
        removal = ceiling(runif(1, 0, num_infected))
        infected[which(infected)][removal] = FALSE;
        num_infected = num_infected - 1;
      }
    }
  }
  return (num_infected >= expected_num)
}
MC_infection(40, 0.2, c(4, 6, 8, 10), 40)

MC_prob = function(N, num_of_computers, inf_prob, k, expected_num) {
  s = 0;
  for (i in 1:N) {
    s = s + MC_infection(num_of_computers, inf_prob, k, expected_num)
  }
  return (s / N)
}
MC_prob(10000, 40, 0.2, c(4, 6, 8, 10), 40)
MC_prob(10000, 40, 0.2, c(4, 6, 8, 10), 15)

alfa = 1 - 0.95
z = qnorm(alfa / 2)
epsilon = 0.01
p = 0.5
N_min = p * (1 - p) * (z / epsilon) ^ 2
N_min
MC_prob(N_min + 1, 40, 0.2, c(4, 6, 8, 10), 40)
