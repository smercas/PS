#I
#sphere_volume = pi * a ^ 2 / 2
#cube_volume = (2 * sqrt(a)) ^ 2 * a = 8 * a ^ 3
paraboloidal_dish_volume = function(N, a) {
  N_C = 0
  for (i in 1:N) {
    x1 = runif(1, -sqrt(a), sqrt(a))
    x2 = runif(1, -sqrt(a), sqrt(a))
    x3 = runif(1, 0, a)
    if (x1 * x1 + x2 * x2 <= x3) {
      N_C = N_C + 1
    }
  }
  return ((2 * sqrt(a)) ^ 2 * a * N_C / N)
}
a = 2
paraboloidal_dish_volume(100000, a)
MC_volume = paraboloidal_dish_volume(100000, a)
volume = pi * a ^ 2 / 2
abs_err = abs(MC_volume - volume)
rel_err = abs_err/volume
cat("estimated area is", MC_volume)
cat("absolute error is", abs_err)
cat("relative error is", rel_err)

#II
#quadrilateral_area = 2 * 3 + 1 * 3 / 2 + 1 * 3 / 2 = 9
#rectangle_area = 4 * 3 = 12
quadrilateral_area = function(N) {
  N_C = 0
  for (i in 1:N) {
    x = runif(1, 0, 4)
    y = runif(1, 0, 3)
    if (3 * y <= x + 6 && y <= 12 - 3 * x) {
      N_C = N_C + 1
    }
  }
  return (4 * 3 * N_C / N)
}
quadrilateral_area(100000)
MC_area = quadrilateral_area(100000)
area = 9
abs_err = abs(MC_area - area)
rel_err = abs_err/area
cat("estimated area is", MC_area)
cat("absolute error is", abs_err)
cat("relative error is", rel_err)

#III
#III.a
MC_integration = function(N, a, b) {
  sum = 0
  for (i in 1:N) {
    u = runif(1, a, b)
    sum = sum + (u + 1) / sqrt(4 - u)
  }
  return ((b - a) * sum / N)
}
MC_integration(10000, -1, 1)

MC_integral = MC_integration(10000, -1, 1)
integral = integrate(function(x) (x + 1) / sqrt(4 - x), -1, 1)$value
abs_err = abs(MC_integral - integral)
rel_err = abs_err / integral
cat("estimated area is", MC_integral)
cat("absolute error is", abs_err)
cat("relative error is", rel_err)

#III.b
MC_integration = function(N, a, b) {
  sum = 0
  for (i in 1:N) {
    u = runif(1, a, b)
    sum = sum + 1 / (u ^ 2 + 4)
  }
  return ((b - a) * sum / N)
}
MC_integration(10000, -50, 0)

MC_integral = MC_integration(10000, -50, 0)
integral = integrate(function(x) 1 / (x ^ 2 + 4), -Inf, 0)$value
abs_err = abs(MC_integral - integral)
rel_err = abs_err / integral
cat("estimated area is", MC_integral)
cat("absolute error is", abs_err)
cat("relative error is", rel_err)

#III.c
MC_integration = function(N, a, b) {
  sum = 0
  for (i in 1:N) {
    u = runif(1, a, b)
    sum = sum + u * exp(u)
  }
  return ((b - a) * sum / N)
}
MC_integration(10000, -50, 0)

MC_integral = MC_integration(10000, -50, 0)
integral = integrate(function(x) x * exp(x), -Inf, 0)$value
abs_err = abs(MC_integral - integral)
rel_err = abs_err / integral
cat("estimated area is", MC_integral)
cat("absolute error is", abs_err)
cat("relative error is", rel_err)

#IV
#IV.a
MC_fake = function(m, n, p, q) {
  fake_users = m
  days = 0
  while (fake_users > 0) {
    fake_users = fake_users + rbinom(1, n, p)
    i = fake_users
    while (i > 0) {
      if (runif(1, 0, 1) < q) {
        fake_users = fake_users - 1
      }
      i = i - 1;
    }
    days = days + 1
  }
  return (days)
}
MC_fake(100000, 500, 0.5, 0.01)

m = 100000
n = 500
p = 0.5
q = 0.01
#d = 40
fake_users = m
days = 0
while (fake_users > 0) { # && days < d
  cat("before:", fake_users, "\n")
  fake_users = fake_users + rbinom(1, n, p)
  i = fake_users
  while (i > 0) {
    if (runif(1, 0, 1) < q) {
      fake_users = fake_users - 1
    }
    i = i - 1;
  }
  days = days + 1
  cat("after", fake_users, "\n")
}
#fake_users
days
#for each iteration:
#rbinom(1, 500, 0.5) should return something like 250
#250-ish new users
#runif(1, 0, 1) < 0.01 returns true approx. 1% of the time
#1%-ish users are deleted
#if at some point there are 25000 fake users the deleted accounts will mostly be replaced
#if the number of fake users at the beginning of the loop is less than 25000 the number of fake users will increase until approx 25000

#IV.b
MC_fake_days = function(m, n, p, q, d) {
  fake_users = m
  days = 0
  while (fake_users > 0 && days < d) {
    fake_users = fake_users + rbinom(1, n, p)
    i = fake_users
    while (i > 0) {
      if (runif(1, 0, 1) < q) {
        fake_users = fake_users - 1
      }
      i = i - 1;
    }
    days = days + 1
  }
  return (max(fake_users, 0))
}
MC_fake_days(100000, 500, 0.5, 0.01, 40)

#IV.c
MC_prob = function(N, m, n, p, q, d) {
  s = 0;
  for (i in 1:N) {
    s = s + MC_fake_days(m, n, p, q, d)
  }
  return (s / N)
}
MC_prob(100, 100000, 500, 0.5, 0.01, 40)

alfa = 1 - 0.99
z = qnorm(alfa / 2)
epsilon = 0.01
p = 0.5
N_min = p * (1 - p) * (z / epsilon) ^ 2
N_min
MC_prob(N_min + 1, 100000, 500, 0.5, 0.01, 40)
