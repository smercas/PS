#I.a
threeplots = function(k, n, p, lambda) {
  x = k:n
  y_pois = dpois(x, lambda)
  y_geom = dgeom(x, p)
  y_binom = dbinom(x, n, p)
  
  plot(x,                                           #x-axis values
       y_pois,                                      #y-axis values
       type = "l",                                  #lines
       main = "Mass Probability Functions",         #title
       lwd = 2,                                     #line width(optional graphics glitter)
       lty = 1,                                     #line type
       col = "blue",                                #color
       ylim = c(0, max(y_pois, y_geom, y_binom)),   #lower and upper limit for the y axis
       xlab = "k:n",                                #x-axis title
       ylab = "Probability")                        #y-axis title
  lines(x, y_geom, type = "l", lwd = 2, lty = 2, col = "red")
  lines(x, y_binom, type = "l", lwd = 2, lty = 3, col = "green")
  legend("topright",                                #position within the plot
         c("Poisson", "Geometric", "Binomial"),     #names
         lty = c(1, 2, 3),                          #line type(1 - solid, 2 - dashed, 3 - dotted)
         lwd = 2,                                   #line width(same as before)
         col = c("blue", "red", "green"))           #colors
}
threeplots(3, 10, 0.4, 1)

#I.b
#P(X == odd) = P(x == 1) + P(x == 3) + ... = (1 - p) ^ 0 * p + (1 - p) ^ 2 * p + ... =
#            = p * sum(k = 0, k < inf, (1 - p) ^ (2k)) = p / (1 - 1 + 2 * p - p ^ 2) =
#            = p / (2 * p - p ^ 2) = 1 / (2 - p)
#P(X == a) = (1 - p) ^ (a - 1) * p
geom_stuff = function(p) {
  cat("P(x = odd) = ", 1 / (2 - p), "\n")
  cat("P(x >= 4) = ", pgeom(4, p, lower.tail = FALSE) + (1 - p) ^ 3 * p, "\n")
  cat("P(x <= 20) = ", pgeom(20, p), "\n")
}
geom_stuff(0.5)

#I.c
#P(X == a) = lambda ^ a * e ^ (-lambda) / a!
pois_stuff = function(lambda) {
  k = 0
  limit = 10 ^ (-7)
  x_eq_a = exp(-lambda)
  while (ppois(k, lambda, lower.tail = FALSE) + x_eq_a < limit) {
    k = k + 1;
    x_eq_a = x_eq_a * lambda / k
  }
  cat("The least value of k0 such that P(Y >= k0) < 10 ^ (-7) is:", k, "\n")
}
pois_stuff(10000000)

#II.a
calculate_statistics <- function(file_name) {
  data = read.csv(file_name)
  sample_P = data[["P"]]
  sample_S = data[["S"]]
  cat("Statistics for sample P:\n")
  cat("Median:", median(sample_P), "\n")
  cat("Mean:", mean(sample_P), "\n")
  cat("Standard Deviation:", sd(sample_P), "\n")
  cat("Quartiles:\n")
  cat("Q1:", quantile(sample_P, 0.25), "\n")
  cat("Q2:", quantile(sample_P, 0.5), "\n")
  cat("Q3:", quantile(sample_P, 0.75), "\n")
  cat("\n")
  cat("Statistics for sample S:\n")
  cat("Median:", median(sample_S), "\n")
  cat("Mean:", mean(sample_S), "\n")
  cat("Standard Deviation:", sd(sample_S), "\n")
  cat("Quartiles:\n")
  cat("Q1:", quantile(sample_S, 0.25), "\n")
  cat("Q2:", quantile(sample_S, 0.5), "\n")
  cat("Q3:", quantile(sample_S, 0.75), "\n")
}
calculate_statistics("note.csv")

#II.b
remove_outliers <- function(file_name, sample_name) {
  data = read.csv(file_name)
  sample = data[[sample_name]]
  Q1 = quantile(sample, 0.25)
  Q3 = quantile(sample, 0.75)
  IQR = Q3 - Q1
  lower_bound = Q1 - 1.5 * IQR
  upper_bound = Q3 + 1.5 * IQR
  cat(lower_bound, " : ", upper_bound, "\n")
  trimmed_sample = sample[sample >= lower_bound & sample <= upper_bound]
  return (trimmed_sample)
}
remove_outliers("note.csv", "P")
remove_outliers("note.csv", "S")

#II.c
plot_frequency_distribution <- function(file_name) {
  par(mfrow = c(1, 2))
  hist(remove_outliers(file_name, "P"),
       breaks = seq(1, 10, by = 1),
       main = "Sample P Frequency Distribution",
       xlab = "Values",
       ylab = "Frequency",
       col = "red",
       border = "brown")
  hist(remove_outliers(file_name, "S"),
       breaks = seq(1, 10, by = 1),
       main = "Sample S Frequency Distribution",
       xlab = "Values",
       ylab = "Frequency",
       col = "red",
       border = "brown")
}
plot_frequency_distribution("note.csv")
