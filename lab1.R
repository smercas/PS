#II
#creating a vector
  #concatenation of values
x = c(1, 3, 2, 15, 6, 21, 34, 54, 7)
x
x = c(T, T, F, T, F)
x
  #range of integers (a:b)==a a+1 ... b - 1 b
x = -5:13
x
  #sequence (seq(a, b, length = c))==a .. b(c values)
x = seq(-3, 3, length = 100)
x

#accessing a vector
x = c(23, 21, 32, 25, 34, 19, 32, 45, 67)
  #the i-th element: x[i] (starting from 1)
x[4]
  #elements from a to b: x[a:b] (starting form 1)
x[2:6]
  #all elements except the i-th: x[-i] (starting from 1)
x[-3]

#III
#arithmetic operations
sin(1)
log(2)
x = 3
x ^ 2
exp(x)

#vector operations (done componentwise)
x = c(1, 3, 2, 15, 6, 21, 34, 54, 7)
y = c(22, 11, 32, 25, 54, 13, 27, 36, 2)
x ^ 2
x + y

#other useful stuff
x = c(1, 3, 2, 15, 6, 21, 34, 54, 7)
length(x)
sort(x)
sqrt(x)
exp(x)
  #info about a certain function can be obtained
  #using help(function_name)

#IV
#graphics
x = seq(0.001, 10, length = 200)
y = log2(x)
plot(x, y, type = 'l', main = 'graphic', sub =
     'subtitle', xlab = 'x axis', ylab = 'y axis')

#V
#user defined functions
variance = function(x, p) {
  expectation = sum(p * x)
  variance = sum(p * (x - expectation)^2)
  return (variance)
}
y = c(23, 32, 31, 27, 27, 33, 25, 21)
q = c(1/8, 1/16, 1/8, 1/16, 1/8, 1/16, 1/8, 5/16)
variance(y, q)

vector_sqrt = function(x) {
  for (i in 1:length(x)) {
    if (x[i] > 0) {
      x [i] = sqrt(x[i])
    }
    else {
      x[i] = sqrt(-x[i])
    }
  }
  return (x)
}
vector_sqrt(y)

fix(variance) #used to modify a function

#VI
#manipulating data files

#simple array of data
x = scan("my_file");

#has headers
y = read.table("my_file", header = T)
x1 = y[['col1']]
x2 = y[['col2']]

#comma separated values
x = read.csv(file = "date.csv", header = T);

#Exercises:
#1
b = scan(n = 7)
min(b)
max(b)
mean(b)
sum(b)
min(b)/max(b)
length(b[b >= 40])
length(b[b < 40])/length(b)
#2
x = c(1, 3, 2, 15, 6, 21, 34, 54, 7)

a = function(x) {
  return (x / sum(x))
}
a(x)

b = function(x) {
  return ((x - min(x)) / max(x))
}
b(x)

c = function(x) {
  for (i in 1:(length(x) - 1)) {
    result[i] = sum(x[1:i] / sum(x[(i + 1):length(x)]))
  }
  return (result)
}
c(x)

d = function(x) {
  for (i in 1:(length(x) - 1)) {
    result[i] = max(x[1:i] / max(x[(i + 1):length(x)]))
  }
  return (result)
}
d(x)

#3
af = function(name) {
  x = read.table(name, header = T)
  return (x[["vector"]] / sum(x[["vector"]]))
}

bf = function(name) {
  x = read.table(name, header = T)
  return ((x[["vector"]] - min(x[["vector"]])) /
  max(x[["vector"]]))
}

cf = function(name) {
  x = read.table(name, header = T)
  for (i in 1:(length(x[["vector"]]) - 1)) {
    result[i] = sum(x[["vector"]][1:i] /
    sum(x[["vector"]][(i + 1):length(x[["vector"]])]))
  }
  return (result)
}

df = function(name) {
  x = read.table(name, header = T)
  for (i in 1:(length(x[["vector"]]) - 1)) {
    result[i] = max(x[["vector"]][1:i] /
    max(x[["vector"]][(i + 1):length(x[["vector"]])]))
  }
  return (result)
}
af("vector.txt")
bf("vector.txt")
cf("vector.txt")
df("vector.txt")

#4.
binom = function(n, p) {
  x = dbinom(0:n, n, p)
  print(x)
  barplot(x, space = 0)
}
binom(18, 0.25)
binom(40, 0.5)
binom(30, 0.8)

#5.a
binom_max = function(n, p) {
  return (max(dbinom(0:n, n, p)))
}
binom_max(18, 0.25)
binom_max(40, 0.5)
binom_max(30, 0.8)

#5.b
binom_sum_1 = function(n, p, k) {
  return (sum(dbinom(0:k, n, p)))
}
binom_sum_1(18, 0.25, 9)
binom_sum_1(40, 0.5, 15)
binom_sum_1(30, 0.8, 12)

#5.c
binom_sum_2 = function(n, p, k, m) {
  return (sum(dbinom(k:m, n, p)))
}
binom_sum_2(18, 0.25, 9, 15)
binom_sum_2(40, 0.5, 15, 30)
binom_sum_2(30, 0.8, 12, 19)

#6.a
geom_1 = function(p, n) {
  return (sum(dgeom(0:n, p)))
}

#6.b
geom_2 = function(p, n) {
  return (1 - sum(dgeom(0:n, p)))
}
geom_2(0.65, 20)

#7.a
poiss_1 = function(lambda, n) {
  return (sum(dpois(0:(n - 1), lambda)))
}

#7.b
poiss_1 = function(lambda, n) {
  return (max(dpois(0:(n - 1), lambda)))
}

#7.c
poiss_1 = function(lambda, m) {
  return (ppois(m), lambda)
}

#8.a
plot_pairs = function(file_name) {
  data = read.table(file_name, header = TRUE)
  x = data[["AA"]]
  y = data[["BB"]]
  plot(x, y, main = "Pairs (xi, yi)", xlab = "x", ylab = "y", col = "blue", pch = 16)
}
plot_pairs("test.txt")

#8.b
compute_product = function(file_name) {
  data = read.table(file_name, header = TRUE)
  x = data[["AA"]]
  y = data[["BB"]]
  product = x * y
  return(product)
}
compute_product("test.txt")

#8.c
compute_difference = function(file_name) {
  data = read.table(file_name, header = TRUE)
  x = data[["AA"]]
  y = data[["BB"]]
  difference = abs(x - y) / sum((x - y) ^ 2)
  return (difference)
}
compute_difference("test.txt")

#8.d
find_closest_point = function(file_name, p, q) {
  data <- read.table(file_name, header = TRUE)
  x = data[["AA"]]
  y = data[["BB"]]
  distances = sqrt((x - p) ^ 2 + (y - q) ^ 2)
  closest_index = which.min(distances)
  closest_point = c(x[closest_index], y[closest_index])
  return (closest_point)
}
find_closest_point("test.txt", 15, 14)

#9
ppPoisson = function (halflife, n) {
  x = 1:n;
  y = dpois(x, halflife);
  barplot(y);
  return (y);
}
ppPoisson(0.1, 800)

#10
ppGeometric = function (p, n) {
  x = 1:n
  y = dgeom(x, p);
  barplot(y);
  return (y);
}
ppGeometric(1/36, 800);
