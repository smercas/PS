#I
sample(49, 6)
sample(7:49, 6)
x = c(2.1, 3.2, 2.3, 2.5, 3.1, 2.9, 2.6, 2.2, 3.3)
sample(x, 5)
sample(7:10, 6, replace = TRUE)
sample(10)
runif(10, 2, 4.5)

#II
matrix_product = function(A, B, C) {
  n = nrow(A);
  r = matrix(0 , nrow = n, ncol = 1);
  x = matrix(0 , nrow = n, ncol = 1);
  y = matrix(0 , nrow = n, ncol = 1);
  r = sample(0:1, n, replace = TRUE);
  for(i in 1:nrow(B)) {# x = Br
    x[i] = 0;
    for(j in 1:ncol(B)) {
      x[i] = (x[i] + B[i,j] * r[j]) %% 2;
    }
  }
  for(i in 1:nrow(A)) {# y = Ax = ABr
    y[i] = 0;
    for(j in 1:ncol(A)) {
      y[i] = (y[i] + A[i,j] * x[j]) %% 2;
    }
  }
  for(i in 1:nrow(C)) {# x = Cr
    x[i] = 0;
    for(j in 1:ncol(C)) {
      x[i] = (x[i] + C[i,j] * r[j]) %% 2;
    }
  }
  for(i in 1:n) {# verify if ABr==Cr
    if(y[i] != x[i]) {
      return (FALSE);
    }
  }
  return (TRUE);
}
x = sample(0:1, 9, replace = TRUE)
y = sample(0:1, 9, replace = TRUE)
z = sample(0:1, 9, replace = TRUE)
A = matrix(x, 3, 3)
B = matrix(y, 3, 3)
C = matrix(z, 3, 3)
A * B
C
matrix_product(A, B, C)

matrix_product_reduce = function(A, B, C, k) {
  for(i in 1:k) {
    if(!matrix_product(A, B, C)) {
      return(FALSE);
    }
  }
  return (TRUE);
}
matrix_product_reduce(A, B, C, 10)

tree_eval = function(i, leaves) {
  a = runif(1, 0, 1);
  len = length(leaves);
  if (log(i,2) >= log(len,2) - 1) { # the children of node i are leaves
    if (a <= 0.5) {
      if (leaves[2 * i - len + 1] == 0) {
        return (leaves[2 * i + 1 - len + 1]);
      }
      return (1);
    }
    else {
      if(leaves[2 * i - len + 2] == 0) {
        return(leaves[2 * i - len + 1]);
      }
      return(1);
    }
  }
  if ((floor(log(i,2)) %% 2 == 0)) {# the node i is a MIN one
    if (a <= 0.5) {
      if (tree_eval(2 * i, leaves) == 1) {
        return (tree_eval(2 * i + 1, leaves));
      }
      return (0);
    }
    else {
      if (tree_eval(2 * i + 1, leaves) == 1) {
        return (tree_eval(2 * i, leaves));
      }
      return (0);
    }
  }
  else {# the node i is a MAX one
    if (a <= 0.5) {
      if (tree_eval(2 * i, leaves) == 0) {
        return (tree_eval(2 * i + 1, leaves));
      }
      return (1);
    }
    else {
      if (tree_eval(2 * i + 1, leaves) == 0) {
        return (tree_eval(2 * i, leaves));
      }
      return (1);
    }
  }
}
leaves = c(0, 1, 0, 1, 1, 1, 1, 0, 0, 1, 0, 1, 1, 0, 0, 0)
tree_eval(1, leaves)

#II.1
custom_random_variable = function(X, P) {
  i = 1;
  sum = 0;
  prob = runif(1, 0, 1)
  while (sum + P[i] < prob) {
    sum = sum + P[i]
    i = i + 1
  }
  return (X[i])
}
custom_random_variable(c(  1,   2,   3,   4,   5,   6,   7,   8,   9,  10),
                       c(0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1))
custom_random_variable_tries = function(X, P, n) {
  probs = vector(mode = "numeric", length = length(P))
  for (i in 1:length(X)) {
    probs[i] = 0
  }
  for (i in 1:n) {
    value = custom_random_variable(X, P)
    probs[which(X == value)] = probs[which(X == value)] + 1
  }
  for (i in 1:length(X)) {
    probs[i] = probs[i] / n
  }
  return (probs)
}
custom_random_variable_tries(c(  1,   2,   3,   4,   5,   6,   7,   8,   9,  10),
                             c(0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1),
                             100000)