ppPoisson = function (halflife, n) {
  x = 1:n;
  y = dpois(x, halflife);
  barplot(y);
  return (y);
}
ppPoisson(0.1, 800)

ppGeometric = function (p, n) {
  x = 1:n
  y = dgeom(x, p);
  barplot(y);
  return (y);
}
ppGeometric(1/36, 800)
