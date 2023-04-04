ppPoisson = function (halflife, n) {
  x = 1:n;
  y = dpois(x, halflife);
  barplot(y);
  return (y);
}
ppGeometric = function (p, n) {
  x = 1:n
  y = dgeom(x, p);
  barplot(y);
  return (y);
}
x = scan("my_file.txt");

ppGeometric(1/36, 800);
ppPoisson(0.1, 800)


