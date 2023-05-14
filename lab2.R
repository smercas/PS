x = c(11, 14)
stem(x)
sample = scan("sample1.txt")
hist(sample, breaks = int)

#I.1
stem(sample)

#I.2
tablou = read.csv("unemploy2012.csv", header = T, sep = ';')
rate = tablou[['rate']]
interval2 = c(0, 4, 6, 8, 10, 12, 14, 30)
hist(rate, breaks = interval2, right = T, freq - T)

#I.3
life_expectancy = read.csv("life_expect.csv", header = T)
male_expectancy = life_expectancy[["male"]]
female_expectancy = life_expectancy[["female"]]
par(mfrow = c(1, 2))
hist(male_expectancy, breaks = seq(min(male_expectancy),
  max(male_expectancy), length = 8), right = F, freq = T,
  main = paste("Male"))
hist(female_expectancy, breaks = seq(min(female_expectancy),
  max(female_expectancy), length = 8), right = F, freq = T,
  main = paste("Female"))
#II.1
mean(sample)
median(sample)

#II.2
life_expectancy = read.csv("life_expect.csv", header = T)
male_exp = life_exp[["male"]]
female_exp = life_exp[["female"]]
mean(male_exp)
median(male_exp)
mean(female_exp)
median(female_exp)

#II.3

mode = function(sample) {
  values = vector()
  freq = vector()
  for (x in sample) {
    if (x %in% values) {
      index = which(values==x)
      freq[index] = freq[index] + 1
    }
    else {
      values = c(values, x)
      freq = c(freq, 1)
    }
  }
  freq_max = max(freq)
  print(values)
  print(freq)
  mode=values[freq==freq_max]
  return(mode)
}
sample=c(3, 6, 4, 3, 6, 7, 8, 5, 3, 6)
mode(sample)
table(sample)

sample = c(1, 91, 38, 72, 13, 27, 11, 85, 5, 22, 20, 19, 8, 17, 11, 15, 13, 23, 14, 17)
m = mean(sample)
s = sd(sample)
outliers = vector()
j = 0
for(i in 1:length(sample))
  if(sample[i] < m - 2 * s | sample[i] > m + 2 * s) {
    j = j + 1
    outliers[j] = sample[i]
  }
outliers

#III.1
outliers_mean = function(sample) {
  m = mean(sample)
  s = sd(sample)
  outliers = vector()
  j = 0
  for(i in 1:length(sample))
    if(sample[i] < m - 2 * s | sample[i] > m + 2 * s) {
      j = j + 1
      outliers[j] = sample[i]
    }
  return (outliers)
}
#outliers_mean(sample)

#III.2
outliers_iqr = function(sample) {
  q1 = as.vector(quantile(sample))[2]
  q3 = as.vector(quantile(sample))[4]
  iqr = q3 - q1
  outliers = vector()
  j = 0
  for(i in 1:length(sample))
    if(sample[i] < q1 - 1.5 * iqr | sample[i] > q3 + 1.5 * iqr) {
      j = j + 1
      outliers[j] = sample[i]
    }
  return (outliers)
}
#outliers_iqr(sample)

#III.3
sample = scan("sample2.txt")
summary(sample)
outliers_mean(sample)
outliers_iqr(sample)
