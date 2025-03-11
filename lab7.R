library(tidyverse)
library(cumstats)
library(patchwork)
library(stringr)

# Task 1: Describe the Population Distribution

distribution = function(alpha, beta){
  q1.fig.dat = tibble(x = seq(-0.25, 1.25, length.out=1000))|> # generate a grid of points
    mutate(beta.pdf = dbeta(x, alpha, beta))
      
  dist = ggplot(data= q1.fig.dat)+ # specify data
    geom_line(aes(x=x, y=beta.pdf, color = paste("Beta Dist(Alpha = ", alpha, ", Beta = ", beta, ")", sep = ""))) + # plot beta dist
    geom_hline(yintercept=0)+ # plot x axis
    theme_bw()+ # change theme
    xlab("x")+ # label x axis
    ylab("Density") + # label y axis
    labs(color = " ")
  return(dist)
}


(beta.2.5 = distribution(2,5))

(beta.5.5 = distribution(5,5))

(beta.5.2 = distribution(5,2))

(beta.0.5 = distribution(0.5,0.5))


charac = function(alpha, beta){
  mean = alpha / (alpha + beta)
  var = (alpha * beta) / ((alpha + beta)^2 * (alpha+beta+1))
  skew = (2*(beta-alpha) * sqrt(alpha + beta + 1)) / ((alpha + beta + 2) * sqrt(alpha*beta))
  kurt = 6*((alpha-beta)^2 * (alpha + beta + 1) - (alpha*beta)*(alpha + beta + 2)) / 
    ((alpha*beta)*(alpha + beta + 2)*(alpha + beta + 3))
  summary = data.frame(alpha = alpha, beta = beta, mean = mean, 
                      var = var, skew = skew, kurt = kurt)
  return(summary)
}

beta.charac = bind_rows(
  charac(2,5),
  charac(5,5),
  charac(5,2),
  charac(0.5,0.5)
)

# view(beta.charac)

# Task 2

beta.moment = function(alpha, beta, k, centered){
  
  if(centered == F){
    integrand = function (x) {(x^k)*dbeta(x, alpha, beta)}
    int = integrate(integrand, lower = 0, upper = 1)
    return(int)
  }
  else{
    integrand = function (x) {(x -  (alpha/(alpha+beta)))^k * dbeta(x, alpha, beta)}
    int = integrate(integrand, lower = 0, upper =  1)
    return(int)
  }
}

# Mean
x1 = beta.moment(2, 5, 1, F)[1]
x1 = as.numeric(x1)
x1

# Variance
x2 = beta.moment(2, 5, 2, T)[1]
x2 = as.numeric(x2)
x2

# Skew        
x3 = beta.moment(2, 5, 3, T)[1]
x3 = as.numeric(x3)
y3 = beta.moment(2, 5, 2, T)[1]
y3 = as.numeric(y3)
skew = x3/y3^(3/2)
skew

# Kurtosis
x4 = beta.moment(2, 5, 4, T)[1]
x4 = as.numeric(x4)
kurtosis = (x4 / x2^2) - 3
kurtosis

# Task 3

sample.histo = function(alpha, beta){
  set.seed(7272) # Set seed so we all get the same results.
  sample.size = 500 # Specify sample details
  beta.sample = rbeta(n = sample.size, # sample size
                       shape1 = alpha, # alpha parameter
                       shape2 = beta) |>
    as_tibble() |>
    rename(x = value)
  
  dist = ggplot(data = beta.sample, aes(x = x)) +
    geom_histogram(aes(y = after_stat(density)), breaks = seq(0,1,0.1)) + 
    geom_density(color = "blue") + 
    geom_line(aes(x=x, y=dbeta(x, alpha, beta), color = paste("Beta Dist(Alpha = ", alpha, ", Beta = ", 
                                                 beta, ")", sep = "")))+ # plot beta dist
    geom_hline(yintercept=0)+
    theme_bw()+ # change theme
    xlab("x")+ # label x axis
    ylab("Density") + # label y axis
    labs(color = "")
    
  return(dist)
    
}


plot1 = sample.histo(2,5)
plot2 = sample.histo(5,5)
plot3 = sample.histo(5,2)
plot4 = sample.histo(0.5,0.5)

(plot1 | plot2 / plot3 | plot4)

compute.sample = function(alpha, beta){
  set.seed(7272)
  sample.size = 500
  beta.sample = rbeta(n = sample.size, # sample size
                      shape1 = alpha, # alpha parameter
                      shape2 = beta) |>
    as_tibble() |>
    rename(x = value)
  
  summary.stats = beta.sample |>
    summarize(
      mean = mean(x),
      variance = var(x),
      skew = mean((x - mean(x))^3) / (var(x))^(2/3),
      kurt = mean((x - mean(x))^4) / (var(x)^2) - 3
    )
  return(summary.stats)
}

sample.summary = bind_rows(
  tibble(alpha = 2, beta = 5, compute.sample(2,5)),
  tibble(alpha = 5, beta = 5, compute.sample(5,5)),
  tibble(alpha = 5, beta = 2, compute.sample(5,2)),
  tibble(alpha = 0.5, beta = 0.5, compute.sample(0.5,0.5))
)

view(sample.summary)

# Task 4
set.seed(7272)
beta.sample = rbeta(500, 2, 5) |>
  as_tibble() |>
  rename(x = value)

cum.stats = beta.sample |>
  mutate(
    n = row_number(),
    cum.mean = cummean(x),
    cum.var = cumvar(x),
    cum.skew = cumskew(x),
    cum.kurt = cumkurt(x)
  )

pl1 = ggplot(cum.stats, aes(x = n, y = cum.mean)) +
  geom_line() +
  geom_hline(yintercept = charac(2,5)$mean) +
  theme_bw()
pl2 = ggplot(cum.stats, aes(x = n, y = cum.var)) +
  geom_line() +
  geom_hline(yintercept = charac(2,5)$var) +
  theme_bw()
pl3 = ggplot(cum.stats, aes(x = n, y = cum.skew)) +
  geom_line() +
  geom_hline(yintercept = charac(2,5)$skew) +
  theme_bw()
pl4 = ggplot(cum.stats, aes(x = n, y = cum.kurt)) +
  geom_line() +
  geom_hline(yintercept = charac(2,5)$kurt) +
  theme_bw()

(pl1 | pl2) / (pl3 | pl4)


# Task 5

sample = 500
result = tibble(mean = numeric(), variance = numeric(), skew = numeric(), kurt = numeric())

for (i in 1:1000) {
  set.seed(7272+i)
  beta.sample = rbeta(sample, 2, 5)
  mean = mean(beta.sample)
  var = var(beta.sample)
  skew = mean((beta.sample - mean)^3) / (var)^(3/2)
  kurt = mean((beta.sample - mean)^4) / (var)^2 - 3
  
  result = bind_rows(result, tibble(mean = mean, variance = var, skew = skew, kurt = kurt))
}

mean.plot = ggplot(result, aes(x = mean)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "blue") +
  geom_density(color = "red") +
  theme_bw() +
  ggtitle("Histogram of Sample Means")

var.plot = ggplot(result, aes(x = variance)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "blue") +
  geom_density(color = "red") +
  theme_bw() +
  ggtitle("Histogram of Sample Variances")

skew.plot = ggplot(result, aes(x = skew)) + 
  geom_histogram(aes(y = ..density..), bins = 30, fill = "blue") +
  geom_density(color = "red") +
  theme_bw() +
  ggtitle("Histogram of Sample Skewness")

kurt.plot = ggplot(result, aes(x = kurt)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "blue") +
  geom_density(color = "red") +
  theme_bw() +
  ggtitle("Histogram of Sample Kurtosis")

(mean.plot | var.plot) / (skew.plot | kurt.plot)
  

# Task 6

data = read_csv("DeathData/Data.csv")
view(data)
data = data |>
  select("Country Name", "2022") |>
  mutate(`2022` = `2022` / 1000) |>
  rename(`Death Rate 2022` = "2022") |>
  filter(!is.na(`Death Rate 2022`))

view(data)

# Task 7

library(nleqslv)

###################
# MOM
###################
MOM.beta = function(data, par){
  alpha = par[1]
  beta = par[2]
  EX1 = alpha / (alpha + beta)
  m1 = mean(data)
  EX2 = ((alpha + 1) * alpha) / ((alpha + beta + 1) * (alpha + beta))
  m2 = mean(data^2)
  
  return(c(EX1 - m1, EX2 - m2)) # Goal: find lambda so this is 0
}

nleqslv(x = c(2,3), # guess
        fn = MOM.beta,
        data=data$`Death Rate 2022`)

###################
# MLE
###################
MLE.beta = function(data, par, neg=FALSE){
  alpha = par[1]
  beta = par[2]
  loglik = sum(log(dbeta(x=data, alpha = alpha, beta = beta)))
  
  return(ifelse(neg, -loglik, loglik))
}
help(optim)
optim(par = c(2,5),
      fn = MLE.beta,
      data=data$`Death Rate 2022`,
      method = "Brent",
      lower = 0,
      upper = 1,
      neg = T)

