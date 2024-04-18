#Libraries
library(readr)
library(tidyverse)
library(rstan)
library(loo)
library(ggplot2)
library(HDInterval)

#Import data
data <- read.csv2("data01.csv", header=TRUE)


#Change names
names(data) = c('date','age','dist_metro','num_stores','lat','lon','price_sq')


#data$price_sq = 30+data$date*0.01+data$age*(-0.005)+data$dist_metro*(-0.005)+data$num_stores*0.03+data$lat*0.015+data$lon*(0.015)+rnorm(dim(data)[1],mean = 0, sd = 0.001)
hist(data$price_sq)


#lin_mod = lm(data$price_sq ~ data$date + data$age + data$dist_metro + data$num_stores + data$lat + data$lon)
#summary(lin_mod)


#Brief data inspection
plot(data$lon, data$lat)
hist(data$price_sq)
min(data$price_sq)

pairs(data)

#Regression model ####

##Fitting of the model #####

#Compute mean and sd of covariates to standardize the data
data = na.omit(data)
data_std <- data %>%
  summarise(across(everything(), list(mean = mean, sd = sd))) %>%
  pivot_longer(
    cols = everything(),
    names_to = c(".value", "stat"),
    names_pattern = "(.*)_(.*)"
  )
print(data_std)

library(corrplot)
corrplot(cor(data),method = "shade", type = "upper", tl.col = "black")

#Specify the data for rstan
model_data_full <- list(
  N = dim(data)[1],      #number of observation
  K = dim(data)[2] - 1,  #number of covariates
  x = select(data, -c(price_sq)),  #select the covariates
  y = data$price_sq            #select the response
)

#Create the simulation with rstan
model_fit <- stan(
  file = "linear_regression_uninfpriors.stan",
  data = model_data_full,
  chains = 4,
  iter = 10000,
  warmup = 500,
  thin = 10
)

## Diagnostics ####
#Look at the summary of the model : all rhat close to 1 and good effective size sample ratio
print(model_fit)

#Traceplots for all params of the model : well mixing chains
x11()
rstan::traceplot(model_fit)

#Extract the synthetic response
y_gen <- rstan::extract(model_fit, pars = "y_gen")[["y_gen"]] %>%
  t() %>%
  as_tibble()
#Each row of y_gen is a new synthetic dataset of the response

#Compute the mean of y from the data and check if it is well represented by the model
mean_y_gen <- tibble(mean_smpl = apply(y_gen, MARGIN = c(2), FUN = mean))
g2 <- ggplot(data = mean_y_gen, mapping = aes(x = mean_smpl)) +
  geom_density(fill = "lightblue") +
  geom_vline(xintercept = filter(data_std, stat == "mean")[["price_sq"]], colour = "red")
print(g2)


#Compute the std dev of y from the data and check if it is well represented by the model
sd_y_gen <- tibble(sd_smpl = apply(y_gen, MARGIN = c(2), FUN = sd))
g3 <- ggplot(data = sd_y_gen, mapping = aes(x = sd_smpl)) +
  geom_density(fill = "lightblue") +
  geom_vline(xintercept = filter(data_std, stat == "sd")[["price_sq"]], colour = "red")
print(g3)

#Compute the minimum of y from the data and check if it is well represented by the model
min_y_gen <- tibble(min_smpl = apply(y_gen, MARGIN = c(2), FUN = min))
g4 <- ggplot(data = min_y_gen, mapping = aes(x = min_smpl)) +
  geom_density(fill = "lightblue") +
  geom_vline(xintercept = min(data$price_sq), colour = "red")
print(g4)

#Compute the max of y from the data and check if it is well represented by the model
max_y_gen <- tibble(max_smpl = apply(y_gen, MARGIN = c(2), FUN = max))
g5 <- ggplot(data = max_y_gen, mapping = aes(x = max_smpl)) +
  geom_density(fill = "lightblue") +
  geom_vline(xintercept = max(data$price_sq), colour = "red")
print(g5)

#Compute the median of y from the data and check if it is well represented by the model
median_y_gen <- tibble(median_smpl = apply(y_gen, MARGIN = c(2), FUN = median))
g6 <- ggplot(data = median_y_gen, mapping = aes(x = median_smpl)) +
  geom_density(fill = "lightblue") +
  geom_vline(xintercept = median(data$price_sq), colour = "red")
print(g6)

## Simulated posterior distributions of the parameters

#Extract parameters of the model
params <- tibble(alpha = rstan::extract(model_fit, pars = "alpha")$alpha, beta = rstan::extract(model_fit, pars = c("beta_v"))[[1]])

#Plot of the simulated distributions
hist(params$alpha)
hist(params$beta[,1])
hist(params$beta[,2])
hist(params$beta[,3])
hist(params$beta[,4])
hist(params$beta[,5])
hist(params$beta[,6])

#Model plot
plot(model_fit)
#Credible intervals for params : da mettere a posto per vedere i params NON std

## Post predictive diagnostics

#Residuals
res_smpl <- matrix(NA, dim(y_gen)[2], dim(data)[1])
for(i in 1:dim(data)[1]) {
  res_smpl[, i] <- as.numeric(y_gen[i, ]) - data$price_sq[i]
}
res_smpl <- as_tibble(res_smpl)
colnames(res_smpl) <- paste("y", 1:dim(data)[1], sep = "")

#Residuals part 2 
res_smpl_g <- pivot_longer(res_smpl, cols = everything(), names_to = "observation", values_to = "residual")
g8 <- ggplot(data = res_smpl_g, mapping = aes(x = observation, y = residual)) + geom_boxplot() + geom_hline(yintercept = 0, color = "blue")
print(g8)

#Goodness of fit
loo_full <- loo(model_fit)
print(loo_full)

## Model comparison ####

#Create a second model using just year and coords
model_data_simple <- list(
  N = dim(data)[1],      #number of observation
  K = 3,  #number of covariates
  x = select(data, c(date,lat,lon)),  #select the covariates
  y = data$price_sq            #select the response
)

#Create the simulation with rstan
model_fit_siple <- stan(
  file = "Ex1.stan",
  data = model_data_simple,
  chains = 4,
  iter = 10000,
  warmup = 500,
  thin = 10
)

loo_simple = loo(model_fit_siple)

print(loo_compare(list(simple = loo_simple, full = loo_full)))
#Table: if abs(elpd_diff of simple model)> 2*se_diff than the simple is too simple and we use the full 
#Obs: The row of the full is all zeros since we are making the difference between its own terms

## Simulate a new observation ####
alpha_post = rstan::extract(model_fit, pars = 'alpha')[[1]]
beta_post = rstan::extract(model_fit, pars = 'beta_v')[[1]]
sigma = rstan::extract(model_fit, pars = 'sigma')[[1]]

dim_sample_ynew = 10000
alpha_post_sample = as.vector(sample(alpha_post, replace = TRUE, size = dim_sample_ynew))
beta1_post_sample = sample(beta_post[,1], replace = TRUE, size = dim_sample_ynew)
beta2_post_sample = sample(beta_post[,2], replace = TRUE, size = dim_sample_ynew)
beta3_post_sample = sample(beta_post[,3], replace = TRUE, size = dim_sample_ynew)
beta4_post_sample = sample(beta_post[,4], replace = TRUE, size = dim_sample_ynew)
beta5_post_sample = sample(beta_post[,5], replace = TRUE, size = dim_sample_ynew)
beta6_post_sample = sample(beta_post[,6], replace = TRUE, size = dim_sample_ynew)

ynew_sample = alpha_post_sample + 2013.5 * beta1_post_sample + 17 * beta2_post_sample + 1100 * beta3_post_sample + 4 * beta4_post_sample + 24.95 * beta5_post_sample + 121.5 * beta6_post_sample
hist(ynew_sample)
abline(v = 46, col = 'red', lwd = 2)

price = ynew_sample * 150
hdi(price, credMass = 0.95)
hist(price)



