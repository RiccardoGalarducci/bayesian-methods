library(tidyverse)
library(rstan)
library(loo)

# 246759609 seed

data <- read_csv('group4_data2.csv')

data$site = as.factor(data$site)

model_intercept_slope_data <- list(
  N_obs = dim(data)[1],
  N_pts = nlevels(data$site),
  K = 1 + 1,
  pid = as.numeric(data$site),
  x = cbind(1, data$weeks),
  y = data$height
)


### Intercept and Slope 

model_intercept_slope_dep_fit <- stan(
  file = "hierarchical_normal_slope_dep.stan",
  data = model_intercept_slope_data,
  chains = 4,
  iter = 10000,
  warmup = 500,
  thin = 10
)

# Traceplots
rstan:: traceplot(model_intercept_slope_dep_fit, pars=c('beta_p[1,1]', 'beta_p[1,2]',
                                                        'beta_p[2,1]', 'beta_p[2,2]',
                                                        'beta_p[3,1]', 'beta_p[3,2]',
                                                        'beta_p[4,1]', 'beta_p[4,2]',
                                                        'beta_p[5,1]', 'beta_p[5,2]',
                                                        'beta_p[6,1]', 'beta_p[6,2]',
                                                        'beta_p[7,1]', 'beta_p[7,2]',
                                                        'sigma_p[1]', 'sigma_p[2]', 
                                                        'beta_0[1]', 'beta_0[2]', 
                                                        'Omega[1,1]', 'Omega[1,2]',
                                                        'Omega[2,1]', 'Omega[2,2]',
                                                        'sigma'))

#traceplot(model_intercept_slope_dep_fit) 

print(model_intercept_slope_dep_fit)
monitor(model_intercept_slope_dep_fit)

#Posterior distributions
post_params <- rstan :: extract(model_intercept_slope_dep_fit)

g_beta_1_1 <- ggplot(data = as.data.frame(post_params), mapping = aes(x = beta_p.1.1)) +
  geom_density(fill = "lightblue")
g_beta_1_2 <- ggplot(data = as.data.frame(post_params), mapping = aes(x = beta_p.1.2)) +
  geom_density(fill = "lightblue") 
g_beta_2_1 <- ggplot(data = as.data.frame(post_params), mapping = aes(x = beta_p.2.1)) +
  geom_density(fill = "lightblue") 
g_beta_2_2 <- ggplot(data = as.data.frame(post_params), mapping = aes(x = beta_p.2.2)) +
  geom_density(fill = "lightblue") 
g_beta_3_1 <- ggplot(data = as.data.frame(post_params), mapping = aes(x = beta_p.3.1)) +
  geom_density(fill = "lightblue") 
g_beta_3_2 <- ggplot(data = as.data.frame(post_params), mapping = aes(x = beta_p.3.2)) +
  geom_density(fill = "lightblue") 
g_beta_4_1 <- ggplot(data = as.data.frame(post_params), mapping = aes(x = beta_p.4.1)) +
  geom_density(fill = "lightblue") 
g_beta_4_2 <- ggplot(data = as.data.frame(post_params), mapping = aes(x = beta_p.4.1)) +
  geom_density(fill = "lightblue") 
g_beta_5_1 <- ggplot(data = as.data.frame(post_params), mapping = aes(x = beta_p.5.1)) +
  geom_density(fill = "lightblue") 
g_beta_5_2 <- ggplot(data = as.data.frame(post_params), mapping = aes(x = beta_p.5.2)) +
  geom_density(fill = "lightblue") 
g_beta_6_1 <- ggplot(data = as.data.frame(post_params), mapping = aes(x = beta_p.6.1)) +
  geom_density(fill = "lightblue") 
g_beta_6_2 <- ggplot(data = as.data.frame(post_params), mapping = aes(x = beta_p.6.2)) +
  geom_density(fill = "lightblue") 
g_beta_7_1 <- ggplot(data = as.data.frame(post_params), mapping = aes(x = beta_p.7.1)) +
  geom_density(fill = "lightblue") 
g_beta_7_2 <- ggplot(data = as.data.frame(post_params), mapping = aes(x = beta_p.7.2)) +
  geom_density(fill = "lightblue") 

g_sigma_p_1 <- ggplot(data = as.data.frame(post_params), mapping = aes(x = sigma_p.1)) +
  geom_density(fill = "lightblue")
g_sigma_p_2 <- ggplot(data = as.data.frame(post_params), mapping = aes(x = sigma_p.2)) +
  geom_density(fill = "lightblue")

g_beta0_1 <- ggplot(data = as.data.frame(post_params), mapping = aes(x = beta_0.1)) +
  geom_density(fill = "lightblue")
g_beta0_2 <- ggplot(data = as.data.frame(post_params), mapping = aes(x = beta_0.2)) +
  geom_density(fill = "lightblue")

g_sigma <- ggplot(data = as.data.frame(post_params), mapping = aes(x = sigma)) +
  geom_density(fill = "lightblue")

print(g_beta_1_1)
print(g_beta_1_2)
print(g_beta_2_1)
print(g_beta_2_2)
print(g_beta_3_1)
print(g_beta_3_2)
print(g_beta_4_1)
print(g_beta_4_2)
print(g_beta_5_1)
print(g_beta_5_2)
print(g_beta_6_1)
print(g_beta_6_2)
print(g_beta_7_1)
print(g_beta_7_2)
print(g_sigma_p_1)
print(g_sigma_p_2)
print(g_beta0_1)
print(g_beta0_2)
print(g_sigma)



#Prediction 1 #####

ynew_sample = rnorm(dim(post_params$beta_p)[1], mean = post_params$beta_p[,3,1] + post_params$beta_p[,3,2]*5, sd = post_params$sigma )
g_ynew <- ggplot(data = as.data.frame(ynew_sample), mapping = aes(x = ynew_sample)) +
  geom_density(fill = "lightblue")
print(g_ynew)



#Prediction 2 #####
sigma_p_Omega_sigma_p = array(0, dim = c(3800, 2, 2))
for(i in 1:3800){
  sigma_p_Omega_sigma_p[i,,] = diag(post_params$sigma_p[i,])%*%post_params$Omega[i,,]%*%diag(post_params$sigma_p[i,])
}

sample_beta_p = array(0,dim = c(3800,2))
for(i in 1:3800){
  sample_beta_p[i,] = rmvnorm(1, mean = post_params$beta_0[i,], sigma = sigma_p_Omega_sigma_p[i,,])
}

#sample_beta_p = rmvnorm(dim(post_params$beta_p)[1], mean = post_params$beta_0, sigma = sigma_p_Omega_sigma_p)

ynew_sample2 = rnorm(dim(post_params$beta_p)[1], mean = sample_beta_p[,1] + sample_beta_p[,2]*5, sd = post_params$sigma)

g_ynew2 <- ggplot(data = as.data.frame(ynew_sample2), mapping = aes(x = ynew_sample2)) +
  geom_density(fill = "lightblue")
print(g_ynew2)



