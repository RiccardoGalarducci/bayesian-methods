data {
    int<lower = 0> N;   // number of data items
    int<lower = 0> K;   // number of predictors
    matrix[N, K] x;   // predictor matrix
    vector[N] y;      // outcome vector
}

transformed data {
    real m_y;
    real<lower = 0> sd_y;
    vector[N] y_std;
    vector[K] m_x;
    vector<lower = 0>[K] sd_x;
    matrix[N, K] x_std;

    m_y = mean(y);
    sd_y = sd(y);
    y_std = (y - m_y) / sd_y;

    for(j in 1:K) {
        m_x[j] = mean(x[, j]);
        sd_x[j] = sd(x[, j]);
        x_std[, j] = (x[, j] - m_x[j]) / sd_x[j];
    }
}

parameters {
    real alpha_std;           // intercept
    vector[K] beta_std;       // coefficients for predictors
    real logsigma_std;       // error scale
}

transformed parameters {

}

model {
    y_std ~ normal_id_glm(x_std, alpha_std, beta_std, exp(logsigma_std));  // likelihood
}

generated quantities {
    real alpha;           // intercept
    vector[K] beta_v;       // coefficients for predictors
    real<lower = 0> sigma;
    vector[N] log_lik;
    vector[N] y_gen;

    beta_v = sd_y * beta_std ./ sd_x;
    alpha = sd_y * alpha_std + m_y - dot_product(beta_v, m_x);
    sigma = sd_y * exp(logsigma_std);

    for(i in 1:N) {
        log_lik[i] = normal_lpdf(y_std[i] | alpha_std + x_std[i, ] * beta_std, exp(logsigma_std));
        y_gen[i] = normal_rng(alpha + x[i, ] * beta_v, sigma);
    }
}
