// Standard logistic regression model with cluster RE's
// Author: Witold Wiecek
// July 2023

data {
  int<lower=0> N;
  int<lower=0> K;
  int death[N];
  int cluster_id[N];
  vector[N] treatment;
}

parameters {
  real baseline;
  real trt_effect;
  vector[K] eta;
  real<lower=0> cluster_sigma;
}

transformed parameters {
  vector[K] cluster_effect;
  cluster_effect = eta * cluster_sigma;
}

model {
  baseline ~ normal(-4.6, 10);
  trt_effect ~ normal(0, 5);
  cluster_sigma ~ cauchy(0, 2.5);
  eta ~ normal(0,1);
  death ~ bernoulli_logit(baseline + trt_effect * treatment + cluster_effect[cluster_id]);
}
