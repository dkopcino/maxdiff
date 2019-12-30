// Hierarchical multinomial logit model with LCA
data {
  int<lower = 2> C; // # of alternatives (choices) in each scenario
  int<lower = 1> K; // # of covariates of alternatives
  int<lower = 1> R; // # of respondents
  int<lower = 1> S; // # of scenarios per respondent
  int<lower = 0> G; // # of respondent covariates 
  int<lower = 1, upper = C> Y[R, S]; // observed choices
  matrix[C, K] X[R, S]; // matrix of attributes for each obs
  int<lower=1,upper=C> XC[R, S]; // actual number of alternatives for each respondent/scenario
  matrix[G, R] Z; // vector of covariates for each respondent
  
  // LCA
  int<lower = 1> CL; // # of latent classes
}

parameters {
  matrix[K, CL] Beta;
  vector[K] BetaMu;
  corr_matrix[K] BetaOmega;
  vector<lower = 0>[K] BetaTau;

  matrix[CL, G] Gamma; // matrix of coefficients from classes to covariates
  vector[CL] GammaMu; // mean coefficients for Gamma
  corr_matrix[CL] GammaOmega;
  vector<lower = 0>[CL] GammaTau;
}

transformed parameters {
  cov_matrix[K] BetaSigma = quad_form_diag(BetaOmega, BetaTau);
  cov_matrix[CL] GammaSigma = quad_form_diag(GammaOmega, GammaTau);
  
  matrix[CL, R] Theta = Gamma * Z;
}

model {
  // to_vector(BetaMu) ~ normal(0, 10);
  // BetaTau ~ cauchy(0, 2.5);
  // BetaOmega ~ lkj_corr(2);
  // 
  // to_vector(GammaMu) ~ normal(0, 10);
  // GammaTau ~ cauchy(0, 2.5);
  // GammaOmega ~ lkj_corr(2);

  int rclass[R] = rep_array(1, R);

  to_vector(BetaMu) ~ normal(0, 5);
  BetaTau ~ normal(0, 5);
  BetaOmega ~ lkj_corr(5); // η=1: uniform, η>1: favoring less correlation, η<1: favoring more correlation
  
  to_vector(GammaMu) ~ normal(0, 1);
  GammaTau ~ normal(0, 1);
  GammaOmega ~ lkj_corr(5);
  
  for (g in 1:G) {
    Gamma[, g] ~ multi_normal(GammaMu, GammaSigma);
  }

  for (cl in 1:CL) {
    Beta[, cl] ~ multi_normal(BetaMu, BetaSigma);
  }

  for (r in 1:R) {
    rclass[r] ~ categorical_logit(Theta[, r]);
  }

  for (r in 1:R) {
    for (s in 1:S) {
      Y[r, s] ~ categorical_logit((X[r, s][1:XC[r,s], ]) * Beta[, rclass[r]]);
    }
  }
}

