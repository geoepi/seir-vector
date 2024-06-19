functions {
  real[] seirsei(real t, real[] y, real[] theta, 
             real[] x_r, int[] x_i) {

      real N_h = x_i[1];
     
      real beta_h = theta[1];
      real gamma_h = theta[2];
      real sigma_h = theta[3];
      real sigma_v = theta[4];
      real rho_h = theta[5];
	  real rho_v = theta[6];
      real beta_v = theta[7];
      real mu_v = theta[8];
	  real upsilon_h = theta[9];
	  real upsilon_v = theta[10];
      real i_0 = theta[11];
      real e_0 = theta[12];
      real ev_0 = theta[13];
      real iv_0 = theta[14];
      real N_v = theta[15];
      
      real init[7] = {N_h - i_0 - e_0, e_0, i_0, 0, N_v - iv_0 - ev_0, ev_0, iv_0}; 
      real Sh = y[1] + init[1];
      real Eh = y[2] + init[2];
      real Ih = y[3] + init[3];
      real Rh = y[4] + init[4];
      real Sv = y[5] + init[5];
      real Ev = y[6] + init[6];
      real Iv = y[7] + init[7];
	  
	  real lambda_h = upsilon_h * beta_h * Iv / N_v;
      real lambda_v = upsilon_v * beta_v * Ih / N_h;
	  
	  real dS_dt = -lambda_h * Sh;
      real dE_dt =  lambda_h * Sh - sigma_h * Eh;
      real dI_dt = sigma_h * Eh*rho_h - gamma_h * Ih;
      real dR_dt =  gamma_h * Ih;
	  
	  real dSv_dt = mu_v*N_v - lambda_v*Sv - mu_v*Sv;
      real dEv_dt = lambda_v*Sv - (sigma_v + mu_v)*Ev;
      real dIv_dt =  sigma_v * Ev*rho_v - mu_v*Iv;
     
      return {dS_dt, dE_dt, dI_dt, dR_dt, dSv_dt, dEv_dt, dIv_dt};
  }
}
data {
  int<lower=1> n_days;
  real t0;
  real ts[n_days];
  int N_h;
  int N_v;
  int cases[n_days];
}
transformed data {
  real x_r[0];
  int x_i[1] = { N_h };
}
parameters {
  real<lower=0, upper=1> beta_h;
  real<lower=0> gamma_h;
  real<lower=0> sigma_h;
  real<lower=0> sigma_v;
  real<lower=0, upper=1> rho_h;
  real<lower=0, upper=1> rho_v;
  real<lower=0, upper=1> beta_v;
  real<lower=0> mu_v;
  real<lower=0> upsilon_h;
  real<lower=0> upsilon_v;
  real<lower=0> phi_inv;
  real<lower=0, upper=1> kappa;
  real<lower=0> i_0; 
  real<lower=0> e_0; 
  real<lower=0> ev_0; 
  real<lower=0> iv_0; 
}
transformed parameters{
  real y[n_days, 7];
  real incidence[n_days - 1];
  real phi = 1. / phi_inv;
  real theta[15] = {beta_h, gamma_h, sigma_h, sigma_v, rho_h, rho_v, beta_v, mu_v, upsilon_h, upsilon_v, i_0, e_0, ev_0, iv_0, N_v};
  y = integrate_ode_rk45(seirsei, rep_array(0.0, 7), t0, ts, theta, x_r, x_i);
  for (i in 1:n_days-1){
    incidence[i] = -(y[i+1, 2] - y[i, 2] + y[i+1, 1] - y[i, 1])*kappa + 1e-8;
  }
}
model {
  beta_h ~ beta(10, 1);
  gamma_h ~ lognormal(log(0.15), 0.1);  
  sigma_h ~ lognormal(log(0.17), 0.1);
  sigma_v ~ lognormal(log(0.3), 0.1);
  rho_h ~ beta(10, 100);
  rho_v ~ beta(5, 10); 
  beta_v ~ beta(1, 10);
  mu_v ~ lognormal(log(0.1), 0.1);
  upsilon_h ~ lognormal(log(0.25), 0.1);
  upsilon_v ~ lognormal(log(0.15), 0.1);
  phi_inv ~ exponential(2);
  kappa ~ beta(15, 100);
  i_0 ~ lognormal(0, 0.1);
  e_0 ~ lognormal(0, 0.1);
  ev_0 ~ lognormal(0, 0.1);
  iv_0 ~ lognormal(0, 0.1);

  cases[1:(n_days-1)] ~ neg_binomial_2(incidence, phi);
}
generated quantities {
  real duration = 1 / gamma_h;
  real incub_h = 1 / sigma_h;
  real incub_v = 1/ sigma_v;
  real log_lik[n_days-1];
  real pred_infected[n_days-1];
  
  pred_infected = neg_binomial_2_rng(incidence, phi);
  
  for (i in 1:(n_days - 1)) {
    log_lik[i] = neg_binomial_2_lpmf(cases[i] | incidence[i], phi);
  }
}
