data{
  int <lower=1> T;
  int <lower=1> P;
  matrix[T, P] X;
  vector[T] y;
  real<lower=0> alpha2;
  real<lower=0> beta2;
  vector[2] phiPriorTransf;
  vector[2] sigmaEpsPrior;
  vector[2] xiPrior;
}
             
parameters{
  vector<lower = 0, upper = 1>[P] phiTransf;
  
  real<lower = 0, upper = 1> uZero;
  vector<lower = 0, upper = 1>[P] uJ;
  
  matrix<lower = 0, upper = 1>[T-1, P] x;
  vector[P] hZeroNonCent;

  matrix[T-1,P] ZBetas;
  vector[P] betaZeroNonCent;

  real<lower = 0> sigmaEps;
  
  vector<lower = 0>[P] xi;
}

transformed parameters{
  vector[P] phi;
  
  real tauZero;
  vector[P] tauJ;
  
  vector[P] mu;
  
  matrix[T-1, P] eta;
  
  matrix[T, P] h;
  matrix[T, P] betas;
  vector[P] betaZeroReal;
  vector[P] hZeroReal;
  
  phi = (2*phiTransf)-1;
  
  tauZero = (sigmaEps/sqrt(T*P))*tan((pi()/2)*uZero);
  // we used the s * tan(...), like in the derivation I did!
  tauJ = tan((pi()/2)*uJ); // 1 * tan, but one doesn't matter...
  
  mu = 2*log(tauZero) + 2*log(tauJ);
  
  hZeroReal = mu + hZeroNonCent./sqrt(xi);
  
  betaZeroReal = exp(hZeroReal/2).*betaZeroNonCent; //mean = 0
  
  h[1, ] = to_row_vector(hZeroReal);
  betas[1, ] = to_row_vector(betaZeroReal);

  eta = logit(x);
  
  for (t in 2:T){
    for(p in 1:P){
      h[t,p] = mu[p] + phi[p]*(h[t-1,p] - mu[p]) + eta[t-1,p];
    }
  }
  
  for (t in 2:T){
    for(p in 1:P){
      betas[t,p] = betas[t-1,p] + exp(h[t,p]/2)*ZBetas[t-1,p];
    }
  }

}

model{
  sigmaEps ~ scaled_inv_chi_square(sigmaEpsPrior[1], sigmaEpsPrior[2]); //Note that this is the SD of the epsilon
  
  phiTransf ~ beta(phiPriorTransf[1], phiPriorTransf[2]);
  
  uZero ~ uniform(0,1);
  uJ ~ uniform(0,1);
  
  xi ~ lognormal(xiPrior[1], xiPrior[2]);
  
  for (t in 2:T){
    for (p in 1:P){
      x[t-1,p] ~ beta(alpha2, beta2);
    }
  }
  
  
  hZeroNonCent ~ std_normal(); 
  
  betaZeroNonCent ~ std_normal();
  
  for (t in 2:T){
    for (p in 1:P){
      ZBetas[t-1,p] ~ std_normal();
    }
  }
  
  y ~ normal(rows_dot_product(X, betas), sigmaEps);
  
}

generated quantities {
  array[T] real yPred;
  
  yPred = normal_rng(rows_dot_product(X, betas), sigmaEps);
  
}


