///SV-Noncente
data {
  int <lower=1> T;
  int <lower=1> P;
  matrix[T, P] X;
  vector[T] y;
  real<lower=0> alpha2;
  real<lower=0> beta2;
  vector[2] phiPriorTransf;
  vector[2] xiPrior;
  
  //SV
  vector[2] muPriorSV;
  vector[2] phiTransfPriorSV;
  vector[2] varianceEtaPriorSV;
}


parameters {
  vector<lower = 0, upper = 1>[P] phiTransf;
  
  real<lower = 0, upper = 1> u;
  
  matrix<lower = 0, upper = 1>[T-1, P] x;
  vector[P] hZeroNonCent;

  matrix[T-1,P] ZBetas;
  vector[P] betaZeroNonCent;
  
  vector<lower = 0>[P] xi;
  
  //SV
  real muSV;
  real<lower = 0, upper = 1> phiTransfSV;
  real<lower=0> varianceEtaSV; 
  real Z1SV;
  vector[T-1] ZSV;
  
  }

transformed parameters{
  vector[P] phi;
  
  real tau;
  
  real mu;
  
  matrix[T-1, P] eta;
  
  matrix[T, P] h;
  matrix[T, P] betas;
  vector[P] betaZeroReal;
  vector[P] hZeroReal;
  
  //SV
  real phiSV;
  vector[T] htSV;
  
  phi = (2*phiTransf)-1;
  
  tau = (1/sqrt(T))*tan((pi()/2)*u);
  
  mu = 2*log(tau);
  
  hZeroReal = mu + hZeroNonCent./sqrt(xi);
  
  betaZeroReal = exp(hZeroReal/2).*betaZeroNonCent; //mean = 0
  
  h[1, ] = to_row_vector(hZeroReal);
  betas[1, ] = to_row_vector(betaZeroReal);

  eta = logit(x);
  
  for (t in 2:T){
    for(p in 1:P){
      h[t,p] = mu + phi[p]*(h[t-1,p] - mu) + eta[t-1,p];
    }
  }
  
  for (t in 2:T){
    for(p in 1:P){
      betas[t,p] = betas[t-1,p] + exp(h[t,p]/2)*ZBetas[t-1,p];
    }
  }
  
  phiSV = (2*phiTransfSV)-1;
  
  htSV[1] = muSV + sqrt(varianceEtaSV/(1-square(phiSV)))*Z1SV;
  
  for(t in 2:T){
    htSV[t] = (muSV + (phiSV*(htSV[t-1]- muSV))) + (sqrt(varianceEtaSV)*ZSV[t-1]); 
  }
}

model {
  phiTransf ~ beta(phiPriorTransf[1], phiPriorTransf[2]);
  
  u ~ uniform(0,1);

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
  
  y ~ normal(rows_dot_product(X, betas), exp(htSV/2));
  
  //SV
  muSV ~ normal(muPriorSV[1], muPriorSV[2]);
  phiTransfSV ~ beta(phiTransfPriorSV[1], phiTransfPriorSV[2]);
  varianceEtaSV ~ gamma(varianceEtaPriorSV[1], varianceEtaPriorSV[2]);
  
  Z1SV ~ std_normal();
  
  for (t in 2:T){
    ZSV[t-1] ~ std_normal();
  }
  
  
}

generated quantities {
  array[T] real yPred;
  
  yPred = normal_rng(rows_dot_product(X, betas), exp(htSV/2));
  
}
