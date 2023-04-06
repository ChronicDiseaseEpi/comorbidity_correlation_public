// This code is adapted from Ben Goodrich's https://github.com/stan-dev/example-models/blob/master/misc/multivariate-probit/probit-multi-good.stan
// Any mistakes are my own.

// Create multivariate probit function - this improves code readability and allows
// for easy conversion to reduce_sum within chain parallelisation if needed (not
// implemented here)

functions { 
   real multiprobit_lpmf(
   int[] y, // Integer array of binary response dimensions
   vector x, // Covariate vector
   matrix beta, // Matrix of coefficients
   real[] u, // Nuiance parameter to help with numerical instability
   int D, // Number of response dimensions
   matrix L_Omega // Cholesky factor for correlation matrix
   ){
      vector[D] mu; // Sum of linear predictor
      vector[D] z; // Continuous latent state
      vector[D] logLik; // Log likelihood
      real prev; // Boundary of previous dimension
      mu = beta * x; // Sum linear predictor
      prev = 0; // Starting boundary condition
     for (d in 1:D) { // Phi and inv_Phi may overflow and / or be numerically inaccurate
        real bound; // threshold at which utility = 0
        bound = Phi( -(mu[d] + prev) / L_Omega[d,d] );
        if (y[d] == 1) {
          real t;
          t = bound + (1 - bound) * u[d];
          z[d] = inv_Phi(t);       // implies utility is positive
          logLik[d] = log1m(bound);  // Jacobian adjustment
        }
        else {
          real t;
          t = bound * u[d];
          z[d] = inv_Phi(t);     // implies utility is negative
          logLik[d]= log(bound);  // Jacobian adjustment
        }
        if (d < D) prev = L_Omega[d+1,1:d] * head(z, d);
        // Jacobian adjustments imply z is truncated standard normal
        // thus utility --- mu + L_Omega * z --- is truncated multivariate normal
      }
      return sum(logLik);
   }
}
data {
  int<lower=1> K; // Number of covariates + intercept (e.g. intercept + age)
  int<lower=1> D; // Number of response dimensions
  int<lower=0> N; // Size of training set
  int<lower=0,upper=1> y[N,D]; // Response data for training set
  vector[K] x[N]; // Covariate data for training set (N K length vectors)
}
parameters {
  matrix[D,K] beta; // Covariate coefficients
  cholesky_factor_corr[D] L_Omega; // Cholesky factor for correlation matrix
  real<lower=0,upper=1> u[N,D]; // nuisance that absorbs inequality constraints
}
model {
  L_Omega ~ lkj_corr_cholesky(1); // Prior for correlation matrix prior
  to_vector(beta) ~ std_normal();// Prior for covariate coefficients
  // Fit model to training data
  for(m in 1:N){
    target += multiprobit_lpmf(y[m] | x[m], beta, u[m], D, L_Omega);
  }
}
generated quantities {
  corr_matrix[D] Omega;
  Omega = multiply_lower_tri_self_transpose(L_Omega); // Go from cholesky factor to full matrix
}
