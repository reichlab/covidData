functions {
  // forward declaration for recursion
  matrix[] basis_calculation_internal(
    int n_x,
    int order,
    int n_aug_knots,
    vector aug_knots,
    matrix x_m_aug_knot,
    int return_deriv
  );
  
  // n_interior_knots: number of interior knots
  // boundary_knots: vector of two boundary knots
  // interior_knots: vector of n_interior_knots interior knots
  // order: order of splines, e.g. order 4 is degree 3 splines
  // n_x: number of locations at which to evaluate the spline basis
  // x: locations at which to evaluate the spline basis
  // natural: 1 if natural splines, 0 if not
  //
    // return: n_x by (n_interior_knots + 8 - order) matrix of knots
  matrix bspline_basis(
    int n_x,
    vector x,
    int order,
    int n_interior_knots,
    vector boundary_knots,
    vector interior_knots,
    int natural
  ) {
    int n_aug_knots = n_interior_knots + 8;
    vector[n_aug_knots] aug_knots;
    matrix[n_x, n_aug_knots] x_m_aug_knot;
    matrix[2, n_aug_knots] boundary_m_aug_knot;
    
    int n_basis = n_aug_knots - order;
    matrix[n_x, n_basis] basis[1];
    matrix[2, n_basis] boundary_basis_and_deriv[2];
    
    // set up augmented knots: repeat boundary_knots 4 times at edges
    for(i in 1:4) {
      aug_knots[i] = boundary_knots[1];
      aug_knots[4 + n_interior_knots + i] = boundary_knots[2];
    }
    for(i in 1:n_interior_knots) {
      aug_knots[i+4] = interior_knots[i];
    }
    
    // matrix of differences between x and augmented knots
    for(i in 1:n_aug_knots) {
      x_m_aug_knot[:, i] = x - aug_knots[i];
    }
    
    basis = basis_calculation_internal(
      n_x,
      order,
      n_aug_knots,
      aug_knots,
      x_m_aug_knot,
      0 // return_deriv
    );
    if(natural == 1) {
      // matrix of differences between boundary knots and augmented knots
      for(i in 1:n_aug_knots) {
        boundary_m_aug_knot[:, i] = boundary_knots - aug_knots[i];
      }
      
      // values of basis functions and their derivatives at the boundaries
      boundary_basis_and_deriv = basis_calculation_internal(
        2, // n_x
        order,
        n_aug_knots,
        aug_knots,
        boundary_m_aug_knot, // x_m_aug_knot
        1 // return_deriv
      );
      
      // for observations beyond boundary knots, add linearization of
      // basis functions
      for(i in 1:n_x) {
        for(j in 1:n_basis) {
          if(x[i] < boundary_knots[1]) {
            basis[1, i, j] = basis[1, i, j] +
              boundary_basis_and_deriv[1, 1, j] +
              (x[i] - boundary_knots[1]) * boundary_basis_and_deriv[2, 1, j];
          }
          
          if(x[i] >= boundary_knots[2]) {
            basis[1, i, j] = basis[1, i, j] +
              boundary_basis_and_deriv[1, 2, j] +
              (x[i] - boundary_knots[2]) * boundary_basis_and_deriv[2, 2, j];
          }
        }
      }
    }
    
    return basis[1];
  }
  
  matrix[] basis_calculation_internal(
    int n_x,
    int order,
    int n_aug_knots,
    vector aug_knots,
    matrix x_m_aug_knot,
    int is_boundary
  ) {
    int n_basis = n_aug_knots - order;
    
    // array of 1 or 2 matrices
    // basis_and_deriv[1] is the basis
    // if is_boundary is 1, basis_and_deriv[2] is the derivative of the basis
    matrix[n_x, n_basis] basis_and_deriv[is_boundary+1];
    
    // similar structure as above, but for the basis with next lower order
    // (which has 1 more basis function)
    matrix[n_x, n_basis + 1] prev_basis_and_deriv[is_boundary+1];
    
    // structure for intermediate quantities used in getting from previous order
    // to current order
    matrix[n_x, n_basis + 1] basis_div_knot_diff[is_boundary+1];
    
    if(order == 1) {
      // different behavior needed depending on whether this calculation is for
      // the boundary points
      if(is_boundary) {
        // initialize to 0
        for(k in 1:(is_boundary+1)) {
          for(i in 1:n_x) {
            for(j in 1:n_basis) {
              basis_and_deriv[k, i, j] = 0.0;
            }
          }
        }
        
        // only nonzero entries are at the first boundary knots for basis
        basis_and_deriv[1, 1, 4] = 1.0;
        basis_and_deriv[1, 2, n_basis - 4 + 1] = 1.0;
      } else { // not boundary
        // interior basis calculations
        for(i in 1:n_x) {
          for(j in 1:n_basis) {
            basis_and_deriv[1, i, j] =
              x_m_aug_knot[i, j] >= 0.0 && x_m_aug_knot[i, j + 1] < 0.0;
          }
        }
      }
    } else { // order > 1
      // calculate for one lower order
      prev_basis_and_deriv = basis_calculation_internal(
        n_x,
        order-1,
        n_aug_knots,
        aug_knots,
        x_m_aug_knot,
        is_boundary
      );
      
      // pre-calculate quantities used repeatedly below
      for(j in 1:(n_basis + 1)) {
        real knot_diff = aug_knots[j + order - 1] - aug_knots[j];
        if(knot_diff > 0.00000001) {
          for(k in 1:(is_boundary + 1)) {
            for(i in 1:n_x) {
              basis_div_knot_diff[k, i, j] =
                prev_basis_and_deriv[k, i, j] / knot_diff;
            }
          }
        }
      }
      
      // actual calculation of basis functions and their derivatives
      // see Eq 5.78 in Elements of Statistical Learning 2E
      for(j in 1:n_basis) {
        // term 1
        if(aug_knots[j + order - 1] - aug_knots[j] > 0.00000001) {
          // calculation for basis
          for(i in 1:n_x) {
            basis_and_deriv[1, i, j] = x_m_aug_knot[i, j]
            * basis_div_knot_diff[1, i, j];
          }
          
          // calculation for derivative, if applicable
          if(is_boundary) {
            for(i in 1:n_x) {
              basis_and_deriv[2, i, j] = basis_div_knot_diff[1, i, j]
              + x_m_aug_knot[i, j] * basis_div_knot_diff[2, i, j];
            }
          }
        } else {
          // set all to 0.0
          for(k in 1:(is_boundary + 1)) {
            for(i in 1:n_x) {
              basis_and_deriv[k, i, j] = 0.0;
            }
          }
        }
        
        // term 2
        if(aug_knots[j + order] - aug_knots[j + 1] > 0.00000001) {
          // calculation for basis
          for(i in 1:n_x) {
            basis_and_deriv[1, i, j] -= x_m_aug_knot[i, j + order]
            * basis_div_knot_diff[1, i, j + 1];
          }
          
          // calculation for derivative, if applicable
          if(is_boundary) {
            for(i in 1:n_x) {
              basis_and_deriv[2, i, j] -= basis_div_knot_diff[1, i, j + 1]
              + x_m_aug_knot[i, j + order] * basis_div_knot_diff[2, i, j + 1];
            }
          }
        }
      }
    }
    
    return basis_and_deriv;
  }
}

data {
  // number of observation times; assumed to be evenly spaced 1 time unit apart
  int<lower=0> T;
  
  // incident deaths or cases at observed times, as counts
  int y[T];
  
  // knots for spline
  // first two positions are boundary knots, remaining are interior knots
  int spline_order;
  int n_interior_knots;
  vector[n_interior_knots] interior_knots;
  vector[2] boundary_knots;
  
  // number of time points forward to forecast
  int forecast_horizon;
  
  // number of simulations to use for forecasting
  int nsim;
}

transformed data {
  vector[T+forecast_horizon] ts;
  
  int n_basis = n_interior_knots + 8 - spline_order;
  matrix[T + forecast_horizon, n_basis] basis;
  
  // time points with observations or to forecast
  for(t in 1:(T+forecast_horizon)) {
    ts[t] = t;
  }
  
  // b-spline basis function matrix
  basis = bspline_basis(
    (T + forecast_horizon), // n_x, number of time points at which to evaluate
    ts, // x, time points at which we evaluate
    spline_order, // order, order of b-spline basis
    n_interior_knots,
    boundary_knots,
    interior_knots,
    1 // natural, natural b-spline basis
  );
}

parameters {
  // beta vector
  real beta_mean;
  real beta_sd;
  vector[n_basis] raw_beta;
  
  // gamma vector
  real gamma_mean;
  real gamma_sd;
  vector[6] raw_gamma;
  
  // negative binomial dispersion paramter
  real phi_mean;
  real phi_sd;
  real raw_phi;
}

transformed parameters {
  // variable definitions
  // beta vector
  
  vector[7] gamma;
  vector[n_basis] beta;

  
  // parameters of nb distribution
  real y_mean[T+forecast_horizon];
  real y_mean_with_daily[T+forecast_horizon];
  real phi;
  
  // variable calculations
  {
    int day_index = 0;
    real gamma_sum = 0.0;
    
    // this is a numerically stable calculation of
    // beta = log{1 + exp(beta_mean + beta_sd * raw_beta)};
    for(i in 1:n_basis) {
      beta[i] = fma(beta_sd, raw_beta[i], beta_mean);
    }
    
    for(i in 1:6) {
      gamma[i] = log1p_exp(fma(gamma_sd, raw_gamma[i], 0.5413));
      gamma_sum += gamma[i];
    }
    
    gamma[7] = 1.0;
    gamma_sum += 1.0;
    
    for(i in 1:7) {
      gamma[i] = 7.0 * gamma[i]/gamma_sum;
    }
    
    y_mean = to_array_1d(basis * beta);
    for(i in 1:(T+forecast_horizon)) {
      day_index += 1;
      if(day_index > 7) {
        day_index = 1;
      }
      y_mean[i] = log1p_exp(y_mean[i]);
      y_mean_with_daily[i] = y_mean[i] * gamma[day_index] + 0.1;
    }
    
    // negative binomial dispersion
    // this is a numerically stable calculation of
    // phi = log{1 + exp(phi_mean + phi_sd * raw_phi)};
    phi = log1p_exp(fma(phi_sd, raw_phi, phi_mean)) + 0.01;
  }
}

model {
  // priors
  raw_beta ~ normal(0, 1);
  //  beta_mean ~ normal(0.0, 10.0);
  //  beta_sd ~ gamma(1.0, 10.0);
  
  raw_gamma ~ normal(0, 1);
  
  raw_phi ~ normal(0, 1);
  //  phi_mean ~ normal(0.0, 10.0);
  //  phi_sd ~ gamma(1.0, 10.0);
  
  // data model
  for(t in 1:T) {
    //    y[t] ~ poisson(y_mean[t]);
    y[t] ~ neg_binomial_2(y_mean_with_daily[t], phi * y_mean_with_daily[t]);
  }
}

// generated quantities {
//   // data model
//   matrix[nsim, T+forecast_horizon] y_pred;
//   for(i in 1:nsim) {
//     for(t in 1:(T+forecast_horizon)) {
//       //      y_pred[t, i] = poisson_rng(y_mean[t]);
//       y_pred[i, t] = neg_binomial_2_rng(y_mean_with_daily[t], phi * y_mean_with_daily[t]);
//     }
//   }
// }
