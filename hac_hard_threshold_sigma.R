# THRESHOLD ESTIMATOR

hac_hard_threshold_sigma <- function(D, M = NULL, L = NULL, mask = NULL, c = 0, C = 1, step_size = 0.05) {
  # D: Tt×N data matrix
  # M: threshold constant (if NULL, selected via cross-validation)
  # L: lag length; if NULL, uses Newey–West rule
  # c, C: min/max bounds for M grid when cross-validating
  
  D <- as.matrix(D)
  Tt <- nrow(D)
  N <- ncol(D)
  if (is.null(L)) L <- floor(4 * (Tt / 100)^(2 / 9))
  L <- max(L, 0)
  
  # ---------- helper for cross-validation ----------
  CV_threshold <- function(S_u, X, Uhat, L, N, Tt, c, C) {
    K <- dim(X)[2]
    P <- round(log(Tt))
    T_J2 <- floor(Tt / P)
    cv_grid <- seq(c, C, by = step_size)
    rate <- if (L > 0) L * sqrt(log((L + 1) * N^2 * Tt) / Tt) else 0
    
    norms <- outer(1:N, 1:N, Vectorize(function(i, j) norm(S_u[, , i, j], "2")))
    obj <- numeric(length(cv_grid))
    
    for (q in seq_along(cv_grid)) {
      Mq <- cv_grid[q]
      lambda <- outer(1:N, 1:N, Vectorize(
        function(i, j) Mq * rate * sqrt(norm(S_u[, , i, i], "2") * norm(S_u[, , j, j], "2"))
      ))
      S_u_hard <- array(0, dim = dim(S_u))
      for (i in 1:N)
        for (j in 1:N)
          if (i == j || norms[i, j] > lambda[i, j])
            S_u_hard[, , i, j] <- S_u[, , i, j]
      
      Vhat_train <- apply(S_u_hard, c(1, 2), sum) / N
      diff_vals <- numeric(P)
      for (p in 1:P) {
        k <- (p - 1) * T_J2 + 1
        idx <- k:(k + T_J2 - 1)
        X_val <- X[idx, , , drop = FALSE]
        U_val <- Uhat[, idx, drop = FALSE]
        S_u1_val <- array(0, c(K, K, N, N))
        for (i in 1:N)
          for (j in 1:N)
            for (t in 1:T_J2)
              S_u1_val[, , i, j] <- S_u1_val[, , i, j] +
          tcrossprod(X_val[t, , i]) * U_val[i, t] * U_val[j, t]
        S_u1_val <- S_u1_val / T_J2
        S_u_val <- S_u1_val
        Vhat_val <- apply(S_u_val, c(1, 2), sum) / N
        diff_vals[p] <- norm(Vhat_train - Vhat_val, "F")^2
      }
      obj[q] <- mean(diff_vals)
    }
    M_CV <- cv_grid[which.min(obj)]
    list(M_CV = M_CV, obj = obj)
  }
  
  # ---------- compute kernel-weighted S_hat ----------
  S <- crossprod(D) / Tt # = t(D) %*% D / Tt
  if (L > 0)
    for (h in 1:L) {
      w <- 1 - h / (L + 1)
      G <- crossprod(D[(h + 1):Tt, , drop = FALSE], D[1:(Tt - h), , drop = FALSE])
      S <- S + (w / Tt) * (G + t(G))
    }
  
  # ---------- automatic M via CV if missing ----------
  if (is.null(M)) {
    # create simplified S_u, X, Uhat objects from D for CV
    K <- 1
    S_u <- array(S, c(1, 1, N, N))
    X <- array(D, c(Tt, 1, N))
    Uhat <- t(D)
    M <- CV_threshold(S_u, X, Uhat, L, N, Tt, c, C)$M_CV
    message(sprintf("Selected M via CV: %.2f", M))
  }
  
  # ---------- apply threshold ----------
  diag_vals <- diag(S)
  scale <- sqrt(abs(outer(diag_vals, diag_vals)))
  
  if (is.null(mask)) {
    omega_NT <- if (L > 0) L * sqrt(log((L + 1) * N^2 * Tt) / Tt) else 0
    Lambda <- M * omega_NT * scale
    mask <- abs(S) > Lambda
    diag(mask) <- TRUE
    mask[is.na(mask)] <- FALSE # required for L=0
  }
  
  sigma2_hat <- sum(S[mask]) / N
  
  list(sigma2_hat = sigma2_hat, S_hat = S, mask = mask, M = M, L = L)
}
