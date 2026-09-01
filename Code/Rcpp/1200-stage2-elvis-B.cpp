// Stage-2 ELVIS, moment set B -- split out from 1200-stage2-elvis.cpp
// (2026-08-31) specifically so this file (and only this file) can take on
// an Accelerate/CBLAS dependency for a post-loop speedup, without forcing
// the main file (old 15-moment system + moment set A, both already
// validated at real scale) to require Accelerate too. See CLAUDE.md's
// 2026-08-31 entries for the full measurement/decision trail: cov(Ghat) in
// R's cue_objective_common was ~96% of the R-side per-BOBYQA-call cost
// (17.4ms of 18.3ms) and ~12% of total per-call time including the C++
// Metropolis sampler; a raw cblas_dsyrk call was ~13.7x faster (1.28ms)
// verified numerically identical to R's cov() (relative diff ~1e-12 in the
// covariance matrix itself, ~3e-13 in the downstream objective value).
//
// [[Rcpp::depends(RcppParallel)]]
#include "1200-stage2-elvis-common.h"
// CBLAS header only (not the full <Accelerate/Accelerate.h> umbrella): that
// umbrella pulls in vDSP.h, which redefines COMPLEX as a typedef, colliding
// with R's own Rinternals.h (COMPLEX as a function) -- a real compile error
// hit and fixed in-session, not a hypothetical. Needs
// Sys.setenv(PKG_LIBS="-framework Accelerate") before sourceCpp() on this
// file specifically -- that's why this file is split from the main one.
#include <vecLib/cblas.h>

// ---- Moment set B ------------------------------------------------------
// g_out is a caller-allocated std::vector<double> sized 8+J+6 (d_g varies at
// runtime with J = number of industries in the run sample, so this cannot
// use a compile-time std::array like GVecA/GVec in the main file); allocated
// ONCE per firm (outside the MCMC step loop) in the worker below, not per
// step.
static void moment_g_B_one(
    double M, double Mstar, double V, double Wt, double tau_rho,
    double beta, double lambda, double delta0, double delta1, double delta2,
    double mu_omega_i, double sigma_omega_i,
    int industry_idx_i, const RVector<double>& mu_m, int J,
    std::vector<double>& g_out
) {
    double e     = e_of_M(M, Mstar);
    double eps   = eps_of_M(M, Mstar, V);
    double om    = omega_of_M(M, Mstar, V, Wt, beta);
    double psi   = h_of_e(e, tau_rho, lambda) - delta0 + delta1 * om - delta2 * om * om;
    double lnM   = std::log(M);
    double mu_m_i = mu_m[industry_idx_i];
    double lnM_c  = lnM - mu_m_i;
    double om_c   = om - mu_omega_i;
    double om2_c  = om * om - sigma_omega_i;

    g_out[0] = psi;
    g_out[1] = eps;
    g_out[2] = psi * lnM;
    g_out[3] = psi * om;
    g_out[4] = psi * om * om;
    g_out[5] = eps * lnM;
    g_out[6] = eps * e;
    g_out[7] = eps * om;

    for (int jj = 0; jj < J; jj++) g_out[8 + jj] = 0.0;
    g_out[8 + industry_idx_i] = lnM_c;

    int base = 8 + J;
    g_out[base + 0] = om_c;
    g_out[base + 1] = om2_c;
    g_out[base + 2] = psi * om_c;
    g_out[base + 3] = psi * om2_c;
    g_out[base + 4] = psi * lnM_c;
    g_out[base + 5] = psi * eps * om;
}

struct TiltedMomentWorkerB : public Worker {
    const RVector<double>  Mstar, V, Wt, tau_rho, beta, mu_omega, sigma_omega, gamma, mu_m;
    const RVector<int>     row_id, corner, industry_idx;
    const double lambda, delta0, delta1, delta2, eta;
    const int n_burn, n_keep, base_seed, J, d_g;
    RMatrix<double> Ghat;

    TiltedMomentWorkerB(
        const NumericVector& Mstar_, const NumericVector& V_, const NumericVector& Wt_, const NumericVector& tau_rho_,
        const NumericVector& beta_, const NumericVector& mu_omega_, const NumericVector& sigma_omega_,
        const IntegerVector& row_id_, const IntegerVector& corner_, const IntegerVector& industry_idx_,
        double lambda_, double delta0_, double delta1_, double delta2_, double eta_,
        const NumericVector& gamma_, const NumericVector& mu_m_,
        int n_burn_, int n_keep_, int base_seed_, int J_, int d_g_,
        NumericMatrix& Ghat_
    ) : Mstar(Mstar_), V(V_), Wt(Wt_), tau_rho(tau_rho_), beta(beta_),
        mu_omega(mu_omega_), sigma_omega(sigma_omega_), gamma(gamma_), mu_m(mu_m_),
        row_id(row_id_), corner(corner_), industry_idx(industry_idx_),
        lambda(lambda_), delta0(delta0_), delta1(delta1_), delta2(delta2_), eta(eta_),
        n_burn(n_burn_), n_keep(n_keep_), base_seed(base_seed_), J(J_), d_g(d_g_),
        Ghat(Ghat_) {}

    void operator()(std::size_t begin, std::size_t end) {
        for (std::size_t ii = begin; ii < end; ii++) {
            int i = static_cast<int>(ii);
            int idx = industry_idx[i];

            if (corner[i] == 1) {
                double eps_pt = eps_of_M(Mstar[i], Mstar[i], V[i]);
                double om_pt  = omega_of_M(Mstar[i], Mstar[i], V[i], Wt[i], beta[i]);
                double lnM_pt = std::log(Mstar[i]);
                for (int t = 0; t < d_g; t++) Ghat(i, t) = 0.0;
                Ghat(i, 1) = eps_pt;
                Ghat(i, 5) = eps_pt * lnM_pt;
                Ghat(i, 7) = eps_pt * om_pt;
                Ghat(i, 8 + idx)   = lnM_pt - mu_m[idx];
                Ghat(i, 8 + J + 0) = om_pt - mu_omega[i];
                Ghat(i, 8 + J + 1) = om_pt * om_pt - sigma_omega[i];
                continue;
            }

            std::mt19937_64 rng(static_cast<uint64_t>(base_seed) + static_cast<uint64_t>(row_id[i]));
            std::uniform_real_distribution<double> unif(0.0, 1.0);

            std::vector<double> g_current(d_g), g_try(d_g), g_run(d_g, 0.0);
            double M_current = draw_from_rho_eta(unif(rng), Mstar[i], lambda, eta);
            moment_g_B_one(M_current, Mstar[i], V[i], Wt[i], tau_rho[i], beta[i], lambda,
                           delta0, delta1, delta2, mu_omega[i], sigma_omega[i], idx, mu_m, J, g_current);

            for (int r = -n_burn + 1; r <= n_keep; r++) {
                double M_try = draw_from_rho_eta(unif(rng), Mstar[i], lambda, eta);
                moment_g_B_one(M_try, Mstar[i], V[i], Wt[i], tau_rho[i], beta[i], lambda,
                               delta0, delta1, delta2, mu_omega[i], sigma_omega[i], idx, mu_m, J, g_try);

                double log_ratio = 0.0;
                for (int t = 0; t < d_g; t++) log_ratio += gamma[t] * (g_try[t] - g_current[t]);

                if (std::log(unif(rng)) < log_ratio) {
                    g_current = g_try;
                }
                if (r > 0) {
                    for (int t = 0; t < d_g; t++) g_run[t] += g_current[t] / n_keep;
                }
            }
            for (int t = 0; t < d_g; t++) Ghat(i, t) = g_run[t];
        }
    }
};

// [[Rcpp::export]]
NumericMatrix mh_tilted_average_B_cpp(
    NumericVector Mstar, NumericVector V, NumericVector Wt, NumericVector tau_rho,
    NumericVector beta, NumericVector mu_omega, NumericVector sigma_omega,
    IntegerVector row_id, IntegerVector corner, IntegerVector industry_idx,  // industry_idx: 0-based, < J
    double lambda, double delta0, double delta1, double delta2, double eta,
    NumericVector gamma, NumericVector mu_m,   // mu_m: length J, one per industry
    int n_burn, int n_keep,
    int base_seed = 20260829
) {
    int n = Mstar.size();
    int J = mu_m.size();
    int d_g = 8 + J + 6;
    if (gamma.size() != d_g) stop("gamma length must equal 8+J+6 (moment set B)");
    if (corner.size() != n) stop("corner length must equal n");
    if (row_id.size() != n) stop("row_id length must equal n");
    if (industry_idx.size() != n) stop("industry_idx length must equal n");
    if (mu_omega.size() != n || sigma_omega.size() != n) stop("mu_omega/sigma_omega length must equal n");
    if (eta < 0.0 || eta >= 1.0) stop("eta must be in [0,1)");

    NumericMatrix Ghat(n, d_g);
    TiltedMomentWorkerB worker(
        Mstar, V, Wt, tau_rho, beta, mu_omega, sigma_omega,
        row_id, corner, industry_idx,
        lambda, delta0, delta1, delta2, eta, gamma, mu_m,
        n_burn, n_keep, base_seed, J, d_g, Ghat
    );
    RcppParallel::parallelFor(0, n, worker);
    return Ghat;
}

// ---- Post-loop fusion: dvec + Omega via Accelerate, Ghat never returned --
// (2026-08-31) Same Metropolis chain as mh_tilted_average_B_cpp above (the
// TiltedMomentWorkerB call below is byte-for-byte identical to it) -- the
// ONLY change is what happens after Ghat is built: instead of returning the
// full n x d_g matrix to R (32,378 x 43 here, ~11MB) just so R's
// cue_objective_common can immediately consume it and discard it, this
// computes dvec=colMeans(Ghat) and Omega=cov(Ghat) HERE, in C++, and returns
// only those two small objects (43-vector, 43x43 matrix). R still does
// eigen(Omega)+the final division (cue_objective_common, unchanged) --
// eigen() is already cheap (~0.11ms, measured) and Omega/dvec are small
// enough that marshalling them back to R costs nothing worth optimizing;
// the actual win is (i) never marshalling the LARGE Ghat matrix at all, and
// (ii) computing cov() via cblas_dsyrk instead of R's own cov() -- measured
// 17.5ms -> 1.28ms on the real problem shape, verified bit-close (objective
// value matches to ~3e-13 relative difference; see CLAUDE.md's 2026-08-31
// entries for the full measurement trail before this was written).
//
// Uses the symmetric rank-k update (dsyrk), not a general dgemm, because
// Omega=X'X/(n-1) is symmetric by construction -- half the FLOPs, and the
// mathematically correct routine for this shape (matches the earlier note
// that dgesv/general solves were never right for this problem's covariance
// matrices).
//
// [[Rcpp::export]]
List mh_tilted_moments_B_cpp(
    NumericVector Mstar, NumericVector V, NumericVector Wt, NumericVector tau_rho,
    NumericVector beta, NumericVector mu_omega, NumericVector sigma_omega,
    IntegerVector row_id, IntegerVector corner, IntegerVector industry_idx,
    double lambda, double delta0, double delta1, double delta2, double eta,
    NumericVector gamma, NumericVector mu_m,
    int n_burn, int n_keep,
    int base_seed = 20260829
) {
    int n = Mstar.size();
    int J = mu_m.size();
    int d_g = 8 + J + 6;
    if (gamma.size() != d_g) stop("gamma length must equal 8+J+6 (moment set B)");
    if (corner.size() != n) stop("corner length must equal n");
    if (row_id.size() != n) stop("row_id length must equal n");
    if (industry_idx.size() != n) stop("industry_idx length must equal n");
    if (mu_omega.size() != n || sigma_omega.size() != n) stop("mu_omega/sigma_omega length must equal n");
    if (eta < 0.0 || eta >= 1.0) stop("eta must be in [0,1)");

    NumericMatrix Ghat(n, d_g);   // local -- never returned to R
    TiltedMomentWorkerB worker(
        Mstar, V, Wt, tau_rho, beta, mu_omega, sigma_omega,
        row_id, corner, industry_idx,
        lambda, delta0, delta1, delta2, eta, gamma, mu_m,
        n_burn, n_keep, base_seed, J, d_g, Ghat
    );
    RcppParallel::parallelFor(0, n, worker);

    // dvec = colMeans(Ghat)
    NumericVector dvec(d_g);
    for (int j = 0; j < d_g; j++) {
        double s = 0.0;
        const double* col = Ghat.begin() + (size_t)j * n;
        for (int i = 0; i < n; i++) s += col[i];
        dvec[j] = s / n;
    }

    // center a working copy, column-major (matches Ghat's own layout)
    std::vector<double> Xc((size_t)n * d_g);
    std::copy(Ghat.begin(), Ghat.end(), Xc.begin());
    for (int j = 0; j < d_g; j++) {
        double* col = Xc.data() + (size_t)j * n;
        double mu = dvec[j];
        for (int i = 0; i < n; i++) col[i] -= mu;
    }

    NumericMatrix Omega(d_g, d_g);
    // C := alpha * A^T * A + beta*C, A is n x d_g (col-major), result d_g x d_g, upper triangle only
    cblas_dsyrk(CblasColMajor, CblasUpper, CblasTrans,
                d_g, n, 1.0 / (n - 1), Xc.data(), n, 0.0, Omega.begin(), d_g);
    for (int i = 0; i < d_g; i++)
        for (int j = 0; j < i; j++)
            Omega(i, j) = Omega(j, i);   // mirror upper -> lower, R's cov() returns the full symmetric matrix

    return List::create(Named("dvec") = dvec, Named("Omega") = Omega);
}

// ---- Auxiliary-parameter diagnostic: pooled e AND omega, moment set B ----
// (2026-09-01) Mirrors tilted_e_omega_diag_A_cpp in the main file, for B --
// no B-specific diagnostic previously existed at all. Same chain as
// TiltedMomentWorkerB above, additionally tracking e_current/om_current
// alongside g_current so both can be pooled in one pass at the already-
// fitted (theta_hat, gamma_hat). No Accelerate needed here (this is a
// per-firm Metropolis diagnostic, not the CUE objective's covariance step),
// but it lives in this file since that's where moment_g_B_one/
// TiltedMomentWorkerB's helpers already are.
struct TiltedEOmegaDiagWorkerB : public Worker {
    const RVector<double>  Mstar, V, Wt, tau_rho, beta, mu_omega, sigma_omega, gamma, mu_m;
    const RVector<int>     row_id, corner, industry_idx;
    const double lambda, delta0, delta1, delta2, eta;
    const int n_burn, n_keep, base_seed, J, d_g, n_pool;
    RMatrix<double> e_pool_out, omega_pool_out;

    TiltedEOmegaDiagWorkerB(
        const NumericVector& Mstar_, const NumericVector& V_, const NumericVector& Wt_, const NumericVector& tau_rho_,
        const NumericVector& beta_, const NumericVector& mu_omega_, const NumericVector& sigma_omega_,
        const IntegerVector& row_id_, const IntegerVector& corner_, const IntegerVector& industry_idx_,
        double lambda_, double delta0_, double delta1_, double delta2_, double eta_,
        const NumericVector& gamma_, const NumericVector& mu_m_,
        int n_burn_, int n_keep_, int base_seed_, int J_, int d_g_, int n_pool_,
        NumericMatrix& e_pool_out_, NumericMatrix& omega_pool_out_
    ) : Mstar(Mstar_), V(V_), Wt(Wt_), tau_rho(tau_rho_), beta(beta_),
        mu_omega(mu_omega_), sigma_omega(sigma_omega_), gamma(gamma_), mu_m(mu_m_),
        row_id(row_id_), corner(corner_), industry_idx(industry_idx_),
        lambda(lambda_), delta0(delta0_), delta1(delta1_), delta2(delta2_), eta(eta_),
        n_burn(n_burn_), n_keep(n_keep_), base_seed(base_seed_), J(J_), d_g(d_g_), n_pool(n_pool_),
        e_pool_out(e_pool_out_), omega_pool_out(omega_pool_out_) {}

    void operator()(std::size_t begin, std::size_t end) {
        int pool_stride = std::max(1, n_keep / n_pool);
        for (std::size_t ii = begin; ii < end; ii++) {
            int i = static_cast<int>(ii);
            int idx = industry_idx[i];

            if (corner[i] == 1) {
                double om_pt = omega_of_M(Mstar[i], Mstar[i], V[i], Wt[i], beta[i]);
                for (int p = 0; p < n_pool; p++) { e_pool_out(i, p) = 0.0; omega_pool_out(i, p) = om_pt; }
                continue;
            }

            std::mt19937_64 rng(static_cast<uint64_t>(base_seed) + static_cast<uint64_t>(row_id[i]));
            std::uniform_real_distribution<double> unif(0.0, 1.0);

            std::vector<double> g_current(d_g), g_try(d_g);
            double M_current = draw_from_rho_eta(unif(rng), Mstar[i], lambda, eta);
            moment_g_B_one(M_current, Mstar[i], V[i], Wt[i], tau_rho[i], beta[i], lambda,
                           delta0, delta1, delta2, mu_omega[i], sigma_omega[i], idx, mu_m, J, g_current);
            double e_current  = e_of_M(M_current, Mstar[i]);
            double om_current = omega_of_M(M_current, Mstar[i], V[i], Wt[i], beta[i]);

            int pool_idx = 0;

            for (int r = -n_burn + 1; r <= n_keep; r++) {
                double M_try = draw_from_rho_eta(unif(rng), Mstar[i], lambda, eta);
                moment_g_B_one(M_try, Mstar[i], V[i], Wt[i], tau_rho[i], beta[i], lambda,
                               delta0, delta1, delta2, mu_omega[i], sigma_omega[i], idx, mu_m, J, g_try);
                double e_try  = e_of_M(M_try, Mstar[i]);
                double om_try = omega_of_M(M_try, Mstar[i], V[i], Wt[i], beta[i]);

                double log_ratio = 0.0;
                for (int t = 0; t < d_g; t++) log_ratio += gamma[t] * (g_try[t] - g_current[t]);

                if (std::log(unif(rng)) < log_ratio) {
                    g_current = g_try;
                    e_current = e_try;
                    om_current = om_try;
                }
                if (r > 0) {
                    if ((r - 1) % pool_stride == 0 && pool_idx < n_pool) {
                        e_pool_out(i, pool_idx) = e_current;
                        omega_pool_out(i, pool_idx) = om_current;
                        pool_idx++;
                    }
                }
            }
            while (pool_idx < n_pool) { e_pool_out(i, pool_idx) = e_current; omega_pool_out(i, pool_idx) = om_current; pool_idx++; }
        }
    }
};

// [[Rcpp::export]]
List tilted_e_omega_diag_B_cpp(
    NumericVector Mstar, NumericVector V, NumericVector Wt, NumericVector tau_rho,
    NumericVector beta, NumericVector mu_omega, NumericVector sigma_omega,
    IntegerVector row_id, IntegerVector corner, IntegerVector industry_idx,
    double lambda, double delta0, double delta1, double delta2, double eta,
    NumericVector gamma, NumericVector mu_m,
    int n_burn, int n_keep, int n_pool = 20,
    int base_seed = 20260901
) {
    int n = Mstar.size();
    int J = mu_m.size();
    int d_g = 8 + J + 6;
    if (gamma.size() != d_g) stop("gamma length must equal 8+J+6 (moment set B)");
    if (corner.size() != n) stop("corner length must equal n");
    if (row_id.size() != n) stop("row_id length must equal n");
    if (industry_idx.size() != n) stop("industry_idx length must equal n");
    if (mu_omega.size() != n || sigma_omega.size() != n) stop("mu_omega/sigma_omega length must equal n");
    if (eta < 0.0 || eta >= 1.0) stop("eta must be in [0,1)");

    NumericMatrix e_pool(n, n_pool), omega_pool(n, n_pool);
    TiltedEOmegaDiagWorkerB worker(
        Mstar, V, Wt, tau_rho, beta, mu_omega, sigma_omega,
        row_id, corner, industry_idx,
        lambda, delta0, delta1, delta2, eta, gamma, mu_m,
        n_burn, n_keep, base_seed, J, d_g, n_pool,
        e_pool, omega_pool
    );
    RcppParallel::parallelFor(0, n, worker);
    return List::create(Named("e_pool") = e_pool, Named("omega_pool") = omega_pool);
}
