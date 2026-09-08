// Stage-2 ELVIS, moment set C (2026-09-07) -- R/Rcpp side, for a fair speed
// comparison against the standalone Code/C-estimator/grid_estimator on the
// SAME 7-moment set. Mirrors moment set B's file exactly: RcppParallel for
// the per-firm loop, Accelerate/cblas_dsyrk for cov(Ghat) (same routine/
// reasoning already validated for B), eigen() left in R (already cheap,
// unchanged) -- so this isolates specifically "does eliminating R's role
// between BOBYQA iterations save time beyond what Accelerate-in-Rcpp
// already gets," not "does Accelerate help" (already answered, yes, when B
// was built).
//
// g = ( psi, psi*om, psi*om^2, eps*h_prime_bounded, psi*eps, psi*eps*om, eps )
// -- identical to Code/C-estimator/grid_estimator.cpp's moment_g_C_one,
// same shared common.h (h_prime_bounded etc.), so there is exactly one
// place either version's math could diverge: this file's own arithmetic
// vs. grid_estimator.cpp's own (duplicated, not shared, since the standalone
// build and Rcpp builds can't literally share a .cpp file) -- worth a
// bit-identical cross-check the same way moment_g_C_one's original 6-row
// version was checked (Research-log/log.md, 2026-09-07).
//
// [[Rcpp::depends(RcppParallel)]]
#include "1200-stage2-elvis-common.h"
#include <vecLib/cblas.h>

static const int D_G_C = 7;
typedef std::array<double, D_G_C> GVecC;

static inline void moment_g_C_one(
    double M, double Mstar, double V, double Wt, double tau_rho, double beta,
    double lambda, double delta0, double delta1, double delta2,
    GVecC &g_out
) {
    double e      = e_of_M(M, Mstar);
    double eps    = eps_of_M(M, Mstar, V);
    double om     = omega_of_M(M, Mstar, V, Wt, beta);
    double psi    = h_of_e(e, tau_rho, lambda) - delta0 + delta1 * om - delta2 * om * om;
    double hbound = h_prime_bounded(e, lambda);

    g_out[0] = psi;
    g_out[1] = psi * om;
    g_out[2] = psi * om * om;
    g_out[3] = eps * hbound;
    g_out[4] = psi * eps;
    g_out[5] = psi * eps * om;
    g_out[6] = eps;
}

struct TiltedMomentWorkerC : public Worker {
    const RVector<double>  Mstar, V, Wt, tau_rho, beta, gamma;
    const RVector<int>     row_id, corner;
    const double lambda, delta0, delta1, delta2, eta;
    const int n_burn, n_keep, base_seed;
    RMatrix<double> Ghat;

    TiltedMomentWorkerC(
        const NumericVector& Mstar_, const NumericVector& V_, const NumericVector& Wt_, const NumericVector& tau_rho_,
        const NumericVector& beta_, const NumericVector& gamma_,
        const IntegerVector& row_id_, const IntegerVector& corner_,
        double lambda_, double delta0_, double delta1_, double delta2_, double eta_,
        int n_burn_, int n_keep_, int base_seed_,
        NumericMatrix& Ghat_
    ) : Mstar(Mstar_), V(V_), Wt(Wt_), tau_rho(tau_rho_), beta(beta_), gamma(gamma_),
        row_id(row_id_), corner(corner_),
        lambda(lambda_), delta0(delta0_), delta1(delta1_), delta2(delta2_), eta(eta_),
        n_burn(n_burn_), n_keep(n_keep_), base_seed(base_seed_),
        Ghat(Ghat_) {}

    void operator()(std::size_t begin, std::size_t end) {
        for (std::size_t ii = begin; ii < end; ii++) {
            int i = static_cast<int>(ii);

            if (corner[i] == 1) {
                double eps_pt = eps_of_M(Mstar[i], Mstar[i], V[i]);
                for (int t = 0; t < D_G_C; t++) Ghat(i, t) = 0.0;
                Ghat(i, 6) = eps_pt;
                continue;
            }

            std::mt19937_64 rng(static_cast<uint64_t>(base_seed) + static_cast<uint64_t>(row_id[i]));
            std::uniform_real_distribution<double> unif(0.0, 1.0);

            GVecC g_current, g_try, g_run;
            double M_current = draw_from_rho_eta(unif(rng), Mstar[i], lambda, eta);
            moment_g_C_one(M_current, Mstar[i], V[i], Wt[i], tau_rho[i],
                           beta[i], lambda, delta0, delta1, delta2, g_current);
            g_run.fill(0.0);

            for (int r = -n_burn + 1; r <= n_keep; r++) {
                double M_try = draw_from_rho_eta(unif(rng), Mstar[i], lambda, eta);
                moment_g_C_one(M_try, Mstar[i], V[i], Wt[i], tau_rho[i],
                               beta[i], lambda, delta0, delta1, delta2, g_try);

                double log_ratio = 0.0;
                for (int t = 0; t < D_G_C; t++) log_ratio += gamma[t] * (g_try[t] - g_current[t]);

                if (std::log(unif(rng)) < log_ratio) g_current = g_try;
                if (r > 0) for (int t = 0; t < D_G_C; t++) g_run[t] += g_current[t] / n_keep;
            }
            for (int t = 0; t < D_G_C; t++) Ghat(i, t) = g_run[t];
        }
    }
};

// [[Rcpp::export]]
List mh_tilted_moments_C_cpp(
    NumericVector Mstar, NumericVector V, NumericVector Wt, NumericVector tau_rho, NumericVector beta,
    IntegerVector row_id, IntegerVector corner,
    double lambda, double delta0, double delta1, double delta2, double eta,
    NumericVector gamma,
    int n_burn, int n_keep,
    int base_seed = 20260907
) {
    int n = Mstar.size();
    if (gamma.size() != D_G_C) stop("gamma length must equal 7 (moment set C)");
    if (corner.size() != n) stop("corner length must equal n");
    if (row_id.size() != n) stop("row_id length must equal n");
    if (eta < 0.0 || eta >= 1.0) stop("eta must be in [0,1)");

    NumericMatrix Ghat(n, D_G_C);
    TiltedMomentWorkerC worker(
        Mstar, V, Wt, tau_rho, beta, gamma, row_id, corner,
        lambda, delta0, delta1, delta2, eta, n_burn, n_keep, base_seed, Ghat
    );
    RcppParallel::parallelFor(0, n, worker);

    NumericVector dvec(D_G_C);
    for (int j = 0; j < D_G_C; j++) {
        double s = 0.0;
        const double* col = Ghat.begin() + (size_t)j * n;
        for (int i = 0; i < n; i++) s += col[i];
        dvec[j] = s / n;
    }

    std::vector<double> Xc((size_t)n * D_G_C);
    std::copy(Ghat.begin(), Ghat.end(), Xc.begin());
    for (int j = 0; j < D_G_C; j++) {
        double* col = Xc.data() + (size_t)j * n;
        double mu = dvec[j];
        for (int i = 0; i < n; i++) col[i] -= mu;
    }

    NumericMatrix Omega(D_G_C, D_G_C);
    cblas_dsyrk(CblasColMajor, CblasUpper, CblasTrans,
                D_G_C, n, 1.0 / (n - 1), Xc.data(), n, 0.0, Omega.begin(), D_G_C);
    for (int i = 0; i < D_G_C; i++)
        for (int j = 0; j < i; j++)
            Omega(i, j) = Omega(j, i);

    return List::create(Named("dvec") = dvec, Named("Omega") = Omega);
}
