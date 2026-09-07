// Stage-2 ELVIS: moment set A, CONCAVE detection function q=1-exp(-lambda*e)
// (2026-09-05) -- a cheap diagnostic variant of the linear-q moment set A in
// 1200-stage2-elvis.cpp, testing whether the linear-q result (lambda pinned
// near 0, apparently by a few extreme-e firms whose ceiling 2*lambda*e->1
// makes h(e) blow up -- confirmed this session via the firm-exclusion trim
// check) survives under a functional form whose own ceiling sits at TWICE
// the lambda*e value (lambda*e<1, not <1/2) -- see CLAUDE.md's same-date
// entry for the derivative comparison motivating this.
//
// Identical structure to TiltedMomentWorkerA/mh_tilted_average_A_cpp in
// 1200-stage2-elvis.cpp -- same 8-moment vector, same per-firm Metropolis
// sampler, same corner-firm short-circuit -- ONLY h_of_e and the sampler's
// ceiling (draw_from_rho_eta_concave instead of draw_from_rho_eta) differ.
// Kept as a SEPARATE file/exports (mh_tilted_average_A_concave_cpp, not a
// flag on the existing function) so the validated linear-q code path is
// untouched -- mirrors how moment set B got its own file rather than
// overloading A's.
//
// [[Rcpp::depends(RcppParallel)]]
#include "1200-stage2-elvis-common.h"

static const int D_G_AC = 8;
typedef std::array<double, D_G_AC> GVecAC;

static void moment_g_A_concave_one(
    double M, double Mstar, double V, double Wt, double tau_rho,
    double beta, double lambda, double delta0, double delta1, double delta2,
    GVecAC& g_out
) {
    double e     = e_of_M(M, Mstar);
    double eps   = eps_of_M(M, Mstar, V);
    double om    = omega_of_M(M, Mstar, V, Wt, beta);
    double psi   = h_of_e_concave(e, tau_rho, lambda) - delta0 + delta1 * om - delta2 * om * om;
    double lnM   = std::log(M);

    g_out[0] = psi;
    g_out[1] = eps;
    g_out[2] = psi * lnM;
    g_out[3] = psi * om;
    g_out[4] = psi * om * om;
    g_out[5] = eps * lnM;
    g_out[6] = eps * e;
    g_out[7] = eps * om;
}

struct TiltedMomentWorkerAConcave : public Worker {
    const RVector<double>  Mstar, V, Wt, tau_rho, beta, gamma;
    const RVector<int>     row_id, corner;
    const double lambda, delta0, delta1, delta2, eta;
    const int n_burn, n_keep, base_seed;
    RMatrix<double> Ghat;

    TiltedMomentWorkerAConcave(
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
                double om_pt  = omega_of_M(Mstar[i], Mstar[i], V[i], Wt[i], beta[i]);
                double lnM_pt = std::log(Mstar[i]);
                for (int t = 0; t < D_G_AC; t++) Ghat(i, t) = 0.0;
                Ghat(i, 1) = eps_pt;
                Ghat(i, 5) = eps_pt * lnM_pt;
                Ghat(i, 7) = eps_pt * om_pt;
                continue;
            }

            std::mt19937_64 rng(static_cast<uint64_t>(base_seed) + static_cast<uint64_t>(row_id[i]));
            std::uniform_real_distribution<double> unif(0.0, 1.0);

            GVecAC g_current, g_try, g_run;
            double M_current = draw_from_rho_eta_concave(unif(rng), Mstar[i], lambda, eta);
            moment_g_A_concave_one(M_current, Mstar[i], V[i], Wt[i], tau_rho[i],
                                    beta[i], lambda, delta0, delta1, delta2, g_current);

            g_run.fill(0.0);

            for (int r = -n_burn + 1; r <= n_keep; r++) {
                double M_try = draw_from_rho_eta_concave(unif(rng), Mstar[i], lambda, eta);
                moment_g_A_concave_one(M_try, Mstar[i], V[i], Wt[i], tau_rho[i],
                                        beta[i], lambda, delta0, delta1, delta2, g_try);

                double log_ratio = 0.0;
                for (int t = 0; t < D_G_AC; t++) log_ratio += gamma[t] * (g_try[t] - g_current[t]);

                if (std::log(unif(rng)) < log_ratio) {
                    g_current = g_try;
                }
                if (r > 0) {
                    for (int t = 0; t < D_G_AC; t++) g_run[t] += g_current[t] / n_keep;
                }
            }
            for (int t = 0; t < D_G_AC; t++) Ghat(i, t) = g_run[t];
        }
    }
};

// [[Rcpp::export]]
NumericMatrix mh_tilted_average_A_concave_cpp(
    NumericVector Mstar, NumericVector V, NumericVector Wt, NumericVector tau_rho,
    IntegerVector row_id, NumericVector beta,
    double lambda, double delta0, double delta1, double delta2, double eta,
    NumericVector gamma,
    IntegerVector corner,
    int n_burn, int n_keep,
    int base_seed = 20260905
) {
    int n = Mstar.size();
    if (gamma.size() != D_G_AC) stop("gamma length must equal 8 (moment set A, concave)");
    if (corner.size() != n) stop("corner length must equal n");
    if (row_id.size() != n) stop("row_id length must equal n");
    if (eta < 0.0 || eta >= 1.0) stop("eta must be in [0,1)");
    if (lambda <= 0.0) stop("lambda must be > 0");

    NumericMatrix Ghat(n, D_G_AC);
    TiltedMomentWorkerAConcave worker(
        Mstar, V, Wt, tau_rho, beta, gamma, row_id, corner,
        lambda, delta0, delta1, delta2, eta, n_burn, n_keep, base_seed, Ghat
    );
    RcppParallel::parallelFor(0, n, worker);
    return Ghat;
}
