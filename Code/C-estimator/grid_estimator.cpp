// Standalone (lambda, delta1, delta2) grid estimator, moment set C (2026-09-07)
// --------------------------------------------------------------------------
// Never touches R. Reads a CSV built by Code/Deconvolution/1225-stage2-grid-
// export.R (one row per firm-period, already trim/corner-filtered exactly as
// build_run_sample() in 1211-stage2-elvis-driver-AB.R does), grids
// (lambda,delta1,delta2) jointly, profiles (delta0,eta,gamma[1:6]) at each
// point via NLopt's C API (BOBYQA, two-pass refinement, mirroring
// fit_one_lambda_A's own convention), computes the CUE objective via
// Accelerate (cblas_dsyrk for the covariance -- same routine/reasoning as
// Code/Rcpp/1200-stage2-elvis-B.cpp's mh_tilted_moments_B_cpp -- and dsyevr
// for the eigendecomposition, replacing R's eigen()), and writes one result
// row per grid point to an output CSV. Code/Deconvolution/1226-stage2-grid-
// plot.R reads that back for the profile/heatmap figures.
//
// Moment set C (6 rows, user-specified, 2026-09-07) -- deliberately smaller
// than sets A (8) and B (15): drops every ln(M)-based row, so there is no
// materials-FOC/per-industry mu_m machinery here at all, not just fewer rows:
//   g = ( psi, psi*om, psi*om^2, eps*h_prime_bounded, psi*eps, psi*eps*om, eps )
// Row 6 (raw eps) added back 2026-09-07: the original 6-row set left corner
// firms contributing EXACTLY ZERO to every row (h_prime_bounded(0,.)=0 too,
// not just the psi rows) -- 10.36% of the sample carrying no identifying
// content at all. eps alone is added first, gradually (not all of A's three
// eps-only rows at once) -- see Research-log/log.md.
// eps*exp(h') is the score moment for lambda (see h_prime_of_e's own comment
// in 1200-stage2-elvis-common.h for why eps is a valid, non-tautological
// partner). psi*eps and psi*eps*om were previously B-only (there, over-
// identifying independence checks); here they are two of only six rows.
//
// Threading: parallelized at the GRID-POINT level (a std::thread pool
// consuming the flattened lambda x delta1 x delta2 queue), not the per-firm
// level RcppParallel uses -- with 1000 independent grid points >> 12 cores,
// this gets good load balancing without nesting parallelism two levels deep.
// Per-firm CRN (std::mt19937_64 seeded by base_seed+row_id) is unaffected:
// a firm's chain is identical regardless of which thread/grid-point touches
// it, exactly as in the existing RcppParallel-based worker.
//
// Corner (tau_P=0) firms: same treatment as A/B -- M fixed at M*, ALL rows
// forced to exactly 0 (uninformative, not "satisfied at 0"), including row 3
// (eps*h_prime_bounded): h_prime_bounded(0,lambda)=0 exactly at e=0, so the
// row is genuinely zero there too, matching TiltedMomentWorkerA/B's own
// convention of leaving their analogous row at the zeroed default for
// corner firms (never special-cased to eps_pt).

#define STAGE2_STANDALONE_BUILD
#include "../Rcpp/1200-stage2-elvis-common.h"

#include <nlopt.h>
#include <Accelerate/Accelerate.h>

#include <array>
#include <atomic>
#include <chrono>
#include <cmath>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <map>
#include <mutex>
#include <sstream>
#include <string>
#include <thread>
#include <vector>

static const int D_G_C = 7;   // 2026-09-07: 6 -> 7, added back raw `eps` (row 6) -- see file header
typedef std::array<double, D_G_C> GVecC;
static double DELTA_BOUND = 60.0;
// delta0_fixed / delta1_fixed / delta2_fixed (2026-10-01, Hans: grid over the cost parameters with the rest profiled):
// pin that delta at the given value (equal bounds) in lambdagrid's joint fit; NaN = free (default).
// kappa_fixed (2026-10-02, Hans: k x kappa grid / kappa profile): pin kappa (KAPPA_FREE builds), NaN = free.
static double g_kappa_fix = std::numeric_limits<double>::quiet_NaN();
static double g_dfix[3] = {std::numeric_limits<double>::quiet_NaN(), std::numeric_limits<double>::quiet_NaN(), std::numeric_limits<double>::quiet_NaN()};   // box for delta0..2 (CLI delta_max in lambdagrid, 2026-10-01; default 60 = every earlier run)

// Generalized inverse of Omega for EVERY CUE objective in this file (2026-09-30, audit finding 7): cut=ak (default) =
// AK2020 objMCcu -- Omega/n, keep eigenvalues > 0; cut=rel = the old relative cut w > 1e-8*max with Omega/(n-1), a
// porting error kept only to reproduce pre-2026-09-30 runs. Set once from the CLI before any thread starts.
static bool g_cut_ak = true;
static inline bool keep_eig(double wk, double max_eig) { return g_cut_ak ? wk > 0.0 : wk > 1e-8 * max_eig; }
static inline double omega_div(int n) { return g_cut_ak ? 1.0 / n : 1.0 / (n - 1); }

// Per-firm RNG seed (2026-09-30, audit finding 6). seed=add (old): base_seed + row_id, so seed s+1 gives firm r the
// stream firm r+1 had at seed s -- different base seeds are NOT independent replications. seed=hash (default from
// 2026-09-30): splitmix64 of base_seed combined with splitmix64 of row_id, so streams are unrelated across seeds and
// firms. Set once from the CLI before any thread starts.
static bool g_seed_hash = true;
static inline uint64_t splitmix64(uint64_t x) {
    x += 0x9E3779B97F4A7C15ULL; x = (x ^ (x >> 30)) * 0xBF58476D1CE4E5B9ULL; x = (x ^ (x >> 27)) * 0x94D049BB133111EBULL;
    return x ^ (x >> 31);
}
static inline uint64_t firm_seed(uint64_t base_seed, long row_id) {
    if (!g_seed_hash) return base_seed + static_cast<uint64_t>(row_id);
    return splitmix64(splitmix64(base_seed) ^ splitmix64(0xD1B54A32D192ED03ULL + static_cast<uint64_t>(row_id)));
}

// ---- data ------------------------------------------------------------------

struct FirmData {
    double Mstar, V, Wt, tau_rho, beta;
    int corner;
    long row_id;
    // t1 (raw sales-tax-paid-on-sales, CLAUDE.md's "t1") and pgdp (deflator,
    // CLAUDE.md's "p_gdp_new") -- optional, only populated/required by the
    // revenue_baseline mode (2026-09-10); left NaN by any CSV that doesn't
    // carry them, which every other mode never reads.
    double t1 = std::numeric_limits<double>::quiet_NaN();
    double pgdp = std::numeric_limits<double>::quiet_NaN();
    // Mbar (2026-09-28, S3): lagged industry mean of reported M* (detection reference scale); only read by
    // qform=exp_scale. NaN when the CSV has no Mbar column.
    double Mbar = std::numeric_limits<double>::quiet_NaN();
    // ltau_bar (2026-09-28, TAU_ROW build): ln of the leave-one-out industry-year mean purchases tax rate.
    double ltau_bar = std::numeric_limits<double>::quiet_NaN();
    // yidx (2026-09-28, YEAR_FE build): year - 81 from the input's `year` column (0 = 1981); -1 if absent.
    int yidx = -1;
    // sig2eps (2026-09-29, EPSVAR build): corporations' eps variance in the firm's industry.
    double sig2eps = std::numeric_limits<double>::quiet_NaN();
    // sic / jidx (2026-09-30, IND5 build): 3-digit industry from column sic_3, and its index 0..N_IND-1 among the
    // interior firms' industries (sorted); -1 if absent. Used by the per-industry eps*lnM rows.
    int sic = -1, jidx = -1;
    // plant_id (2026-09-30, audit finding 4): plant identifier from column plant_id (-1 if absent); cl = dense cluster
    // index 0..g_ncl-1 set when cluster=plant.
    long plant = -1; int cl = -1;
    // design inputs (2026-10-01): audit_g = 1 if in the audit group G (top 10% of capital within industry); umed =
    // deconvolved median of u for the firm's industry (-1 = none). Read from columns audit_g, umed when present.
    int audit_g = 0, audit_gv = 0; double umed = -1.0;   // audit_gv: group by V (robustness), used when audit_group=v
    double pshare = -1.0;   // IND5P (2026-10-02): deconvolved P(u >= share_u) of the firm's industry (column pshare; -1 = none)
    double cw = 1.0;        // ind_rows=eps_cw (2026-10-03): claims weight tau_P M* / industry mean of tau_P M* (interior firms)
};
// Which optional design columns the input carried (review 4: a missing column used to leave its rows silently zero).
static bool g_has_audit_g = false, g_has_audit_gv = false, g_has_umed = false, g_has_pshare = false;
// cluster=plant (2026-09-30): Omega = n^-1 sum_p (sum_{i in p} (g_i - dbar)) (sum_{i in p} (g_i - dbar))', firm-periods of
// the same plant summed before the outer product (Theorem F.1's i.i.d. unit is the plant, not the firm-period).
// Every CUE objective of moment set A and adiag use it. Default cluster=none (i.i.d. firm-periods, every earlier run).
static bool g_cluster_on = false;
static int g_ncl = 0;

// Detection function used by moment set A's chain (firm_chain_A): 0 = linear q=min(lambda*e,1) (default, every
// result before 2026-09-28); 1 = exp_scale q=lambda1*(1-exp(-e/Mbar)) (ladder step S3). Set once from the CLI
// (qform=linear|exp_scale) before any thread starts; never changed afterwards.
static int g_qform = 0;
// Row [6] of moment set A under qform=exp_scale (2026-09-28): 0 = eps*e (default, every earlier run; e in pesos
// dominates Omega and collapses the eigen-cut, log 2026-09-28), 1 = eps*psi (both mean-zero anchors, valid under
// eps _|_ (psi, omega)). CLI row6=eps_e|eps_psi.
static int g_row6 = 0;
// Simulated-annealing global stage before the Nelder-Mead passes in the lambdagrid fit (2026-09-28; Nail Kashaev's
// suggestion; AK2020's code uses the same global-then-local pattern with BlackBoxOptim's adaptive DE, 100 s, then
// BOBYQA). g_sa_time = seconds of annealing (0 = off, default). CLI sa_time=<s>.
static double g_sa_time = 0.0;
// algo2 (2026-09-30, Hans): algorithm for lambdagrid's SECOND pass (after the optional SA); default = same as pass 1.
// CLI algo2=bobyqa|neldermead.
static int g_algo2 = -1;
// n_passes (2026-09-30, Hans): total optimizer passes in lambdagrid (default 2); passes 3.. restart the pass-2
// algorithm from the previous endpoint. Lhat_pass1 still reports pass 1; wander = pass 1 -> final. CLI n_passes=<int>.
static int g_npasses = 2;
// Optimizer settings for lambdagrid (2026-09-30, audit 7.5). maxeval per pass (CLI maxeval; default 200 x free dims);
// initial simplex/step: delta 0.5, k 0.1, s 0.05, kappa 0.1, gamma_t 0.2/D_t (D_t = rho_D when rho=prop21, else 1)
// (CLI init_step=auto|nlopt; nlopt = NLopt's default, every earlier run). algo2=lbfgs: L-BFGS with central
// finite-difference gradients (step 1e-5 x max(1,|x|)); meant for sampler=is, where the objective is smooth in gamma.
static int g_maxeval = -1;
static double g_kappa_max = 5.0;   // upper bound on kappa when estimated (KAPPA_FREE); CLI kappa_max
static bool g_init_step_auto = true;
struct FDWrap { nlopt_func f; void *data; const double *lb; const double *ub; };
static double fd_objective(unsigned n, const double *x, double *grad, void *d) {
    FDWrap *w = static_cast<FDWrap *>(d);
    double f0 = w->f(n, x, nullptr, w->data);
    if (grad) {
        std::vector<double> xp(x, x + n);
        for (unsigned i = 0; i < n; i++) {
            if (w->lb[i] == w->ub[i]) { grad[i] = 0.0; continue; }
            double h = 1e-5 * std::max(1.0, std::fabs(x[i]));
            double hi = std::min(x[i] + h, w->ub[i]), lo = std::max(x[i] - h, w->lb[i]);
            xp[i] = hi; double fp = w->f(n, xp.data(), nullptr, w->data);
            xp[i] = lo; double fm = w->f(n, xp.data(), nullptr, w->data);
            xp[i] = x[i];
            grad[i] = (fp - fm) / (hi - lo);
        }
    }
    return f0;
}

// ---- moment set C, one firm at one candidate M -----------------------------
// (byte-for-byte the same structural maps as moment_g_A_one/B_one in
// 1200-stage2-elvis.cpp/-B.cpp -- e_of_M/eps_of_M/omega_of_M/h_of_e/
// h_prime_of_e come from the shared, already-floored common.h unchanged.)
static inline void moment_g_C_one(
    double M, double Mstar, double V, double Wt, double tau_rho, double beta,
    double lambda, double delta0, double delta1, double delta2,
    GVecC &g_out
) {
    double e    = e_of_M(M, Mstar);
    double eps  = eps_of_M(M, Mstar, V);
    double om   = omega_of_M(M, Mstar, V, Wt, beta);
    double psi  = h_of_e(e, tau_rho, lambda) - delta0 + delta1 * om - delta2 * om * om;
    double hbound = h_prime_bounded(e, lambda);   // bounded (-1,0], see h_prime_bounded's own comment (2026-09-07)

    g_out[0] = psi;
    g_out[1] = psi * om;
    g_out[2] = psi * om * om;
    g_out[3] = eps * hbound;
    g_out[4] = psi * eps;
    g_out[5] = psi * eps * om;
    g_out[6] = eps;   // 2026-09-07: added back -- an eps-ONLY row, matching moment set A/B's own
                       // `eps` row, so corner firms (psi AND h_prime_bounded both structurally 0
                       // at e=0) still contribute something instead of carrying zero weight in
                       // every single row. Added ONE row at a time, gradually, per this session's
                       // "don't rush" correction -- eps*lnM and eps*om (A's other two eps-only
                       // rows) deliberately NOT added yet.
}

// ---- one firm's whole Metropolis-tilted chain ------------------------------
// Same log-ratio Metropolis-Hastings acceptance and burn-in/averaging pattern
// as TiltedMomentWorkerA::operator() in 1200-stage2-elvis.cpp (ported, not
// redesigned): r ranges over the WHOLE chain (burn-in + averaging), only
// accumulate once r>0.
static inline void firm_chain_C(
    const FirmData &f, double lambda, double delta0, double delta1, double delta2,
    double eta, const double gamma[D_G_C], int n_burn, int n_keep, uint64_t base_seed,
    double *ghat_row   // length D_G_C
) {
    if (f.corner == 1) {
        // ALL rows zero for corner firms, including row 3 (2026-09-07 fix):
        // h_prime_bounded(0,lambda) = 0/(1-0) = 0 exactly, so eps*h_prime_bounded
        // = 0 at e=0 regardless of eps -- matches TiltedMomentWorkerA/B's own
        // established convention (their row 6/analog is also left at the
        // zeroed default for corner firms, never special-cased to eps_pt).
        // The OLD exp(h') transform's corner value (eps_pt, since exp(0)=1)
        // was inconsistent with that established convention -- caught before
        // committing, not shipped.
        for (int t = 0; t < D_G_C; t++) ghat_row[t] = 0.0;
        ghat_row[6] = eps_of_M(f.Mstar, f.Mstar, f.V);   // 2026-09-07: row 6 (raw eps) computed for
                                                          // real at the corner -- eps(M) is well-
                                                          // defined at M=Mstar regardless of e, tau_rho,
                                                          // lambda; matches A/B's own eps-only-row
                                                          // treatment for corner firms.
        return;
    }

    std::mt19937_64 rng(firm_seed(base_seed, f.row_id));
    std::uniform_real_distribution<double> unif(0.0, 1.0);

    GVecC g_current, g_try, g_run;
    double M_current = draw_from_rho_eta(unif(rng), f.Mstar, lambda, eta);
    moment_g_C_one(M_current, f.Mstar, f.V, f.Wt, f.tau_rho, f.beta, lambda, delta0, delta1, delta2, g_current);
    g_run.fill(0.0);

    for (int r = -n_burn + 1; r <= n_keep; r++) {
        double M_try = draw_from_rho_eta(unif(rng), f.Mstar, lambda, eta);
        moment_g_C_one(M_try, f.Mstar, f.V, f.Wt, f.tau_rho, f.beta, lambda, delta0, delta1, delta2, g_try);

        double log_ratio = 0.0;
        for (int t = 0; t < D_G_C; t++) log_ratio += gamma[t] * (g_try[t] - g_current[t]);

        if (std::log(unif(rng)) < log_ratio) g_current = g_try;
        if (r > 0) for (int t = 0; t < D_G_C; t++) g_run[t] += g_current[t] / n_keep;
    }
    for (int t = 0; t < D_G_C; t++) ghat_row[t] = g_run[t];
}

// ---- dvec/Omega for the whole sample, one (theta,gamma) --------------------
// Same "post-loop fusion" as mh_tilted_moments_B_cpp: build Ghat once (never
// written out), colMeans -> dvec, cov() via cblas_dsyrk -> Omega.
//
// Per-firm std::thread parallelism (2026-09-07, replaces the earlier grid-
// point-level-only design): measured single-threaded, this call was NOT
// faster than R/RcppParallel's own per-firm-threaded version at the same
// settings (1.73s vs 1.72s single-threaded, bit-identical dvec) -- the
// standalone build's only real edge is Accelerate for cov()/eigen(), which
// is small on its own. Per-firm threading recovers the SAME parallelism
// RcppParallel already provides, mirroring its exact pattern: each thread
// handles a contiguous firm-index range, writes only its own rows of Ghat
// (no cross-thread write contention, no locking needed), per-firm CRN
// (base_seed+row_id) is unaffected by which thread processes a given firm.
// n_threads here is threads-PER-GRID-POINT; grid-point-level parallelism now
// lives at the shell level (see run_grid_shards.sh / shard_id, n_shards
// below) instead of a second, nested in-process thread pool -- simpler to
// reason about and lets each invocation be sized independently (e.g. 3
// concurrent processes x 4 threads each, or 1 process x 12, whatever split
// the user wants for a given machine).
static void compute_dvec_omega_C(
    const std::vector<FirmData> &firms,
    double lambda, double delta0, double delta1, double delta2, double eta, const double gamma[D_G_C],
    int n_burn, int n_keep, uint64_t base_seed, int n_threads,
    double dvec[D_G_C], double Omega[D_G_C * D_G_C]
) {
    int n = static_cast<int>(firms.size());
    std::vector<double> Ghat((size_t)n * D_G_C);   // column-major: Ghat[j*n+i]

    auto firm_worker = [&](int begin, int end) {
        double row[D_G_C];
        for (int i = begin; i < end; i++) {
            firm_chain_C(firms[i], lambda, delta0, delta1, delta2, eta, gamma, n_burn, n_keep, base_seed, row);
            for (int j = 0; j < D_G_C; j++) Ghat[(size_t)j * n + i] = row[j];
        }
    };

    if (n_threads <= 1) {
        firm_worker(0, n);
    } else {
        std::vector<std::thread> pool;
        int chunk = (n + n_threads - 1) / n_threads;
        for (int t = 0; t < n_threads; t++) {
            int begin = t * chunk, end = std::min(n, begin + chunk);
            if (begin >= end) break;
            pool.emplace_back(firm_worker, begin, end);
        }
        for (auto &th : pool) th.join();
    }

    for (int j = 0; j < D_G_C; j++) {
        double s = 0.0;
        const double *col = Ghat.data() + (size_t)j * n;
        for (int i = 0; i < n; i++) s += col[i];
        dvec[j] = s / n;
    }

    std::vector<double> Xc((size_t)n * D_G_C);
    std::copy(Ghat.begin(), Ghat.end(), Xc.begin());
    for (int j = 0; j < D_G_C; j++) {
        double *col = Xc.data() + (size_t)j * n;
        double mu = dvec[j];
        for (int i = 0; i < n; i++) col[i] -= mu;
    }

    // Omega = Xc'Xc/(n-1), symmetric rank-k update -- same routine/reasoning
    // as mh_tilted_moments_B_cpp (half the FLOPs of a general dgemm, the
    // mathematically correct routine for a PSD covariance matrix).
    cblas_dsyrk(CblasColMajor, CblasUpper, CblasTrans,
                D_G_C, n, omega_div(n), Xc.data(), n, 0.0, Omega, D_G_C);
    for (int i = 0; i < D_G_C; i++)
        for (int j = i + 1; j < D_G_C; j++)
            Omega[j + i * D_G_C] = Omega[i + j * D_G_C];   // mirror upper -> lower (col-major: Omega[row+col*ld])
}

// ---- CUE objective, via Accelerate's dsyevr --------------------------------
// Exactly cue_objective_from_moments(dvec,Omega) from 1211-stage2-elvis-
// driver-AB.R, ported: eigendecompose Omega, keep eigenvalues > 1e-8*max,
// project dvec onto those eigenvectors, 0.5*sum(d2^2/eigenvalue). dsyevr
// returns eigenvalues in ASCENDING order (R's eigen() returns descending --
// doesn't matter here, the loop only checks each value against the max).
static double cue_objective_C(const double dvec[D_G_C], const double Omega_in[D_G_C * D_G_C]) {
    double A[D_G_C * D_G_C];
    std::copy(Omega_in, Omega_in + D_G_C * D_G_C, A);

    double w[D_G_C];
    __CLPK_integer n = D_G_C, lda = D_G_C, il = 1, iu = D_G_C, m, ldz = D_G_C, info;
    double vl = 0, vu = 0, abstol = 1e-10;
    __CLPK_integer lwork = -1, liwork = -1, iwork_query;
    double work_query;
    double Z[D_G_C * D_G_C];
    __CLPK_integer isuppz[2 * D_G_C];

    dsyevr_((char *)"V", (char *)"A", (char *)"U", &n, A, &lda, &vl, &vu, &il, &iu,
            &abstol, &m, w, Z, &ldz, isuppz, &work_query, &lwork, &iwork_query, &liwork, &info);
    lwork = (__CLPK_integer)work_query;
    liwork = iwork_query;
    std::vector<double> work(lwork);
    std::vector<__CLPK_integer> iworkv(liwork);
    dsyevr_((char *)"V", (char *)"A", (char *)"U", &n, A, &lda, &vl, &vu, &il, &iu,
            &abstol, &m, w, Z, &ldz, isuppz, work.data(), &lwork, iworkv.data(), &liwork, &info);

    if (info != 0 || m < 1) return std::numeric_limits<double>::infinity();

    double max_eig = w[m - 1];
    double obj = 0.0;
    for (int k = 0; k < m; k++) {
        if (keep_eig(w[k], max_eig)) {
            double d2 = 0.0;
            for (int i = 0; i < D_G_C; i++) d2 += Z[i + k * D_G_C] * dvec[i];
            obj += 0.5 * d2 * d2 / w[k];
        }
    }
    return obj;
}

// ---- inner problem: profile (delta0,eta,gamma[1:6]) at fixed (lambda,delta1,delta2) --

struct InnerParams {
    const std::vector<FirmData> *firms;
    double lambda, delta1, delta2;
    int n_burn, n_keep, n_threads;
    uint64_t base_seed;
};

static double inner_obj(unsigned n, const double *x, double *grad, void *data) {
    (void)n; (void)grad;
    InnerParams *p = static_cast<InnerParams *>(data);
    double delta0 = x[0], eta = x[1];
    double gamma[D_G_C];
    for (int t = 0; t < D_G_C; t++) gamma[t] = x[2 + t];

    double dvec[D_G_C], Omega[D_G_C * D_G_C];
    compute_dvec_omega_C(*(p->firms), p->lambda, delta0, p->delta1, p->delta2, eta, gamma,
                          p->n_burn, p->n_keep, p->base_seed, p->n_threads, dvec, Omega);
    return cue_objective_C(dvec, Omega);
}

struct FitResult {
    double delta0, eta, gamma[D_G_C], Lhat;
    int convergence, iters;       // pass-2's own iters (kept for back-compat with existing callers)
    double Lhat_pass1;   // pass-1's own Lhat, for the "did pass 2 wander" diagnostic (2026-09-07):
    double wander;       // Euclidean distance in (delta0,eta,gamma) between pass-1 and pass-2 solutions --
                          // large wander = pass 2 moved far from where pass 1 landed, a sign that point
                          // may not have found a stable optimum (vs. a small/near-zero wander, pass 2
                          // quickly re-confirming pass 1's point -- NOT by itself proof of a GLOBAL
                          // optimum, just of local stability around wherever pass 1 landed; see chat).
    int iters_pass1, convergence_pass1;   // full iteration/convergence accounting for BOTH passes (2026-09-08)
                                           // -- needed to actually calibrate real per-point convergence cost,
                                           // not guess at it (this session's whole point in starting with a
                                           // small run first).
    double point_seconds;   // this point's OWN wall-clock time (both passes), not the run's cumulative
                             // elapsed -- the earlier shell run only logged cumulative time, which made it
                             // impossible to see individual convergence times for points sharing a batch.
};

// Two-pass BOBYQA refinement (AK2020-style, same convention as
// fit_one_lambda_A/B): run once, re-run warm-started from the result with
// the SAME unbounded gamma bounds (not narrowed), as a cheap convergence
// check. gamma unbounded per Schennach/AK2020 (ELVIS.pdf p.359) -- see
// CLAUDE.md's ELVIS-implementation-conventions section.
//
// x0_in (2026-09-07): optional caller-supplied starting point (delta0, eta,
// gamma[1:7]) -- used by the shell-based grid runner below to warm-start
// each new point from its nearest ALREADY-SOLVED neighbor rather than a
// fixed (0,...,0) default (the trim-sweep's own path-dependence lesson,
// generalized to a 3-D lattice via Chebyshev-shell BFS + nearest-neighbor
// chaining instead of a 1-D sequential ordering). nullptr = old behavior
// (start from zero), still used by the flat/points_csv modes.
static FitResult fit_one_grid_point(
    const std::vector<FirmData> &firms, double lambda, double delta1, double delta2,
    int n_burn, int n_keep, uint64_t base_seed, int n_threads, double maxtime,
    const double *x0_in = nullptr
) {
    const int n_par = 2 + D_G_C;   // delta0, eta, gamma[1:7]
    InnerParams params{&firms, lambda, delta1, delta2, n_burn, n_keep, n_threads, base_seed};

    double lower[n_par], upper[n_par], x[n_par];
    lower[0] = -DELTA_BOUND; upper[0] = DELTA_BOUND;
    lower[1] = 0.0;          upper[1] = 0.999;
    for (int t = 0; t < D_G_C; t++) { lower[2 + t] = -HUGE_VAL; upper[2 + t] = HUGE_VAL; }
    if (x0_in) {
        for (int t = 0; t < n_par; t++) x[t] = x0_in[t];
        x[1] = std::min(std::max(x[1], 0.0), 0.998);   // eta must stay strictly inside its box
                                                         // even if the warm-start source was near the edge
    } else {
        for (int t = 0; t < n_par; t++) x[t] = 0.0;
    }

    auto run_bobyqa = [&](double *xstart) -> FitResult {
        nlopt_opt opt = nlopt_create(NLOPT_LN_BOBYQA, n_par);
        nlopt_set_lower_bounds(opt, lower);
        nlopt_set_upper_bounds(opt, upper);
        nlopt_set_min_objective(opt, inner_obj, &params);
        nlopt_set_xtol_rel(opt, 1e-4);   // loosened vs default 1e-6: real MC noise from the finite chain,
                                          // same reasoning as fit_one_lambda_A/B's own xtol_rel
        nlopt_set_maxeval(opt, 2000);
        nlopt_set_maxtime(opt, maxtime);
        double minf = HUGE_VAL;
        nlopt_result res = nlopt_optimize(opt, xstart, &minf);
        int iters = nlopt_get_numevals(opt);
        nlopt_destroy(opt);
        FitResult r;
        r.delta0 = xstart[0]; r.eta = xstart[1];
        for (int t = 0; t < D_G_C; t++) r.gamma[t] = xstart[2 + t];
        r.Lhat = minf; r.convergence = static_cast<int>(res); r.iters = iters;
        return r;
    };

    auto t_start = std::chrono::steady_clock::now();
    FitResult r1 = run_bobyqa(x);
    double x2[n_par];
    x2[0] = r1.delta0; x2[1] = r1.eta;
    for (int t = 0; t < D_G_C; t++) x2[2 + t] = r1.gamma[t];
    FitResult r2 = run_bobyqa(x2);
    r2.point_seconds = std::chrono::duration<double>(std::chrono::steady_clock::now() - t_start).count();

    r2.Lhat_pass1 = r1.Lhat;
    r2.iters_pass1 = r1.iters;
    r2.convergence_pass1 = r1.convergence;
    double sq = (r2.delta0 - r1.delta0) * (r2.delta0 - r1.delta0) + (r2.eta - r1.eta) * (r2.eta - r1.eta);
    for (int t = 0; t < D_G_C; t++) sq += (r2.gamma[t] - r1.gamma[t]) * (r2.gamma[t] - r1.gamma[t]);
    r2.wander = std::sqrt(sq);
    return r2;
}

// ---- grid + threading -------------------------------------------------------

struct GridPoint { double lambda, delta1, delta2; };
struct GridResult { GridPoint gp; FitResult fit; };

static std::vector<double> log_space(double lo, double hi, int n) {
    if (n == 1) return {lo};   // n==1 would otherwise divide by (n-1)==0 -- caught via a real NaN/Inf
                               // run during timing measurement (2026-09-07), not assumed away
    std::vector<double> v(n);
    double llo = std::log(lo), lhi = std::log(hi);
    for (int i = 0; i < n; i++) v[i] = std::exp(llo + (lhi - llo) * i / (n - 1));
    return v;
}
static std::vector<double> lin_space(double lo, double hi, int n) {
    if (n == 1) return {lo};
    std::vector<double> v(n);
    for (int i = 0; i < n; i++) v[i] = lo + (hi - lo) * i / (n - 1);
    return v;
}

// ---- tiny CSV I/O (fixed known schema, no library needed for ~30K rows) ----

static std::vector<FirmData> read_firm_csv(const std::string &path) {
    std::ifstream f(path);
    if (!f) { std::cerr << "Cannot open " << path << "\n"; std::exit(1); }
    std::string header;
    std::getline(f, header);
    std::vector<std::string> cols;
    { std::stringstream ss(header); std::string tok; while (std::getline(ss, tok, ',')) cols.push_back(tok); }
    std::map<std::string, int> idx;
    for (size_t i = 0; i < cols.size(); i++) idx[cols[i]] = static_cast<int>(i);
    for (const char *req : {"M_star", "cal_V", "tilde_cal_W", "sales_tax_rate_purchases", "beta", "corner", "row_id"})
        if (idx.find(req) == idx.end()) { std::cerr << "Missing column: " << req << "\n"; std::exit(1); }
    g_has_audit_g = idx.count("audit_g") > 0; g_has_audit_gv = idx.count("audit_gv") > 0; g_has_umed = idx.count("umed") > 0; g_has_pshare = idx.count("pshare") > 0;

    std::vector<FirmData> out;
    std::string line;
    while (std::getline(f, line)) {
        if (line.empty()) continue;
        std::vector<std::string> fields;
        { std::stringstream ss(line); std::string tok; while (std::getline(ss, tok, ',')) fields.push_back(tok); }
        FirmData d;
        d.Mstar   = std::strtod(fields[idx["M_star"]].c_str(), nullptr);
        d.V       = std::strtod(fields[idx["cal_V"]].c_str(), nullptr);
        d.Wt      = std::strtod(fields[idx["tilde_cal_W"]].c_str(), nullptr);
        d.tau_rho = std::strtod(fields[idx["sales_tax_rate_purchases"]].c_str(), nullptr);
        d.beta    = std::strtod(fields[idx["beta"]].c_str(), nullptr);
        d.corner  = static_cast<int>(std::strtod(fields[idx["corner"]].c_str(), nullptr));
        d.row_id  = std::strtol(fields[idx["row_id"]].c_str(), nullptr, 10);
        if (idx.count("t1"))   d.t1   = std::strtod(fields[idx["t1"]].c_str(), nullptr);
        if (idx.count("pgdp")) d.pgdp = std::strtod(fields[idx["pgdp"]].c_str(), nullptr);
        if (idx.count("Mbar")) d.Mbar = std::strtod(fields[idx["Mbar"]].c_str(), nullptr);
        if (idx.count("ltau_bar")) d.ltau_bar = std::strtod(fields[idx["ltau_bar"]].c_str(), nullptr);
        if (idx.count("sig2eps")) d.sig2eps = std::strtod(fields[idx["sig2eps"]].c_str(), nullptr);
        if (idx.count("year")) d.yidx = static_cast<int>(std::strtol(fields[idx["year"]].c_str(), nullptr, 10)) - 81;
        if (idx.count("sic_3")) d.sic = static_cast<int>(std::strtol(fields[idx["sic_3"]].c_str(), nullptr, 10));
        if (idx.count("plant_id")) d.plant = std::strtol(fields[idx["plant_id"]].c_str(), nullptr, 10);
        if (idx.count("audit_g")) d.audit_g = static_cast<int>(std::strtol(fields[idx["audit_g"]].c_str(), nullptr, 10));
        if (idx.count("audit_gv")) d.audit_gv = static_cast<int>(std::strtol(fields[idx["audit_gv"]].c_str(), nullptr, 10));
        if (idx.count("umed")) d.umed = std::strtod(fields[idx["umed"]].c_str(), nullptr);
        if (idx.count("pshare")) d.pshare = std::strtod(fields[idx["pshare"]].c_str(), nullptr);
        out.push_back(d);
    }
    {   // industry index among interior firms (IND5): sorted distinct sic_3 of corner==0 firms
        std::vector<int> sics;
        for (const FirmData &d : out) if (d.corner == 0 && d.sic >= 0) sics.push_back(d.sic);
        std::sort(sics.begin(), sics.end()); sics.erase(std::unique(sics.begin(), sics.end()), sics.end());
        for (FirmData &d : out) if (d.sic >= 0) {
            auto it = std::lower_bound(sics.begin(), sics.end(), d.sic);
            d.jidx = (it != sics.end() && *it == d.sic) ? static_cast<int>(it - sics.begin()) : -1;
        }
        if (!sics.empty()) { std::cout << "industries (interior, jidx order):"; for (int v : sics) std::cout << " " << v; std::cout << "\n"; }
    }
    return out;
}

// ---- CLI: plain key=value args, mirroring Code/Deconvolution/utils-cli.R's ---
// spirit (self-labeled run header, one config per invocation).

static std::map<std::string, std::string> parse_cli(int argc, char **argv) {
    std::map<std::string, std::string> opt;
    for (int i = 1; i < argc; i++) {
        std::string a = argv[i];
        auto pos = a.find('=');
        if (pos != std::string::npos) opt[a.substr(0, pos)] = a.substr(pos + 1);
    }
    return opt;
}
static std::string get_opt(std::map<std::string, std::string> &opt, const std::string &key, const std::string &def) {
    auto it = opt.find(key);
    return it == opt.end() ? def : it->second;
}

// ---- Shell mode (2026-09-07) ------------------------------------------------
// Replaces random point selection: process the (lambda,delta1,delta2) grid in
// Chebyshev-shell order outward from the CENTER index, warm-starting every
// point from its nearest ALREADY-SOLVED neighbor rather than a fixed
// (0,...,0) default -- the trim-sweep's path-dependence lesson (naive/reset
// warm starts produced 2-4 orders of magnitude of spurious noise there),
// generalized from a 1-D sequential ordering to a 3-D lattice. The center
// point (shell 0) has no prior neighbor to chain from, so it's solved via
// multi-start instead (several genuinely different x0's, keep the best) --
// otherwise a bad center would get silently propagated outward through every
// later shell's chained warm start, LOOKING stable (each point's own pass-2
// quickly re-confirms pass-1) while actually being consistently wrong.
//
// Parallelism: shells are processed strictly in order (shell d+1 only starts
// once shell d is fully solved and in the pool), but points WITHIN a shell
// don't depend on each other, only on already-solved earlier shells -- so a
// shell's points run concurrently, with per-point thread count sized
// dynamically: shell_size<=n_threads -> split n_threads across the shell's
// points; shell_size>n_threads -> process in single-threaded batches of
// n_threads points (pool updated after each batch, so later batches in an
// over-cap shell can still chain from earlier batches in the SAME shell).
static const int N_PAR_C = 2 + D_G_C;

struct SolvedPoint { double lambda, delta1, delta2; FitResult fit; };

static void nearest_x0(const std::vector<SolvedPoint> &pool, double lam, double d1, double d2, double *x0_out) {
    double best_d = std::numeric_limits<double>::infinity();
    const SolvedPoint *best = nullptr;
    double llam = std::log10(lam);
    for (auto &sp : pool) {
        double dl = llam - std::log10(sp.lambda), dd1 = d1 - sp.delta1, dd2 = d2 - sp.delta2;
        double dist = dl * dl + dd1 * dd1 + dd2 * dd2;
        if (dist < best_d) { best_d = dist; best = &sp; }
    }
    x0_out[0] = best->fit.delta0; x0_out[1] = best->fit.eta;
    for (int t = 0; t < D_G_C; t++) x0_out[2 + t] = best->fit.gamma[t];
}

static void run_shell_mode(
    const std::vector<FirmData> &firms, const std::vector<double> &lambdas,
    const std::vector<double> &delta1s, const std::vector<double> &delta2s,
    int max_shell, int n_burn, int n_keep, uint64_t base_seed, int n_threads, double maxtime,
    const std::string &output_csv, int tpp_override
) {
    int n_lambda = (int)lambdas.size(), n_delta1 = (int)delta1s.size(), n_delta2 = (int)delta2s.size();
    int cl = (n_lambda - 1) / 2, cd1 = (n_delta1 - 1) / 2, cd2 = (n_delta2 - 1) / 2;
    std::cout << "Center index: (lambda[" << cl << "]=" << lambdas[cl] << ", delta1[" << cd1 << "]="
              << delta1s[cd1] << ", delta2[" << cd2 << "]=" << delta2s[cd2] << ")\n";

    std::map<int, std::vector<GridPoint>> shells;
    for (int i = 0; i < n_lambda; i++)
        for (int j = 0; j < n_delta1; j++)
            for (int k = 0; k < n_delta2; k++) {
                int cheb = std::max({std::abs(i - cl), std::abs(j - cd1), std::abs(k - cd2)});
                if (cheb <= max_shell) shells[cheb].push_back({lambdas[i], delta1s[j], delta2s[k]});
            }
    for (auto &kv : shells) std::cout << "  shell " << kv.first << ": " << kv.second.size() << " points\n";

    std::vector<SolvedPoint> pool;
    std::vector<GridResult> results;
    auto t0 = std::chrono::steady_clock::now();
    auto elapsed = [&]() { return std::chrono::duration<double>(std::chrono::steady_clock::now() - t0).count(); };

    // ---- Shell 0: the center, solved via multi-start (not chaining) --------
    // Three genuinely different starting points: the naive default, and two
    // real converged solutions from earlier this-session testing (different
    // grid points, but real basins, not arbitrary guesses) -- if they agree
    // (similar Lhat), reasonable confidence the center found a stable
    // optimum; if they don't, that's important to know BEFORE it propagates.
    {
        GridPoint c = shells[0][0];
        double starts[3][N_PAR_C] = {
            {0,0, 0,0,0,0,0,0,0},
            {-7.27548579644105, 0.791243131559717, -0.207738611357925,-0.0922965563596676,
             -2.32248151072362,-0.458939026599753,-0.148946021474787,-0.0888590597159847,0.142960100417117},
            {-8.25139495463983, 0.504605456412412, -0.646403311952324,-3.52725324079963,
             -1.00948958366341,1.51592513084617,-2.50949563274311,-8.84430989507563,-1.94411380250129}
        };
        FitResult best; double best_lhat = std::numeric_limits<double>::infinity();
        for (int s = 0; s < 3; s++) {
            FitResult r = fit_one_grid_point(firms, c.lambda, c.delta1, c.delta2, n_burn, n_keep, base_seed,
                                              n_threads, maxtime, starts[s]);
            std::cout << "  [center multi-start " << s << "] Lhat=" << r.Lhat
                      << " conv=" << r.convergence << "(pass1 conv=" << r.convergence_pass1 << ")"
                      << " iters=" << r.iters_pass1 << "+" << r.iters << " point_seconds=" << r.point_seconds
                      << " wander=" << r.wander << " run_elapsed=" << elapsed() << "s\n" << std::flush;
            if (r.Lhat < best_lhat) { best_lhat = r.Lhat; best = r; }
        }
        std::cout << "  -> center solved, best Lhat=" << best.Lhat << "\n";
        pool.push_back({c.lambda, c.delta1, c.delta2, best});
        results.push_back({c, best});
    }

    // ---- Shells 1..max_shell: nearest-solved-neighbor chained, shell-wise parallel --
    for (int d = 1; d <= max_shell; d++) {
        auto &pts = shells[d];
        int shell_size = (int)pts.size();
        std::cout << "== shell " << d << ": " << shell_size << " points ==\n" << std::flush;

        // tpp_override (2026-09-08): explicit threads-per-point, when given,
        // beats the dynamic batch-size-driven default -- the dynamic rule
        // (maximize concurrent points) was throughput-optimal under the
        // Amdahl analysis, but that analysis assumed points always converge;
        // it didn't account for a fixed maxtime interacting with convergence
        // RELIABILITY (measured directly: shell 1 at tpp=1 hit maxtime on
        // 20/26 points). tpp_override<=0 keeps the old dynamic behavior.
        int concurrent = (tpp_override > 0) ? std::max(1, n_threads / tpp_override) : n_threads;
        for (int start = 0; start < shell_size; start += concurrent) {
            int batch = std::min(concurrent, shell_size - start);
            int tpp = (tpp_override > 0) ? tpp_override : std::max(1, n_threads / batch);

            std::vector<double> x0s((size_t)batch * N_PAR_C);
            for (int i = 0; i < batch; i++)
                nearest_x0(pool, pts[start + i].lambda, pts[start + i].delta1, pts[start + i].delta2,
                           &x0s[(size_t)i * N_PAR_C]);

            std::vector<FitResult> batch_results(batch);
            std::vector<std::thread> workers;
            for (int i = 0; i < batch; i++) {
                workers.emplace_back([&, i]() {
                    batch_results[i] = fit_one_grid_point(firms, pts[start + i].lambda, pts[start + i].delta1,
                                                           pts[start + i].delta2, n_burn, n_keep, base_seed,
                                                           tpp, maxtime, &x0s[(size_t)i * N_PAR_C]);
                });
            }
            for (auto &w : workers) w.join();

            for (int i = 0; i < batch; i++) {
                GridPoint gp = pts[start + i];
                FitResult &r = batch_results[i];
                std::cout << "  [shell " << d << ", " << (start + i + 1) << "/" << shell_size << ", tpp=" << tpp << "] "
                          << "lambda=" << gp.lambda << " d1=" << gp.delta1 << " d2=" << gp.delta2
                          << " Lhat=" << r.Lhat << " conv=" << r.convergence << "(pass1 conv=" << r.convergence_pass1 << ")"
                          << " iters=" << r.iters_pass1 << "+" << r.iters << " point_seconds=" << r.point_seconds
                          << " wander=" << r.wander << " run_elapsed=" << elapsed() << "s\n" << std::flush;
                pool.push_back({gp.lambda, gp.delta1, gp.delta2, r});
                results.push_back({gp, r});
            }
        }
    }

    std::ofstream out(output_csv);
    if (!out.is_open()) { std::cerr << "ERROR: could not open output_csv: " << output_csv << "\n"; return; }
    out << std::setprecision(15);
    out << "lambda,delta1,delta2,delta0_hat,eta_hat,gamma1,gamma2,gamma3,gamma4,gamma5,gamma6,gamma7,"
           "Lhat,Lhat_pass1,wander,convergence,convergence_pass1,iters,iters_pass1,point_seconds,n\n";
    for (auto &r : results) {
        out << r.gp.lambda << "," << r.gp.delta1 << "," << r.gp.delta2 << ","
            << r.fit.delta0 << "," << r.fit.eta << ",";
        for (int t = 0; t < D_G_C; t++) out << r.fit.gamma[t] << ",";
        out << r.fit.Lhat << "," << r.fit.Lhat_pass1 << "," << r.fit.wander << ","
            << r.fit.convergence << "," << r.fit.convergence_pass1 << ","
            << r.fit.iters << "," << r.fit.iters_pass1 << "," << r.fit.point_seconds << "," << firms.size() << "\n";
    }
    out.close();
    std::cout << "Saved: " << output_csv << " (" << results.size() << " points)\n";
}

// ============================================================================
// ---- Moment set A (9 rows), (delta1,delta2)-grid mode (2026-09-08) --------
// ============================================================================
// Added alongside moment set C above (not replacing it) to port the LIVE
// 9-row moment set A (Code/Rcpp/1200-stage2-elvis.cpp's moment_g_A_one,
// restored row [6]=eps*e + appended row [8]=h_prime_bounded*eps, see
// CLAUDE.md/Research-log/log.md's 2026-09-08 entries) into the standalone
// build, for a (delta1,delta2) grid with LAMBDA as a profiled NUISANCE
// parameter (bounded, not fixed) -- the opposite fixed/free split from
// fit_one_grid_point above (which fixes lambda,delta1,delta2 and frees
// delta0,eta,gamma). Every grid point seeded INDEPENDENTLY from the SAME
// caller-supplied x0 -- deliberately NOT using run_shell_mode's nearest-
// solved-neighbor chaining (tollgate 1, 2026-09-08: chaining reintroduces
// path dependence; same-point seeding is what actually found the sensible-
// sign basin for lag_2_cal_W).
// TAU_ROW (2026-09-28, compile-time, separate binary grid_estimator_tau): moment set A gains row [9] =
// psi * ln(tau_bar), tau_bar = leave-one-out industry-year mean purchases tax rate (input column ltau_bar).
// The cost shock is independent of the exogenous benefit shifter, so the tax rate's variation must be absorbed by
// evasion through h -- the missing identifying restriction for the detection level (log 2026-09-28). Only for
// qform=exp_scale; lambdagrid mode only (main() rejects every other mode in this build). Default build: 9 rows,
// unchanged.
// YEAR_FE (2026-09-28, S4, separate binary grid_estimator_yfe; implies TAU_ROW): year intercepts in the cost level,
// psi = h - (delta0 + delta0_t) + delta1*om - delta2*om^2, t = year - 81 (1981 = base, delta0_81 = 0), plus rows
// [10..19] = psi * 1{t = k}, k = 1..10. Parameter vector: delta0, delta1, delta2, delta0_82..91, gamma[1..20].
// The intercepts reach the firm chains through the global g_d0yr, set at each objective evaluation -- safe only
// with ONE grid point per process (main() enforces it); firm threads only read it.
// KINK (2026-09-28, Hans; separate binary grid_estimator_kink; implies TAU_ROW; not combined with YEAR_FE): power
// detection q = x^k, x = e/(kappa*Mbar), with a kink at the FOC ceiling c_k = (1+k)^(-1/k): beyond it q is flat and the
// FOC cannot rationalize the firm. Latent support is the whole physical range M in (0, M*]; a draw with x < c_k gets
// every row, a draw with x >= c_k only the eps rows (psi, score and tax rows = 0, as for corner firms). Row [10] =
// 1{x >= c_k} - s fixes the share beyond the kink at s (CLI kink_share), so the tilt cannot push every firm past it.
// k is ESTIMATED (extra parameter, bounds [0.02, 0.99]); the grid parameter "lambda" is the scale multiplier kappa.
// k reaches the chains through the global g_kpow (one point per process, as YEAR_FE).
#if (defined(YEAR_FE) || defined(KINK)) && !defined(TAU_ROW)
#define TAU_ROW
#endif
#if defined(YEAR_FE) && defined(KINK)
#error "YEAR_FE and KINK are not combined yet"
#endif
// KINK_S (2026-09-29, Hans; with KINK; binary grid_estimator_kinks): k FIXED (CLI k_fixed, equal bounds), the share s
// beyond the kink ESTIMATED (extra parameter x[4], bounds [0.02, 0.6], start = kink_share), and a score row for the
// scale: row [11] = eps * softsign(dh/dkappa), dh/dkappa = k(1+k) x^k / (kappa B). h does not depend on s, so s has
// no score row; it is pinned only through the share row [10]. Runs 1558-1560 used KINK alone (11 rows).
#if defined(KINK_S) && !defined(KINK)
#define KINK
#endif
// Build-flag guards (2026-09-30, audit finding 8): combinations that compiled but read/wrote the wrong array slots.
#if defined(KINK) && !defined(KINK_S)
#error "KINK without KINK_S no longer builds from this source (g_dropmask, share/score rows); runs 1558-1560 used an older revision"
#endif
#if defined(EPSVAR) && !defined(KINK_S)
#error "EPSVAR requires KINK_S (row 12 sits after the KINK_S rows)"
#endif
#if defined(IND5) && !(defined(KINK_S) && defined(EPSVAR))
#error "IND5 requires KINK_S and EPSVAR (industry rows start at 13)"
#endif
#if defined(KAPPA_FREE) && !defined(KINK_S)
#error "KAPPA_FREE requires KINK_S (kappa is x[5] after k, s)"
#endif
#ifdef YEAR_FE
static const int N_XFE = 10;
static double g_d0yr[11] = {0};
#elif defined(KINK_S)
// KAPPA_FREE (2026-09-30, Hans; with KINK_S; binary grid_estimator_kf): the scale kappa is ESTIMATED as x[5] (bounds
// [0.02, 5]); the lambdagrid value is only its start. Output column kappa_hat; adiag par gains kappa at the end of the
// k,s block (par[6]) and uses it instead of par[0].
#ifdef KAPPA_FREE
static const int N_XFE = 3;
#else
static const int N_XFE = 2;
#endif
static double g_kpow = 0.5;
static double g_kshare = 0.3;
static double g_kmax = 0.99;   // unused with k_fixed; kept so the KINK code paths compile unchanged
static double g_kfixed = -1.0; // CLI k_fixed (required in this build)
static double g_sfixed = -1.0; // CLI s_fixed (optional): if in (0,1), s is held fixed at it (equal bounds) instead of estimated
static double g_kmin = 0.05;   // lower bound on k when k_free=1 (CLI k_min)
static bool g_kfree = false;   // CLI k_free=1 (2026-09-30): k estimated (bounds [0.05, k_max]), start = x0's k; k_fixed then only labels the run
// drop_rows (2026-09-29): bitmask of moment rows set to 0 inside the moment function (interior draws), so a dropped row
// neither enters the objective (zero variance -> its eigen-direction is cut) nor steers the tilt through gamma.
// Corner firms only fill rows 1, 5, 7 (and 12, 13+ in EPSVAR/IND5); the mask is applied in the corner branch too.
// CLI drop_rows=11,6,...
static unsigned g_dropmask = 0;
#elif defined(KINK)
static const int N_XFE = 1;
static double g_kpow = 0.5;
static double g_kshare = 0.3;
static double g_kmax = 0.99;   // upper bound on k (CLI k_max; 0.99 = concave only, as in 1558/1559; up to 2.9 allowed: SOC 1-k<2)
#else
static const int N_XFE = 0;
#endif
// EPSVAR (2026-09-29, with KINK_S; binary grid_estimator_eps): row [12] = eps^2 - sig2eps_j, sig2eps_j = corporations'
// first-stage eps variance in the firm's industry (input column sig2eps); interior firms only, every draw (like the eps
// rows). Disciplines the tilt's dispersion of measurement error (tilted var(eps) 1.17 vs data bound 0.18, log 2026-09-29).
// IND5 (2026-09-30, Hans; with KINK_S+EPSVAR; binary grid_estimator_ind5): rows [13..13+N_IND-1] = eps*lnM*1{industry j},
// the pooled row [5] broken by interior industry (drop row 5 when using them: it is their sum, Omega would be singular).
// Every draw, like row 5; corner firms too.
// IND5P (2026-10-02, Hans: share of overreporters; build grid_estimator_ind5p): on top of IND5's industry block, rows
// [13+N_IND .. 13+2N_IND-1] = (1{u >= share_u} - pshare_j) * 1{j}, pshare_j = deconvolved P(u >= share_u) of industry j
// (input column pshare; CLI share_u, default 0.05). Bounded indicator rows: left out of the rho penalty (D = inf).
#ifdef IND5
static const int N_IND = 9;
#endif
// IND5 row content (2026-10-01, CLI ind_rows): epslnm = eps*lnM*1{j} (default, 1588), eps = eps*1{j} (design i:
// E[u] - E[V] = 0 by industry, i.e. E[eps | j] = 0; drop the pooled row 1, their sum), median = (1{u <= umed_j} - 1/2)*1{j}
// for firms whose industry has a deconvolved median (design i robustness; other firms 0), eps_cw = w_i*eps*1{j} with
// w_i = tau_P M*_i / (mean of tau_P M* over interior firms of industry j) (2026-10-03, Hans: fit claims, not firm counts;
// E[w eps | j] = 0 is valid because eps, output measurement error, is independent of the observables tau_P and M*; mean
// one within industry, so the rows keep the eps scale; drop the pooled row 1 as with eps); eps_cwj = c_j*eps*1{j}, c_j =
// industry mean claims / interior mean claims (one constant per industry: under the CUE a reparametrization of eps).
static int g_ind_mode = 0;
[[maybe_unused]] static double g_share_u = 0.05;   // IND5P threshold (CLI share_u)
// Audit moment (2026-10-01, design ii, CLI audit_p; power_nokink only): row 10 (unused without the kink) becomes
// audit_g * (q(e) - audit_p), q = (e/(kappa Mbar))^k: the model's detection probability matched to an external audit
// probability in the group G (top 10% of capital within industry). audit_p < 0 = off.
static bool g_audit_on = false;
static double g_audit_p = -1.0;
// Counterfactual (2026-10-02, mode=cfprofile; power_nokink only): row 10 (unused without the kink) becomes the credit
// moment (1+Delta) tau_P [M + (1 - q(e')) e'] / scale - T, where e'(Delta) is the firm's new evasion at the same true M
// (and hence the same omega and psi): from the FOC, x'^k = [1 - B(x)/(1+Delta)] / (1+k), x = e/(kappa Mbar), corner e' = 0
// when the bracket is <= 0. T = expected purchases credit per firm (in units of scale), the auxiliary parameter; theta is
// fixed at the operating point and only gamma is free (AK2020 App. F). Real revenue per firm = t1/pgdp - T * scale.
static bool g_cf_on = false;
static double g_cf_Delta = 0.0, g_cf_T = 0.0, g_cf_scale = 1.0;
// cf_target (2026-10-02, Hans): what row 10 targets; every target is linear in the parameter T, row = a - T b:
//   0 level      : C(Delta)/scale - T                                  (b = 1; T = expected claimed credits)
//   1 diff_beh   : [C(Delta) - (1+Delta) C(0)]/scale - T              (behavioural change; paired on the same draw)
//   2 diff_total : [C(Delta) - C(0)]/scale - T                         (total change, paired)
//   3 elast_x    : (1+Delta)[xbar(Delta+h) - xbar(Delta-h)]/(2h) - T xbar(Delta)   (T = elasticity of mean overreporting
//                  e'/M w.r.t. tau_P; ratio moment, T = E[slope]/E[level])
//   4 elast_claims: (1+Delta)[C(Delta+h) - C(Delta-h)]/(2h)/scale - T C(Delta)/scale (T = elasticity of claimed credits)
//   5 overrep    : M*/M - 1 - T  (static, Delta ignored; 2026-10-03, Hans: T = E[filed claims / true claims - 1] =
//                  E[M*/M] - 1, the mean of firm-level overreporting ratios; unbounded as M -> 0, a test of whether it breaks)
//   6 gap        : L/scale - T P/scale  (static; 2026-10-03 Hans: VAT gap among interior firms, ratio of means T = E[L]/E[P],
//                  L = tau_P (1-q(e)) e = loss from undetected overreporting, P = t1/pgdp - tau_P M = potential revenue
//                  (credits on true materials only); actual R = P - L, so T = 1 - R/P, and with P < 0, R/P - 1 = L/|P|.
//                  The moment function returns b = -tau_P M/scale; cfprofile adds the firm's t1/pgdp/scale.)
//   7 true_credit: tau_P M/scale - T  (static; expected credits on true materials, in units of scale)
//   8 loss_t1    : L/scale - T (t1/pgdp + x)/scale  (static; 2026-10-04 Hans: revenue lost to undetected overreporting as a
//                  share of the sales tax owed on sales; x = cf_t1_extra, pesos per interior firm added to the denominator
//                  for other firms' observed t1 (0 = interior firms only; (A) trimmed firms; (B) whole economy))
//   9 revenue    : [t1/pgdp - C(Delta)]/scale - T  (2026-10-05 Hans: T = expected real net sales-tax revenue per firm, in
//                  units of scale. The moment function returns a = -C(Delta)/scale; cfprofile adds the firm's observed
//                  t1/pgdp/scale to a, so the variance of t1 enters Omega -- unlike revenue_hat in the CSV, which is
//                  mean(t1/pgdp) - T_hat*scale from the claims target, t1 treated as known)
//  10 elast_revenue: (1+D)[R(D+h) - R(D-h)]/(2h)/scale - T R(D)/scale, R = t1 r^beta/pgdp - C (2026-10-06 Hans: elasticity of net
//                  revenue w.r.t. tau_P; R < 0 at the baseline, so T > 0 means revenue becomes more negative). The moment function
//                  returns the -C parts; cfprofile adds the t1 parts (a: (1+D) t1/pgdp [r(D+h)^beta - r(D-h)^beta]/(2h)/scale,
//                  b: t1/pgdp r(D)^beta/scale)
//  11 mrev       : 0.01 (1+D)[R(D+h) - R(D-h)]/(2h)/scale - T  (change in net revenue per 1 percent of the rate; same split)
//  12 diff_input : (1+D) tau_P (r(D) - 1) M/scale - T   (input-demand part of C(D) - C(0); 0 unless cf_mresp=1)
//  13 diff_evasion: (1+D) tau_P [(1 - q(e')) e' - (1 - q(e)) e]/scale - T   (evasion part; diff_beh = diff_input + diff_evasion,
//                  and C(D) - C(0) = D C(0) + diff_input + diff_evasion)
//  15 mean_q     : q(e) - T  (static; 2026-10-06 Hans: mean detection probability at the baseline evasion, q = x^k; CI by test
//                  inversion. Quantiles of q are not linear in T and stay forward-simulated (adiag TARGETED lines))
//  14 diff_revenue: [R(D) - R(0)]/scale - T, paired on the same draw (2026-10-06 Hans: revenue elasticity like the claims one, at
//                  Delta = +-1, 1.5, 2 percent: T/(Delta R(0)) with R(0) from the Delta = 0 revenue run). Moment function: -(C(D) - C(0));
//                  cfprofile adds t1 (r(D)^beta - 1)/pgdp/scale (0 under fixed M).
// C(D) = (1+D) tau_P [r M + (1 - q') e'(D)], xbar(D) = e'(D)/(r M); h = cf_h (default 0.01: one percent up and down).
static int g_cf_target = 0;
static bool g_cf_cold = false;   // cf_cold=1 (2026-10-03): every profiled gamma solve starts from the operating gamma (no warm path along T)
static double g_cf_h = 0.01;
static double g_cf_t1_extra = 0.0;
// cf_mresp=1 (2026-10-05, Hans): true materials respond to the purchases rate through the two-tax wedge in the materials FOC,
// (1-tau_S) P beta Y/M = (1 - (1+Delta) tau_P) rho, with K, L, omega, eps fixed (Cobb-Douglas): M(Delta) = r M, where
// r = [(1 - (1+Delta) tau_P)/(1 - tau_P)]^(-1/(1-beta)) is the same for every draw of a firm; output and the sales tax on sales
// scale by r^beta. Claims use r M; the revenue target uses t1 r^beta. e'(Delta) is unchanged (the evasion FOC has no M).
// Default 0 = true M fixed (every run before 2026-10-05).
static bool g_cf_mresp = false;
static inline double cf_mresp_r(double D, double tau_P, double beta) {
    return g_cf_mresp ? std::pow((1.0 - (1.0 + D) * tau_P) / (1.0 - tau_P), -1.0 / (1.0 - beta)) : 1.0;
}
static bool g_cf_multi = false;   // cf_multi=1 (2026-10-05, audit): each profiled gamma solve runs from several starts and keeps the lowest L:
                                   // the operating gamma, the warm gamma, and the operating gamma with gamma[10] (the counterfactual tilt) set to
                                   // each value in cf_g10, and the best gamma seen so far at any T. Cold-only solves often left gamma[10] at 0 (TS_min stuck at the operating 23.689).
static bool g_cf_op = true;   // cf_op=0 (2026-10-05): with cf_multi, skip the separate operating-gamma start (the warm start is the
                              // operating gamma at the first T anyway; the operating start won 1 of 95 solves, as a tie, in the start diagnostics)
static std::vector<double> g_cf_g10 = {3.0, -3.0, 10.0, -10.0, 30.0, -30.0};
// cf_decomp=1 (2026-10-09, Hans; read-only diagnostic, cf_target=diff_evasion only): for each Delta > 0, the overreporting response
// at +Delta and -Delta at the OPERATING weights (theta and gamma of the fit, row 10's gamma = 0; no gamma solve, no profile), split
// by the draw's current detection probability q = x^k into bins [0, Delta/(1+k)) (draws that stop at the cut -Delta: e' = 0),
// [Delta/(1+k), 0.02), [0.02, 0.05), [0.05, 0.15), [0.15, inf). Per bin: weight share and contribution to E[diff_evasion(+-Delta)]
// (units of scale); the bins add up to the "T at operating gamma" of the cfprofile runs at +-Delta. Asymmetry per bin:
// resp_plus + resp_minus (the first-order parts cancel). Writes one CSV row per Delta and bin; no profiling.
static bool g_cf_decomp = false;
static std::vector<double> g_cf_grid;   // cf_grid=T1,T2,...: also print 2nL at these T (diagnostic of the profile's shape)   // cf_target=loss_t1: extra t1/pgdp per interior firm in the denominator
#if defined(YEAR_FE)
static const int D_G_A = 20;
#elif defined(KINK_S) && defined(EPSVAR) && defined(IND5) && defined(IND5P)
static const int D_G_A = 13 + 2 * N_IND;   // IND5P: + share rows [13+N_IND .. 13+2N_IND-1]
#elif defined(KINK_S) && defined(EPSVAR) && defined(IND5)
static const int D_G_A = 13 + N_IND;
#elif defined(KINK_S) && defined(EPSVAR)
static const int D_G_A = 13;
#elif defined(KINK_S)
static const int D_G_A = 12;
#elif defined(KINK)
static const int D_G_A = 11;
#elif defined(TAU_ROW)
static const int D_G_A = 10;
#else
static const int D_G_A = 9;
#endif
typedef std::array<double, D_G_A> GVecA;
static_assert(D_G_A <= 32, "drop_rows bitmask is 32 bits");

// cut=ak (2026-09-29, Hans): moment-set-A objective exactly as AK2020's objMCcu (Appendix_B/cudafunctions/
// cuda_fastoptim.jl): Omega divided by n (not n-1), eigen-directions kept iff Lambda > 0 (not > 1e-8*max, a porting
// error), LAPACK default abstol (Julia's eigen). Rows dropped by drop_rows are removed from Omega and dvec before the
// eigendecomposition (= AK building the system without them). Default (cut=rel) unchanged, bit-for-bit.
static inline unsigned a_rowmask() {
#ifdef KINK_S
    return g_dropmask;
#else
    return 0u;
#endif
}
// Eigenpairs of Omega (ascending w[0..m-1]); Z is D_G_A x m col-major, zero on removed rows. Returns m, or -1 on failure.
static int eig_A_active(const double *Omega, double *w, double *Z) {
    const unsigned mask = g_cut_ak ? a_rowmask() : 0u;
    int act[D_G_A]; int p = 0;
    for (int i = 0; i < D_G_A; i++) if (!(mask & (1u << i))) act[p++] = i;
    double A[D_G_A * D_G_A], Zc[D_G_A * D_G_A];
    for (int b = 0; b < p; b++) for (int a = 0; a < p; a++) A[a + b * p] = Omega[act[a] + act[b] * D_G_A];
    __CLPK_integer n = p, lda = p, il = 1, iu = p, m, ldz = p, info, isuppz[2 * D_G_A];
    double vl = 0, vu = 0, abstol = g_cut_ak ? -1.0 : 1e-10;
    __CLPK_integer lwork = -1, liwork = -1, iwq; double wq;
    dsyevr_((char *)"V", (char *)"A", (char *)"U", &n, A, &lda, &vl, &vu, &il, &iu, &abstol, &m, w, Zc, &ldz, isuppz, &wq, &lwork, &iwq, &liwork, &info);
    lwork = (__CLPK_integer)wq; liwork = iwq;
    std::vector<double> work(lwork); std::vector<__CLPK_integer> iw(liwork);
    dsyevr_((char *)"V", (char *)"A", (char *)"U", &n, A, &lda, &vl, &vu, &il, &iu, &abstol, &m, w, Zc, &ldz, isuppz, work.data(), &lwork, iw.data(), &liwork, &info);
    if (info != 0 || m < 1) return -1;
    std::fill(Z, Z + (size_t)D_G_A * m, 0.0);
    for (int k = 0; k < m; k++) for (int a = 0; a < p; a++) Z[act[a] + k * D_G_A] = Zc[a + k * p];
    return static_cast<int>(m);
}
static inline bool keep_eig_A(double wk, double max_eig) { return keep_eig(wk, max_eig); }
// Null-direction guard (2026-10-01; medians review S2; revised after code review 5, Hans approved): under cut=ak the
// CUE quadratic is computed on the CORRELATION-scaled Omega, C = S^-1 Omega S^-1 (S = diag sqrt(Omega_tt)), with
// dt = S^-1 dbar: 0.5 dt' C^+ dt equals 0.5 dbar' Omega^+ dbar whenever Omega has full rank, but the null test
// (lambda_C < NULL_EIG_REL * max lambda_C) no longer depends on the rows' units (review 5: on the raw Omega, large deltas
// pushed the eps*score_kappa direction below the relative threshold). A null direction with a non-negligible projection
// (|z'dt| > NULL_DPROJ_REL * max(1, |dt|)) cannot be matched (Schennach's GAUSS code: singular Omega -> rejection); its
// eigenvalue is floored at NULL_EIG_REL * max lambda_C, a continuous penalty with a slope (the first version returned a
// flat 1e10). A null direction with ~zero projection (an exact identity between rows) is skipped. Regular directions keep
// AK's "> 0" rule. cut=rel keeps the old raw-Omega relative cut.
static const double NULL_EIG_REL = 1e-12, NULL_DPROJ_REL = 1e-8;
static inline bool null_violated(double zd, double dnorm) { return std::fabs(zd) > NULL_DPROJ_REL * std::max(1.0, dnorm); }
struct CueAkInfo { int n_null_floored = 0, n_null_skipped = 0; };
// 0.5 dt' C^+ dt with the guard; v (optional) = Omega^+ dbar in raw units (= S^-1 C^+ dt), for nested_L's gradient.
static double cue_core_ak(const double *d, const double *Om, double *v, CueAkInfo *info) {
    const unsigned mask = a_rowmask();
    double sc[D_G_A], C[D_G_A * D_G_A], dt[D_G_A];
    for (int t = 0; t < D_G_A; t++) { const double o = Om[t + t * D_G_A]; sc[t] = (!(mask & (1u << t)) && o > 0.0) ? std::sqrt(o) : 1.0; dt[t] = d[t] / sc[t]; }
    for (int b = 0; b < D_G_A; b++) for (int a = 0; a < D_G_A; a++) C[a + b * D_G_A] = Om[a + b * D_G_A] / (sc[a] * sc[b]);
    double w[D_G_A], Z[D_G_A * D_G_A];
    const int m = eig_A_active(C, w, Z);
    if (m < 1) return std::numeric_limits<double>::infinity();
    const double tol = NULL_EIG_REL * w[m - 1];
    double dn = 0.0; for (int t = 0; t < D_G_A; t++) if (!(mask & (1u << t))) dn += dt[t] * dt[t];
    dn = std::sqrt(dn);
    if (v) for (int t = 0; t < D_G_A; t++) v[t] = 0.0;
    double L = 0.0;
    for (int k = 0; k < m; k++) {
        double zd = 0.0; for (int t = 0; t < D_G_A; t++) zd += Z[t + k * D_G_A] * dt[t];
        double lam = w[k];
        if (lam < tol) {
            if (!null_violated(zd, dn)) { if (info) info->n_null_skipped++; continue; }
            lam = tol; if (info) info->n_null_floored++;
        }
        L += 0.5 * zd * zd / lam;
        if (v) for (int t = 0; t < D_G_A; t++) v[t] += Z[t + k * D_G_A] * zd / lam / sc[t];
    }
    return L;
}
// Dominating measure (2026-09-30, Phase 1, audit finding 1): Schennach (2014) Proposition 2.1,
//   drho(M | z; theta) proportional to exp(-|| D^{-1} (g(M; theta) - g(ubar; theta)) ||^2) dlambda(M),
// lambda = uniform on (0, M*] (the proposal), ubar = M* (e = 0). ||g||^2 grows faster than any gamma'g in the tail
// (u^4 vs u^2), so the tilted measure is proper for every gamma (Definition 2.2(ii)); by Remark 2.3 the shape does
// not affect the estimand. Implemented as the rho ratio in the MH acceptance (as in Schennach's GAUSS avg_mom).
// D: fixed per-row scales (CLI rho_D=..., one value per row; dropped rows ignored), computed once by mode=rhoD and
// passed unchanged to every run that is compared. Her construction also puts a point mass q at ubar; omitted here
// (it is not needed for condition (ii)). CLI rho=prop21 (default rho=uniform = every run before 2026-09-30).
static bool g_rho_on = false;
// gamma's initial NM step (2026-10-01): 0.2 / D_t under rho=prop21 (1 otherwise); a row left out of the rho penalty
// (D_t = inf, bounded indicator rows) gets 0.4 = 0.2 / 0.5, 0.5 being the largest sd of a +-1/2 indicator.
static double g_rhoD_fwd(int t);
static inline double gamma_step(int t) { const double D = g_rho_on ? g_rhoD_fwd(t) : 1.0; return std::isfinite(D) ? 0.2 / D : 0.4; }
// sampler=is (2026-09-30, Phase 2, audit 7.2): self-normalized importance sampling on FIXED draws instead of MH --
// per firm, n_keep iid proposals M_j from the uniform (same stream every evaluation), weights
// w_j = exp(gamma'g_j - Q_j) (Q_j = rho quadratic form under rho=prop21, else 0), gtilde_i = sum w g / sum w.
// Smooth in (theta, gamma) (Schennach 2014, p. 360: reweighting fixed draws). n_burn unused. Default sampler=mh.
static bool g_sampler_is = false;
static double g_rhoD[D_G_A];
static double g_rhoD_fwd(int t) { return g_rhoD[t]; }
static inline double rho_Q(const GVecA &g, const GVecA &g0) {
    const unsigned msk = a_rowmask(); double q = 0.0;
    for (int t = 0; t < D_G_A; t++) if (!(msk & (1u << t))) { double z = (g[t] - g0[t]) / g_rhoD[t]; q += z * z; }
    return q;
}

static inline void moment_g_A_one(
    double M, double Mstar, double V, double Wt, double tau_rho, double beta,
    double lambda, double delta0, double delta1, double delta2,
    GVecA &g_out
) {
    double e      = e_of_M(M, Mstar);
    double eps    = eps_of_M(M, Mstar, V);
    double om     = omega_of_M(M, Mstar, V, Wt, beta);
    double psi    = h_of_e(e, tau_rho, lambda) - delta0 + delta1 * om - delta2 * om * om;
    double lnM    = std::log(M);
    double hprime = h_prime_bounded(e, lambda);

    g_out[0] = psi;
    g_out[1] = eps;
    g_out[2] = psi * lnM;
    g_out[3] = psi * om;
    g_out[4] = psi * om * om;
    g_out[5] = eps * lnM;
    g_out[6] = eps * e;
    g_out[7] = eps * om;
    g_out[8] = hprime * eps;
#ifdef TAU_ROW
    for (int t = 9; t < D_G_A; t++) g_out[t] = 0.0;   // never used: TAU_ROW/YEAR_FE builds require qform=exp_scale
#endif
}

// Same 9 rows as moment_g_A_one, with h and its lambda-score from the exp_scale detection function (S3,
// 2026-09-28; header: h_of_e_exp_scale). Only h and the row-[8] score change; lambda here is lambda1.
// Dispatch on g_qform for the "new-rows" path (all forms share rows 0-9 [+ year rows]):
//   1 exp_scale    q = lambda1 (1 - exp(-e/Mbar))        lambda = lambda1
//   2 power_scale  q = (e/Mbar)^k                         lambda = k
//   3 linear_new   q = lambda e (levels, old headline q)  lambda = lambda; with the corrected rows
static inline double q_h(double e, double tau_rho, double lambda, double Mbar) {
    if (g_qform == 2) return h_of_e_power_scale(e, tau_rho, lambda, Mbar);
    if (g_qform == 3) return h_of_e(e, tau_rho, lambda);
    return h_of_e_exp_scale(e, tau_rho, lambda, Mbar);
}
static inline double q_score(double e, double lambda, double Mbar) {
    if (g_qform == 2) return h_prime_bounded_power_scale(e, lambda, Mbar);
    if (g_qform == 3) return h_prime_bounded(e, lambda);
    return h_prime_bounded_exp_scale(e, lambda, Mbar);
}
static inline double q_lnB(double e, double lambda, double Mbar) {
#ifdef KINK
    if (g_qform == 4 || g_qform == 5) return std::log(B_power_scale(e, g_kpow, lambda * Mbar));   // floored beyond the kink (4)
#endif
    if (g_qform == 2) return std::log(B_power_scale(e, lambda, Mbar));
    if (g_qform == 3) return std::log(h_denom(e, lambda));
    return std::log(B_exp_scale(e, lambda, Mbar));
}
static inline double q_draw(std::mt19937_64 &rng, double Mstar, double lambda, double Mbar) {
#ifdef KINK
    if (g_qform == 5) return draw_from_rho_power_scale(rng, Mstar, g_kpow, lambda * Mbar);   // no kink: FOC-ceiling support
#endif
    if (g_qform == 4) {   // KINK: whole physical support M in (0, Mstar] (u01 in [0,1) keeps M > 0)
        std::uniform_real_distribution<double> unif(0.0, 1.0);
        (void)lambda; (void)Mbar;
        return Mstar * (1.0 - unif(rng));
    }
    if (g_qform == 2) return draw_from_rho_power_scale(rng, Mstar, lambda, Mbar);
    if (g_qform == 3) return draw_from_rho_checked(rng, Mstar, lambda);
    return draw_from_rho_fixed_scale(rng, Mstar, Mbar);
}
// proposal=mix (2026-09-30, review point 2; sampler=is only): the importance-sampling proposal is a 50/50 mixture of
// uniform in M on the support (lo, M*] and log-uniform in M (u = ln(M*/M) uniform on [0, U), U = min(mix_umax,
// ln(M*/lo))), so half the draws cover the u-range the tilt reaches (the uniform alone puts mass e^-u there). Each draw
// carries lw = log(uniform density / mixture density), added to its log weight, so the estimand is unchanged.
// Floor-edge redraw as in draw_from_rho_power_scale. Default proposal=uniform.
static bool g_prop_mix = false;
static double g_mix_umax = 25.0;
// Third component (review 3, 2026-09-30): when lo > 0 (support bounded below by the FOC ceiling), log-uniform in
// (M - lo) on [lo + eps_e (M* - lo), M*): targets the ceiling edge, where degenerate tilts pile up (B -> floor).
// Mixture weights then 1/3 each; with lo = 0 the u-component already covers M -> 0 and the weights stay 1/2, 1/2.
static const double MIX_EDGE_EPS = 1e-10;
static inline double is_draw(std::mt19937_64 &rng, const FirmData &f, double lambda, double &lw) {
    if (!g_prop_mix) { lw = 0.0; return q_draw(rng, f.Mstar, lambda, f.Mbar); }
    double lo = 0.0;
#ifdef KINK
    if (g_qform == 5) lo = std::max(0.0, f.Mstar - power_ceiling(g_kpow) * lambda * f.Mbar);
#endif
    const double U = (lo > 0.0) ? std::min(g_mix_umax, std::log(f.Mstar / lo)) : g_mix_umax;
    const double W = f.Mstar - lo, pu = 1.0 / W;
    const bool edge = lo > 0.0;
    const double LE = -std::log(MIX_EDGE_EPS);          // edge component: ln(M - lo) uniform on [ln(eps W), ln W)
    const double wu = edge ? 1.0 / 3.0 : 0.5, wl = wu, we = edge ? 1.0 / 3.0 : 0.0;
    std::uniform_real_distribution<double> unif(0.0, 1.0);
    for (;;) {
        double c = unif(rng), v = unif(rng), M;
        if (c < wu) M = f.Mstar - v * W;
        else if (c < wu + wl) M = f.Mstar * std::exp(-v * U);
        else M = lo + W * std::exp(-v * LE);
        if (!(M > 0.0) || !(M > lo)) continue;
#ifdef KINK
        if (g_qform == 5 && 1.0 - (1.0 + g_kpow) * std::pow((f.Mstar - M) / (lambda * f.Mbar), g_kpow) <= g_h_floor_power) continue;
#endif
        const double u = std::log(f.Mstar / M);
        const double pl = (u < U) ? 1.0 / (U * M) : 0.0;
        const double d = M - lo;
        const double pe = (edge && d >= MIX_EDGE_EPS * W) ? 1.0 / (LE * d) : 0.0;
        lw = std::log(pu / (wu * pu + wl * pl + we * pe));
        return M;
    }
}
static inline void moment_g_A_one_exp_scale(
    double M, double Mstar, double V, double Wt, double tau_rho, double beta, double Mbar, double ltau_bar, int yidx,
    double lambda, double delta0, double delta1, double delta2,
    GVecA &g_out, double sig2eps = 0.0, int jidx = -1, double umed_in = -1.0, int audit_in = 0, double pshare_in = -1.0,
    double cw_in = 1.0
) {
    (void)sig2eps; (void)jidx; (void)umed_in; (void)audit_in; (void)pshare_in; (void)cw_in;
    double e      = e_of_M(M, Mstar);
    double eps    = eps_of_M(M, Mstar, V);
    double om     = omega_of_M(M, Mstar, V, Wt, beta);
#ifdef KINK
    if (g_qform == 4 || g_qform == 5) {   // power detection, lambda = kappa (scale multiplier); 4: kink at the FOC ceiling,
                                          // 5 (power_nokink): support restricted below the ceiling, so the beyond branch never fires
        double k = g_kpow, sc = lambda * Mbar, x = e / sc, lnM = std::log(M);
        for (int t = 0; t < D_G_A; t++) g_out[t] = 0.0;
        g_out[1] = eps; g_out[5] = eps * lnM; g_out[7] = eps * om;   // eps rows: every draw
#ifdef EPSVAR
        g_out[12] = eps * eps - sig2eps;   // eps-variance row, every draw
#endif
#ifdef IND5
        if (jidx >= 0 && jidx < N_IND) {   // per-industry rows, every draw (content by ind_rows)
            if (g_ind_mode == 0) g_out[13 + jidx] = eps * lnM;
            else if (g_ind_mode == 1) g_out[13 + jidx] = eps;
            else if (g_ind_mode >= 3) g_out[13 + jidx] = cw_in * eps;
            else if (umed_in >= 0.0) g_out[13 + jidx] = (std::log(Mstar / M) <= umed_in ? 1.0 : 0.0) - 0.5;
#ifdef IND5P
            if (pshare_in >= 0.0) g_out[13 + N_IND + jidx] = (std::log(Mstar / M) >= g_share_u ? 1.0 : 0.0) - pshare_in;
#endif
        }
#endif
        if (x >= power_ceiling(k)) {   // beyond the kink
            g_out[10] = 1.0 - g_kshare; (void)yidx;
            for (int t = 0; t < D_G_A; t++) if (g_dropmask & (1u << t)) g_out[t] = 0.0;
            return;
        }
        double psi = h_of_e_power_scale(e, tau_rho, k, sc) - delta0 + delta1 * om - delta2 * om * om;
        g_out[0] = psi; g_out[2] = psi * lnM; g_out[3] = psi * om; g_out[4] = psi * om * om;
        g_out[6] = (g_row6 == 1) ? eps * psi : eps * e;
        g_out[8] = h_prime_bounded_power_scale(e, k, sc) * eps;
        g_out[9] = psi * ltau_bar;
        g_out[10] = g_audit_on ? audit_in * (std::pow(x, k) - g_audit_p) : -g_kshare;
        if (g_cf_on) {   // counterfactual moment in row 10 (see g_cf_on, g_cf_target)
            const double Bx = B_power_scale(e, k, sc);
            auto epq = [&](double D, double &ep, double &qp) {
                const double br = 1.0 - Bx / (1.0 + D);
                const double xp = br > 0.0 ? std::pow(br / (1.0 + k), 1.0 / k) : 0.0;
                ep = sc * xp; qp = std::pow(xp, k); };
            auto Cr = [&](double D) { double ep, qp; epq(D, ep, qp); return (1.0 + D) * tau_rho * (cf_mresp_r(D, tau_rho, beta) * M + (1.0 - qp) * ep); };
            auto Xb = [&](double D) { double ep, qp; epq(D, ep, qp); return ep / (cf_mresp_r(D, tau_rho, beta) * M); };
            const double D = g_cf_Delta, hh = g_cf_h;
            double a = 0.0, b = 1.0;
            switch (g_cf_target) {
                case 0: a = Cr(D) / g_cf_scale; break;
                case 1: a = (Cr(D) - (1.0 + D) * Cr(0.0)) / g_cf_scale; break;
                case 2: a = (Cr(D) - Cr(0.0)) / g_cf_scale; break;
                case 3: a = (1.0 + D) * (Xb(D + hh) - Xb(D - hh)) / (2.0 * hh); b = Xb(D); break;
                case 4: a = (1.0 + D) * (Cr(D + hh) - Cr(D - hh)) / (2.0 * hh) / g_cf_scale; b = Cr(D) / g_cf_scale; break;
                case 5: a = Mstar / M - 1.0; break;
                case 6: { double ep, qp; epq(0.0, ep, qp); a = tau_rho * (1.0 - qp) * ep / g_cf_scale; b = -tau_rho * M / g_cf_scale; break; }
                case 7: a = tau_rho * M / g_cf_scale; break;
                case 8: { double ep, qp; epq(0.0, ep, qp); a = tau_rho * (1.0 - qp) * ep / g_cf_scale; b = 0.0; break; }
                case 9: a = -Cr(D) / g_cf_scale; break;
                case 10: a = -(1.0 + D) * (Cr(D + hh) - Cr(D - hh)) / (2.0 * hh) / g_cf_scale; b = -Cr(D) / g_cf_scale; break;
                case 11: a = -0.01 * (1.0 + D) * (Cr(D + hh) - Cr(D - hh)) / (2.0 * hh) / g_cf_scale; break;
                case 12: a = (1.0 + D) * tau_rho * (cf_mresp_r(D, tau_rho, beta) - 1.0) * M / g_cf_scale; break;
                case 14: a = -(Cr(D) - Cr(0.0)) / g_cf_scale; break;
                case 15: { double ep, qp; epq(0.0, ep, qp); a = qp; break; }
                case 13: { double ep, qp, e0, q0; epq(D, ep, qp); epq(0.0, e0, q0);
                           a = (1.0 + D) * tau_rho * ((1.0 - qp) * ep - (1.0 - q0) * e0) / g_cf_scale; break; }
            }
            g_out[10] = a - g_cf_T * b;
        }
#ifdef KINK_S
        {   // score for the scale kappa: dh/dkappa = k(1+k) x^k / (kappa B) >= 0; softsign-bounded, paired with eps
            double hk = k * (1.0 + k) * std::pow(x, k) / (lambda * B_power_scale(e, k, sc));
            g_out[11] = eps * hk / (1.0 + std::fabs(hk));
        }
#endif
        for (int t = 0; t < D_G_A; t++) if (g_dropmask & (1u << t)) g_out[t] = 0.0;   // drop_rows
        return;
    }
#endif
#ifdef YEAR_FE
    double d0     = delta0 + g_d0yr[yidx];
#else
    double d0     = delta0; (void)yidx;
#endif
    double psi    = q_h(e, tau_rho, lambda, Mbar) - d0 + delta1 * om - delta2 * om * om;
    double lnM    = std::log(M);
    double hprime = q_score(e, lambda, Mbar);

    g_out[0] = psi;
    g_out[1] = eps;
    g_out[2] = psi * lnM;
    g_out[3] = psi * om;
    g_out[4] = psi * om * om;
    g_out[5] = eps * lnM;
    g_out[6] = (g_row6 == 1) ? eps * psi : eps * e;
    g_out[7] = eps * om;
    g_out[8] = hprime * eps;
#ifdef TAU_ROW
    g_out[9] = psi * ltau_bar;
#ifdef YEAR_FE
    for (int k = 1; k <= N_XFE; k++) g_out[9 + k] = (yidx == k) ? psi : 0.0;
#endif
#else
    (void)ltau_bar;
#endif
}

// Corner (tau_P=0) firms: same convention as A's Rcpp worker
// (TiltedMomentWorkerA) -- zero every row by default, then compute the THREE
// eps-only rows ([1] eps, [5] eps*lnM, [7] eps*om) for real; rows [6] (eps*e)
// and [8] (h_prime_bounded*eps) are correctly left at 0 since e=0 exactly at
// a corner (h_prime_bounded(0,.)=0 too), not just "uninformative by
// convention" -- both already mathematically zero there.
static inline void firm_chain_A(
    const FirmData &f, double lambda, double delta0, double delta1, double delta2,
    const double gamma[D_G_A], int n_burn, int n_keep, uint64_t base_seed,
    double *ghat_row
) {
    if (f.corner == 1) {
        for (int t = 0; t < D_G_A; t++) ghat_row[t] = 0.0;
        double eps_pt = eps_of_M(f.Mstar, f.Mstar, f.V);
        double om_pt  = omega_of_M(f.Mstar, f.Mstar, f.V, f.Wt, f.beta);
        double lnM_pt = std::log(f.Mstar);
        ghat_row[1] = eps_pt;
        ghat_row[5] = eps_pt * lnM_pt;
        ghat_row[7] = eps_pt * om_pt;
#ifdef EPSVAR
        ghat_row[12] = eps_pt * eps_pt - f.sig2eps;   // eps-variance row for corner firms too (Hans, 2026-09-29)
#endif
#ifdef IND5
        if (f.jidx >= 0 && f.jidx < N_IND) {   // corner firm: M = M*, u = 0
            if (g_ind_mode == 0) ghat_row[13 + f.jidx] = eps_pt * lnM_pt;
            else if (g_ind_mode == 1) ghat_row[13 + f.jidx] = eps_pt;
            else if (g_ind_mode >= 3) ghat_row[13 + f.jidx] = f.cw * eps_pt;
            else if (f.umed >= 0.0) ghat_row[13 + f.jidx] = 0.5;
#ifdef IND5P
            if (f.pshare >= 0.0) ghat_row[13 + N_IND + f.jidx] = (0.0 >= g_share_u ? 1.0 : 0.0) - f.pshare;   // u = 0
#endif
        }
#endif
#ifdef KINK_S
        for (int t = 0; t < D_G_A; t++) if (g_dropmask & (1u << t)) ghat_row[t] = 0.0;   // drop_rows, corner firms too
#endif
        return;
    }

    std::mt19937_64 rng(firm_seed(base_seed, f.row_id));
    std::uniform_real_distribution<double> unif(0.0, 1.0);   // MH accept/reject only -- the M-proposal draw has its own internal distribution inside draw_from_rho_checked

    GVecA g_current, g_try, g_run;
    g_run.fill(0.0);

    if (g_qform >= 1) {   // new-rows path: exp_scale, power_scale, linear_new
        // S3 (2026-09-28): exp_scale detection, fixed support M in (max(0,Mstar-Mbar), Mstar]. Same MH scheme,
        // same RNG stream layout (one proposal draw, then one accept draw, per step) as the linear branch below.
        GVecA g_bar; double q_cur = 0.0;   // rho=prop21: g at ubar = M*, and the current draw's quadratic form
        if (g_rho_on) moment_g_A_one_exp_scale(f.Mstar, f.Mstar, f.V, f.Wt, f.tau_rho, f.beta, f.Mbar, f.ltau_bar, f.yidx, lambda, delta0, delta1, delta2, g_bar, f.sig2eps, f.jidx, f.umed, f.audit_g, f.pshare, f.cw);
        if (g_sampler_is) {   // self-normalized IS on n_keep fixed draws
            std::vector<GVecA> G(n_keep); std::vector<double> lw(n_keep); double lmax = -HUGE_VAL;
            for (int j = 0; j < n_keep; j++) {
                double lwp = 0.0; double Mj = is_draw(rng, f, lambda, lwp);
                moment_g_A_one_exp_scale(Mj, f.Mstar, f.V, f.Wt, f.tau_rho, f.beta, f.Mbar, f.ltau_bar, f.yidx, lambda, delta0, delta1, delta2, G[j], f.sig2eps, f.jidx, f.umed, f.audit_g, f.pshare, f.cw);
                double a = lwp; for (int t = 0; t < D_G_A; t++) a += gamma[t] * G[j][t];
                if (g_rho_on) a -= rho_Q(G[j], g_bar);
                lw[j] = a; if (a > lmax) lmax = a;
            }
            double sw = 0.0; for (int t = 0; t < D_G_A; t++) g_run[t] = 0.0;
            for (int j = 0; j < n_keep; j++) { double w = std::exp(lw[j] - lmax); sw += w; for (int t = 0; t < D_G_A; t++) g_run[t] += w * G[j][t]; }
            for (int t = 0; t < D_G_A; t++) ghat_row[t] = g_run[t] / sw;
            return;
        }
        double M_current = q_draw(rng, f.Mstar, lambda, f.Mbar);
        moment_g_A_one_exp_scale(M_current, f.Mstar, f.V, f.Wt, f.tau_rho, f.beta, f.Mbar, f.ltau_bar, f.yidx, lambda, delta0, delta1, delta2, g_current, f.sig2eps, f.jidx, f.umed, f.audit_g, f.pshare, f.cw);
        if (g_rho_on) q_cur = rho_Q(g_current, g_bar);
        for (int r = -n_burn + 1; r <= n_keep; r++) {
            double M_try = q_draw(rng, f.Mstar, lambda, f.Mbar);
            moment_g_A_one_exp_scale(M_try, f.Mstar, f.V, f.Wt, f.tau_rho, f.beta, f.Mbar, f.ltau_bar, f.yidx, lambda, delta0, delta1, delta2, g_try, f.sig2eps, f.jidx, f.umed, f.audit_g, f.pshare, f.cw);
            double log_ratio = 0.0, q_try = 0.0;
            for (int t = 0; t < D_G_A; t++) log_ratio += gamma[t] * (g_try[t] - g_current[t]);
            if (g_rho_on) { q_try = rho_Q(g_try, g_bar); log_ratio -= (q_try - q_cur); }
            if (std::log(unif(rng)) < log_ratio) { g_current = g_try; q_cur = q_try; }
            if (r > 0) for (int t = 0; t < D_G_A; t++) g_run[t] += g_current[t] / n_keep;
        }
        for (int t = 0; t < D_G_A; t++) ghat_row[t] = g_run[t];
        return;
    }

    double M_current = draw_from_rho_checked(rng, f.Mstar, lambda);
    moment_g_A_one(M_current, f.Mstar, f.V, f.Wt, f.tau_rho, f.beta, lambda, delta0, delta1, delta2, g_current);

    for (int r = -n_burn + 1; r <= n_keep; r++) {
        double M_try = draw_from_rho_checked(rng, f.Mstar, lambda);
        moment_g_A_one(M_try, f.Mstar, f.V, f.Wt, f.tau_rho, f.beta, lambda, delta0, delta1, delta2, g_try);

        double log_ratio = 0.0;
        for (int t = 0; t < D_G_A; t++) log_ratio += gamma[t] * (g_try[t] - g_current[t]);

        if (std::log(unif(rng)) < log_ratio) g_current = g_try;
        if (r > 0) for (int t = 0; t < D_G_A; t++) g_run[t] += g_current[t] / n_keep;
    }
    for (int t = 0; t < D_G_A; t++) ghat_row[t] = g_run[t];
}

// ============================================================================
// ---- Counterfactual revenue, fixed-(theta,gamma) forward simulation ------
// ============================================================================
// (2026-09-10) Phase 1 "step 1": NO optimization -- run the SAME MCMC
// M-sampler as firm_chain_A at an ALREADY-FIXED operating point, and for
// each kept draw compute R_i(Delta) via CLAUDE.md's Counterfactual closed
// form. This is a diagnostic ballpark to size the (Delta,R) grid's own
// R-axis -- NOT the reported estimate, which must come from jointly
// re-profiling (theta,gamma) with the auxiliary R-moment at every (Delta,R)
// grid cell (Schennach 2022 JEL p.1250's warning against reading population
// aggregates off a post-hoc tilted average).
//
// Closed-form simplification (derived 2026-09-10): psi is DEFINED as
// h_of_e(e,tau_rho,lambda) - delta0 + delta1*om - delta2*om^2, so
// C(om,psi) = exp(delta0-delta1*om+delta2*om^2+psi) collapses algebraically
// to exp(h_of_e(e,tau_rho,lambda)) = tau_rho*(1-2*lambda*e) -- delta0,
// delta1,delta2 cancel out of the counterfactual e'-resolve step entirely
// once the firm's baseline e_i is known (they still shape WHICH e_i the
// chain accepts in the first place, upstream of this). Substituting into
// e'(tau_tilde;M)=max(0,(tau_tilde-C)/(2*lambda*tau_tilde)) with
// tau_tilde=(1+Delta)*tau_rho and simplifying, tau_rho ALSO cancels:
//     e'(Delta) = max(0, (Delta + 2*lambda*e_i) / (2*lambda*(1+Delta)))
// a function of ONLY the firm's baseline e_i, lambda, and Delta. Verified:
// e'(0)=e_i exactly (the Delta=0 case must recover the baseline draw); and
// d e'/d Delta = 2*lambda*(1-2*lambda*e_i) / (2*lambda*(1+Delta))^2 > 0
// since 1-2*lambda*e_i>0 is required by the FOC domain -- matches
// CLAUDE.md's d e*/d tau_P > 0 result (evasion rises with the tax rate).
static inline double resolve_e_prime_revenue(double e_baseline, double lambda, double Delta) {
    double num = Delta + 2.0 * lambda * e_baseline;
    double den = 2.0 * lambda * (1.0 + Delta);
    return std::max(0.0, num / den);
}

struct RevenueResult { double R_nominal, R_real; double e_mean; double om_mean; double Loss_nominal, Loss_real; };

// Loss_nominal = tau_tilde*(1-q(e'))*e' -- the UNCAUGHT-evasion leakage only
// (excludes the M*tau_tilde "legitimate credit" term that dominates R's own
// firm-size heterogeneity). 2026-09-12, added alongside the R-moment's
// control-variate fix to let the two variance-reduction strategies (CV on R
// vs. testing Loss directly) be compared on the SAME forward-sim draws.
static inline RevenueResult firm_revenue_baseline_A(
    const FirmData &f, double lambda, double delta0, double delta1, double delta2,
    const double gamma[D_G_A], int n_burn, int n_keep, uint64_t base_seed,
    double Delta
) {
    // Corner (tau_P=0, so tau_rho=0): tau_tilde=(1+Delta)*0=0 for ANY Delta
    // -- the credit side vanishes regardless of the counterfactual shock,
    // matching CLAUDE.md's R_i=t1-tau_tilde*Mstar collapsing to R_i=t1.
    // Corner firms are non-evaders by construction -- e_mean=0, not NaN/skip,
    // so population E[e]/Med[e] over ALL firms (below) stays well-defined.
    // Loss=0 too: tau_tilde=0 kills the leakage term regardless of e'.
    if (f.corner == 1) {
        double tau_tilde = (1.0 + Delta) * f.tau_rho;
        double R = f.t1 - tau_tilde * f.Mstar;
        double om_pt = omega_of_M(f.Mstar, f.Mstar, f.V, f.Wt, f.beta);
        return {R, R / f.pgdp, 0.0, om_pt, 0.0, 0.0};
    }

    std::mt19937_64 rng(firm_seed(base_seed, f.row_id));
    std::uniform_real_distribution<double> unif(0.0, 1.0);

    GVecA g_current, g_try;
    double M_current = draw_from_rho_checked(rng, f.Mstar, lambda);
    moment_g_A_one(M_current, f.Mstar, f.V, f.Wt, f.tau_rho, f.beta, lambda, delta0, delta1, delta2, g_current);

    double R_sum = 0.0, e_sum = 0.0, om_sum = 0.0, Loss_sum = 0.0;
    for (int r = -n_burn + 1; r <= n_keep; r++) {
        double M_try = draw_from_rho_checked(rng, f.Mstar, lambda);
        moment_g_A_one(M_try, f.Mstar, f.V, f.Wt, f.tau_rho, f.beta, lambda, delta0, delta1, delta2, g_try);

        double log_ratio = 0.0;
        for (int t = 0; t < D_G_A; t++) log_ratio += gamma[t] * (g_try[t] - g_current[t]);
        if (std::log(unif(rng)) < log_ratio) { M_current = M_try; g_current = g_try; }

        if (r > 0) {
            double e_i = e_of_M(M_current, f.Mstar);
            double om_i = omega_of_M(M_current, f.Mstar, f.V, f.Wt, f.beta);
            double e_prime = resolve_e_prime_revenue(e_i, lambda, Delta);
            double q_eprime = std::min(lambda * e_prime, 1.0);
            double tau_tilde = (1.0 + Delta) * f.tau_rho;
            double R = f.t1 - tau_tilde * (M_current + (1.0 - q_eprime) * e_prime);
            double Loss = tau_tilde * (1.0 - q_eprime) * e_prime;
            R_sum += R / n_keep;
            e_sum += e_i / n_keep;   // baseline e_i (Delta=0's own draw), not e_prime -- meaningful at any Delta
            om_sum += om_i / n_keep;
            Loss_sum += Loss / n_keep;
        }
    }
    return {R_sum, R_sum / f.pgdp, e_sum, om_sum, Loss_sum, Loss_sum / f.pgdp};
}

static void run_revenue_baseline_mode(
    const std::vector<FirmData> &firms, double lambda, double delta0, double delta1, double delta2,
    const double gamma[D_G_A], int n_burn, int n_keep, uint64_t base_seed, int n_threads,
    double Delta, const std::string &output_csv
) {
    int n = static_cast<int>(firms.size());
    std::vector<double> Rnom(n), Rreal(n), Emean(n), Ommean(n), LossNom(n), LossReal(n);

    std::atomic<int> next_idx{0};
    auto worker = [&]() {
        int i;
        while ((i = next_idx.fetch_add(1, std::memory_order_relaxed)) < n) {
            RevenueResult r = firm_revenue_baseline_A(firms[i], lambda, delta0, delta1, delta2, gamma,
                                                       n_burn, n_keep, base_seed, Delta);
            Rnom[i] = r.R_nominal; Rreal[i] = r.R_real; Emean[i] = r.e_mean; Ommean[i] = r.om_mean;
            LossNom[i] = r.Loss_nominal; LossReal[i] = r.Loss_real;
        }
    };
    if (n_threads <= 1) worker();
    else {
        std::vector<std::thread> pool;
        for (int t = 0; t < n_threads; t++) pool.emplace_back(worker);
        for (auto &th : pool) th.join();
    }

    double sum_nom = 0.0, sum_real = 0.0, sum_loss_nom = 0.0, sum_loss_real = 0.0;
    for (int i = 0; i < n; i++) { sum_nom += Rnom[i]; sum_real += Rreal[i]; sum_loss_nom += LossNom[i]; sum_loss_real += LossReal[i]; }
    double mean_nom = sum_nom / n, mean_real = sum_real / n;
    double mean_loss_nom = sum_loss_nom / n, mean_loss_real = sum_loss_real / n;

    // Ballpark E[e]/Med[e] (2026-09-11): population mean/median of each
    // firm's OWN mean-e over its post-burn-in chain -- NOT Schennach's
    // formal joint auxiliary-moment device (that needs its own moment,
    // E[e]-mu_e=0, jointly re-profiled with theta/gamma, per CLAUDE.md's
    // ELVIS conventions). This is a quick forward-simulation readout off
    // the already-fitted (theta,gamma) for a progress-report ballpark only.
    std::vector<double> Esorted = Emean;
    std::sort(Esorted.begin(), Esorted.end());
    double e_sum = 0.0; for (double e : Emean) e_sum += e;
    double e_mean_pop = e_sum / n;
    double e_med_pop = (n % 2 == 0) ? 0.5 * (Esorted[n/2 - 1] + Esorted[n/2]) : Esorted[n/2];

    std::cout << std::setprecision(10)
              << "revenue_baseline: Delta=" << Delta << " n=" << n
              << " mean_R_nominal=" << mean_nom << " total_R_nominal=" << sum_nom
              << " mean_R_real=" << mean_real << " total_R_real=" << sum_real
              << " mean_Loss_nominal=" << mean_loss_nom << " mean_Loss_real=" << mean_loss_real
              << " E[e]_ballpark=" << e_mean_pop << " Med[e]_ballpark=" << e_med_pop << "\n";

    if (!output_csv.empty()) {
        std::ofstream out(output_csv);
        out << std::setprecision(12) << "row_id,corner,R_nominal,R_real,e_mean,om_mean,Loss_nominal,Loss_real\n";
        for (int i = 0; i < n; i++)
            out << firms[i].row_id << "," << firms[i].corner << "," << Rnom[i] << "," << Rreal[i] << "," << Emean[i]
                << "," << Ommean[i] << "," << LossNom[i] << "," << LossReal[i] << "\n";
        out.close();
        std::cout << "Saved per-firm: " << output_csv << "\n";
    }
}

// ============================================================================
// ---- Counterfactual (Delta,R) joint grid -- FULL profiling (2026-09-10) --
// ============================================================================
// Step 3 of Phase 1. Per the user's explicit call (confirmed with Nail
// Kashaev) -- NOTHING is fixed at the Phase-0 operating point here: at every
// (Delta,R) grid cell, (lambda,delta0,delta1,delta2,eta,gamma[1..10]) are
// ALL jointly profiled via NLopt, matching CLAUDE.md's original Inference
// plan design (this supersedes the "fix theta" simplification floated
// earlier in chat -- that was explicitly rejected). D_G_R=10: moment set
// A's 9 rows plus one new auxiliary moment for R, R_i(Delta)/pgdp_i -
// R_candidate = 0, appended as row [9] -- Schennach's own device for a
// population aggregate as an auxiliary ELVIS parameter (never read off a
// post-hoc tilted average). Uses the SAME closed-form e'(Delta) simplifi-
// cation as revenue_baseline above (a function of the CURRENT MCMC draw's
// own e_i and the CANDIDATE lambda being tested, not a fixed lambda).
static const int D_G_R = 10;
typedef std::array<double, D_G_R> GVecR;

// row9_mode (2026-09-12, added for the R-vs-Loss / control-variate
// comparison -- see CLAUDE.md/Research-log.md "Diagnosing why the (Delta,R)
// confidence sets are so wide"): 0=raw R (original), 1=control-variate-
// adjusted R (subtract cv_beta*(t1/pgdp), re-add cv_beta*cv_mu_c so the
// target is untouched -- cv_beta,cv_mu_c are FIXED precomputed constants,
// never NLopt decision variables), 2=revenue LOSS due to uncaught evasion
// only (tau_tilde*(1-q(e'))*e'/pgdp), which excludes the M*tau_tilde
// "legitimate credit" term that carries the same firm-size heterogeneity as
// raw R. cv_beta/cv_mu_c are unused (pass 0) when row9_mode != 1.
static inline double compute_row9(
    double M, double t1, double pgdp, double tau_tilde, double e_prime, double q_eprime,
    double target, int row9_mode, double cv_beta, double cv_mu_c
) {
    if (row9_mode == 2) {
        double Loss_nominal = tau_tilde * (1.0 - q_eprime) * e_prime;
        return Loss_nominal / pgdp - target;
    }
    double R_nominal = t1 - tau_tilde * (M + (1.0 - q_eprime) * e_prime);
    double R_real = R_nominal / pgdp;
    if (row9_mode == 1) return (R_real - cv_beta * (t1 / pgdp)) - (target - cv_beta * cv_mu_c);
    // row9_mode==3 (2026-09-18): the THEORY-coefficient control variate --
    // R_real - t1/pgdp, i.e. beta FIXED AT EXACTLY 1 (from the accounting
    // identity R=t1-tau_tilde*[...], not regression-estimated like row9_mode
    // ==1's beta_hat=0.998838). Algebraically identical to
    // -tau_tilde*(M+(1-q)*e')/pgdp - target, the user's proposed "Loss"
    // (the FULL purchases credit, unlike row9_mode==2 which deliberately
    // drops M). Unlike row9_mode==1, no cv_beta/cv_mu_c needed -- the
    // coefficient is exact, not fitted.
    if (row9_mode == 3) return (R_real - t1 / pgdp) - target;
    return R_real - target;
}

static inline void moment_g_A_revenue_one(
    double M, double Mstar, double V, double Wt, double tau_rho, double t1, double pgdp, double beta,
    double lambda, double delta0, double delta1, double delta2,
    double Delta_cf, double R_candidate, int row9_mode, double cv_beta, double cv_mu_c,
    GVecR &g_out
) {
    double e      = e_of_M(M, Mstar);
    double eps    = eps_of_M(M, Mstar, V);
    double om     = omega_of_M(M, Mstar, V, Wt, beta);
    double psi    = h_of_e(e, tau_rho, lambda) - delta0 + delta1 * om - delta2 * om * om;
    double lnM    = std::log(M);
    double hprime = h_prime_bounded(e, lambda);

    g_out[0] = psi;
    g_out[1] = eps;
    g_out[2] = psi * lnM;
    g_out[3] = psi * om;
    g_out[4] = psi * om * om;
    g_out[5] = eps * lnM;
    g_out[6] = eps * e;
    g_out[7] = eps * om;
    g_out[8] = hprime * eps;

    double e_prime   = resolve_e_prime_revenue(e, lambda, Delta_cf);
    double q_eprime  = std::min(lambda * e_prime, 1.0);
    double tau_tilde = (1.0 + Delta_cf) * tau_rho;
    g_out[9] = compute_row9(M, t1, pgdp, tau_tilde, e_prime, q_eprime, R_candidate, row9_mode, cv_beta, cv_mu_c);
}

// Corner (tau_P=0, tau_rho=0): rows [0],[2],[3],[4] (psi-carrying) stay 0 as
// in firm_chain_A -- BUT row [9] (the R-moment) IS genuinely informative
// even at a corner: R_i(Delta)=t1 for ANY Delta there (nothing to detect),
// so it is computed for real, not zeroed by convention. tau_tilde=0 here
// (tau_rho=0), so e_prime/q_eprime are irrelevant to compute_row9's result
// (Loss=0 always; CV/raw R both collapse to t1-based formulas).
static inline void firm_chain_R(
    const FirmData &f, double lambda, double delta0, double delta1, double delta2,
    const double gamma[D_G_R], double Delta_cf, double R_candidate,
    int n_burn, int n_keep, uint64_t base_seed, double *ghat_row,
    int row9_mode, double cv_beta, double cv_mu_c
) {
    if (f.corner == 1) {
        for (int t = 0; t < D_G_R; t++) ghat_row[t] = 0.0;
        double eps_pt = eps_of_M(f.Mstar, f.Mstar, f.V);
        double om_pt  = omega_of_M(f.Mstar, f.Mstar, f.V, f.Wt, f.beta);
        double lnM_pt = std::log(f.Mstar);
        ghat_row[1] = eps_pt;
        ghat_row[5] = eps_pt * lnM_pt;
        ghat_row[7] = eps_pt * om_pt;
        ghat_row[9] = compute_row9(f.Mstar, f.t1, f.pgdp, 0.0, 0.0, 0.0, R_candidate, row9_mode, cv_beta, cv_mu_c);
        return;
    }

    std::mt19937_64 rng(firm_seed(base_seed, f.row_id));
    std::uniform_real_distribution<double> unif(0.0, 1.0);

    GVecR g_current, g_try, g_run;
    double M_current = draw_from_rho_checked(rng, f.Mstar, lambda);
    moment_g_A_revenue_one(M_current, f.Mstar, f.V, f.Wt, f.tau_rho, f.t1, f.pgdp, f.beta,
                            lambda, delta0, delta1, delta2, Delta_cf, R_candidate,
                            row9_mode, cv_beta, cv_mu_c, g_current);
    g_run.fill(0.0);

    for (int r = -n_burn + 1; r <= n_keep; r++) {
        double M_try = draw_from_rho_checked(rng, f.Mstar, lambda);
        moment_g_A_revenue_one(M_try, f.Mstar, f.V, f.Wt, f.tau_rho, f.t1, f.pgdp, f.beta,
                                lambda, delta0, delta1, delta2, Delta_cf, R_candidate,
                                row9_mode, cv_beta, cv_mu_c, g_try);

        double log_ratio = 0.0;
        for (int t = 0; t < D_G_R; t++) log_ratio += gamma[t] * (g_try[t] - g_current[t]);
        if (std::log(unif(rng)) < log_ratio) { M_current = M_try; g_current = g_try; }
        if (r > 0) for (int t = 0; t < D_G_R; t++) g_run[t] += g_current[t] / n_keep;
    }
    for (int t = 0; t < D_G_R; t++) ghat_row[t] = g_run[t];
}

static void compute_dvec_omega_R(
    const std::vector<FirmData> &firms,
    double lambda, double delta0, double delta1, double delta2, const double gamma[D_G_R],
    double Delta_cf, double R_candidate,
    int n_burn, int n_keep, uint64_t base_seed, int n_threads,
    double dvec[D_G_R], double Omega[D_G_R * D_G_R],
    int row9_mode = 0, double cv_beta = 0.0, double cv_mu_c = 0.0
) {
    int n = static_cast<int>(firms.size());
    std::vector<double> Ghat((size_t)n * D_G_R);

    std::atomic<int> next_idx{0};
    auto firm_worker = [&]() {
        double row[D_G_R];
        int i;
        while ((i = next_idx.fetch_add(1, std::memory_order_relaxed)) < n) {
            firm_chain_R(firms[i], lambda, delta0, delta1, delta2, gamma, Delta_cf, R_candidate,
                         n_burn, n_keep, base_seed, row, row9_mode, cv_beta, cv_mu_c);
            for (int j = 0; j < D_G_R; j++) Ghat[(size_t)j * n + i] = row[j];
        }
    };
    if (n_threads <= 1) firm_worker();
    else {
        std::vector<std::thread> pool;
        for (int t = 0; t < n_threads; t++) pool.emplace_back(firm_worker);
        for (auto &th : pool) th.join();
    }

    for (int j = 0; j < D_G_R; j++) {
        double s = 0.0;
        const double *col = Ghat.data() + (size_t)j * n;
        for (int i = 0; i < n; i++) s += col[i];
        dvec[j] = s / n;
    }
    std::vector<double> Xc((size_t)n * D_G_R);
    std::copy(Ghat.begin(), Ghat.end(), Xc.begin());
    for (int j = 0; j < D_G_R; j++) {
        double *col = Xc.data() + (size_t)j * n;
        double mu = dvec[j];
        for (int i = 0; i < n; i++) col[i] -= mu;
    }
    cblas_dsyrk(CblasColMajor, CblasUpper, CblasTrans,
                D_G_R, n, omega_div(n), Xc.data(), n, 0.0, Omega, D_G_R);
    for (int i = 0; i < D_G_R; i++)
        for (int j = i + 1; j < D_G_R; j++)
            Omega[j + i * D_G_R] = Omega[i + j * D_G_R];
}

static double cue_objective_R_std(const double dvec[D_G_R], const double Omega_in[D_G_R * D_G_R]) {
    double A[D_G_R * D_G_R];
    std::copy(Omega_in, Omega_in + D_G_R * D_G_R, A);

    double w[D_G_R];
    __CLPK_integer n = D_G_R, lda = D_G_R, il = 1, iu = D_G_R, m, ldz = D_G_R, info;
    double vl = 0, vu = 0, abstol = 1e-10;
    __CLPK_integer lwork = -1, liwork = -1, iwork_query;
    double work_query;
    double Z[D_G_R * D_G_R];
    __CLPK_integer isuppz[2 * D_G_R];

    dsyevr_((char *)"V", (char *)"A", (char *)"U", &n, A, &lda, &vl, &vu, &il, &iu,
            &abstol, &m, w, Z, &ldz, isuppz, &work_query, &lwork, &iwork_query, &liwork, &info);
    lwork = (__CLPK_integer)work_query;
    liwork = iwork_query;
    std::vector<double> work(lwork);
    std::vector<__CLPK_integer> iworkv(liwork);
    dsyevr_((char *)"V", (char *)"A", (char *)"U", &n, A, &lda, &vl, &vu, &il, &iu,
            &abstol, &m, w, Z, &ldz, isuppz, work.data(), &lwork, iworkv.data(), &liwork, &info);

    if (info != 0 || m < 1) return std::numeric_limits<double>::infinity();

    double max_eig = w[m - 1];
    double obj = 0.0;
    for (int k = 0; k < m; k++) {
        if (keep_eig(w[k], max_eig)) {
            double d2 = 0.0;
            for (int i = 0; i < D_G_R; i++) d2 += Z[i + k * D_G_R] * dvec[i];
            obj += 0.5 * d2 * d2 / w[k];
        }
    }
    return obj;
}

// Inner problem: profile (delta0,eta,lambda,delta1,delta2,gamma[1..10]) --
// 15 free dims, NOTHING fixed except (Delta,R) themselves (the grid axes).
struct InnerParamsRevGrid {
    const std::vector<FirmData> *firms;
    double Delta_cf, R_candidate;
    int n_burn, n_keep, n_threads;
    uint64_t base_seed;
    int row9_mode; double cv_beta, cv_mu_c;
};

static double inner_obj_revgrid(unsigned n, const double *x, double *grad, void *data) {
    (void)n; (void)grad;
    InnerParamsRevGrid *p = static_cast<InnerParamsRevGrid *>(data);
    double delta0 = x[0], lambda = x[1], delta1 = x[2], delta2 = x[3];
    double gamma[D_G_R];
    for (int t = 0; t < D_G_R; t++) gamma[t] = x[4 + t];

    double dvec[D_G_R], Omega[D_G_R * D_G_R];
    compute_dvec_omega_R(*(p->firms), lambda, delta0, delta1, delta2, gamma,
                          p->Delta_cf, p->R_candidate, p->n_burn, p->n_keep, p->base_seed, p->n_threads,
                          dvec, Omega, p->row9_mode, p->cv_beta, p->cv_mu_c);
    return cue_objective_R_std(dvec, Omega);
}

struct FitResultRevGrid {
    double delta0, lambda, delta1, delta2, gamma[D_G_R], Lhat;
    int convergence, iters;
    double Lhat_pass1, wander;
    int iters_pass1, convergence_pass1;
    double point_seconds;
};

// lambda bounds (2026-09-10): derived directly from the M_star distribution
// under the two extremes discussed in chat -- LAMBDA_LO=1/(2*max(M_star))
// ("nothing is evasion": below this the FOC ceiling doesn't restrict even
// the single largest firm at all) and LAMBDA_HI=1/(2*min(M_star)) ("all is
// evasion": above this even the smallest firm is restricted) -- computed
// from the untrimmed lag_m interior sample (max=30,702,781, min=31).
constexpr double REVGRID_LAMBDA_LO = 1.6284919e-08;
constexpr double REVGRID_LAMBDA_HI = 0.0161290323;

static FitResultRevGrid fit_one_revgrid_point(
    const std::vector<FirmData> &firms, double Delta_cf, double R_candidate,
    int n_burn, int n_keep, uint64_t base_seed, int n_threads, double maxtime,
    const double *x0_in, nlopt_algorithm algo,
    int row9_mode = 0, double cv_beta = 0.0, double cv_mu_c = 0.0
) {
    const int n_par = 4 + D_G_R;   // delta0, lambda, delta1, delta2, gamma[1..10] -- eta DROPPED 2026-09-10
    InnerParamsRevGrid params{&firms, Delta_cf, R_candidate, n_burn, n_keep, n_threads, base_seed,
                               row9_mode, cv_beta, cv_mu_c};

    double lower[14], upper[14], x[14];
    lower[0] = -DELTA_BOUND; upper[0] = DELTA_BOUND;   // delta0
    lower[1] = REVGRID_LAMBDA_LO; upper[1] = REVGRID_LAMBDA_HI;   // lambda
    lower[2] = -DELTA_BOUND; upper[2] = DELTA_BOUND;    // delta1
    lower[3] = -DELTA_BOUND; upper[3] = DELTA_BOUND;    // delta2
    for (int t = 0; t < D_G_R; t++) { lower[4 + t] = -HUGE_VAL; upper[4 + t] = HUGE_VAL; }
    for (int t = 0; t < n_par; t++) x[t] = x0_in[t];
    x[1] = std::min(std::max(x[1], lower[1]), upper[1]);

    auto run_opt = [&](double *xstart) -> FitResultRevGrid {
        nlopt_opt opt = nlopt_create(algo, n_par);
        nlopt_set_lower_bounds(opt, lower);
        nlopt_set_upper_bounds(opt, upper);
        nlopt_set_min_objective(opt, inner_obj_revgrid, &params);
        nlopt_set_xtol_rel(opt, 1e-4);
        nlopt_set_maxeval(opt, 2000);
        nlopt_set_maxtime(opt, maxtime);
        double minf = HUGE_VAL;
        nlopt_result res = nlopt_optimize(opt, xstart, &minf);
        int iters = nlopt_get_numevals(opt);
        nlopt_destroy(opt);
        FitResultRevGrid r;
        r.delta0 = xstart[0]; r.lambda = xstart[1];
        r.delta1 = xstart[2]; r.delta2 = xstart[3];
        for (int t = 0; t < D_G_R; t++) r.gamma[t] = xstart[4 + t];
        r.Lhat = minf; r.convergence = static_cast<int>(res); r.iters = iters;
        return r;
    };

    auto t_start = std::chrono::steady_clock::now();
    FitResultRevGrid r1 = run_opt(x);
    double x2[14];
    x2[0] = r1.delta0; x2[1] = r1.lambda; x2[2] = r1.delta1; x2[3] = r1.delta2;
    for (int t = 0; t < D_G_R; t++) x2[4 + t] = r1.gamma[t];
    FitResultRevGrid r2 = run_opt(x2);
    r2.point_seconds = std::chrono::duration<double>(std::chrono::steady_clock::now() - t_start).count();

    r2.Lhat_pass1 = r1.Lhat; r2.iters_pass1 = r1.iters; r2.convergence_pass1 = r1.convergence;
    double sq = (r2.delta0 - r1.delta0) * (r2.delta0 - r1.delta0)
              + (r2.lambda - r1.lambda) * (r2.lambda - r1.lambda) + (r2.delta1 - r1.delta1) * (r2.delta1 - r1.delta1)
              + (r2.delta2 - r1.delta2) * (r2.delta2 - r1.delta2);
    for (int t = 0; t < D_G_R; t++) sq += (r2.gamma[t] - r1.gamma[t]) * (r2.gamma[t] - r1.gamma[t]);
    r2.wander = std::sqrt(sq);
    return r2;
}

// One shard = one (or more, round-robin) R value(s); each shard walks its
// own R column through ALL Delta values in the user's specified order:
// Delta=0 first (seeded from the shared x0), then positive Deltas ascending
// (each seeded from the PREVIOUS positive point, first one from center),
// then negative Deltas descending in magnitude (each from the previous
// negative point, first one from center too) -- two independent chains
// radiating out from the center, not one long chain through everything.
struct RevGridResult { double Delta_cf, R_candidate; FitResultRevGrid fit; };

static void run_revenue_grid_mode(
    const std::vector<FirmData> &firms, const std::vector<double> &Delta_values, const std::vector<double> &R_values,
    const double *x0,
    int n_burn, int n_keep, uint64_t base_seed, int n_threads, double maxtime,
    int shard_id, int n_shards, const std::string &output_csv, nlopt_algorithm algo
) {
    std::vector<size_t> my_R_indices;
    for (size_t ri = 0; ri < R_values.size(); ri++) if ((int)(ri % n_shards) == shard_id) my_R_indices.push_back(ri);

    std::vector<double> pos, neg;
    bool has_zero = false;
    for (double d : Delta_values) {
        if (d == 0.0) has_zero = true;
        else if (d > 0.0) pos.push_back(d);
        else neg.push_back(d);
    }
    std::sort(pos.begin(), pos.end());
    std::sort(neg.begin(), neg.end(), std::greater<double>());

    std::cout << "revgrid: " << Delta_values.size() << " Deltas x " << R_values.size() << " Rs, "
              << my_R_indices.size() << " R-columns assigned to this shard, " << n_threads << " threads/point\n";

    std::vector<RevGridResult> results;
    auto t0 = std::chrono::steady_clock::now();
    size_t done = 0, total = my_R_indices.size() * Delta_values.size();

    // Seeding, corrected 2026-09-10 (twice): chaining EVERY parameter across
    // Delta (the original version) let (lambda,delta0,delta1,delta2,
    // gamma[1:9]) drift into different basins as Delta moved away from 0
    // (delta1 swung 3.19-5.41 across adjacent R at Delta=0.5) -- a real
    // local-optimum artifact, not a finding. Fix: reset (delta0,lambda,
    // delta1,delta2) to the FIXED x0 operating point at EVERY cell (same-
    // point independent seeding, per Tollgate 1); chain ALL of gamma[1..10]
    // (not just gamma[10]) along Delta in the same two-direction order as
    // before -- gamma is a Lagrange multiplier, expected to vary smoothly
    // with Delta for every moment row, not just the new revenue one; the
    // structural parameters are what caused the earlier drift, not gamma.
    for (size_t ri : my_R_indices) {
        double R_candidate = R_values[ri];
        double seed_buf[14];
        double init_gamma[D_G_R];
        for (int t = 0; t < D_G_R; t++) init_gamma[t] = x0[4 + t];

        auto make_seed = [&](const double *g) -> double* {
            for (int t = 0; t < 4; t++) seed_buf[t] = x0[t];
            for (int t = 0; t < D_G_R; t++) seed_buf[4 + t] = g[t];
            return seed_buf;
        };

        auto run_and_record = [&](double Delta_cf, const double *g_seed) {
            FitResultRevGrid fit = fit_one_revgrid_point(firms, Delta_cf, R_candidate, n_burn, n_keep,
                                                          base_seed, n_threads, maxtime, make_seed(g_seed), algo);
            results.push_back({Delta_cf, R_candidate, fit});
            done++;
            auto elapsed = std::chrono::duration<double>(std::chrono::steady_clock::now() - t0).count();
            std::cout << "  [" << done << "/" << total << "] elapsed=" << elapsed << "s"
                      << "  Delta=" << Delta_cf << " R=" << R_candidate
                      << " Lhat=" << fit.Lhat << " (pass1=" << fit.Lhat_pass1 << ")"
                      << " lambda=" << fit.lambda << " d1=" << fit.delta1 << " d2=" << fit.delta2
                      << " conv=" << fit.convergence << " iters=" << fit.iters
                      << " point_seconds=" << fit.point_seconds << "\n" << std::flush;
            return fit;
        };

        double center_gamma[D_G_R];
        for (int t = 0; t < D_G_R; t++) center_gamma[t] = init_gamma[t];
        bool have_center = false;
        if (has_zero) {
            FitResultRevGrid f0 = run_and_record(0.0, init_gamma);
            for (int t = 0; t < D_G_R; t++) center_gamma[t] = f0.gamma[t];
            have_center = true;
        }

        double g_walk[D_G_R];
        for (int t = 0; t < D_G_R; t++) g_walk[t] = have_center ? center_gamma[t] : init_gamma[t];
        for (double d : pos) {
            FitResultRevGrid f = run_and_record(d, g_walk);
            for (int t = 0; t < D_G_R; t++) g_walk[t] = f.gamma[t];
        }
        for (int t = 0; t < D_G_R; t++) g_walk[t] = have_center ? center_gamma[t] : init_gamma[t];
        for (double d : neg) {
            FitResultRevGrid f = run_and_record(d, g_walk);
            for (int t = 0; t < D_G_R; t++) g_walk[t] = f.gamma[t];
        }
    }

    std::ofstream out(output_csv);
    if (!out.is_open()) { std::cerr << "ERROR: could not open output_csv: " << output_csv << "\n"; std::exit(1); }
    out << std::setprecision(15);
    out << "Delta,R,delta0_hat,lambda_hat,delta1_hat,delta2_hat,";
    for (int t = 1; t <= D_G_R; t++) out << "gamma" << t << ",";
    out << "Lhat,Lhat_pass1,wander,convergence,convergence_pass1,iters,iters_pass1,point_seconds,n\n";
    for (auto &rr : results) {
        out << rr.Delta_cf << "," << rr.R_candidate << ","
            << rr.fit.delta0 << "," << rr.fit.lambda << ","
            << rr.fit.delta1 << "," << rr.fit.delta2 << ",";
        for (int t = 0; t < D_G_R; t++) out << rr.fit.gamma[t] << ",";
        out << rr.fit.Lhat << "," << rr.fit.Lhat_pass1 << "," << rr.fit.wander << ","
            << rr.fit.convergence << "," << rr.fit.convergence_pass1 << ","
            << rr.fit.iters << "," << rr.fit.iters_pass1 << "," << rr.fit.point_seconds << "," << firms.size() << "\n";
    }
    out.close();
    std::cout << "Saved: " << output_csv << " (" << results.size() << " points)\n";
}

// Independent-seeding variant of the (Delta,R) grid (2026-09-10), added
// alongside run_revenue_grid_mode above rather than replacing it -- that
// function's own within-shard gamma chaining stays intact for anyone who
// wants it again. This one applies the SAME "same-point beats chaining"
// lesson already validated for lambdagrid: every (Delta,R) cell is fully
// independent, seeded from the identical x0 (whose gamma block is meant to
// be an already-converged anchor fit -- solve ONE cell externally first,
// typically Delta=0 at the central/baseline R candidate, with a small
// arbitrary starting value for gamma[10] (the new revenue moment's own
// multiplier, which has no prior estimate the way gamma[1..9] do from the
// lag_m fit), then pass its converged (delta0,lambda,delta1,delta2,
// gamma[1..10]) in here as x0 for the full grid). No chaining at all --
// (delta0,lambda,delta1,delta2) still reset to x0 every cell per the
// existing convention, and now gamma does too. Flattens all (Delta,R) pairs
// into one list and reuses the exact two-level work-stealing pattern from
// run_lambdagrid_mode (atomic counter over cells feeding n_groups=
// min(n_cells,n_threads) concurrent thread-groups, each with its own
// existing firm-level work-stealing) -- valid here specifically because
// removing the chaining also removes the only reason cells needed to be
// processed in a particular order.
static void run_revenue_grid_indep_mode(
    const std::vector<FirmData> &firms, const std::vector<double> &Delta_values, const std::vector<double> &R_values,
    const double *x0,
    int n_burn, int n_keep, uint64_t base_seed, int n_threads_total, double maxtime,
    const std::string &output_csv, nlopt_algorithm algo = NLOPT_LN_NELDERMEAD,
    int row9_mode = 0, double cv_beta = 0.0, double cv_mu_c = 0.0
) {
    struct Cell { double Delta_cf, R_candidate; };
    std::vector<Cell> cells;
    for (double d : Delta_values) for (double r : R_values) cells.push_back({d, r});
    int n_points = static_cast<int>(cells.size());
    int n_groups = std::max(1, std::min(n_points, n_threads_total));
    int base_tpg = n_threads_total / n_groups;
    int remainder = n_threads_total - base_tpg * n_groups;

    std::cout << "revgrid_indep: " << Delta_values.size() << " Deltas x " << R_values.size()
              << " Rs = " << n_points << " independent cells, " << n_groups
              << " concurrent point-groups (point-level work-stealing), "
              << base_tpg << "-" << (base_tpg + (remainder > 0 ? 1 : 0))
              << " threads/group (firm-level work-stealing within each), "
              << n_threads_total << " threads total, ALL cells seeded identically from x0 (no chaining)\n";

    std::vector<RevGridResult> results(n_points);
    std::atomic<int> next_point_idx{0};
    std::atomic<int> done_count{0};
    std::mutex print_mutex;
    auto t0 = std::chrono::steady_clock::now();

    auto group_worker = [&](int tpg) {
        int idx;
        while ((idx = next_point_idx.fetch_add(1, std::memory_order_relaxed)) < n_points) {
            const Cell &c = cells[idx];
            FitResultRevGrid fit = fit_one_revgrid_point(firms, c.Delta_cf, c.R_candidate, n_burn, n_keep,
                                                          base_seed, tpg, maxtime, x0, algo,
                                                          row9_mode, cv_beta, cv_mu_c);
            results[idx] = {c.Delta_cf, c.R_candidate, fit};   // each idx written by exactly one group
            int done = ++done_count;
            auto elapsed = std::chrono::duration<double>(std::chrono::steady_clock::now() - t0).count();
            std::lock_guard<std::mutex> lock(print_mutex);
            std::cout << "  [" << done << "/" << n_points << "] elapsed=" << elapsed << "s"
                      << "  Delta=" << c.Delta_cf << " R=" << c.R_candidate
                      << " Lhat=" << fit.Lhat << " (pass1=" << fit.Lhat_pass1 << ")"
                      << " lambda=" << fit.lambda << " d1=" << fit.delta1 << " d2=" << fit.delta2
                      << " conv=" << fit.convergence << " iters=" << fit.iters
                      << " point_seconds=" << fit.point_seconds << " (tpg=" << tpg << ")\n" << std::flush;
        }
    };

    std::vector<std::thread> pool;
    for (int g = 0; g < n_groups; g++) {
        int tpg = base_tpg + (g < remainder ? 1 : 0);
        pool.emplace_back(group_worker, tpg);
    }
    for (auto &th : pool) th.join();

    std::ofstream out(output_csv);
    if (!out.is_open()) { std::cerr << "ERROR: could not open output_csv: " << output_csv << "\n"; std::exit(1); }
    out << std::setprecision(15);
    out << "Delta,R,delta0_hat,lambda_hat,delta1_hat,delta2_hat,";
    for (int t = 1; t <= D_G_R; t++) out << "gamma" << t << ",";
    out << "Lhat,Lhat_pass1,wander,convergence,convergence_pass1,iters,iters_pass1,point_seconds,n\n";
    for (auto &rr : results) {
        out << rr.Delta_cf << "," << rr.R_candidate << ","
            << rr.fit.delta0 << "," << rr.fit.lambda << ","
            << rr.fit.delta1 << "," << rr.fit.delta2 << ",";
        for (int t = 0; t < D_G_R; t++) out << rr.fit.gamma[t] << ",";
        out << rr.fit.Lhat << "," << rr.fit.Lhat_pass1 << "," << rr.fit.wander << ","
            << rr.fit.convergence << "," << rr.fit.convergence_pass1 << ","
            << rr.fit.iters << "," << rr.fit.iters_pass1 << "," << rr.fit.point_seconds << "," << firms.size() << "\n";
    }
    out.close();
    std::cout << "Saved: " << output_csv << " (" << results.size() << " points)\n";
}

// Fixed-theta variant of revgrid (2026-09-12): theta_smooth=(lambda,delta0,
// delta1,delta2) held COMPLETELY FIXED at a caller-supplied point -- not
// merely seeded there and left free to NLopt like revgrid/revgrid_indep
// above. Only gamma (all D_G_R=10) is ever optimized. Motivated by two
// findings the same week: (1) AK2020's own counterfactual code fixes their
// structural/support parameter (theta0) as a hardcoded constant and profiles
// only gamma -- the only real precedent for a policy counterfactual, and it
// does the opposite of the earlier "profile everything" design; (2) leaving
// lambda free in the first revgrid_indep run let it swing 56x across cells
// (silently rationalizing almost any candidate R), landing every cell inside
// the confidence set -- a vacuous result, not a finding. Same two-level
// work-stealing pattern as revgrid_indep; every cell seeded identically
// (same-point independent seeding for gamma, no chaining), since there is
// no longer any theta search to chain across.
struct InnerParamsRevGridFixedTheta {
    const std::vector<FirmData> *firms;
    double delta0, lambda, delta1, delta2;
    double Delta_cf, R_candidate;
    int n_burn, n_keep, n_threads;
    uint64_t base_seed;
    int row9_mode; double cv_beta, cv_mu_c;
};

static double inner_obj_revgrid_fixedtheta(unsigned n, const double *x, double *grad, void *data) {
    (void)n; (void)grad;
    InnerParamsRevGridFixedTheta *p = static_cast<InnerParamsRevGridFixedTheta *>(data);
    double gamma[D_G_R];
    for (int t = 0; t < D_G_R; t++) gamma[t] = x[t];

    double dvec[D_G_R], Omega[D_G_R * D_G_R];
    compute_dvec_omega_R(*(p->firms), p->lambda, p->delta0, p->delta1, p->delta2, gamma,
                          p->Delta_cf, p->R_candidate, p->n_burn, p->n_keep, p->base_seed, p->n_threads,
                          dvec, Omega, p->row9_mode, p->cv_beta, p->cv_mu_c);
    return cue_objective_R_std(dvec, Omega);
}

struct FitResultRevGridFixedTheta {
    double gamma[D_G_R], Lhat;
    int convergence, iters;
    double Lhat_pass1, wander;
    int iters_pass1, convergence_pass1;
    double point_seconds;
};

static FitResultRevGridFixedTheta fit_one_revgrid_point_fixedtheta(
    const std::vector<FirmData> &firms, double delta0, double lambda, double delta1, double delta2,
    double Delta_cf, double R_candidate,
    int n_burn, int n_keep, uint64_t base_seed, int n_threads, double maxtime,
    const double *gamma0_in, nlopt_algorithm algo,
    int row9_mode = 0, double cv_beta = 0.0, double cv_mu_c = 0.0
) {
    const int n_par = D_G_R;   // gamma only -- theta fixed, not a decision variable at all
    InnerParamsRevGridFixedTheta params{&firms, delta0, lambda, delta1, delta2,
                                         Delta_cf, R_candidate, n_burn, n_keep, n_threads, base_seed,
                                         row9_mode, cv_beta, cv_mu_c};

    double lower[D_G_R], upper[D_G_R], x[D_G_R];
    for (int t = 0; t < D_G_R; t++) { lower[t] = -HUGE_VAL; upper[t] = HUGE_VAL; x[t] = gamma0_in[t]; }

    auto run_opt = [&](double *xstart) -> FitResultRevGridFixedTheta {
        nlopt_opt opt = nlopt_create(algo, n_par);
        nlopt_set_lower_bounds(opt, lower);
        nlopt_set_upper_bounds(opt, upper);
        nlopt_set_min_objective(opt, inner_obj_revgrid_fixedtheta, &params);
        nlopt_set_xtol_rel(opt, 1e-4);
        nlopt_set_maxeval(opt, 2000);
        nlopt_set_maxtime(opt, maxtime);
        double minf = HUGE_VAL;
        nlopt_result res = nlopt_optimize(opt, xstart, &minf);
        int iters = nlopt_get_numevals(opt);
        nlopt_destroy(opt);
        FitResultRevGridFixedTheta r;
        for (int t = 0; t < D_G_R; t++) r.gamma[t] = xstart[t];
        r.Lhat = minf; r.convergence = static_cast<int>(res); r.iters = iters;
        return r;
    };

    auto t_start = std::chrono::steady_clock::now();
    FitResultRevGridFixedTheta r1 = run_opt(x);
    double x2[D_G_R];
    for (int t = 0; t < D_G_R; t++) x2[t] = r1.gamma[t];
    FitResultRevGridFixedTheta r2 = run_opt(x2);
    r2.point_seconds = std::chrono::duration<double>(std::chrono::steady_clock::now() - t_start).count();

    r2.Lhat_pass1 = r1.Lhat; r2.iters_pass1 = r1.iters; r2.convergence_pass1 = r1.convergence;
    double sq = 0.0;
    for (int t = 0; t < D_G_R; t++) sq += (r2.gamma[t] - r1.gamma[t]) * (r2.gamma[t] - r1.gamma[t]);
    r2.wander = std::sqrt(sq);
    return r2;
}

struct RevGridFixedThetaResult { double Delta_cf, R_candidate; FitResultRevGridFixedTheta fit; };

static void run_revenue_grid_fixedtheta_mode(
    const std::vector<FirmData> &firms, const std::vector<double> &Delta_values, const std::vector<double> &R_values,
    double delta0, double lambda, double delta1, double delta2, const double *gamma0,
    int n_burn, int n_keep, uint64_t base_seed, int n_threads_total, double maxtime,
    const std::string &output_csv, nlopt_algorithm algo = NLOPT_LN_NELDERMEAD,
    int row9_mode = 0, double cv_beta = 0.0, double cv_mu_c = 0.0
) {
    struct Cell { double Delta_cf, R_candidate; };
    std::vector<Cell> cells;
    for (double d : Delta_values) for (double r : R_values) cells.push_back({d, r});
    int n_points = static_cast<int>(cells.size());
    int n_groups = std::max(1, std::min(n_points, n_threads_total));
    int base_tpg = n_threads_total / n_groups;
    int remainder = n_threads_total - base_tpg * n_groups;

    std::cout << "revgrid_fixedtheta: theta=(delta0=" << delta0 << ",lambda=" << lambda
              << ",delta1=" << delta1 << ",delta2=" << delta2 << ") FIXED, "
              << Delta_values.size() << " Deltas x " << R_values.size() << " Rs = " << n_points
              << " independent cells, " << n_groups << " concurrent point-groups, "
              << base_tpg << "-" << (base_tpg + (remainder > 0 ? 1 : 0))
              << " threads/group, " << n_threads_total << " threads total, gamma-only optimization\n";

    std::vector<RevGridFixedThetaResult> results(n_points);
    std::atomic<int> next_point_idx{0};
    std::atomic<int> done_count{0};
    std::mutex print_mutex;
    auto t0 = std::chrono::steady_clock::now();

    auto group_worker = [&](int tpg) {
        int idx;
        while ((idx = next_point_idx.fetch_add(1, std::memory_order_relaxed)) < n_points) {
            const Cell &c = cells[idx];
            FitResultRevGridFixedTheta fit = fit_one_revgrid_point_fixedtheta(
                firms, delta0, lambda, delta1, delta2, c.Delta_cf, c.R_candidate,
                n_burn, n_keep, base_seed, tpg, maxtime, gamma0, algo, row9_mode, cv_beta, cv_mu_c);
            results[idx] = {c.Delta_cf, c.R_candidate, fit};
            int done = ++done_count;
            auto elapsed = std::chrono::duration<double>(std::chrono::steady_clock::now() - t0).count();
            std::lock_guard<std::mutex> lock(print_mutex);
            std::cout << "  [" << done << "/" << n_points << "] elapsed=" << elapsed << "s"
                      << "  Delta=" << c.Delta_cf << " R=" << c.R_candidate
                      << " Lhat=" << fit.Lhat << " (pass1=" << fit.Lhat_pass1 << ")"
                      << " conv=" << fit.convergence << " iters=" << fit.iters
                      << " point_seconds=" << fit.point_seconds << " (tpg=" << tpg << ")\n" << std::flush;
        }
    };

    std::vector<std::thread> pool;
    for (int g = 0; g < n_groups; g++) {
        int tpg = base_tpg + (g < remainder ? 1 : 0);
        pool.emplace_back(group_worker, tpg);
    }
    for (auto &th : pool) th.join();

    std::ofstream out(output_csv);
    if (!out.is_open()) { std::cerr << "ERROR: could not open output_csv: " << output_csv << "\n"; std::exit(1); }
    out << std::setprecision(15);
    out << "Delta,R,delta0,lambda,delta1,delta2,";
    for (int t = 1; t <= D_G_R; t++) out << "gamma" << t << ",";
    out << "Lhat,Lhat_pass1,wander,convergence,convergence_pass1,iters,iters_pass1,point_seconds,n\n";
    for (auto &rr : results) {
        out << rr.Delta_cf << "," << rr.R_candidate << ","
            << delta0 << "," << lambda << "," << delta1 << "," << delta2 << ",";
        for (int t = 0; t < D_G_R; t++) out << rr.fit.gamma[t] << ",";
        out << rr.fit.Lhat << "," << rr.fit.Lhat_pass1 << "," << rr.fit.wander << ","
            << rr.fit.convergence << "," << rr.fit.convergence_pass1 << ","
            << rr.fit.iters << "," << rr.fit.iters_pass1 << "," << rr.fit.point_seconds << "," << firms.size() << "\n";
    }
    out.close();
    std::cout << "Saved: " << output_csv << " (" << results.size() << " points)\n";
}

// Read-only diagnostic (2026-09-12): dump the FULL 10-component dvec (and
// each row's own sampling SE, sqrt(diag(Omega)/n)) at a GIVEN, already-fixed
// (theta,gamma,Delta,R) -- no optimization at all, just one evaluation of
// the same compute_dvec_omega_R() the revgrid_fixedtheta optimizer calls
// internally. Purpose: when gamma barely moves across R candidates (as it
// doesn't at Delta=0, see CLAUDE.md/chat), this is how to see WHICH moment
// row is actually absorbing the difference between a passing and a rejected
// R -- Lhat alone doesn't show that.
static void run_dvecdiag_mode(
    const std::vector<FirmData> &firms, const std::vector<double> &Delta_values, const std::vector<double> &R_values,
    double delta0, double lambda, double delta1, double delta2, const double *gamma,
    int n_burn, int n_keep, uint64_t base_seed, int n_threads,
    const std::string &output_csv,
    int row9_mode = 0, double cv_beta = 0.0, double cv_mu_c = 0.0
) {
    std::ofstream out(output_csv);
    if (!out.is_open()) { std::cerr << "ERROR: could not open output_csv: " << output_csv << "\n"; std::exit(1); }
    out << std::setprecision(15);
    out << "Delta,R";
    for (int t = 1; t <= D_G_R; t++) out << ",dvec" << t;
    for (int t = 1; t <= D_G_R; t++) out << ",se" << t;
    out << ",Lhat,n\n";
    for (double d : Delta_values) {
        for (double r : R_values) {
            double dvec[D_G_R], Omega[D_G_R * D_G_R];
            compute_dvec_omega_R(firms, lambda, delta0, delta1, delta2, gamma, d, r,
                                  n_burn, n_keep, base_seed, n_threads, dvec, Omega,
                                  row9_mode, cv_beta, cv_mu_c);
            double Lhat = cue_objective_R_std(dvec, Omega);
            out << d << "," << r;
            for (int t = 0; t < D_G_R; t++) out << "," << dvec[t];
            for (int t = 0; t < D_G_R; t++) out << "," << std::sqrt(Omega[t + t * D_G_R] / (double)firms.size());
            out << "," << Lhat << "," << firms.size() << "\n";
            std::cout << "  Delta=" << d << " R=" << r << " Lhat=" << Lhat
                      << " dvec[R-moment]=" << dvec[9] << " se[R-moment]=" << std::sqrt(Omega[9 + 9 * D_G_R] / (double)firms.size())
                      << "\n" << std::flush;
        }
    }
    out.close();
    std::cout << "Saved: " << output_csv << "\n";
}

// Read-only diagnostic (2026-09-18, for the Nail Kashaev raw-R-vs-CV-R
// numerical-stability question): dump the FULL eigen-spectrum of Omega,
// which eigenvalue-direction each row/column loads onto, and how many
// directions the CUE objective's relative truncation (w[k] > 1e-8*max_eig,
// see cue_objective_R_std above) actually keeps vs. discards, at a GIVEN
// fixed (theta,gamma,Delta,R) -- no optimization, reuses compute_dvec_omega_R
// then duplicates cue_objective_R_std's own eigendecomposition instead of
// throwing it away. dsyevr returns eigenvalues ASCENDING.
static void run_omegadiag_mode(
    const std::vector<FirmData> &firms, const std::vector<double> &Delta_values, const std::vector<double> &R_values,
    double delta0, double lambda, double delta1, double delta2, const double *gamma,
    int n_burn, int n_keep, uint64_t base_seed, int n_threads,
    const std::string &output_csv,
    int row9_mode = 0, double cv_beta = 0.0, double cv_mu_c = 0.0
) {
    std::ofstream out(output_csv);
    if (!out.is_open()) { std::cerr << "ERROR: could not open output_csv: " << output_csv << "\n"; std::exit(1); }
    out << std::setprecision(15);
    out << "Delta,R,eig_rank,eigenvalue,kept,d2,d2_sq_over_eig,Lhat,condition_number_kept,n_kept,n";
    for (int i = 0; i < D_G_R; i++) out << ",load_row" << (i + 1);
    out << "\n";
    for (double d : Delta_values) {
        for (double r : R_values) {
            double dvec[D_G_R], Omega[D_G_R * D_G_R];
            compute_dvec_omega_R(firms, lambda, delta0, delta1, delta2, gamma, d, r,
                                  n_burn, n_keep, base_seed, n_threads, dvec, Omega,
                                  row9_mode, cv_beta, cv_mu_c);

            double A[D_G_R * D_G_R];
            std::copy(Omega, Omega + D_G_R * D_G_R, A);
            double w[D_G_R];
            __CLPK_integer n_lp = D_G_R, lda = D_G_R, il = 1, iu = D_G_R, m, ldz = D_G_R, info;
            double vl = 0, vu = 0, abstol = 1e-10;
            __CLPK_integer lwork = -1, liwork = -1, iwork_query;
            double work_query;
            double Z[D_G_R * D_G_R];
            __CLPK_integer isuppz[2 * D_G_R];
            dsyevr_((char *)"V", (char *)"A", (char *)"U", &n_lp, A, &lda, &vl, &vu, &il, &iu,
                    &abstol, &m, w, Z, &ldz, isuppz, &work_query, &lwork, &iwork_query, &liwork, &info);
            lwork = (__CLPK_integer)work_query;
            liwork = iwork_query;
            std::vector<double> work(lwork);
            std::vector<__CLPK_integer> iworkv(liwork);
            dsyevr_((char *)"V", (char *)"A", (char *)"U", &n_lp, A, &lda, &vl, &vu, &il, &iu,
                    &abstol, &m, w, Z, &ldz, isuppz, work.data(), &lwork, iworkv.data(), &liwork, &info);

            double max_eig = w[m - 1];
            double obj = 0.0;
            int n_kept = 0;
            double min_kept_eig = std::numeric_limits<double>::infinity();
            for (int k = 0; k < m; k++) {
                if (keep_eig(w[k], max_eig)) {
                    double d2 = 0.0;
                    for (int i = 0; i < D_G_R; i++) d2 += Z[i + k * D_G_R] * dvec[i];
                    obj += 0.5 * d2 * d2 / w[k];
                    n_kept++;
                    if (w[k] < min_kept_eig) min_kept_eig = w[k];
                }
            }
            double cond_kept = n_kept > 0 ? max_eig / min_kept_eig : std::numeric_limits<double>::infinity();
            std::cout << "Delta=" << d << " R=" << r << " Lhat=" << obj
                      << " n_kept=" << n_kept << "/" << m << " cond_kept=" << cond_kept
                      << " max_eig=" << max_eig << " min_eig=" << w[0] << "\n" << std::flush;
            for (int k = 0; k < m; k++) {
                bool kept = keep_eig(w[k], max_eig);
                double d2 = 0.0;
                for (int i = 0; i < D_G_R; i++) d2 += Z[i + k * D_G_R] * dvec[i];
                double contrib = kept ? 0.5 * d2 * d2 / w[k] : 0.0;
                out << d << "," << r << "," << k << "," << w[k] << "," << (kept ? 1 : 0) << ","
                    << d2 << "," << contrib << ","
                    << obj << "," << cond_kept << "," << n_kept << "," << firms.size();
                for (int i = 0; i < D_G_R; i++) out << "," << Z[i + k * D_G_R];
                out << "\n";
            }
        }
    }
    out.close();
    std::cout << "Saved: " << output_csv << "\n";
}

static void compute_dvec_omega_A(
    const std::vector<FirmData> &firms,
    double lambda, double delta0, double delta1, double delta2, const double gamma[D_G_A],
    int n_burn, int n_keep, uint64_t base_seed, int n_threads,
    double dvec[D_G_A], double Omega[D_G_A * D_G_A]
) {
    int n = static_cast<int>(firms.size());
    std::vector<double> Ghat((size_t)n * D_G_A);

    // Atomic-counter work-stealing (2026-09-09): a static even split
    // (chunk = ceil(n/n_threads)) assigns every thread the same NUMBER of
    // firms, but per-firm MCMC cost is not what stalls -- what varies is
    // which physical core (P vs E) each thread lands on, on Apple Silicon's
    // asymmetric cores. A static split leaves whichever thread drew an
    // E-core as the straggler the other threads all wait on. Replacing the
    // fixed chunk with a shared atomic index lets every thread keep pulling
    // the next unclaimed firm until none remain, so faster cores naturally
    // finish more firms and the join() wait is bounded by total work, not by
    // the slowest core's static share.
    std::atomic<int> next_idx{0};
    auto firm_worker = [&]() {
        double row[D_G_A];
        int i;
        while ((i = next_idx.fetch_add(1, std::memory_order_relaxed)) < n) {
            firm_chain_A(firms[i], lambda, delta0, delta1, delta2, gamma, n_burn, n_keep, base_seed, row);
            for (int j = 0; j < D_G_A; j++) Ghat[(size_t)j * n + i] = row[j];
        }
    };

    if (n_threads <= 1) {
        firm_worker();
    } else {
        std::vector<std::thread> pool;
        for (int t = 0; t < n_threads; t++) pool.emplace_back(firm_worker);
        for (auto &th : pool) th.join();
    }

    for (int j = 0; j < D_G_A; j++) {
        double s = 0.0;
        const double *col = Ghat.data() + (size_t)j * n;
        for (int i = 0; i < n; i++) s += col[i];
        dvec[j] = s / n;
    }

    std::vector<double> Xc((size_t)n * D_G_A);
    std::copy(Ghat.begin(), Ghat.end(), Xc.begin());
    for (int j = 0; j < D_G_A; j++) {
        double *col = Xc.data() + (size_t)j * n;
        double mu = dvec[j];
        for (int i = 0; i < n; i++) col[i] -= mu;
    }

    if (g_cluster_on) {   // sum centred rows within plant, then Omega = n^-1 S'S (S: g_ncl x D_G_A)
        std::vector<double> Sm((size_t)g_ncl * D_G_A, 0.0);
        for (int j = 0; j < D_G_A; j++) {
            const double *col = Xc.data() + (size_t)j * n; double *sc = Sm.data() + (size_t)j * g_ncl;
            for (int i = 0; i < n; i++) sc[firms[i].cl] += col[i];
        }
        cblas_dsyrk(CblasColMajor, CblasUpper, CblasTrans,
                    D_G_A, g_ncl, 1.0 / n, Sm.data(), g_ncl, 0.0, Omega, D_G_A);
    } else {
        cblas_dsyrk(CblasColMajor, CblasUpper, CblasTrans,
                    D_G_A, n, omega_div(n), Xc.data(), n, 0.0, Omega, D_G_A);
    }
    for (int i = 0; i < D_G_A; i++)
        for (int j = i + 1; j < D_G_A; j++)
            Omega[j + i * D_G_A] = Omega[i + j * D_G_A];
}

static double cue_objective_A_std(const double dvec[D_G_A], const double Omega_in[D_G_A * D_G_A]) {
    if (g_cut_ak) return cue_core_ak(dvec, Omega_in, nullptr, nullptr);
    double w[D_G_A], Z[D_G_A * D_G_A];
    int m = eig_A_active(Omega_in, w, Z);
    if (m < 1) return std::numeric_limits<double>::infinity();
    double max_eig = w[m - 1];
    double obj = 0.0;
    for (int k = 0; k < m; k++) {
        if (keep_eig_A(w[k], max_eig)) {
            double d2 = 0.0;
            for (int i = 0; i < D_G_A; i++) d2 += Z[i + k * D_G_A] * dvec[i];
            obj += 0.5 * d2 * d2 / w[k];
        }
    }
    return obj;
}

// ---- adiag (2026-09-28): read-only diagnostic for moment set A at a FIXED (lambda,delta0,delta1,delta2,gamma) ----
// No optimization. Prints (1) each row's mean, SE = sqrt(Omega_jj/n), t; (2) Omega's eigen-spectrum with the
// objective's own truncation rule (w_k > 1e-8 * max), each direction's contribution 0.5*(z_k'd)^2/w_k to Lhat, and
// its loading on the last row; (3) for interior firms, per-firm tilted means of u = ln(M*/M), x = e/Mbar and
// omega(M) (same MH chain and RNG layout as firm_chain_A, exp_scale branch), their cross-firm summaries, and their
// correlation with the tax rate. Reproduces Lhat exactly when run at a fitted point (self-check).
// mode=rhoD (2026-09-30): per-row sd of g under the uniform proposal at a given par (interior firms, n_keep draws
// each, pooled), printed as the rho_D string to pass to every compared run. Dropped rows get 1.
static void run_rhoD_mode(const std::vector<FirmData> &firms, double lambda, double delta0, double delta1, double delta2,
                          int n_keep, uint64_t base_seed) {
    double s1[D_G_A] = {0}, s2[D_G_A] = {0}; double cnt = 0;
    for (const FirmData &f : firms) {
        if (f.corner == 1) continue;
        std::mt19937_64 rng(firm_seed(base_seed, f.row_id));
        GVecA g;
        for (int r = 0; r < n_keep; r++) {
            double M = q_draw(rng, f.Mstar, lambda, f.Mbar);
            moment_g_A_one_exp_scale(M, f.Mstar, f.V, f.Wt, f.tau_rho, f.beta, f.Mbar, f.ltau_bar, f.yidx, lambda, delta0, delta1, delta2, g, f.sig2eps, f.jidx, f.umed, f.audit_g, f.pshare, f.cw);
            for (int t = 0; t < D_G_A; t++) { s1[t] += g[t]; s2[t] += g[t] * g[t]; }
            cnt += 1;
        }
    }
    const unsigned msk = a_rowmask();
    std::cout << "RHO_D: ";
    for (int t = 0; t < D_G_A; t++) {
        double sd = std::sqrt(std::max(0.0, s2[t] / cnt - (s1[t] / cnt) * (s1[t] / cnt)));
        if (msk & (1u << t)) sd = 0.0;               // dropped here: 0 = "not computed"; a run with this row live refuses it
        else if (!(sd > 0)) sd = 1.0;
#ifdef IND5
        // bounded +-1/2 indicator rows (ind_rows=median) stay out of the rho penalty (2026-10-01, medians review S1): for
        // them (g - g(M*))^2 / D^2 is linear in g, i.e. only a shift of gamma by 1/D^2, which put the start gamma = 0 on a
        // saturated plateau. inf = left out (rho_Q adds 0); exact reparametrization, estimand unchanged.
        if (g_ind_mode == 2 && t >= 13 && t < 13 + N_IND && !(msk & (1u << t))) { std::cout << "inf" << (t + 1 < D_G_A ? "," : "\n"); continue; }
#ifdef IND5P
        if (t >= 13 + N_IND && t < 13 + 2 * N_IND && !(msk & (1u << t))) { std::cout << "inf" << (t + 1 < D_G_A ? "," : "\n"); continue; }   // share rows: bounded
#endif
#endif
        std::cout << std::setprecision(10) << sd << (t + 1 < D_G_A ? "," : "\n");
    }
}

static void run_adiag_mode(
    const std::vector<FirmData> &firms, double lambda, double delta0, double delta1, double delta2,
    const double gamma[D_G_A], int n_burn, int n_keep, uint64_t base_seed, int n_threads
) {
    int n = static_cast<int>(firms.size());
    double dvec[D_G_A], Omega[D_G_A * D_G_A];
    compute_dvec_omega_A(firms, lambda, delta0, delta1, delta2, gamma, n_burn, n_keep, base_seed, n_threads, dvec, Omega);
    double obj = cue_objective_A_std(dvec, Omega);
    std::cout << std::setprecision(6) << "Lhat (recomputed) = " << obj << "   TS = 2 n Lhat = " << 2.0 * n * obj << "   n = " << n << "\n";
    std::cout << "row  mean          se            t\n";
    for (int j = 0; j < D_G_A; j++) {
        double se = std::sqrt(Omega[j + j * D_G_A] / n);
        std::cout << "  " << j << "  " << dvec[j] << "  " << se << "  " << (se > 0 ? dvec[j] / se : 0.0) << "\n";
    }
    std::cout << "gamma:";
    for (int j = 0; j < D_G_A; j++) std::cout << " " << gamma[j];
    std::cout << "\nOmega: row sd (per firm)";
    for (int j = 0; j < D_G_A; j++) std::cout << " " << std::sqrt(Omega[j + j * D_G_A]);
    std::cout << "\nOmega as correlation matrix (rows/cols 0.." << D_G_A - 1 << "):\n";
    for (int i = 0; i < D_G_A; i++) {
        std::cout << "  " << i << ":";
        for (int j = 0; j < D_G_A; j++) {
            double den = std::sqrt(Omega[i + i * D_G_A] * Omega[j + j * D_G_A]);
            char buf[16]; std::snprintf(buf, sizeof buf, " %6.3f", den > 0 ? Omega[i + j * D_G_A] / den : 0.0);
            std::cout << buf;
        }
        std::cout << "\n";
    }
    if (g_cut_ak) {   // the objective's own basis: correlation-scaled Omega with the null-direction guard
        const unsigned mask = a_rowmask(); double sc[D_G_A], C[D_G_A * D_G_A], dt[D_G_A];
        for (int t = 0; t < D_G_A; t++) { const double o = Omega[t + t * D_G_A]; sc[t] = (!(mask & (1u << t)) && o > 0.0) ? std::sqrt(o) : 1.0; dt[t] = dvec[t] / sc[t]; }
        for (int b = 0; b < D_G_A; b++) for (int a = 0; a < D_G_A; a++) C[a + b * D_G_A] = Omega[a + b * D_G_A] / (sc[a] * sc[b]);
        double wc[D_G_A], Zc[D_G_A * D_G_A]; const int mc = eig_A_active(C, wc, Zc);
        if (mc >= 1) {
            const double tol = NULL_EIG_REL * wc[mc - 1]; double dn = 0.0; for (int t = 0; t < D_G_A; t++) if (!(mask & (1u << t))) dn += dt[t] * dt[t]; dn = std::sqrt(dn);
            CueAkInfo inf; cue_core_ak(dvec, Omega, nullptr, &inf);
            std::cout << "CORRELATION-SCALED Omega (the objective's basis under cut=ak; null tol " << tol << "): " << inf.n_null_floored
                      << " null direction(s) floored (penalty), " << inf.n_null_skipped << " skipped (exact identity)\n";
            std::cout << "ceigen k  lambda_C      status   contrib_to_Lhat  top_row(|loading|)\n";
            for (int k = mc - 1; k >= 0; k--) {
                double zd = 0.0; int top = 0; double topv = 0.0;
                for (int t = 0; t < D_G_A; t++) { zd += Zc[t + k * D_G_A] * dt[t]; if (std::fabs(Zc[t + k * D_G_A]) > topv) { topv = std::fabs(Zc[t + k * D_G_A]); top = t; } }
                const bool nul = wc[k] < tol, vio = nul && null_violated(zd, dn);
                const double lam = vio ? tol : wc[k];
                std::cout << "  " << k << "  " << wc[k] << "  " << (nul ? (vio ? "NULL-floor" : "NULL-skip ") : "kept      ") << "  "
                          << ((nul && !vio) ? 0.0 : 0.5 * zd * zd / lam) << "  " << top << "(" << topv << ")\n";
            }
        }
    }
    // eigen-decomposition of the raw Omega (diagnostic; under cut=ak the objective uses the correlation-scaled table above)
    double w[D_G_A], Z[D_G_A * D_G_A];
    int m = eig_A_active(Omega, w, Z);
    if (m < 1) { std::cout << "eigendecomposition failed\n"; return; }
    double max_eig = w[m - 1];
    std::cout << "eigen k  w_k           kept  contrib_to_Lhat  loading_on_last_row  top_row(|loading|)\n";
    for (int k = m - 1; k >= 0; k--) {
        double d2 = 0.0; int top = 0; double topv = 0.0;
        for (int i = 0; i < D_G_A; i++) { d2 += Z[i + k * D_G_A] * dvec[i]; if (std::fabs(Z[i + k * D_G_A]) > topv) { topv = std::fabs(Z[i + k * D_G_A]); top = i; } }
        bool kept = keep_eig_A(w[k], max_eig);
        std::cout << "  " << k << "  " << w[k] << "  " << (kept ? "yes " : "NO  ") << "  " << (kept ? 0.5 * d2 * d2 / w[k] : 0.0)
                  << "  " << Z[(D_G_A - 1) + k * D_G_A] << "  " << top << "(" << topv << ")\n";
    }
    // tilted u, x, omega for interior firms (exp_scale branch only)
    if (g_qform == 0) { std::cout << "(tilted u/x/omega summaries need qform=exp_scale)\n"; return; }
    std::vector<int> idx; for (int i = 0; i < n; i++) if (firms[i].corner == 0) idx.push_back(i);
    int ni = static_cast<int>(idx.size());
    std::vector<double> mu_u(ni), mu_x(ni), mu_om(ni), sd_om(ni), om_pt(ni), lt(ni), mu_lnB(ni), mu_eps(ni), mu_eps2(ni), sic(ni);
    std::vector<double> sh_beyond(ni, 0.0);   // share of kept draws beyond the kink (qform 4)
    // tail diagnostic (2026-09-30, audit finding 1): per-firm max kept u, share of kept draws with u > 8, exposure to the
    // beyond-kink tail (M* > c_k kappa Mbar, so M -> 0 is beyond the kink) and the u^2 coefficient a_i of gamma'g there
    // (row 5: -1, row 7: -(1-beta), row 12: +1, row 13+j: -1; dropped rows excluded). a_i > 0 & exposed => improper tilt.
    std::vector<double> umax(ni, 0.0), sh_u8(ni, 0.0), a_tail(ni, 0.0), ess(ni, 0.0), sh_edge(ni, 0.0); std::vector<int> exposed(ni, 0);
    std::vector<double> mu_q(ni, 0.0);   // tilted E[q] per firm (detection probability; power forms: min(x^k, 1))
    std::vector<double> p05(ni, 0.0), p10(ni, 0.0);   // tilted P(u >= 0.05), P(u >= 0.10) per firm (2026-10-02: the deconvolution's share is a probability)
    auto q_of = [&](double e, const FirmData &f) -> double {
#ifdef KINK
        if (g_qform == 4 || g_qform == 5) return std::min(1.0, std::pow(std::max(0.0, e) / (lambda * f.Mbar), g_kpow));
#endif
        (void)e; (void)f; return std::numeric_limits<double>::quiet_NaN(); };
    std::atomic<int> next{0};
    auto worker = [&]() {
        int k;
        while ((k = next.fetch_add(1)) < ni) {
            const FirmData &f = firms[idx[k]];
            std::mt19937_64 rng(firm_seed(base_seed, f.row_id));
            std::uniform_real_distribution<double> unif(0.0, 1.0);
            GVecA gc, gt, gb; double qc = 0.0;
            if (g_rho_on) moment_g_A_one_exp_scale(f.Mstar, f.Mstar, f.V, f.Wt, f.tau_rho, f.beta, f.Mbar, f.ltau_bar, f.yidx, lambda, delta0, delta1, delta2, gb, f.sig2eps, f.jidx, f.umed, f.audit_g, f.pshare, f.cw);
            double Mc = q_draw(rng, f.Mstar, lambda, f.Mbar);
            moment_g_A_one_exp_scale(Mc, f.Mstar, f.V, f.Wt, f.tau_rho, f.beta, f.Mbar, f.ltau_bar, f.yidx, lambda, delta0, delta1, delta2, gc, f.sig2eps, f.jidx, f.umed, f.audit_g, f.pshare, f.cw);
            if (g_rho_on) qc = rho_Q(gc, gb);
            double su = 0, sx = 0, so = 0, so2 = 0, sb = 0, se = 0, se2 = 0;
            if (g_sampler_is) {   // same fixed draws as firm_chain_A's IS branch (first draw Mc is not used there: redo the stream)
                std::mt19937_64 rng2(firm_seed(base_seed, f.row_id));
                std::vector<double> Ms(n_keep), lw(n_keep); double lmax = -HUGE_VAL;
                for (int j = 0; j < n_keep; j++) {
                    double lwp = 0.0; Ms[j] = is_draw(rng2, f, lambda, lwp);
                    moment_g_A_one_exp_scale(Ms[j], f.Mstar, f.V, f.Wt, f.tau_rho, f.beta, f.Mbar, f.ltau_bar, f.yidx, lambda, delta0, delta1, delta2, gt, f.sig2eps, f.jidx, f.umed, f.audit_g, f.pshare, f.cw);
                    double a = lwp; for (int t = 0; t < D_G_A; t++) a += gamma[t] * gt[t];
                    if (g_rho_on) a -= rho_Q(gt, gb);
                    lw[j] = a; if (a > lmax) lmax = a;
                }
                double sw = 0.0, sw2 = 0.0; std::vector<double> w(n_keep);
                for (int j = 0; j < n_keep; j++) { w[j] = std::exp(lw[j] - lmax); sw += w[j]; sw2 += w[j] * w[j]; }
                ess[k] = sw * sw / sw2;
                for (int j = 0; j < n_keep; j++) {
                    double pj = w[j] / sw, Mj = Ms[j];
                    double om = omega_of_M(Mj, f.Mstar, f.V, f.Wt, f.beta), e = e_of_M(Mj, f.Mstar), uu = std::log(f.Mstar / Mj);
                    su += pj * uu; sx += pj * e / f.Mbar; so += pj * om; so2 += pj * om * om;
                    if (pj > 1e-6 && uu > umax[k]) umax[k] = uu; if (uu > 8.0) sh_u8[k] += pj;
                    mu_q[k] += pj * q_of(e, f);
                    if (uu >= 0.05) p05[k] += pj; if (uu >= 0.10) p10[k] += pj;
#ifdef KINK
                    if ((g_qform == 4 || g_qform == 5) && B_power_scale(e, g_kpow, lambda * f.Mbar) < 1e-3) sh_edge[k] += pj;   // FOC-ceiling edge
#endif
                    { double ep = eps_of_M(Mj, f.Mstar, f.V); se += pj * ep; se2 += pj * ep * ep; }
#ifdef KINK
                    if (g_qform == 4 && e / (lambda * f.Mbar) >= power_ceiling(g_kpow)) sh_beyond[k] += pj;
#endif
                    sb += pj * q_lnB(e, lambda, f.Mbar);
                }
                su *= n_keep; sx *= n_keep; so *= n_keep; so2 *= n_keep; se *= n_keep; se2 *= n_keep; sb *= n_keep;   // undone by the /n_keep below
            } else
            for (int r = -n_burn + 1; r <= n_keep; r++) {
                double Mt = q_draw(rng, f.Mstar, lambda, f.Mbar);
                moment_g_A_one_exp_scale(Mt, f.Mstar, f.V, f.Wt, f.tau_rho, f.beta, f.Mbar, f.ltau_bar, f.yidx, lambda, delta0, delta1, delta2, gt, f.sig2eps, f.jidx, f.umed, f.audit_g, f.pshare, f.cw);
                double lr = 0.0, qt = 0.0; for (int t = 0; t < D_G_A; t++) lr += gamma[t] * (gt[t] - gc[t]);
                if (g_rho_on) { qt = rho_Q(gt, gb); lr -= (qt - qc); }
                if (std::log(unif(rng)) < lr) { gc = gt; Mc = Mt; qc = qt; }
                if (r > 0) {
                    double om = omega_of_M(Mc, f.Mstar, f.V, f.Wt, f.beta), e = e_of_M(Mc, f.Mstar);
                    su += std::log(f.Mstar / Mc); sx += e / f.Mbar; so += om; so2 += om * om;
                    { double uu = std::log(f.Mstar / Mc); if (uu > umax[k]) umax[k] = uu; if (uu > 8.0) sh_u8[k] += 1.0 / n_keep; }
                    mu_q[k] += q_of(e, f) / n_keep;
                    { double uu = std::log(f.Mstar / Mc); if (uu >= 0.05) p05[k] += 1.0 / n_keep; if (uu >= 0.10) p10[k] += 1.0 / n_keep; }
                    { double ep = eps_of_M(Mc, f.Mstar, f.V); se += ep; se2 += ep * ep; }
#ifdef KINK
                    if (g_qform == 4 && e / (lambda * f.Mbar) >= power_ceiling(g_kpow)) sh_beyond[k] += 1.0 / n_keep;
#endif
                    sb += q_lnB(e, lambda, f.Mbar);
                }
            }
            mu_u[k] = su / n_keep; mu_x[k] = sx / n_keep; mu_om[k] = so / n_keep; mu_lnB[k] = sb / n_keep;
            mu_eps[k] = se / n_keep; mu_eps2[k] = se2 / n_keep;
            sd_om[k] = std::sqrt(std::max(0.0, so2 / n_keep - mu_om[k] * mu_om[k]));
            om_pt[k] = omega_of_M(f.Mstar, f.Mstar, f.V, f.Wt, f.beta);
            lt[k] = std::log(f.tau_rho);
            {   unsigned msk = a_rowmask(); auto live = [&](int t) { return t < D_G_A && !(msk & (1u << t)); };
                double a = 0.0;
                if (live(5)) a += -gamma[5];
                if (live(7)) a += -(1.0 - f.beta) * gamma[7];
#ifdef EPSVAR
                if (live(12)) a += gamma[12];
#endif
#ifdef IND5
                if (g_ind_mode == 0 && f.jidx >= 0 && live(13 + f.jidx)) a += -gamma[13 + f.jidx];   // only eps*lnM grows like u^2
#endif
                a_tail[k] = a;
#ifdef KINK
                exposed[k] = (g_qform == 4 && f.Mstar >= power_ceiling(g_kpow) * lambda * f.Mbar) ? 1 : 0;
                if (g_qform == 5) exposed[k] = (f.Mstar <= power_ceiling(g_kpow) * lambda * f.Mbar) ? 1 : 0;   // support reaches M -> 0
#endif
            }
        }
    };
    std::vector<std::thread> pool; for (int t = 0; t < std::max(1, n_threads); t++) pool.emplace_back(worker);
    for (auto &th : pool) th.join();
    auto mean = [&](const std::vector<double> &v) { double s = 0; for (double a : v) s += a; return s / v.size(); };
    auto sd = [&](const std::vector<double> &v) { double mu = mean(v), s = 0; for (double a : v) s += (a - mu) * (a - mu); return std::sqrt(s / (v.size() - 1)); };
    auto cor = [&](const std::vector<double> &a, const std::vector<double> &b) {
        double ma = mean(a), mb = mean(b), sab = 0, saa = 0, sbb = 0;
        for (size_t i = 0; i < a.size(); i++) { sab += (a[i] - ma) * (b[i] - mb); saa += (a[i] - ma) * (a[i] - ma); sbb += (b[i] - mb) * (b[i] - mb); }
        return sab / std::sqrt(saa * sbb); };
    {   // TARGETED MOMENTS (2026-10-01, lead): evasion by industry vs data, share overreporting, detection probability
        std::map<int, std::vector<int>> by;
        for (int k2 = 0; k2 < ni; k2++) by[firms[idx[k2]].sic].push_back(k2);
        std::cout << "TARGETED: industry | n | E[V] data | tilted E[u] | share of firms with tilted E[u] >= 0.05 | mean tilted E[q]\n";
        double sh_all = 0;
        for (auto &kv : by) {
            double ev = 0, eu = 0, sh = 0, eq = 0; int nn = (int)kv.second.size();
            for (int k2 : kv.second) { ev += firms[idx[k2]].V; eu += mu_u[k2]; sh += (mu_u[k2] >= 0.05); eq += mu_q[k2]; }
            sh_all += sh;
            char buf[200]; std::snprintf(buf, sizeof buf, "  %d | %d | %.3f | %.3f | %.3f | %.4f\n", kv.first, nn, ev / nn, eu / nn, sh / nn, eq / nn);
            std::cout << buf;
        }
        std::cout << "TARGETED-CW (claims-weighted within industry, w = tau_P M*; industries by claims share): industry | claims share | E_w[V] data | tilted E_w[u] | gap\n";
        {   std::map<int, double> cl; double ctot = 0, aw = 0, av = 0, au = 0;
            for (int k2 = 0; k2 < ni; k2++) { const FirmData &f = firms[idx[k2]]; cl[f.sic] += f.tau_rho * f.Mstar; ctot += f.tau_rho * f.Mstar; }
            for (auto &kv : by) { double w = 0, v = 0, u = 0;
                for (int k2 : kv.second) { const FirmData &f = firms[idx[k2]]; const double c = f.tau_rho * f.Mstar; w += c; v += c * f.V; u += c * mu_u[k2]; }
                double sh = cl[kv.first] / ctot; aw += sh; av += sh * v / w; au += sh * u / w;
                char bc[160]; std::snprintf(bc, sizeof bc, "  %d | %.4f | %.3f | %.3f | %+.3f\n", kv.first, sh, v / w, u / w, (u - v) / w); std::cout << bc; }
            double mae = 0; for (auto &kv : by) { double w = 0, v = 0, u = 0;
                for (int k2 : kv.second) { const FirmData &f = firms[idx[k2]]; const double c = f.tau_rho * f.Mstar; w += c; v += c * f.V; u += c * mu_u[k2]; }
                mae += cl[kv.first] / ctot * std::fabs(u - v) / w; }
            char bc[200]; std::snprintf(bc, sizeof bc, "TARGETED-CW: all | E_w[V] %.3f | tilted E_w[u] %.3f | claims-weighted MAE of industry gaps %.4f\n", av / aw, au / aw, mae);
            std::cout << bc; }
        std::cout << "TARGETED-P: industry | mean tilted P(u >= 0.05) | mean tilted P(u >= 0.10)   (comparable to the deconvolution's P)\n";
        for (auto &kv : by) { double a = 0, b = 0; for (int k2 : kv.second) { a += p05[k2]; b += p10[k2]; }
            char bp[120]; std::snprintf(bp, sizeof bp, "  %d | %.3f | %.3f\n", kv.first, a / kv.second.size(), b / kv.second.size()); std::cout << bp; }
        std::vector<double> qs = mu_q; std::sort(qs.begin(), qs.end());
        double mq = 0; for (double x : mu_q) mq += x; mq /= ni;
        auto qq = [&](double p) { return qs[std::min(qs.size() - 1, (size_t)(p * (qs.size() - 1)))]; };
        char buf[300]; std::snprintf(buf, sizeof buf, "TARGETED: all | share overreporting (E[u] >= 0.05) %.3f | detection E[q]: mean %.4f p50 %.4f p90 %.4f p99 %.4f max %.4f\n",
                                     sh_all / ni, mq, qq(0.5), qq(0.9), qq(0.99), qs.back());
        std::cout << buf;
        std::snprintf(buf, sizeof buf, "TARGETED: detection E[q] by firm: p25 %.4f p50 %.4f p75 %.4f p80 %.4f p90 %.4f p95 %.4f (firm-level tilted means, %d firms)\n",
                      qq(0.25), qq(0.5), qq(0.75), qq(0.8), qq(0.9), qq(0.95), ni);
        std::cout << buf;
        double qg = 0; int ng = 0; for (int k2 = 0; k2 < ni; k2++) if (firms[idx[k2]].audit_g) { qg += mu_q[k2]; ng++; }
        if (ng) { std::snprintf(buf, sizeof buf, "TARGETED: mean E[q] in audit group (%d firms) %.4f\n", ng, qg / ng); std::cout << buf; }
    }
    std::cout << "interior firms: " << ni << "\n"
              << "  tilted eps    : mean of firm means " << mean(mu_eps) << "; mean of firm E[eps^2] " << mean(mu_eps2)
              << " (=> tilted var(eps) " << mean(mu_eps2) - mean(mu_eps) * mean(mu_eps) << ")\n"
              << "  tilted u      : mean of firm means " << mean(mu_u) << ", sd across firms " << sd(mu_u) << "\n"
              << [&]() { std::vector<double> v = mu_u; std::sort(v.begin(), v.end());
                         auto q = [&](double p) { return v[std::min(v.size() - 1, (size_t)(p * (v.size() - 1)))]; };
                         size_t z = 0, z2 = 0; for (double a : v) { if (a < 0.01) z++; if (a < 0.05) z2++; }
                         std::ostringstream o; o << "  tilted u quantiles (firm means): p10 " << q(0.10) << " p25 " << q(0.25) << " p50 " << q(0.5)
                           << " p75 " << q(0.75) << " p90 " << q(0.9) << " p99 " << q(0.99) << " max " << v.back()
                           << " | share of firms with mean u < 0.01: " << double(z) / v.size() << ", < 0.05: " << double(z2) / v.size() << "\n"
                           << "  share of draws beyond the kink (mean over firms) " << mean(sh_beyond) << "; firms with >50% of draws beyond: "
                           << [&]() { size_t c = 0; for (double b : sh_beyond) if (b > 0.5) c++; return double(c) / sh_beyond.size(); }() << "\n";
                         return o.str(); }()
              << [&]() { size_t ex = 0, bad = 0, u8 = 0, u12 = 0; double s8 = 0; std::vector<double> v = umax; std::sort(v.begin(), v.end());
                         for (int k2 = 0; k2 < ni; k2++) { if (exposed[k2]) { ex++; if (a_tail[k2] > 0) bad++; } if (umax[k2] > 8) u8++; if (umax[k2] > 12) u12++; s8 += sh_u8[k2]; }
                         std::ostringstream o;
                         {   double me = 0; size_t h = 0; for (double x : sh_edge) { me += x; if (x > 0.5) h++; }
                             o << "  CEILING EDGE (B < 1e-3, IS only): mean tilted mass " << me / ni << "; firms with > 50% of mass there " << double(h) / ni << "\n"; }
                         if (g_sampler_is) { std::vector<double> e2 = ess; std::sort(e2.begin(), e2.end());
                             o << "  IS effective sample size per firm (of " << n_keep << "): p1 " << e2[(size_t)(0.01 * (ni - 1))] << " p10 " << e2[(size_t)(0.1 * (ni - 1))]
                               << " p50 " << e2[ni / 2] << " mean " << [&]() { double a = 0; for (double x : ess) a += x; return a / ni; }() << "\n"; }
                         o << (g_qform == 5 ? "  TAIL (power_nokink: exposed = support reaches M -> 0): exposed " : "  TAIL: exposed to beyond-kink tail ") << double(ex) / ni << "; exposed with a_i>0 (" << (g_rho_on ? "proper under rho=prop21; tilt still grows like exp(a u^2) before the rho penalty takes over" : "improper tilt under uniform rho") << ") " << double(bad) / ni
                           << " | max kept u per firm: p50 " << v[ni / 2] << " p99 " << v[(size_t)(0.99 * (ni - 1))] << " max " << v.back()
                           << " | firms with a kept u > 8: " << u8 << ", > 12: " << u12 << " | mean share of kept draws with u > 8: " << s8 / ni << "\n";
                         return o.str(); }()
              << "  tilted x=e/Mb : mean " << mean(mu_x) << ", sd " << sd(mu_x) << "\n"
              << "  tilted ln B   : mean " << mean(mu_lnB) << ", sd " << sd(mu_lnB) << "\n"
              << "  omega at M=M* : mean " << mean(om_pt) << ", sd " << sd(om_pt) << "\n"
              << "  tilted omega  : mean " << mean(mu_om) << ", sd across firms " << sd(mu_om) << ", mean within-firm sd " << mean(sd_om) << "\n"
              << "  cor with ln tau_P: omega(M*) " << cor(om_pt, lt) << ", tilted omega " << cor(mu_om, lt)
              << ", tilted ln B " << cor(mu_lnB, lt) << ", tilted u " << cor(mu_u, lt) << "\n"
              << "  var(ln tau_P) " << sd(lt) * sd(lt) << "\n";
}

// ---- inner problem: profile (delta0,eta,LAMBDA,gamma[1:9]) at FIXED (delta1,delta2) --
// x[] = (delta0, eta, lambda, gamma[1..9]) -- 12 free dims, same count as
// fit_one_grid_point's own inner problem (2+7=9 there vs 3+9=12 here -- more
// free dims because lambda moved from "fixed grid value" to "free", and A
// has 2 more gamma rows than C).
struct InnerParamsAFixedDelta {
    const std::vector<FirmData> *firms;
    double delta1, delta2;
    int n_burn, n_keep, n_threads;
    uint64_t base_seed;
};

static double inner_obj_A_fixedDelta(unsigned n, const double *x, double *grad, void *data) {
    (void)n; (void)grad;
    InnerParamsAFixedDelta *p = static_cast<InnerParamsAFixedDelta *>(data);
    double delta0 = x[0], lambda = x[1];
    double gamma[D_G_A];
    for (int t = 0; t < D_G_A; t++) gamma[t] = x[2 + t];

    double dvec[D_G_A], Omega[D_G_A * D_G_A];
    compute_dvec_omega_A(*(p->firms), lambda, delta0, p->delta1, p->delta2, gamma,
                          p->n_burn, p->n_keep, p->base_seed, p->n_threads, dvec, Omega);
    return cue_objective_A_std(dvec, Omega);
}

struct FitResultAFixedDelta {
    double delta0, lambda, gamma[D_G_A], Lhat;
    int convergence, iters;
    double Lhat_pass1, wander;
    int iters_pass1, convergence_pass1;
    double point_seconds;
};

// Two-pass BOBYQA, x0_in ALWAYS supplied (this mode has no zero-default --
// every point seeds from the same caller-provided best-known par, per
// tollgate 1). lambda bounded [lambda_lo,lambda_hi] (2026-09-08: NOT
// unbounded like gamma -- lambda is a real structural parameter with a
// physically meaningful FOC-domain interpretation, unlike gamma's Lagrange-
// multiplier role; also keeps it from drifting back into the "too-small-to-
// matter" region the user explicitly stopped chasing this session).
// algo (2026-09-09): Schennach (ELVIS_supplement.pdf, Appendix G, p.28) uses
// Nelder-Mead specifically for the gamma sub-problem, grids theta separately
// to avoid its own possible multi-modality -- NOT the same design as ours
// (we jointly optimize the remaining theta_smooth block together with gamma
// in one call). This param is deliberately scoped narrower than that: purely
// an algorithm swap on the SAME combined vector/bounds/structure, to check
// whether BOBYQA itself is a source of the instability -- not a redesign.
static FitResultAFixedDelta fit_one_grid_point_A_fixedDelta(
    const std::vector<FirmData> &firms, double delta1, double delta2,
    int n_burn, int n_keep, uint64_t base_seed, int n_threads, double maxtime,
    const double *x0_in, double lambda_lo, double lambda_hi,
    nlopt_algorithm algo = NLOPT_LN_BOBYQA
) {
    const int n_par = 2 + D_G_A;   // delta0, lambda, gamma[1:9] -- eta DROPPED 2026-09-10
    InnerParamsAFixedDelta params{&firms, delta1, delta2, n_burn, n_keep, n_threads, base_seed};

    double lower[n_par], upper[n_par], x[n_par];
    lower[0] = -DELTA_BOUND; upper[0] = DELTA_BOUND;
    lower[1] = lambda_lo;    upper[1] = lambda_hi;
    for (int t = 0; t < D_G_A; t++) { lower[2 + t] = -HUGE_VAL; upper[2 + t] = HUGE_VAL; }
    for (int t = 0; t < n_par; t++) x[t] = x0_in[t];
    x[1] = std::min(std::max(x[1], lambda_lo), lambda_hi);

    auto run_bobyqa = [&](double *xstart) -> FitResultAFixedDelta {
        nlopt_opt opt = nlopt_create(algo, n_par);
        nlopt_set_lower_bounds(opt, lower);
        nlopt_set_upper_bounds(opt, upper);
        nlopt_set_min_objective(opt, inner_obj_A_fixedDelta, &params);
        nlopt_set_xtol_rel(opt, 1e-4);
        nlopt_set_maxeval(opt, 2000);
        nlopt_set_maxtime(opt, maxtime);
        double minf = HUGE_VAL;
        nlopt_result res = nlopt_optimize(opt, xstart, &minf);
        int iters = nlopt_get_numevals(opt);
        nlopt_destroy(opt);
        FitResultAFixedDelta r;
        r.delta0 = xstart[0]; r.lambda = xstart[1];
        for (int t = 0; t < D_G_A; t++) r.gamma[t] = xstart[2 + t];
        r.Lhat = minf; r.convergence = static_cast<int>(res); r.iters = iters;
        return r;
    };

    auto t_start = std::chrono::steady_clock::now();
    FitResultAFixedDelta r1 = run_bobyqa(x);
    double x2[n_par];
    x2[0] = r1.delta0; x2[1] = r1.lambda;
    for (int t = 0; t < D_G_A; t++) x2[2 + t] = r1.gamma[t];
    FitResultAFixedDelta r2 = run_bobyqa(x2);
    r2.point_seconds = std::chrono::duration<double>(std::chrono::steady_clock::now() - t_start).count();

    r2.Lhat_pass1 = r1.Lhat;
    r2.iters_pass1 = r1.iters;
    r2.convergence_pass1 = r1.convergence;
    double sq = (r2.delta0 - r1.delta0) * (r2.delta0 - r1.delta0)
              + (r2.lambda - r1.lambda) * (r2.lambda - r1.lambda);
    for (int t = 0; t < D_G_A; t++) sq += (r2.gamma[t] - r1.gamma[t]) * (r2.gamma[t] - r1.gamma[t]);
    r2.wander = std::sqrt(sq);
    return r2;
}

// ---- (delta1,delta2)-grid runner: every point independent, same x0 -------
static void run_deltagrid_mode(
    const std::vector<FirmData> &firms, const std::vector<std::pair<double,double>> &points,
    const double *x0, double lambda_lo, double lambda_hi,
    int n_burn, int n_keep, uint64_t base_seed, int n_threads, double maxtime,
    int shard_id, int n_shards, const std::string &output_csv,
    nlopt_algorithm algo = NLOPT_LN_BOBYQA
) {
    std::vector<size_t> my_indices;
    for (size_t idx = 0; idx < points.size(); idx++) if ((int)(idx % n_shards) == shard_id) my_indices.push_back(idx);
    std::cout << "deltagrid: " << points.size() << " points total, " << my_indices.size()
              << " assigned to this shard, " << n_threads << " threads/point\n";

    std::vector<std::pair<std::pair<double,double>, FitResultAFixedDelta>> results;
    results.reserve(my_indices.size());
    auto t0 = std::chrono::steady_clock::now();
    size_t done = 0;

    for (size_t idx : my_indices) {
        double d1 = points[idx].first, d2 = points[idx].second;
        FitResultAFixedDelta fit = fit_one_grid_point_A_fixedDelta(
            firms, d1, d2, n_burn, n_keep, base_seed, n_threads, maxtime, x0, lambda_lo, lambda_hi, algo);
        results.push_back({{d1, d2}, fit});
        done++;
        auto elapsed = std::chrono::duration<double>(std::chrono::steady_clock::now() - t0).count();
        std::cout << "  [" << done << "/" << my_indices.size() << "] elapsed=" << elapsed << "s"
                  << "  d1=" << d1 << " d2=" << d2 << " lambda_hat=" << fit.lambda
                  << " Lhat=" << fit.Lhat << " (pass1=" << fit.Lhat_pass1 << ")"
                  << " conv=" << fit.convergence << " iters=" << fit.iters
                  << " point_seconds=" << fit.point_seconds << "\n" << std::flush;
    }

    std::ofstream out(output_csv);
    if (!out.is_open()) { std::cerr << "ERROR: could not open output_csv for writing: " << output_csv << "\n"; std::exit(1); }
    out << std::setprecision(15);
    out << "delta1,delta2,delta0_hat,lambda_hat,gamma1,gamma2,gamma3,gamma4,gamma5,gamma6,gamma7,gamma8,gamma9,"
           "Lhat,Lhat_pass1,wander,convergence,convergence_pass1,iters,iters_pass1,point_seconds,n\n";
    for (auto &r : results) {
        out << r.first.first << "," << r.first.second << ","
            << r.second.delta0 << "," << r.second.lambda << ",";
        for (int t = 0; t < D_G_A; t++) out << r.second.gamma[t] << ",";
        out << r.second.Lhat << "," << r.second.Lhat_pass1 << "," << r.second.wander << ","
            << r.second.convergence << "," << r.second.convergence_pass1 << ","
            << r.second.iters << "," << r.second.iters_pass1 << "," << r.second.point_seconds << "," << firms.size() << "\n";
    }
    out.close();
    std::cout << "Saved: " << output_csv << " (" << results.size() << " points)\n";
}

// ============================================================================
// ---- Moment set A (9 rows), lambda-only grid (2026-09-10) -----------------
// ============================================================================
// Mirror of the deltagrid mode above with lambda/delta1,delta2's roles
// swapped: lambda FIXED per grid point, (delta0,delta1,delta2,gamma[1:9])
// all free -- 12 free dims. Built specifically to re-validate the eta-
// removal + draw_from_rho_checked + widened lambda-bounds changes against
// the ORIGINAL eta-based lambda-grid's own results (same moment set A,
// same independent same-point seeding convention, Tollgate 1 -- no
// chaining, to keep this a clean apples-to-apples comparison, not a
// confound with a different seeding scheme).
struct InnerParamsAFixedLambda {
    const std::vector<FirmData> *firms;
    double lambda;
    int n_burn, n_keep, n_threads;
    uint64_t base_seed;
};

static double inner_obj_A_fixedLambda(unsigned n, const double *x, double *grad, void *data) {
    (void)n; (void)grad;
    InnerParamsAFixedLambda *p = static_cast<InnerParamsAFixedLambda *>(data);
    double delta0 = x[0], delta1 = x[1], delta2 = x[2];
#ifdef YEAR_FE
    for (int k = 1; k <= N_XFE; k++) g_d0yr[k] = x[3 + k - 1];   // one point per process: see YEAR_FE note
#endif
#ifdef KINK
    g_kpow = x[3];                                                // one point per process: see KINK note
#endif
#ifdef KINK_S
    g_kshare = x[4];                                              // s estimated (KINK_S)
#endif
#ifdef KAPPA_FREE
    const double lam_use = x[5];                                  // kappa estimated (KAPPA_FREE)
#else
    const double lam_use = p->lambda;
#endif
    double gamma[D_G_A];
    for (int t = 0; t < D_G_A; t++) gamma[t] = x[3 + N_XFE + t];

    double dvec[D_G_A], Omega[D_G_A * D_G_A];
    compute_dvec_omega_A(*(p->firms), lam_use, delta0, delta1, delta2, gamma,
                          p->n_burn, p->n_keep, p->base_seed, p->n_threads, dvec, Omega);
    return cue_objective_A_std(dvec, Omega);
}

struct FitResultAFixedLambda {
    double delta0, delta1, delta2, gamma[D_G_A], Lhat;
    double d0yr[N_XFE > 0 ? N_XFE : 1];   // year intercepts 82..91 (YEAR_FE build only)
    int convergence, iters;
    double Lhat_pass1, wander;
    int iters_pass1, convergence_pass1;
    double point_seconds;
    long inner_cap = -1, inner_fail = -1;   // nested mode: inner solves at the eval cap / failed (-1 = not nested)
    double ws_L0 = std::numeric_limits<double>::quiet_NaN(), ws_L1 = std::numeric_limits<double>::quiet_NaN();   // gamma_init=solve: L before / after
};

// ============================================================================
// Nested solve (2026-09-30, Phase 2, audit 7.5): outer Nelder-Mead over the free theta entries (delta, k, s, kappa),
// inner L-BFGS over gamma with an ANALYTIC gradient. Requires sampler=is: at fixed theta each firm's R fixed draws
// g_j(theta) and rho terms -Q_j(theta) are computed once (NestedCache) and the inner problem only reweights them.
//   gtilde_i(gamma) = sum_j w_ij g_ij / sum_j w_ij,  w_ij = exp(gamma'g_ij - Q_ij)
//   L(gamma) = 1/2 d' Omega^+ d, d = mean_i gtilde_i, Omega = n^-1 sum_p S_p S_p' (S_p = sum_{i in p} (gtilde_i - d);
//              p = i when cluster=none)
//   dgtilde_i/dgamma = H_i = Cov_w,i(g, g);  with v = Omega^+ d and s_p = v'S_p:
//   grad L = n^-1 sum_i H_i v (1 - s_{p(i)}) + (n^-1 sum_p n_p s_p) Hbar v,   Hbar = n^-1 sum_i H_i
// (the dOmega term uses d(S_p) = sum_{i in p} H_i - n_p Hbar). Omega^+ keeps eigenvalues > 0 on the live rows
// (eig_A_active), as the regular objective. Corner firms: one fixed row, weight 1, H = 0. CLI nested=1.
static bool g_nested = false;

struct NestedCache {
    int n = 0, R = 0;
    std::vector<float> G;      // n x R x D_G_A (firm-major); corner firms use slot j = 0 only
    std::vector<float> lq;     // n x R: -Q_ij (0 when rho=uniform)
    std::vector<int> Ri;       // draws per firm (R interior, 1 corner)
};

static void nested_build_cache(NestedCache &C, const std::vector<FirmData> &firms, double lambda, double delta0,
                               double delta1, double delta2, int R, uint64_t base_seed, int n_threads) {
    C.n = (int)firms.size(); C.R = R;
    C.G.assign((size_t)C.n * R * D_G_A, 0.0f); C.lq.assign((size_t)C.n * R, 0.0f); C.Ri.assign(C.n, R);
    std::atomic<int> next{0};
    auto work = [&]() {
        int i;
        while ((i = next.fetch_add(1)) < C.n) {
            const FirmData &f = firms[i];
            float *Gi = C.G.data() + (size_t)i * R * D_G_A; float *lqi = C.lq.data() + (size_t)i * R;
            if (f.corner == 1) {   // fixed row, exactly as firm_chain_A's corner branch
                double row[D_G_A];
                firm_chain_A(f, lambda, delta0, delta1, delta2, nullptr, 0, 0, base_seed, row);
                for (int t = 0; t < D_G_A; t++) Gi[t] = (float)row[t];
                C.Ri[i] = 1; continue;
            }
            std::mt19937_64 rng(firm_seed(base_seed, f.row_id));   // same stream and order as firm_chain_A's IS branch
            GVecA g, gbar;
            if (g_rho_on) moment_g_A_one_exp_scale(f.Mstar, f.Mstar, f.V, f.Wt, f.tau_rho, f.beta, f.Mbar, f.ltau_bar, f.yidx, lambda, delta0, delta1, delta2, gbar, f.sig2eps, f.jidx, f.umed, f.audit_g, f.pshare, f.cw);
            for (int j = 0; j < R; j++) {
                double lwp = 0.0; double M = is_draw(rng, f, lambda, lwp);
                moment_g_A_one_exp_scale(M, f.Mstar, f.V, f.Wt, f.tau_rho, f.beta, f.Mbar, f.ltau_bar, f.yidx, lambda, delta0, delta1, delta2, g, f.sig2eps, f.jidx, f.umed, f.audit_g, f.pshare, f.cw);
                for (int t = 0; t < D_G_A; t++) Gi[(size_t)j * D_G_A + t] = (float)g[t];
                lqi[j] = (float)((g_rho_on ? -rho_Q(g, gbar) : 0.0) + lwp);
            }
        }
    };
    std::vector<std::thread> pool; for (int t = 0; t < std::max(1, n_threads); t++) pool.emplace_back(work);
    for (auto &th : pool) th.join();
}

struct NestedInner {
    const NestedCache *C; const std::vector<FirmData> *firms; int n_threads;
    std::vector<int> free_idx;   // gamma rows optimized (live rows)
    long evals = 0;
};

// L(gamma) and its gradient (w.r.t. the free gamma rows). gam_full: D_G_A vector.
static double nested_L(const NestedInner &P, const double *gam_full, double *grad_free) {
    const NestedCache &C = *P.C; const int n = C.n;
    std::vector<double> gt((size_t)n * D_G_A);
    std::atomic<int> next{0};
    auto pass1 = [&]() {
        int i; std::vector<double> lw;
        while ((i = next.fetch_add(1)) < n) {
            const int Ri = C.Ri[i]; const float *Gi = C.G.data() + (size_t)i * C.R * D_G_A; const float *lqi = C.lq.data() + (size_t)i * C.R;
            double *gi = gt.data() + (size_t)i * D_G_A;
            if (Ri == 1) { for (int t = 0; t < D_G_A; t++) gi[t] = Gi[t]; continue; }
            lw.resize(Ri); double lmax = -HUGE_VAL;
            for (int j = 0; j < Ri; j++) { double a = lqi[j]; const float *g = Gi + (size_t)j * D_G_A; for (int t = 0; t < D_G_A; t++) a += gam_full[t] * g[t]; lw[j] = a; if (a > lmax) lmax = a; }
            double sw = 0.0; for (int t = 0; t < D_G_A; t++) gi[t] = 0.0;
            for (int j = 0; j < Ri; j++) { double w = std::exp(lw[j] - lmax); lw[j] = w; sw += w; const float *g = Gi + (size_t)j * D_G_A; for (int t = 0; t < D_G_A; t++) gi[t] += w * g[t]; }
            for (int t = 0; t < D_G_A; t++) gi[t] /= sw;
        }
    };
    { std::vector<std::thread> pool; for (int t = 0; t < std::max(1, P.n_threads); t++) pool.emplace_back(pass1); for (auto &th : pool) th.join(); }
    double d[D_G_A] = {0};
    for (int i = 0; i < n; i++) for (int t = 0; t < D_G_A; t++) d[t] += gt[(size_t)i * D_G_A + t];
    for (int t = 0; t < D_G_A; t++) d[t] /= n;
    // S (cluster sums of centred rows) and Omega
    const int ncl = g_cluster_on ? g_ncl : n;
    std::vector<double> S((size_t)ncl * D_G_A, 0.0); std::vector<int> ncount(ncl, 0);
    for (int i = 0; i < n; i++) { int p = g_cluster_on ? (*P.firms)[i].cl : i; ncount[p]++;
        for (int t = 0; t < D_G_A; t++) S[(size_t)p * D_G_A + t] += gt[(size_t)i * D_G_A + t] - d[t]; }
    double Omega[D_G_A * D_G_A] = {0};
    for (int p = 0; p < ncl; p++) { const double *sp = S.data() + (size_t)p * D_G_A;
        for (int a = 0; a < D_G_A; a++) { if (sp[a] == 0.0) continue; for (int b = 0; b < D_G_A; b++) Omega[a + b * D_G_A] += sp[a] * sp[b]; } }
    for (int k = 0; k < D_G_A * D_G_A; k++) Omega[k] /= n;
    double v[D_G_A] = {0};   // v = Omega^+ dbar (guarded, correlation-scaled core; nested=1 requires cut=ak)
    const double L = cue_core_ak(d, Omega, v, nullptr);
    if (!std::isfinite(L)) return HUGE_VAL;
    if (grad_free) {
        std::vector<double> sp(ncl, 0.0); double nps = 0.0;
        for (int p = 0; p < ncl; p++) { double a = 0.0; for (int t = 0; t < D_G_A; t++) a += v[t] * S[(size_t)p * D_G_A + t]; sp[p] = a; nps += ncount[p] * a; }
        nps /= n;
        // per firm h_i = H_i v = E_w[g (g'v)] - gtilde (gtilde'v), stored per firm and summed in firm order afterwards
        // (deterministic regardless of thread scheduling; review 2026-09-30 found per-thread buffers made fits irreproducible)
        std::vector<double> hstore((size_t)n * D_G_A, 0.0);
        std::atomic<int> nx{0};
        auto pass2 = [&](int tid) {
            (void)tid; int i; std::vector<double> ww;
            while ((i = nx.fetch_add(1)) < n) {
                const int Ri = C.Ri[i]; if (Ri == 1) continue;
                const float *Gi = C.G.data() + (size_t)i * C.R * D_G_A; const float *lqi = C.lq.data() + (size_t)i * C.R;
                const double *gi = gt.data() + (size_t)i * D_G_A;
                ww.resize(Ri); double lmax = -HUGE_VAL;
                for (int j = 0; j < Ri; j++) { double a = lqi[j]; const float *g = Gi + (size_t)j * D_G_A; for (int t = 0; t < D_G_A; t++) a += gam_full[t] * g[t]; ww[j] = a; if (a > lmax) lmax = a; }
                double sw = 0.0; for (int j = 0; j < Ri; j++) { ww[j] = std::exp(ww[j] - lmax); sw += ww[j]; }
                double h[D_G_A] = {0};
                for (int j = 0; j < Ri; j++) { const float *g = Gi + (size_t)j * D_G_A; double gv = 0.0; for (int t = 0; t < D_G_A; t++) gv += g[t] * v[t];
                    double c = ww[j] / sw * gv; for (int t = 0; t < D_G_A; t++) h[t] += c * g[t]; }
                double gtv = 0.0; for (int t = 0; t < D_G_A; t++) gtv += gi[t] * v[t];
                for (int t = 0; t < D_G_A; t++) h[t] -= gi[t] * gtv;
                for (int t = 0; t < D_G_A; t++) hstore[(size_t)i * D_G_A + t] = h[t];
            }
        };
        { std::vector<std::thread> pool; for (int t = 0; t < std::max(1, P.n_threads); t++) pool.emplace_back(pass2, t); for (auto &th : pool) th.join(); }
        double g1[D_G_A] = {0}, g2[D_G_A] = {0};
        for (int i = 0; i < n; i++) {
            const double *h = hstore.data() + (size_t)i * D_G_A;
            double sPi = g_cluster_on ? sp[(*P.firms)[i].cl] : sp[i];
            for (int t = 0; t < D_G_A; t++) { g1[t] += h[t] * (1.0 - sPi); g2[t] += h[t]; }
        }
        for (size_t q = 0; q < P.free_idx.size(); q++) { int t = P.free_idx[q]; grad_free[q] = g1[t] / n + nps * g2[t] / n; }
    }
    return L;
}

static double nested_inner_obj(unsigned nf, const double *x, double *grad, void *data) {
    NestedInner *P = static_cast<NestedInner *>(data);
    double gam[D_G_A] = {0};
    for (unsigned q = 0; q < nf; q++) gam[P->free_idx[q]] = x[q];
    P->evals++;
    return nested_L(*P, gam, grad);
}

// Convex dual (Schennach 2014 / AK2020): F(gamma) = n^-1 sum_i log sum_j exp(gamma'g_ij - Q_ij + lw_ij), convex in gamma,
// gradient = dbar(gamma) = n^-1 sum_i gtilde_i(gamma); its minimizer solves dbar = 0 when a finite solution exists
// (otherwise gamma diverges along the direction the moments cannot be matched). Used as the inner start (inner_start=dual,
// default): unique for given theta, independent of the path. Sums in firm order.
static double nested_F(unsigned nf, const double *x, double *grad, void *data) {
    NestedInner *P = static_cast<NestedInner *>(data);
    const NestedCache &C = *P->C; const int n = C.n;
    double gam[D_G_A] = {0}; for (unsigned q = 0; q < nf; q++) gam[P->free_idx[q]] = x[q];
    std::vector<double> lse(n), gt((size_t)n * D_G_A);
    std::atomic<int> next{0};
    auto work = [&]() {
        int i; std::vector<double> lw;
        while ((i = next.fetch_add(1)) < n) {
            const int Ri = C.Ri[i]; const float *Gi = C.G.data() + (size_t)i * C.R * D_G_A; const float *lqi = C.lq.data() + (size_t)i * C.R;
            double *gi = gt.data() + (size_t)i * D_G_A;
            if (Ri == 1) { double a = 0.0; for (int t = 0; t < D_G_A; t++) { gi[t] = Gi[t]; a += gam[t] * Gi[t]; } lse[i] = a; continue; }
            lw.resize(Ri); double lmax = -HUGE_VAL;
            for (int j = 0; j < Ri; j++) { double a = lqi[j]; const float *g = Gi + (size_t)j * D_G_A; for (int t = 0; t < D_G_A; t++) a += gam[t] * g[t]; lw[j] = a; if (a > lmax) lmax = a; }
            double sw = 0.0; for (int t = 0; t < D_G_A; t++) gi[t] = 0.0;
            for (int j = 0; j < Ri; j++) { double w = std::exp(lw[j] - lmax); sw += w; const float *g = Gi + (size_t)j * D_G_A; for (int t = 0; t < D_G_A; t++) gi[t] += w * g[t]; }
            for (int t = 0; t < D_G_A; t++) gi[t] /= sw;
            lse[i] = lmax + std::log(sw);
        }
    };
    { std::vector<std::thread> pool; for (int t = 0; t < std::max(1, P->n_threads); t++) pool.emplace_back(work); for (auto &th : pool) th.join(); }
    double F = 0.0, d[D_G_A] = {0};
    for (int i = 0; i < n; i++) { F += lse[i]; for (int t = 0; t < D_G_A; t++) d[t] += gt[(size_t)i * D_G_A + t]; }
    if (grad) for (unsigned q = 0; q < nf; q++) grad[q] = d[P->free_idx[q]] / n;
    P->evals++;
    return F / n;
}
static int g_inner_nm = 0;     // CLI inner_algo=lbfgs (default) | neldermead (Schennach's simplex on gamma, App. G)
static int g_inner_dual = 0;   // CLI inner_start=fixed (default: gamma start fixed within a pass) | dual (convex dual first)
// gamma_init=solve (2026-10-01, medians review S3; joint NM only): before pass 1, solve gamma alone at the start theta
// (inner L-BFGS with the analytic gradient, as the nested inner step, up to 4 rounds of 300 evaluations) and start the joint NM there
// instead of gamma = 0. Once per fit at its own start theta: no chaining across points. Default gamma_init=zero.
static int g_gamma_init_solve = 0;
// Solve the inner problem at the cached theta, starting from gam (updated in place). Returns L at the solution.
static double nested_solve_gamma(NestedInner &P, double *gam, int *code) {
    const int nf = (int)P.free_idx.size();
    std::vector<double> xg(nf); for (int q = 0; q < nf; q++) xg[q] = gam[P.free_idx[q]];
    if (g_inner_dual) {   // convex dual first, from the given start; its solution is the start of the CUE step
        nlopt_opt od = nlopt_create(NLOPT_LD_LBFGS, nf);
        nlopt_set_min_objective(od, nested_F, &P);
        nlopt_set_ftol_rel(od, 1e-10); nlopt_set_xtol_rel(od, 1e-8); nlopt_set_maxeval(od, 300);
        double fd = HUGE_VAL; nlopt_optimize(od, xg.data(), &fd); nlopt_destroy(od);
    }
    nlopt_opt o = nlopt_create(g_inner_nm ? NLOPT_LN_NELDERMEAD : NLOPT_LD_LBFGS, nf);
    nlopt_set_min_objective(o, nested_inner_obj, &P);
    if (g_inner_nm) {   // derivative-free: steps 0.2 / D_t, budget 200 x free gamma
        std::vector<double> st(nf); for (int q = 0; q < nf; q++) st[q] = gamma_step(P.free_idx[q]);
        nlopt_set_initial_step(o, st.data()); nlopt_set_xtol_rel(o, 1e-6); nlopt_set_ftol_rel(o, 1e-8);   // ftol: reachable from gamma = 0
        nlopt_set_maxeval(o, 200 * nf);
    } else { nlopt_set_ftol_rel(o, 1e-8); nlopt_set_xtol_rel(o, 1e-6); nlopt_set_maxeval(o, 300); }
    double f = HUGE_VAL; nlopt_result r = nlopt_optimize(o, xg.data(), &f);
    nlopt_destroy(o);
    if (code) *code = (int)r;
    for (int q = 0; q < nf; q++) gam[P.free_idx[q]] = xg[q];
    return f;
}

struct NestedOuter {
    const std::vector<FirmData> *firms; double lambda; int R; uint64_t base_seed; int n_threads;
    int OG; std::vector<int> theta_free; const double *x_full0;   // full x layout, theta entries fixed where not free
    double gam[D_G_A]; NestedCache C; NestedInner P; long outer_evals = 0; long inner_evals_total = 0;
    long inner_maxeval_hits = 0, inner_failures = 0;
    double gam_start[D_G_A];                         // inner start, FIXED within a pass (no warm-start chaining)
    double best_f = HUGE_VAL; std::vector<double> best_xt; double best_gam[D_G_A];
};

static void nested_set_theta(const double *x, double &lam_use) {
#ifdef KINK
    g_kpow = x[3];
#endif
#ifdef KINK_S
    g_kshare = x[4];
#endif
#ifdef KAPPA_FREE
    lam_use = x[5];
#else
    (void)x; (void)lam_use;
#endif
}

static double nested_outer_obj(unsigned nt, const double *xt, double *grad, void *data) {
    (void)grad;
    NestedOuter *O = static_cast<NestedOuter *>(data);
    std::vector<double> x(O->x_full0, O->x_full0 + O->OG);
    for (unsigned q = 0; q < nt; q++) x[O->theta_free[q]] = xt[q];
    double lam_use = O->lambda; nested_set_theta(x.data(), lam_use);
    nested_build_cache(O->C, *O->firms, lam_use, x[0], x[1], x[2], O->R, O->base_seed, O->n_threads);
    O->P.C = &O->C; long e0 = O->P.evals;
    std::copy(O->gam_start, O->gam_start + D_G_A, O->gam);   // same inner start for every theta in the pass
    int icode = 0;
    double f = nested_solve_gamma(O->P, O->gam, &icode);
    if (icode == NLOPT_MAXEVAL_REACHED) O->inner_maxeval_hits++;
    if (icode < 0) O->inner_failures++;
    O->inner_evals_total += O->P.evals - e0; O->outer_evals++;
    if (f < O->best_f) { O->best_f = f; O->best_xt.assign(xt, xt + nt); std::copy(O->gam, O->gam + D_G_A, O->best_gam); }
    return f;
}

// ---- mode=cfprofile (2026-10-02): counterfactual revenue at a fixed theta (AK2020 App. F) ----
// For each Delta: cache the draws at theta (sampler=is), row 10 = credit/scale; profile L(T) = min_gamma L(theta, gamma; T)
// by warm-started L-BFGS (the nested inner solver); T_hat = argmin (golden section); bounds by test inversion:
// hard = {T: 2nL <= chi2_{d_g,0.95}}, soft = {T: 2n(L - L_min) <= chi2_{1,0.95} = 3.841}. Writes one CSV row per Delta.
static double chi2_q95(int d) {   // exact 0.95 quantiles for d = 10..25 (R qchisq); Wilson-Hilferty otherwise (error < 0.05 for d >= 3)
    static const double ex[16] = {18.307038, 19.675138, 21.026070, 22.362032, 23.684791, 24.995790, 26.296228, 27.587112,
                                  28.869299, 30.143527, 31.410433, 32.670573, 33.924438, 35.172462, 36.415029, 37.652484};
    if (d >= 10 && d <= 25) return ex[d - 10];
    const double z = 1.6448536269514722, a = 2.0 / (9.0 * d);
    return d * std::pow(1.0 - a + z * std::sqrt(a), 3.0);
}
static void run_cfprofile_mode(const std::vector<FirmData> &firms, const double *par, int n_keep, uint64_t base_seed,
                               int n_threads, const std::vector<double> &deltas, const std::string &output_csv) {
    const int n = (int)firms.size(), R = n_keep;
    double kap = par[0];
    double gam0[D_G_A]; for (int t = 0; t < D_G_A; t++) gam0[t] = par[4 + N_XFE + t];
    gam0[10] = 0.0;
    double mt1p = 0.0, sc = 0.0;
    for (const FirmData &f : firms) { mt1p += f.t1 / f.pgdp; sc += f.tau_rho * f.Mstar; }
    mt1p /= n; sc /= n; g_cf_scale = sc;
    int dg = 0; std::vector<int> fidx; for (int t = 0; t < D_G_A; t++) if (!(g_dropmask & (1u << t))) { dg++; fidx.push_back(t); }
    const double crit = chi2_q95(dg), crit1 = 3.841458820694124;
    std::cout << "cfprofile: n " << n << ", live rows d_g " << dg << " (row 10 = credit), crit chi2_" << dg << " " << crit
              << " | scale (mean tau_P M*) " << sc << " | mean t1/pgdp " << mt1p << "\n" << std::flush;
    for (double Dl : deltas) if (g_cf_target == 10 && std::fabs(Dl) > 0.05 + 1e-12) {   // review 2026-10-06: E[R(D)] crosses 0 near
        std::cerr << "elast_revenue: only for |Delta| <= 0.05 (net revenue changes sign near Delta = -0.15, so the ratio's set can be "
                     "unbounded); use mrev for the revenue derivative elsewhere\n"; std::exit(1); }
    if (g_cf_target == 12 && !g_cf_mresp) { std::cerr << "diff_input is identically 0 without cf_mresp=1\n"; std::exit(1); }
    if (g_cf_mresp) {   // r needs 1 - (1+D) tau_P > 0 at every Delta used (Delta, and Delta +- h for the elasticity targets)
        double tmax = 0.0; for (const FirmData &f : firms) tmax = std::max(tmax, f.tau_rho);
        for (double Dl : deltas) { const double Dm = Dl + ((g_cf_target == 3 || g_cf_target == 4 || g_cf_target == 10 || g_cf_target == 11) ? g_cf_h : 0.0);
            if (!(1.0 - (1.0 + Dm) * tmax > 0.0)) { std::cerr << "cf_mresp: 1 - (1+Delta) tau_P <= 0 at Delta " << Dm << " (max tau_P "
                                                             << tmax << "): the M response is undefined\n"; std::exit(1); } }
    }
    if (g_cf_decomp) {   // read-only diagnostic (see g_cf_decomp): operating weights only, caches built one at a time (each ~1 GB)
        if (g_cf_target != 13) { std::cerr << "cf_decomp needs cf_target=diff_evasion\n"; std::exit(1); }
        std::ofstream dout(output_csv);
        dout << std::setprecision(10) << "Delta,bin,q_lo,q_hi,weight_share,resp_plus,resp_minus,resp_plus_total,resp_minus_total,maxdiff_lw,scale\n";
        const double kk = g_kfixed;
        std::vector<double> wn((size_t)n * R), lw0((size_t)n * R);   // normalized operating weights and their log kernels (from the q cache)
        std::vector<float> qd((size_t)n * R), rp((size_t)n * R), rm((size_t)n * R);
        // log kernel of draw j of firm i at the operating gamma (gam0[10] = 0, so row 10 -- the only row that differs across caches -- drops out)
        auto lker = [&](const NestedCache &Cc, int i, int j) { const float *g = Cc.G.data() + ((size_t)i * R + j) * D_G_A;
            double a = Cc.lq[(size_t)i * R + j]; for (int t = 0; t < D_G_A; t++) a += gam0[t] * g[t]; return a; };
        for (double Dl : deltas) {
            if (!(Dl > 0.0)) { std::cerr << "cf_decomp: give positive deltas (each runs at +Delta and -Delta)\n"; std::exit(1); }
            double maxd = 0.0;
            {   // pass 1: current q (mean_q's row at Delta = 0) and the operating weights
                g_cf_on = true; g_cf_target = 15; g_cf_Delta = 0.0; g_cf_T = 0.0;
                NestedCache Cc; nested_build_cache(Cc, firms, kap, par[1], par[2], par[3], R, base_seed, n_threads);
                for (int i = 0; i < n; i++) {
                    if (Cc.Ri[i] != R) { std::cerr << "cf_decomp: interior firms only\n"; std::exit(1); }
                    double lmax = -HUGE_VAL;
                    for (int j = 0; j < R; j++) { lw0[(size_t)i * R + j] = lker(Cc, i, j); lmax = std::max(lmax, lw0[(size_t)i * R + j]); }
                    double sw = 0.0; for (int j = 0; j < R; j++) sw += std::exp(lw0[(size_t)i * R + j] - lmax);
                    for (int j = 0; j < R; j++) { wn[(size_t)i * R + j] = std::exp(lw0[(size_t)i * R + j] - lmax) / sw;
                                                  qd[(size_t)i * R + j] = Cc.G[((size_t)i * R + j) * D_G_A + 10]; }
                }
            }
            for (int sgn = +1; sgn >= -1; sgn -= 2) {   // passes 2 and 3: diff_evasion at +Delta and -Delta, same draws (same seed and theta)
                g_cf_on = true; g_cf_target = 13; g_cf_Delta = sgn * Dl; g_cf_T = 0.0;
                NestedCache Cc; nested_build_cache(Cc, firms, kap, par[1], par[2], par[3], R, base_seed, n_threads);
                std::vector<float> &dst = sgn > 0 ? rp : rm;
                for (int i = 0; i < n; i++) for (int j = 0; j < R; j++) {
                    maxd = std::max(maxd, std::fabs(lker(Cc, i, j) - lw0[(size_t)i * R + j]));
                    dst[(size_t)i * R + j] = Cc.G[((size_t)i * R + j) * D_G_A + 10]; }
            }
            g_cf_target = 13;
            const std::vector<double> edges = {0.0, Dl / (1.0 + kk), 0.02, 0.05, 0.15, HUGE_VAL};
            const int NB = (int)edges.size() - 1;
            if (!(edges[1] < edges[2])) { std::cerr << "cf_decomp: Delta/(1+k) must be below 0.02 (bin edges must increase)\n"; std::exit(1); }
            std::vector<double> ws(NB, 0.0), sp(NB, 0.0), sm(NB, 0.0);
            for (size_t ij = 0; ij < (size_t)n * R; ij++) {
                int b = 0; while (b < NB - 1 && qd[ij] >= edges[b + 1]) b++;
                ws[b] += wn[ij]; sp[b] += wn[ij] * rp[ij]; sm[b] += wn[ij] * rm[ij]; }
            double tp = 0.0, tm = 0.0; for (int b = 0; b < NB; b++) { ws[b] /= n; sp[b] /= n; sm[b] /= n; tp += sp[b]; tm += sm[b]; }
            std::cout << "  cf_decomp Delta +-" << Dl << ": E[diff_evasion] at +Delta " << tp << ", at -Delta " << tm << " (units of scale; compare T at operating gamma)"
                      << " | max |log-weight difference| across caches " << maxd << "\n";
            for (int b = 0; b < NB; b++) {
                std::cout << "    q in [" << edges[b] << ", " << edges[b + 1] << "): weight " << ws[b] << " | resp at +Delta " << sp[b] << ", at -Delta " << sm[b]
                          << " | asymmetry (sum) " << sp[b] + sm[b] << "\n";
                dout << Dl << "," << b << "," << edges[b] << "," << (std::isfinite(edges[b + 1]) ? edges[b + 1] : -1.0) << "," << ws[b] << ","
                     << sp[b] << "," << sm[b] << "," << tp << "," << tm << "," << maxd << "," << sc << "\n";
            }
            std::cout << std::flush;
        }
        g_cf_on = false;
        std::cout << "Saved: " << output_csv << "\n";
        return;
    }
    std::ofstream out(output_csv);
    out << std::setprecision(10) << "Delta,T_hat,TS_min,d_g,crit,hard_lo,hard_hi,soft_lo,soft_hi,scale,mean_t1p,credit_hat,revenue_hat,"
           "revenue_hard_lo,revenue_hard_hi,T_at_gamma0,evals,cf_target,cf_multi,TS_min_seen,T_min_seen,g10_hat,maxg_hat,g10_hard_lo,maxg_hard_lo,g10_hard_hi,maxg_hard_hi,cf_mresp\n";
    for (double Dl : deltas) {
        g_cf_on = true; g_cf_Delta = Dl; g_cf_T = 0.0;
        double mt1pD = 0.0;   // mean real sales tax on sales at this Delta (scaled by r^beta when true M responds)
        for (const FirmData &f : firms) mt1pD += f.t1 / f.pgdp * std::pow(cf_mresp_r(Dl, f.tau_rho, f.beta), f.beta);
        mt1pD /= n;
        NestedCache C; nested_build_cache(C, firms, kap, par[1], par[2], par[3], R, base_seed, n_threads);
        std::vector<float> c10((size_t)n * R), b10((size_t)n * R, 1.0f);   // row 10 = a - T b
        for (int i = 0; i < n; i++) for (int j = 0; j < C.Ri[i]; j++) c10[(size_t)i * R + j] = C.G[((size_t)i * R + j) * D_G_A + 10];
        if (g_cf_target == 3 || g_cf_target == 4 || g_cf_target == 6 || g_cf_target == 8 || g_cf_target == 10) {   // ratio targets: b from a second pass at T = 1 (b = a - row(T=1))
            g_cf_T = 1.0; NestedCache C1; nested_build_cache(C1, firms, kap, par[1], par[2], par[3], R, base_seed, n_threads);
            for (int i = 0; i < n; i++) for (int j = 0; j < C.Ri[i]; j++) b10[(size_t)i * R + j] = c10[(size_t)i * R + j] - C1.G[((size_t)i * R + j) * D_G_A + 10];
            g_cf_T = 0.0;
            if (g_cf_target == 6 || g_cf_target == 8)   // gap: P = t1/pgdp - tau_P M (fn returned -tau_P M/scale); loss_t1: t1/pgdp + extra (fn returned 0)
                for (int i = 0; i < n; i++) { const double t1s = (firms[i].t1 / firms[i].pgdp + (g_cf_target == 8 ? g_cf_t1_extra : 0.0)) / sc;
                    for (int j = 0; j < C.Ri[i]; j++) b10[(size_t)i * R + j] += (float)t1s; }
            if (g_cf_target == 10)   // elast_revenue: b = R(D)/scale = [t1 r(D)^beta/pgdp - C(D)]/scale (fn returned -C(D)/scale)
                for (int i = 0; i < n; i++) { const double t1s = firms[i].t1 / firms[i].pgdp / sc
                                                                 * std::pow(cf_mresp_r(Dl, firms[i].tau_rho, firms[i].beta), firms[i].beta);
                    for (int j = 0; j < C.Ri[i]; j++) b10[(size_t)i * R + j] += (float)t1s; }
        }
        if (g_cf_target == 14)   // diff_revenue: add the change in the sales tax on sales, t1 (r(D)^beta - 1)/pgdp/scale
            for (int i = 0; i < n; i++) { const double bi = firms[i].beta;
                const double dt1 = firms[i].t1 / firms[i].pgdp / sc * (std::pow(cf_mresp_r(Dl, firms[i].tau_rho, bi), bi) - 1.0);
                for (int j = 0; j < C.Ri[i]; j++) c10[(size_t)i * R + j] += (float)dt1; }
        if (g_cf_target == 10 || g_cf_target == 11) {   // revenue derivatives: add the t1 part of (1+D)[R(D+h) - R(D-h)]/(2h)/scale to a
            const double hh = g_cf_h, mult = (g_cf_target == 11 ? 0.01 : 1.0) * (1.0 + Dl) / (2.0 * hh);
            for (int i = 0; i < n; i++) { const double bi = firms[i].beta;
                const double dt1 = firms[i].t1 / firms[i].pgdp / sc * (std::pow(cf_mresp_r(Dl + hh, firms[i].tau_rho, bi), bi)
                                                                       - std::pow(cf_mresp_r(Dl - hh, firms[i].tau_rho, bi), bi));
                for (int j = 0; j < C.Ri[i]; j++) c10[(size_t)i * R + j] += (float)(mult * dt1); }
        }
        if (g_cf_target == 9)   // revenue: add the firm's observed t1/pgdp/scale to a (fn returned -C(Delta)/scale); with cf_mresp, t1 r^beta
            for (int i = 0; i < n; i++) { const double t1s = firms[i].t1 / firms[i].pgdp / sc
                                                             * std::pow(cf_mresp_r(Dl, firms[i].tau_rho, firms[i].beta), firms[i].beta);
                for (int j = 0; j < C.Ri[i]; j++) c10[(size_t)i * R + j] += (float)t1s; }
        auto setT = [&](double T) { for (int i = 0; i < n; i++) for (int j = 0; j < C.Ri[i]; j++)
                                        C.G[((size_t)i * R + j) * D_G_A + 10] = (float)(c10[(size_t)i * R + j] - T * b10[(size_t)i * R + j]); };
        NestedInner P; P.C = &C; P.firms = &firms; P.n_threads = n_threads; P.free_idx = fidx;
        // T at the operating gamma (row 10's gamma = 0, so the weights are the fit's): starting value
        double T0 = 0.0, Tb = 0.0;
        for (int i = 0; i < n; i++) {
            const int Ri = C.Ri[i]; const float *Gi = C.G.data() + (size_t)i * R * D_G_A; const float *lqi = C.lq.data() + (size_t)i * R;
            std::vector<double> lw(Ri); double lmax = -HUGE_VAL;
            for (int j = 0; j < Ri; j++) { double a = lqi[j]; for (int t = 0; t < D_G_A; t++) a += gam0[t] * Gi[(size_t)j * D_G_A + t]; lw[j] = a; lmax = std::max(lmax, a); }
            double sw = 0.0, sc10 = 0.0, sb10 = 0.0;
            for (int j = 0; j < Ri; j++) { double w = std::exp(lw[j] - lmax); sw += w; sc10 += w * c10[(size_t)i * R + j]; sb10 += w * b10[(size_t)i * R + j]; }
            T0 += sc10 / sw; Tb += sb10 / sw;
        }
        if (g_cf_target == 3 || g_cf_target == 4 || g_cf_target == 6 || g_cf_target == 8 || g_cf_target == 10)
            std::cout << "  ratio target: E[b] at the operating gamma = " << Tb / n << " (units of b; the set assumes E[b] is bounded away from 0)\n" << std::flush;
        T0 /= Tb;   // E[a] / E[b] (= E[a] for b = 1)
        double gwarm[D_G_A]; std::copy(gam0, gam0 + D_G_A, gwarm);
        const int nm_s = g_inner_nm, du_s = g_inner_dual; g_inner_nm = 0; g_inner_dual = 0;
        long evals = 0;
        // every profiled L is an upper bound on the true profile (a failed solve only raises it), so track the lowest L seen at any T
        double Lseen = HUGE_VAL, Tseen = std::numeric_limits<double>::quiet_NaN(), gseen[D_G_A], glast[D_G_A];
        std::copy(gam0, gam0 + D_G_A, gseen); std::copy(gam0, gam0 + D_G_A, glast);
        // hard set = union of every T accepted at the hard level by ANY profiled solve (review 2026-10-05: the bound searches alone
        // miss T's accepted later by the soft search, re-centring or cf_grid)
        double Tacc_lo = HUGE_VAL, Tacc_hi = -HUGE_VAL, gacc_lo[D_G_A], gacc_hi[D_G_A];
        std::copy(gam0, gam0 + D_G_A, gacc_lo); std::copy(gam0, gam0 + D_G_A, gacc_hi);
        auto solve_from = [&](const double *g0, double *gout) -> double {
            std::copy(g0, g0 + D_G_A, gout); int code = 0;
            double L = nested_solve_gamma(P, gout, &code);
            for (int round = 2; round <= 4 && code == NLOPT_MAXEVAL_REACHED; round++) L = nested_solve_gamma(P, gout, &code);
            evals++;
            return L;
        };
        // start diagnostics (cf_multi): per start s, wins (unique or tied best), and its marginal value = min L over the other starts
        // minus the best L (in TS units 2n dL); local measure, since dropping a start would also change later warm paths
        const int NS = 3 + (int)g_cf_g10.size();   // 0 operating, 1 warm, 2 best-so-far, 3.. gamma10 = cf_g10[s-3]
        std::vector<long> s_win(NS, 0), s_ran(NS, 0); std::vector<double> s_msum(NS, 0.0), s_mmax(NS, 0.0);
        auto Lprof = [&](double T) -> double {
            setT(T); double g[D_G_A], gb[D_G_A];
            double L = solve_from((g_cf_cold || g_cf_multi) && g_cf_op ? gam0 : gwarm, gb);   // slot 0: operating (or warm if cf_op=0)
            if (g_cf_multi) {   // more starts: warm path, best so far, then the operating gamma with the counterfactual tilt gamma[10] moved
                std::vector<double> Ls(NS, HUGE_VAL); Ls[0] = std::isfinite(L) ? L : HUGE_VAL;
                auto take = [&](double Lc, int si) { Ls[si] = std::isfinite(Lc) ? Lc : HUGE_VAL;
                                                     if (std::isfinite(Lc) && (!std::isfinite(L) || Lc < L)) { L = Lc; std::copy(g, g + D_G_A, gb); } };
                if (g_cf_op) take(solve_from(gwarm, g), 1);
                const bool ran2 = std::isfinite(Lseen);
                if (ran2) take(solve_from(gseen, g), 2);   // the best gamma found so far, at any T
                for (size_t ci = 0; ci < g_cf_g10.size(); ci++) { double s0[D_G_A]; std::copy(gam0, gam0 + D_G_A, s0); s0[10] = g_cf_g10[ci]; take(solve_from(s0, g), 3 + (int)ci); }
                double best = HUGE_VAL; for (int si = 0; si < NS; si++) best = std::min(best, Ls[si]);
                if (best < HUGE_VAL) for (int si = 0; si < NS; si++) { if ((si == 2 && !ran2) || (si == 1 && !g_cf_op)) continue; s_ran[si]++;
                    if (Ls[si] <= best) { s_win[si]++; double other = HUGE_VAL; for (int sj = 0; sj < NS; sj++) if (sj != si) other = std::min(other, Ls[sj]);
                        const double m = other < HUGE_VAL ? 2.0 * n * (other - best) : 0.0; s_msum[si] += m; s_mmax[si] = std::max(s_mmax[si], m); } }
            }
            if (std::isfinite(L)) std::copy(gb, gb + D_G_A, gwarm);   // warm start along T (theta fixed)
            std::copy(gb, gb + D_G_A, glast);
            if (std::isfinite(L) && L < Lseen) { Lseen = L; Tseen = T; std::copy(gb, gb + D_G_A, gseen); }
            if (std::isfinite(L) && 2.0 * n * L <= crit) {
                if (T < Tacc_lo) { Tacc_lo = T; std::copy(gb, gb + D_G_A, gacc_lo); }
                if (T > Tacc_hi) { Tacc_hi = T; std::copy(gb, gb + D_G_A, gacc_hi); } }
            return L;
        };
        auto maxabs = [](const double *g) { double m = 0.0; for (int t = 0; t < D_G_A; t++) m = std::max(m, std::fabs(g[t])); return m; };
        // golden section on [T0 - h, T0 + h], widened if the minimum sits at an edge
        double h = std::max(0.02 * std::fabs(T0), 1e-4), lo = T0 - h, hi = T0 + h;
        for (int w = 0; w < 6; w++) {
            const double fl = Lprof(lo), fm = Lprof(0.5 * (lo + hi)), fh = Lprof(hi);
            if (fm <= fl && fm <= fh) break;
            if (fl < fm) { hi = 0.5 * (lo + hi); lo -= 2 * h; } else { lo = 0.5 * (lo + hi); hi += 2 * h; }
            h *= 2;
        }
        const double gr = 0.6180339887498949;
        double a = lo, b = hi, x1 = b - gr * (b - a), x2 = a + gr * (b - a), f1 = Lprof(x1), f2 = Lprof(x2);
        while (b - a > 1e-5 * std::max(1.0, std::fabs(T0))) {
            if (f1 < f2) { b = x2; x2 = x1; f2 = f1; x1 = b - gr * (b - a); f1 = Lprof(x1); }
            else { a = x1; x1 = x2; f1 = f2; x2 = a + gr * (b - a); f2 = Lprof(x2); }
        }
        // T_hat and its gamma: the lowest profiled L seen so far (golden-section end points included); with exact solves this is the
        // golden-section minimum, and in warm mode the old code took gamma from the last evaluation instead (audit 2026-10-05)
        double That = Tseen, TSmin = 2.0 * n * Lseen;
        double gbest[D_G_A]; std::copy(gseen, gseen + D_G_A, gbest);
        // bound search: the T where 2nL crosses a level, outward from T_hat (bisection after bracketing)
        // bound search: returns +-Inf (and says so) if 30 doublings never cross the level; gin = gamma at the last accepted T
        auto bound = [&](double level, int dir, double *gin) -> double {
            std::copy(gbest, gbest + D_G_A, gwarm); std::copy(gbest, gbest + D_G_A, gin);
            if (!std::isfinite(TSmin) || TSmin > level) return std::numeric_limits<double>::quiet_NaN();   // also: every solve failed
            double step = std::max(0.01 * std::fabs(That), 1e-4), inside = That, outside = That; bool crossed = false;
            for (int it = 0; it < 30; it++) { outside = That + dir * step;
                if (2.0 * n * Lprof(outside) > level) { crossed = true; break; }
                inside = outside; std::copy(glast, glast + D_G_A, gin); step *= 2; }
            if (!crossed) { std::cout << "  WARNING: " << (dir < 0 ? "lower" : "upper") << " bound at level " << level
                                      << " not crossed after 30 doublings (last accepted T " << inside << "): set UNBOUNDED\n" << std::flush;
                            return dir * std::numeric_limits<double>::infinity(); }
            for (int it = 0; it < 30; it++) { const double mid = 0.5 * (inside + outside);
                if (2.0 * n * Lprof(mid) > level) outside = mid; else { inside = mid; std::copy(glast, glast + D_G_A, gin); }
                if (std::fabs(outside - inside) < 1e-5 * std::max(1.0, std::fabs(That))) break; }
            return 0.5 * (inside + outside);
        };
        double ghl[D_G_A], ghh[D_G_A], gsl[D_G_A], gsh[D_G_A];
        double hlo = bound(crit, -1, ghl), hhi = bound(crit, +1, ghh);   // hard: TS_min-free (each accepted T is valid evidence)
        double slo = bound(TSmin + crit1, -1, gsl), shi = bound(TSmin + crit1, +1, gsh);
        int rc = 0;
        for (; rc < 10 && 2.0 * n * Lseen < TSmin - 1e-6; rc++) {   // a later search found a lower TS: re-centre T_hat and the soft set there
            std::cout << "  NOTE: a bound search found a lower TS than TS_min: " << 2.0 * n * Lseen << " at T " << Tseen << " (was "
                      << TSmin << " at " << That << "); T_hat and the soft set re-centred\n" << std::flush;
            That = Tseen; TSmin = 2.0 * n * Lseen; std::copy(gseen, gseen + D_G_A, gbest);
            if (std::isnan(hlo)) hlo = bound(crit, -1, ghl);   // the old TS_min was above crit: the hard set may now be non-empty
            if (std::isnan(hhi)) hhi = bound(crit, +1, ghh);
            slo = bound(TSmin + crit1, -1, gsl); shi = bound(TSmin + crit1, +1, gsh);
        }
        if (rc == 10 && 2.0 * n * Lseen < TSmin - 1e-6)
            std::cout << "  WARNING: re-centring stopped after 10 rounds; TS_min " << TSmin << " vs lowest seen " << 2.0 * n * Lseen << "\n" << std::flush;
        for (double Tg : g_cf_grid) { std::copy(gbest, gbest + D_G_A, gwarm);
            std::cout << "  profile: T " << Tg << " TS " << 2.0 * n * Lprof(Tg) << "\n" << std::flush; }
        if (2.0 * n * Lseen < TSmin - 1e-6)
            std::cout << "  NOTE: cf_grid found a lower TS (" << 2.0 * n * Lseen << " at T " << Tseen << "); soft set NOT re-centred\n" << std::flush;
        // hard set: extend each bisected bound to the outermost T accepted by any solve (and fill an empty side if anything was accepted)
        if (Tacc_lo <= Tacc_hi) {
            if (std::isnan(hlo) || Tacc_lo < hlo) { if (!std::isnan(hlo)) std::cout << "  NOTE: hard lower bound extended from " << hlo << " to accepted T " << Tacc_lo << "\n";
                                                     hlo = Tacc_lo; std::copy(gacc_lo, gacc_lo + D_G_A, ghl); }
            if (std::isnan(hhi) || Tacc_hi > hhi) { if (!std::isnan(hhi)) std::cout << "  NOTE: hard upper bound extended from " << hhi << " to accepted T " << Tacc_hi << "\n";
                                                     hhi = Tacc_hi; std::copy(gacc_hi, gacc_hi + D_G_A, ghh); }
        }
        g_inner_nm = nm_s; g_inner_dual = du_s;
        if (g_cf_multi) {
            std::cout << "  starts (wins/ran, mean and max marginal TS when it wins):";
            for (int si = 0; si < NS; si++) {
                const std::string nm = si == 0 ? "operating" : si == 1 ? "warm" : si == 2 ? "best-so-far" : "g10=" + std::to_string((int)g_cf_g10[si - 3]);
                std::cout << " | " << nm << " " << s_win[si] << "/" << s_ran[si] << " " << (s_win[si] ? s_msum[si] / s_win[si] : 0.0) << " " << s_mmax[si];
            }
            std::cout << "\n" << std::flush;
        }
        static const char *tnm[16] = {"level", "diff_beh", "diff_total", "elast_x", "elast_claims", "overrep", "gap", "true_credit", "loss_t1", "revenue",
                                      "elast_revenue", "mrev", "diff_input", "diff_evasion", "diff_revenue", "mean_q"};
        const bool lev = g_cf_target == 0;   // credit_hat and revenue_* are claims-derived: meaningful for cf_target=level only (NA otherwise)
        std::cout << "  Delta " << Dl << ": T_hat " << That << " (T at operating gamma " << T0 << "), TS_min " << TSmin
                  << " | hard [" << hlo << ", " << hhi << "] | soft [" << slo << ", " << shi << "] | T_hat*scale " << That * sc
                  << " | gamma10 " << gbest[10] << " max|gamma| " << maxabs(gbest) << " (hard lo " << ghl[10] << "/" << maxabs(ghl)
                  << ", hard hi " << ghh[10] << "/" << maxabs(ghh) << ") | " << evals << " profiled solves\n" << std::flush;
        auto gstr = [](double bnd, double v) { std::ostringstream o; o << std::setprecision(10); if (std::isnan(bnd)) o << "NA"; else o << v; return o.str(); };
        auto NA = [&](double v) { std::ostringstream o; o << std::setprecision(10); if (lev) o << v; else o << "NA"; return o.str(); };
        out << Dl << "," << That << "," << TSmin << "," << dg << "," << crit << "," << hlo << "," << hhi << "," << slo << "," << shi << ","
            << sc << "," << mt1pD << "," << NA(That * sc) << "," << NA(mt1pD - That * sc) << "," << NA(mt1pD - hhi * sc) << "," << NA(mt1pD - hlo * sc) << ","
            << T0 << "," << evals << "," << tnm[g_cf_target] << "," << (g_cf_multi ? 1 : 0) << "," << 2.0 * n * Lseen << "," << Tseen << ","
            << gbest[10] << "," << maxabs(gbest) << "," << gstr(hlo, ghl[10]) << "," << gstr(hlo, maxabs(ghl)) << ","
            << gstr(hhi, ghh[10]) << "," << gstr(hhi, maxabs(ghh)) << "," << (g_cf_mresp ? 1 : 0) << "\n" << std::flush;
    }
    g_cf_on = false;
    std::cout << "Saved: " << output_csv << "\n";
}

static FitResultAFixedLambda fit_one_grid_point_A_fixedLambda(
    const std::vector<FirmData> &firms, double lambda,
    int n_burn, int n_keep, uint64_t base_seed, int n_threads, double maxtime,
    const double *x0_in, nlopt_algorithm algo = NLOPT_LN_NELDERMEAD
) {
    const int n_par = 3 + N_XFE + D_G_A;   // delta0, delta1, delta2, [year intercepts], gamma[1:D_G_A]
    const int OG = 3 + N_XFE;               // offset of gamma in x
    InnerParamsAFixedLambda params{&firms, lambda, n_burn, n_keep, n_threads, base_seed};

    double lower[n_par], upper[n_par], x[n_par];
    for (int t = 0; t < OG; t++) { lower[t] = -DELTA_BOUND; upper[t] = DELTA_BOUND; }
    for (int t = 0; t < 3; t++) if (std::isfinite(g_dfix[t])) { lower[t] = upper[t] = g_dfix[t]; }   // pinned deltas
#ifdef KINK
    lower[3] = 0.02; upper[3] = g_kmax;   // the power k
#endif
#ifdef KINK_S
    lower[3] = upper[3] = g_kfixed;       // k fixed (equal bounds)
    if (g_kfree) { lower[3] = g_kmin; upper[3] = g_kmax; }   // k estimated (k_free=1)
    lower[4] = 0.02; upper[4] = 0.6;      // the share s beyond the kink
    if (g_sfixed > 0 && g_sfixed < 1) lower[4] = upper[4] = g_sfixed;   // s held fixed (s_fixed)
#endif
#ifdef KAPPA_FREE
    lower[5] = 0.02; upper[5] = g_kappa_max;   // the scale kappa (CLI kappa_max, default 5)
#endif
    for (int t = 0; t < D_G_A; t++) { lower[OG + t] = -HUGE_VAL; upper[OG + t] = HUGE_VAL; }
    for (int t = 0; t < n_par; t++) x[t] = x0_in[t];
    for (int t = 0; t < 3; t++) if (std::isfinite(g_dfix[t])) x[t] = g_dfix[t];
#ifdef KINK_S
    if (!g_kfree) x[3] = g_kfixed;                              // pinned k written into the start (audit 7.8)
    if (g_sfixed > 0 && g_sfixed < 1) x[4] = g_sfixed;          // pinned s likewise
    // gamma on rows zeroed by drop_rows has no effect on the objective: pin it at 0 (equal bounds) so the optimizer
    // does not spend evaluations on flat directions (2026-09-30).
    for (int t = 0; t < D_G_A; t++) if (g_dropmask & (1u << t)) { lower[OG + t] = upper[OG + t] = 0.0; x[OG + t] = 0.0; }
#endif
#ifdef KAPPA_FREE
    x[5] = lambda;                        // start kappa at the grid value
    if (std::isfinite(g_kappa_fix)) { lower[5] = upper[5] = g_kappa_fix; x[5] = g_kappa_fix; }   // kappa_fixed
#endif

    if (g_nested) {   // outer NM over free theta, inner L-BFGS over gamma (analytic gradient); see NestedOuter
        auto t0 = std::chrono::steady_clock::now();
        NestedOuter O; O.firms = &firms; O.lambda = lambda; O.R = n_keep; O.base_seed = base_seed; O.n_threads = n_threads;
        O.OG = OG; O.x_full0 = x;
        for (int t = 0; t < OG; t++) if (lower[t] != upper[t]) O.theta_free.push_back(t);
        for (int t = 0; t < D_G_A; t++) { O.gam[t] = x[OG + t]; O.gam_start[t] = x[OG + t]; }
        O.P.firms = &firms; O.P.n_threads = n_threads;
        for (int t = 0; t < D_G_A; t++) if (lower[OG + t] != upper[OG + t]) O.P.free_idx.push_back(t);
        const int nt = (int)O.theta_free.size();
        std::vector<double> xt(nt), lb(nt), ub(nt), st(nt);
        for (int q = 0; q < nt; q++) { int t = O.theta_free[q]; xt[q] = x[t]; lb[q] = lower[t]; ub[q] = upper[t];
            st[q] = (t <= 2) ? 0.5 : (t == 3 ? 0.1 : (t == 4 ? 0.05 : 0.1)); }
        const int mev = g_maxeval > 0 ? g_maxeval : 200 * nt;
        auto outer_pass = [&](int *code, int *nev) -> double {
            nlopt_opt o = nlopt_create(NLOPT_LN_NELDERMEAD, nt);
            nlopt_set_lower_bounds(o, lb.data()); nlopt_set_upper_bounds(o, ub.data());
            nlopt_set_min_objective(o, nested_outer_obj, &O);
            if (g_init_step_auto) nlopt_set_initial_step(o, st.data());
            nlopt_set_xtol_rel(o, 1e-4); nlopt_set_maxeval(o, mev); nlopt_set_maxtime(o, maxtime);
            double f = HUGE_VAL; nlopt_result r = nlopt_optimize(o, xt.data(), &f);
            *nev = nlopt_get_numevals(o); nlopt_destroy(o); *code = (int)r;
            if (r == NLOPT_MAXEVAL_REACHED || r == NLOPT_MAXTIME_REACHED || r < 0)
                std::cout << "    WARNING: nested outer pass NOT converged (NLopt code " << (int)r << ", " << *nev << " evals)\n" << std::flush;
            return f;
        };
        FitResultAFixedLambda R{};
        O.best_xt = xt;
        int c1 = 0, n1 = 0; outer_pass(&c1, &n1);
        double f1 = O.best_f; int c = c1, nv = n1;
        const std::vector<double> p1_xt = O.best_xt; double p1_gam[D_G_A]; std::copy(O.best_gam, O.best_gam + D_G_A, p1_gam);
        for (int pass = 2; pass <= g_npasses; pass++) {   // restart NM from the best (theta, gamma) of the previous pass
            double prev = O.best_f;
            xt = O.best_xt; std::copy(O.best_gam, O.best_gam + D_G_A, O.gam_start);
            outer_pass(&c, &nv);
            std::cout << "    nested pass " << pass << ": best Lhat " << prev << " -> " << O.best_f << " (" << nv << " outer evals)\n" << std::flush;
        }
        // report the best (theta, gamma) evaluated -- no re-solve
        std::vector<double> xf(x, x + OG); for (int q = 0; q < nt; q++) xf[O.theta_free[q]] = O.best_xt[q];
        R.delta0 = xf[0]; R.delta1 = xf[1]; R.delta2 = xf[2];
        for (int k = 0; k < N_XFE; k++) R.d0yr[k] = xf[3 + k];
        for (int t = 0; t < D_G_A; t++) R.gamma[t] = O.best_gam[t];
        const double fin = O.best_f;
        R.Lhat = fin; R.convergence = c; R.iters = nv; R.Lhat_pass1 = f1; R.convergence_pass1 = c1; R.iters_pass1 = n1;
        {   double sq = 0.0; for (int q = 0; q < nt; q++) sq += (O.best_xt[q] - p1_xt[q]) * (O.best_xt[q] - p1_xt[q]);
            for (int t = 0; t < D_G_A; t++) sq += (O.best_gam[t] - p1_gam[t]) * (O.best_gam[t] - p1_gam[t]);
            R.wander = std::sqrt(sq); }   // distance from pass 1's best (theta, gamma) to the final best
        R.point_seconds = std::chrono::duration<double>(std::chrono::steady_clock::now() - t0).count();
        R.inner_cap = O.inner_maxeval_hits; R.inner_fail = O.inner_failures;
        std::cout << "    nested: " << O.outer_evals << " outer evaluations, " << O.inner_evals_total << " inner (gamma) evaluations; inner solves at the eval cap "
                  << O.inner_maxeval_hits << ", failed " << O.inner_failures << "\n" << std::flush;
        return R;
    }

    nlopt_algorithm cur_algo = algo;
    int n_free = 0; for (int t = 0; t < n_par; t++) if (lower[t] != upper[t]) n_free++;
    const int maxeval = g_maxeval > 0 ? g_maxeval : 200 * n_free;
    double step[n_par];
    for (int t = 0; t < n_par; t++) step[t] = 0.1;
    step[0] = step[1] = step[2] = 0.5;
#ifdef KINK_S
    step[3] = 0.1; step[4] = 0.05;
#endif
#ifdef KAPPA_FREE
    step[5] = 0.1;
#endif
    for (int t = 0; t < D_G_A; t++) step[OG + t] = gamma_step(t);
    FDWrap fdw{inner_obj_A_fixedLambda, &params, lower, upper};
    auto run_opt = [&](double *xstart) -> FitResultAFixedLambda {
        nlopt_opt opt = nlopt_create(cur_algo, n_par);
        nlopt_set_lower_bounds(opt, lower);
        nlopt_set_upper_bounds(opt, upper);
        if (cur_algo == NLOPT_LD_LBFGS) nlopt_set_min_objective(opt, fd_objective, &fdw);
        else nlopt_set_min_objective(opt, inner_obj_A_fixedLambda, &params);
        if (g_init_step_auto && cur_algo != NLOPT_LD_LBFGS) nlopt_set_initial_step(opt, step);
        nlopt_set_xtol_rel(opt, 1e-4);
        nlopt_set_maxeval(opt, maxeval);
        nlopt_set_maxtime(opt, maxtime);
        double minf = HUGE_VAL;
        nlopt_result res = nlopt_optimize(opt, xstart, &minf);
        int iters = nlopt_get_numevals(opt);
        nlopt_destroy(opt);
        FitResultAFixedLambda r;
        r.delta0 = xstart[0]; r.delta1 = xstart[1]; r.delta2 = xstart[2];
        for (int k = 0; k < N_XFE; k++) r.d0yr[k] = xstart[3 + k];
        for (int t = 0; t < D_G_A; t++) r.gamma[t] = xstart[OG + t];
        r.Lhat = minf; r.convergence = static_cast<int>(res); r.iters = iters;
        if (res == NLOPT_MAXEVAL_REACHED || res == NLOPT_MAXTIME_REACHED || res < 0)
            std::cout << "    WARNING: optimizer pass NOT converged (NLopt code " << static_cast<int>(res) << ", " << iters << " evals)\n" << std::flush;
        return r;
    };

    auto t_start = std::chrono::steady_clock::now();
    double ws_L0 = std::numeric_limits<double>::quiet_NaN(), ws_L1 = ws_L0;
    if (g_gamma_init_solve) {   // warm start for gamma at the start theta (see g_gamma_init_solve)
        double lam_use = lambda; nested_set_theta(x, lam_use);
        NestedCache C; nested_build_cache(C, firms, lam_use, x[0], x[1], x[2], n_keep, base_seed, n_threads);
        NestedInner P; P.C = &C; P.firms = &firms; P.n_threads = n_threads;
        for (int t = 0; t < D_G_A; t++) if (lower[OG + t] != upper[OG + t]) P.free_idx.push_back(t);
        double gam[D_G_A]; for (int t = 0; t < D_G_A; t++) gam[t] = x[OG + t];
        const double f0 = nested_L(P, gam, nullptr);
        const int nm_save = g_inner_nm, du_save = g_inner_dual; g_inner_nm = 0; g_inner_dual = 0;
        int code = 0; double f1 = nested_solve_gamma(P, gam, &code);
        for (int round = 2; round <= 4 && code == NLOPT_MAXEVAL_REACHED; round++) f1 = nested_solve_gamma(P, gam, &code);   // up to 4 x 300 evals
        g_inner_nm = nm_save; g_inner_dual = du_save;
        double gmax = 0.0; for (int t = 0; t < D_G_A; t++) gmax = std::max(gmax, std::fabs(gam[t]));
        std::cout << "    gamma_init=solve: L at start gamma " << f0 << " -> " << f1 << " (L-BFGS code " << code << ", "
                  << P.evals << " evals, max|gamma| " << gmax << ")\n" << std::flush;
        ws_L0 = f0; ws_L1 = f1;
        if (std::isfinite(f1) && f1 < f0) for (int t = 0; t < D_G_A; t++) x[OG + t] = gam[t];
        else std::cout << "    gamma_init=solve: no improvement, keeping the given gamma start\n";
    }
    FitResultAFixedLambda r1 = run_opt(x);
    double x2[n_par];
    x2[0] = r1.delta0; x2[1] = r1.delta1; x2[2] = r1.delta2;
    for (int k = 0; k < N_XFE; k++) x2[3 + k] = r1.d0yr[k];
    for (int t = 0; t < D_G_A; t++) x2[OG + t] = r1.gamma[t];
    if (g_sa_time > 0) {
        // Simulated annealing from the Nelder-Mead pass-1 endpoint (Nail: NM first, then a global-style search, then
        // polish). Objective is deterministic given base_seed (common random numbers). Full-vector Gaussian proposals,
        // per-coordinate scales s_i = 0.05*max(|x_i|, 0.05), one global multiplier adapted every 50 steps toward ~30%
        // acceptance; temperature T = T0 * (1e-3)^(elapsed/sa_time), T0 = 0.1 * f(start); delta box respected by
        // reflection; the best point found feeds Nelder-Mead pass 2.
        std::mt19937_64 rng(base_seed ^ 0x5A5A5A5AULL);
        std::normal_distribution<double> nz(0.0, 1.0);
        std::uniform_real_distribution<double> un(0.0, 1.0);
        double xc[n_par], xp[n_par], xb[n_par], sc[n_par];
        for (int t = 0; t < n_par; t++) { xc[t] = xb[t] = x2[t]; sc[t] = 0.05 * std::max(std::fabs(x2[t]), 0.05); }
        double fc = inner_obj_A_fixedLambda(n_par, xc, nullptr, &params), fb = fc, T0 = 0.1 * fc, mult = 1.0;
        int steps = 0, acc_win = 0, acc_tot = 0;
        auto sa0 = std::chrono::steady_clock::now();
        for (;;) {
            double el = std::chrono::duration<double>(std::chrono::steady_clock::now() - sa0).count();
            if (el >= g_sa_time) break;
            double T = T0 * std::pow(1e-3, el / g_sa_time);
            for (int t = 0; t < n_par; t++) {
                double v = xc[t] + mult * sc[t] * nz(rng);
                if (v < lower[t]) v = 2 * lower[t] - v;
                if (v > upper[t]) v = 2 * upper[t] - v;
                xp[t] = std::min(std::max(v, lower[t]), upper[t]);
            }
            double fp = inner_obj_A_fixedLambda(n_par, xp, nullptr, &params);
            steps++;
            if (std::isfinite(fp) && (fp <= fc || un(rng) < std::exp(-(fp - fc) / T))) {
                std::copy(xp, xp + n_par, xc); fc = fp; acc_win++; acc_tot++;
                if (fc < fb) { fb = fc; std::copy(xc, xc + n_par, xb); }
            }
            if (steps % 50 == 0) {
                double rate = acc_win / 50.0; acc_win = 0;
                mult *= (rate > 0.3) ? 1.3 : 0.77;
                mult = std::min(std::max(mult, 1e-4), 10.0);
                std::cout << "    SA step " << steps << " t=" << el << "s T=" << T << " f_cur=" << fc << " f_best=" << fb
                          << " acc=" << rate << " mult=" << mult << "\n" << std::flush;
            }
        }
        std::cout << "    SA done: " << steps << " steps, acceptance " << (steps ? double(acc_tot) / steps : 0.0)
                  << ", f: NM pass1 " << r1.Lhat << " -> SA best " << fb << "\n" << std::flush;
        std::copy(xb, xb + n_par, x2);
    }
    if (g_algo2 >= 0) cur_algo = static_cast<nlopt_algorithm>(g_algo2);
    FitResultAFixedLambda r2 = run_opt(x2);
    for (int pass = 3; pass <= g_npasses; pass++) {
        double x3[n_par];
        std::copy(x2, x2 + n_par, x3);   // keeps fixed entries (e.g. k) as set
        x3[0] = r2.delta0; x3[1] = r2.delta1; x3[2] = r2.delta2;
        for (int k = 0; k < N_XFE; k++) x3[3 + k] = r2.d0yr[k];
        for (int t = 0; t < D_G_A; t++) x3[OG + t] = r2.gamma[t];
        double prev = r2.Lhat;
        FitResultAFixedLambda r3 = run_opt(x3);
        std::cout << "    pass " << pass << ": Lhat " << prev << " -> " << r3.Lhat << " (" << r3.iters << " evals)\n" << std::flush;
        r2 = r3;
    }
    r2.point_seconds = std::chrono::duration<double>(std::chrono::steady_clock::now() - t_start).count();
    r2.ws_L0 = ws_L0; r2.ws_L1 = ws_L1;

    r2.Lhat_pass1 = r1.Lhat;
    r2.iters_pass1 = r1.iters;
    r2.convergence_pass1 = r1.convergence;
    double sq = (r2.delta0 - r1.delta0) * (r2.delta0 - r1.delta0)
              + (r2.delta1 - r1.delta1) * (r2.delta1 - r1.delta1)
              + (r2.delta2 - r1.delta2) * (r2.delta2 - r1.delta2);
    for (int t = 0; t < D_G_A; t++) sq += (r2.gamma[t] - r1.gamma[t]) * (r2.gamma[t] - r1.gamma[t]);
    r2.wander = std::sqrt(sq);
    return r2;
}

// Two-level work-stealing (2026-09-10): an OUTER atomic counter over grid
// POINTS, claimed by n_groups concurrent std::thread "group leaders" --
// whichever group finishes its current point first immediately grabs the
// next unclaimed one, so a shard sitting on a hard point never leaves other
// capacity idle the way the old static `idx % n_shards` assignment did
// (confirmed happening in practice on this exact mode, 2026-09-10: two
// shards finished both their points while two others were still grinding
// through their first). Each group's own point-fit then uses the EXISTING,
// unmodified firm-level work-stealing inside compute_dvec_omega_A (via
// fit_one_grid_point_A_fixedLambda's own n_threads argument) -- no nested
// thread-pool machinery, just calling the same single-point function with
// however many threads that group owns.
//
// Group count/size falls out of one formula, deliberately not special-cased
// for n_points==1: n_groups=min(n_points,n_threads_total) (never more
// groups than points -- an idle group with nothing to claim is pure waste
// -- and never more than the thread budget), threads distributed evenly
// across groups with any remainder going to the first few groups so the
// full thread budget is always used exactly. At n_points=1 this collapses
// to n_groups=1, threads_per_group=n_threads_total -- i.e. plain firm-level
// work-stealing with the whole machine on that one point, the single-point
// case asked for, with no separate code path.
static void run_lambdagrid_mode(
    const std::vector<FirmData> &firms, const std::vector<double> &lambdas,
    const double *x0,
    int n_burn, int n_keep, uint64_t base_seed, int n_threads_total, double maxtime,
    const std::string &output_csv,
    nlopt_algorithm algo = NLOPT_LN_NELDERMEAD
) {
    int n_points = static_cast<int>(lambdas.size());
    int n_groups = std::max(1, std::min(n_points, n_threads_total));
    int base_tpg = n_threads_total / n_groups;
    int remainder = n_threads_total - base_tpg * n_groups;

    std::cout << "lambdagrid: " << n_points << " points, " << n_groups
              << " concurrent point-groups (point-level work-stealing), "
              << base_tpg << "-" << (base_tpg + (remainder > 0 ? 1 : 0))
              << " threads/group (firm-level work-stealing within each), "
              << n_threads_total << " threads total\n";

    std::vector<std::pair<double, FitResultAFixedLambda>> results(n_points);
    std::atomic<int> next_point_idx{0};
    std::atomic<int> done_count{0};
    std::mutex print_mutex;
    auto t0 = std::chrono::steady_clock::now();

    auto group_worker = [&](int tpg) {
        int idx;
        while ((idx = next_point_idx.fetch_add(1, std::memory_order_relaxed)) < n_points) {
            double lam = lambdas[idx];
            FitResultAFixedLambda fit = fit_one_grid_point_A_fixedLambda(
                firms, lam, n_burn, n_keep, base_seed, tpg, maxtime, x0, algo);
            results[idx] = {lam, fit};   // each idx written by exactly one group -- no data race
            int done = ++done_count;
            auto elapsed = std::chrono::duration<double>(std::chrono::steady_clock::now() - t0).count();
            std::lock_guard<std::mutex> lock(print_mutex);
            std::cout << "  [" << done << "/" << n_points << "] elapsed=" << elapsed << "s"
                      << "  lambda=" << lam << " d1_hat=" << fit.delta1 << " d2_hat=" << fit.delta2
                      << " Lhat=" << fit.Lhat << " (pass1=" << fit.Lhat_pass1 << ")"
                      << " conv=" << fit.convergence << " iters=" << fit.iters
                      << " point_seconds=" << fit.point_seconds << " (tpg=" << tpg << ")\n" << std::flush;
        }
    };

    std::vector<std::thread> pool;
    for (int g = 0; g < n_groups; g++) {
        int tpg = base_tpg + (g < remainder ? 1 : 0);
        pool.emplace_back(group_worker, tpg);
    }
    for (auto &th : pool) th.join();

    std::ofstream out(output_csv);
    if (!out.is_open()) { std::cerr << "ERROR: could not open output_csv for writing: " << output_csv << "\n"; std::exit(1); }
    out << std::setprecision(15);
    out << "lambda,delta0_hat,delta1_hat,delta2_hat,";
#if defined(KAPPA_FREE)
    out << "k_hat,s_hat,kappa_hat,";
#elif defined(KINK_S)
    out << "k_hat,s_hat,";
#elif defined(KINK)
    out << "k_hat,";
#else
    for (int k = 0; k < N_XFE; k++) out << "d0yr" << (82 + k) << ",";   // YEAR_FE build only
#endif
    for (int t = 0; t < D_G_A; t++) out << "gamma" << (t + 1) << ",";   // D_G_A-sized (9, 10 TAU_ROW, 20 YEAR_FE)
    out << "Lhat,Lhat_pass1,wander,convergence,convergence_pass1,iters,iters_pass1,point_seconds,n,inner_cap,inner_fail,ws_L0,ws_L1\n";
    for (auto &r : results) {
        out << r.first << ","
            << r.second.delta0 << "," << r.second.delta1 << "," << r.second.delta2 << ",";
        for (int k = 0; k < N_XFE; k++) out << r.second.d0yr[k] << ",";
        for (int t = 0; t < D_G_A; t++) out << r.second.gamma[t] << ",";
        out << r.second.Lhat << "," << r.second.Lhat_pass1 << "," << r.second.wander << ","
            << r.second.convergence << "," << r.second.convergence_pass1 << ","
            << r.second.iters << "," << r.second.iters_pass1 << "," << r.second.point_seconds << "," << firms.size() << ","
            << r.second.inner_cap << "," << r.second.inner_fail << "," << r.second.ws_L0 << "," << r.second.ws_L1 << "\n";
    }
    out.close();
    std::cout << "Saved: " << output_csv << " (" << results.size() << " points)\n";
}

// ============================================================================
// ---- Post-hoc acceptance-rate diagnostic (2026-09-08) ---------------------
// ============================================================================
// Given an ALREADY-FITTED (theta,gamma) -- no optimization here -- run each
// interior firm's Metropolis independence sampler once and count how often
// it actually accepts a proposal. Low acceptance = the chain barely moves =
// effective sample size << n_keep regardless of how large n_keep is set
// (general MCMC diagnostic, not ELVIS-specific -- see Geyer 1992, Flegal-
// Haran-Jones 2008 on MCSE-based stopping rules; acceptance rate is the
// standard cheap proxy for an INDEPENDENCE sampler specifically, since poor
// acceptance there means the proposal density rho is a poor match to the
// tilted target, not a step-size tuning issue like a random-walk sampler).
// Corner firms skipped entirely (no MCMC, M fixed at M*).
static void firm_accept_rate_A(
    const FirmData &f, double lambda, double delta0, double delta1, double delta2,
    double eta, const double gamma[D_G_A], int n_burn, int n_keep, uint64_t base_seed,
    int &n_accept, int &n_total
) {
    n_accept = 0; n_total = 0;
    if (f.corner == 1) return;

    std::mt19937_64 rng(firm_seed(base_seed, f.row_id));
    std::uniform_real_distribution<double> unif(0.0, 1.0);

    GVecA g_current, g_try;
    double M_current = draw_from_rho_eta(unif(rng), f.Mstar, lambda, eta);
    moment_g_A_one(M_current, f.Mstar, f.V, f.Wt, f.tau_rho, f.beta, lambda, delta0, delta1, delta2, g_current);

    for (int r = -n_burn + 1; r <= n_keep; r++) {
        double M_try = draw_from_rho_eta(unif(rng), f.Mstar, lambda, eta);
        moment_g_A_one(M_try, f.Mstar, f.V, f.Wt, f.tau_rho, f.beta, lambda, delta0, delta1, delta2, g_try);

        double log_ratio = 0.0;
        for (int t = 0; t < D_G_A; t++) log_ratio += gamma[t] * (g_try[t] - g_current[t]);

        n_total++;
        if (std::log(unif(rng)) < log_ratio) { g_current = g_try; n_accept++; }
    }
}

static double quantile_sorted(const std::vector<double> &sorted_v, double q) {
    if (sorted_v.empty()) return std::numeric_limits<double>::quiet_NaN();
    double pos = q * (sorted_v.size() - 1);
    size_t lo = (size_t)std::floor(pos), hi = (size_t)std::ceil(pos);
    if (lo == hi) return sorted_v[lo];
    return sorted_v[lo] + (pos - lo) * (sorted_v[hi] - sorted_v[lo]);
}

// par packing: (delta0, eta, lambda, delta1, delta2, gamma[1:9]) -- 14 values.
static void run_acceptdiag_mode(
    const std::vector<FirmData> &firms, const double par[14],
    int n_burn, int n_keep, uint64_t base_seed, const std::string &output_csv
) {
    double delta0 = par[0], eta = par[1], lambda = par[2], delta1 = par[3], delta2 = par[4];
    double gamma[D_G_A];
    for (int t = 0; t < D_G_A; t++) gamma[t] = par[5 + t];

    std::vector<double> rates;
    rates.reserve(firms.size());
    long total_accept = 0, total_steps = 0;
    for (const auto &f : firms) {
        int na, nt;
        firm_accept_rate_A(f, lambda, delta0, delta1, delta2, eta, gamma, n_burn, n_keep, base_seed, na, nt);
        if (nt > 0) { rates.push_back((double)na / nt); total_accept += na; total_steps += nt; }
    }
    std::sort(rates.begin(), rates.end());
    double mean_rate = (double)total_accept / total_steps;
    double var_rate = 0.0;
    for (double r : rates) var_rate += (r - mean_rate) * (r - mean_rate);
    var_rate /= (double)(rates.size() - 1);   // sample variance across firms
    std::cout << "Acceptance-rate diagnostic: n_interior=" << rates.size()
              << " pooled_mean=" << mean_rate
              << " median=" << quantile_sorted(rates, 0.50)
              << " variance=" << var_rate
              << " sd=" << std::sqrt(var_rate)
              << " p10=" << quantile_sorted(rates, 0.10)
              << " p25=" << quantile_sorted(rates, 0.25)
              << " p75=" << quantile_sorted(rates, 0.75)
              << " p90=" << quantile_sorted(rates, 0.90) << "\n";

    if (!output_csv.empty()) {
        std::ofstream out(output_csv);
        out << std::setprecision(10) << "firm_idx,accept_rate\n";
        for (size_t i = 0; i < rates.size(); i++) out << i << "," << rates[i] << "\n";
        out.close();
        std::cout << "Saved per-firm rates: " << output_csv << "\n";
    }
}

// ============================================================================
// ---- draw_from_rho_checked redraw-rate diagnostic (2026-09-10) ------------
// ============================================================================
// Empirical answer to "how often does the recursive redraw in
// draw_from_rho_checked actually fire" -- runs the SAME per-firm MCMC chain
// as firm_chain_A (fixed, already-fitted theta/gamma, no optimization) but
// counts every redraw via draw_from_rho_checked_counted instead of the
// production (uncounted) version. Corner firms skipped (no MCMC there).
// par packing (no eta, 2026-09-10 convention): delta0,lambda,delta1,delta2,
// gamma[1:9] -- 13 values, matches revenue_baseline/deltagrid/grid3d.
static void firm_redraw_count_A(
    const FirmData &f, double lambda, double delta0, double delta1, double delta2,
    const double gamma[D_G_A], int n_burn, int n_keep, uint64_t base_seed,
    uint64_t &n_redraws, uint64_t &n_draws
) {
    n_redraws = 0; n_draws = 0;
    if (f.corner == 1) return;

    std::mt19937_64 rng(firm_seed(base_seed, f.row_id));
    std::uniform_real_distribution<double> unif(0.0, 1.0);

    GVecA g_current, g_try;
    double M_current = draw_from_rho_checked_counted(rng, f.Mstar, lambda, n_redraws);
    n_draws++;
    moment_g_A_one(M_current, f.Mstar, f.V, f.Wt, f.tau_rho, f.beta, lambda, delta0, delta1, delta2, g_current);

    for (int r = -n_burn + 1; r <= n_keep; r++) {
        double M_try = draw_from_rho_checked_counted(rng, f.Mstar, lambda, n_redraws);
        n_draws++;
        moment_g_A_one(M_try, f.Mstar, f.V, f.Wt, f.tau_rho, f.beta, lambda, delta0, delta1, delta2, g_try);

        double log_ratio = 0.0;
        for (int t = 0; t < D_G_A; t++) log_ratio += gamma[t] * (g_try[t] - g_current[t]);
        if (std::log(unif(rng)) < log_ratio) g_current = g_try;
    }
}

static void run_redrawdiag_mode(
    const std::vector<FirmData> &firms, const double par[13],
    int n_burn, int n_keep, uint64_t base_seed, int n_threads, const std::string &output_csv
) {
    double delta0 = par[0], lambda = par[1], delta1 = par[2], delta2 = par[3];
    double gamma[D_G_A];
    for (int t = 0; t < D_G_A; t++) gamma[t] = par[4 + t];

    int n = static_cast<int>(firms.size());
    std::vector<uint64_t> redraws(n, 0), draws(n, 0);

    std::atomic<int> next_idx{0};
    auto worker = [&]() {
        int i;
        while ((i = next_idx.fetch_add(1, std::memory_order_relaxed)) < n) {
            firm_redraw_count_A(firms[i], lambda, delta0, delta1, delta2, gamma,
                                 n_burn, n_keep, base_seed, redraws[i], draws[i]);
        }
    };
    if (n_threads <= 1) worker();
    else {
        std::vector<std::thread> pool;
        for (int t = 0; t < n_threads; t++) pool.emplace_back(worker);
        for (auto &th : pool) th.join();
    }

    uint64_t total_draws = 0, total_redraws = 0, max_redraws_one_firm = 0;
    int n_firms_with_any_redraw = 0, n_interior = 0;
    for (int i = 0; i < n; i++) {
        if (draws[i] == 0) continue;   // corner, skipped
        n_interior++;
        total_draws += draws[i];
        total_redraws += redraws[i];
        if (redraws[i] > 0) n_firms_with_any_redraw++;
        if (redraws[i] > max_redraws_one_firm) max_redraws_one_firm = redraws[i];
    }
    double rate = total_draws > 0 ? (double)total_redraws / (double)total_draws : 0.0;
    std::cout << std::setprecision(10)
              << "redrawdiag: lambda=" << lambda << " n_interior=" << n_interior
              << " total_draws=" << total_draws << " total_redraws=" << total_redraws
              << " redraw_rate=" << rate
              << " firms_with_any_redraw=" << n_firms_with_any_redraw << "/" << n_interior
              << " max_redraws_one_firm=" << max_redraws_one_firm << "\n";

    if (!output_csv.empty()) {
        std::ofstream out(output_csv);
        out << "firm_idx,row_id,Mstar,draws,redraws\n";
        for (int i = 0; i < n; i++) {
            if (draws[i] == 0) continue;
            out << i << "," << firms[i].row_id << "," << firms[i].Mstar << "," << draws[i] << "," << redraws[i] << "\n";
        }
        out.close();
        std::cout << "Saved per-firm: " << output_csv << "\n";
    }
}

// ============================================================================
// ---- psi trajectory, cross-firm pooled, per MCMC step (2026-09-08) --------
// ============================================================================
// For an ALREADY-FITTED (theta,gamma), emit Z_r = mean_i(psi_i^(r)) for every
// kept step r=1..n_keep (psi = g_current[0], the row that IS
// h(e)-delta0+delta1*om-delta2*om^2 -- a direct function of the chain's own
// state at every step, unlike delta0/delta1/delta2 themselves which only
// exist after an OUTER BOBYQA re-optimization; this is the correct cheap
// analog of FHJ2008's own toy-example scalar). Batch-means/MCSE arithmetic
// at growing checkpoint lengths is done downstream in R -- this just needs
// to emit the length-n_keep pooled trajectory once per point.
static void run_psitraj_mode(
    const std::vector<FirmData> &firms, const double par[14],
    int n_burn, int n_keep, uint64_t base_seed, int n_threads, const std::string &output_csv
) {
    double delta0 = par[0], eta = par[1], lambda = par[2], delta1 = par[3], delta2 = par[4];
    double gamma[D_G_A];
    for (int t = 0; t < D_G_A; t++) gamma[t] = par[5 + t];

    int n = static_cast<int>(firms.size());
    std::vector<double> Z(n_keep, 0.0);   // cross-firm SUM of psi at each kept step (divide by n_interior after)
    long n_interior = 0;

    auto worker = [&](int begin, int end, std::vector<double> &Zpartial, long &n_interior_partial) {
        for (int i = begin; i < end; i++) {
            const FirmData &f = firms[i];
            if (f.corner == 1) continue;
            n_interior_partial++;

            std::mt19937_64 rng(firm_seed(base_seed, f.row_id));
            std::uniform_real_distribution<double> unif(0.0, 1.0);

            GVecA g_current, g_try;
            double M_current = draw_from_rho_eta(unif(rng), f.Mstar, lambda, eta);
            moment_g_A_one(M_current, f.Mstar, f.V, f.Wt, f.tau_rho, f.beta, lambda, delta0, delta1, delta2, g_current);

            for (int r = -n_burn + 1; r <= n_keep; r++) {
                double M_try = draw_from_rho_eta(unif(rng), f.Mstar, lambda, eta);
                moment_g_A_one(M_try, f.Mstar, f.V, f.Wt, f.tau_rho, f.beta, lambda, delta0, delta1, delta2, g_try);

                double log_ratio = 0.0;
                for (int t = 0; t < D_G_A; t++) log_ratio += gamma[t] * (g_try[t] - g_current[t]);
                if (std::log(unif(rng)) < log_ratio) g_current = g_try;

                if (r > 0) Zpartial[r - 1] += g_current[0];   // psi = index 0
            }
        }
    };

    int nt = std::max(1, n_threads);
    std::vector<std::vector<double>> Zparts(nt, std::vector<double>(n_keep, 0.0));
    std::vector<long> n_interior_parts(nt, 0);
    std::vector<std::thread> pool;
    int chunk = (n + nt - 1) / nt;
    for (int t = 0; t < nt; t++) {
        int begin = t * chunk, end = std::min(n, begin + chunk);
        if (begin >= end) continue;
        pool.emplace_back(worker, begin, end, std::ref(Zparts[t]), std::ref(n_interior_parts[t]));
    }
    for (auto &th : pool) th.join();
    for (int t = 0; t < nt; t++) {
        n_interior += n_interior_parts[t];
        for (int r = 0; r < n_keep; r++) Z[r] += Zparts[t][r];
    }

    std::ofstream out(output_csv);
    out << std::setprecision(12) << "step,Z_pooled_sum\n";
    for (int r = 0; r < n_keep; r++) out << (r + 1) << "," << Z[r] << "\n";
    out.close();
    std::cout << "psitraj: n_interior=" << n_interior << " n_keep=" << n_keep
              << " Saved: " << output_csv << "\n";
}

// ============================================================================
// ---- acceptance-indicator trajectory, cross-firm pooled (2026-09-08) -----
// ============================================================================
// Same architecture as run_psitraj_mode, tracking the ACCEPT INDICATOR
// (1 if step r's proposal was accepted, else 0) instead of psi. Z_r/n_firms
// is then exactly the cross-sectional acceptance RATE at step r -- CBM/MCSE
// on this trajectory answers "how precisely, and how stably, do we know the
// acceptance rate as the chain runs longer" the same way the psi version
// answered it for the moment itself. Chosen over gamma'*psi (the actual CUE
// objective contribution) per the user's own call: rescaling by gamma only
// changes units, not the underlying non-stabilizing pattern already found.
static void run_accepttraj_mode(
    const std::vector<FirmData> &firms, const double par[14],
    int n_burn, int n_keep, uint64_t base_seed, int n_threads, const std::string &output_csv
) {
    double delta0 = par[0], eta = par[1], lambda = par[2], delta1 = par[3], delta2 = par[4];
    double gamma[D_G_A];
    for (int t = 0; t < D_G_A; t++) gamma[t] = par[5 + t];

    int n = static_cast<int>(firms.size());
    std::vector<double> Z(n_keep, 0.0);
    long n_interior = 0;

    auto worker = [&](int begin, int end, std::vector<double> &Zpartial, long &n_interior_partial) {
        for (int i = begin; i < end; i++) {
            const FirmData &f = firms[i];
            if (f.corner == 1) continue;
            n_interior_partial++;

            std::mt19937_64 rng(firm_seed(base_seed, f.row_id));
            std::uniform_real_distribution<double> unif(0.0, 1.0);

            GVecA g_current, g_try;
            double M_current = draw_from_rho_eta(unif(rng), f.Mstar, lambda, eta);
            moment_g_A_one(M_current, f.Mstar, f.V, f.Wt, f.tau_rho, f.beta, lambda, delta0, delta1, delta2, g_current);

            for (int r = -n_burn + 1; r <= n_keep; r++) {
                double M_try = draw_from_rho_eta(unif(rng), f.Mstar, lambda, eta);
                moment_g_A_one(M_try, f.Mstar, f.V, f.Wt, f.tau_rho, f.beta, lambda, delta0, delta1, delta2, g_try);

                double log_ratio = 0.0;
                for (int t = 0; t < D_G_A; t++) log_ratio += gamma[t] * (g_try[t] - g_current[t]);
                bool accepted = std::log(unif(rng)) < log_ratio;
                if (accepted) g_current = g_try;

                if (r > 0) Zpartial[r - 1] += accepted ? 1.0 : 0.0;
            }
        }
    };

    int nt = std::max(1, n_threads);
    std::vector<std::vector<double>> Zparts(nt, std::vector<double>(n_keep, 0.0));
    std::vector<long> n_interior_parts(nt, 0);
    std::vector<std::thread> pool;
    int chunk = (n + nt - 1) / nt;
    for (int t = 0; t < nt; t++) {
        int begin = t * chunk, end = std::min(n, begin + chunk);
        if (begin >= end) continue;
        pool.emplace_back(worker, begin, end, std::ref(Zparts[t]), std::ref(n_interior_parts[t]));
    }
    for (auto &th : pool) th.join();
    for (int t = 0; t < nt; t++) {
        n_interior += n_interior_parts[t];
        for (int r = 0; r < n_keep; r++) Z[r] += Zparts[t][r];
    }

    std::ofstream out(output_csv);
    out << std::setprecision(12) << "step,Z_pooled_sum\n";
    for (int r = 0; r < n_keep; r++) out << (r + 1) << "," << Z[r] << "\n";
    out.close();
    std::cout << "accepttraj: n_interior=" << n_interior << " n_keep=" << n_keep
              << " Saved: " << output_csv << "\n";
}

// ============================================================================
// ---- gamma'g (tilting exponent) per-firm diagnostic (2026-09-08) ---------
// ============================================================================
// For an ALREADY-FITTED (theta,gamma), compute each interior firm's OWN mean
// of gamma'g_current (the scalar log-tilting-weight argument, evaluated at
// the chain's CURRENTLY ACCEPTED state at each kept step) over its own
// n_keep draws, then report cross-firm mean/variance/SD/percentiles of that
// per-firm mean -- same structure as firm_accept_rate_A/run_acceptdiag_mode,
// swapping the tracked per-firm scalar. Tests the hypothesis that a longer
// n_keep during the ORIGINAL BOBYQA fit lets the optimizer land on a gamma
// for which gamma'g behaves more stably (lower cross-firm variance) --
// compares each fit's OWN gamma at ITS OWN original n_keep, not a fixed
// gamma across n_keep (that was the point of the acceptance-rate check
// just done above).
static double firm_mean_gammag_A(
    const FirmData &f, double lambda, double delta0, double delta1, double delta2,
    double eta, const double gamma[D_G_A], int n_burn, int n_keep, uint64_t base_seed
) {
    if (f.corner == 1) return std::numeric_limits<double>::quiet_NaN();

    std::mt19937_64 rng(firm_seed(base_seed, f.row_id));
    std::uniform_real_distribution<double> unif(0.0, 1.0);

    GVecA g_current, g_try;
    double M_current = draw_from_rho_eta(unif(rng), f.Mstar, lambda, eta);
    moment_g_A_one(M_current, f.Mstar, f.V, f.Wt, f.tau_rho, f.beta, lambda, delta0, delta1, delta2, g_current);

    double sum_gammag = 0.0;
    for (int r = -n_burn + 1; r <= n_keep; r++) {
        double M_try = draw_from_rho_eta(unif(rng), f.Mstar, lambda, eta);
        moment_g_A_one(M_try, f.Mstar, f.V, f.Wt, f.tau_rho, f.beta, lambda, delta0, delta1, delta2, g_try);

        double log_ratio = 0.0;
        for (int t = 0; t < D_G_A; t++) log_ratio += gamma[t] * (g_try[t] - g_current[t]);
        if (std::log(unif(rng)) < log_ratio) g_current = g_try;

        if (r > 0) {
            double gammag = 0.0;
            for (int t = 0; t < D_G_A; t++) gammag += gamma[t] * g_current[t];
            sum_gammag += gammag;
        }
    }
    return sum_gammag / n_keep;
}

static void run_gammagdiag_mode(
    const std::vector<FirmData> &firms, const double par[14],
    int n_burn, int n_keep, uint64_t base_seed, const std::string &output_csv
) {
    double delta0 = par[0], eta = par[1], lambda = par[2], delta1 = par[3], delta2 = par[4];
    double gamma[D_G_A];
    for (int t = 0; t < D_G_A; t++) gamma[t] = par[5 + t];

    std::vector<double> vals;
    vals.reserve(firms.size());
    for (const auto &f : firms) {
        if (f.corner == 1) continue;
        vals.push_back(firm_mean_gammag_A(f, lambda, delta0, delta1, delta2, eta, gamma, n_burn, n_keep, base_seed));
    }
    std::sort(vals.begin(), vals.end());
    double sum = 0.0; for (double v : vals) sum += v;
    double mean_v = sum / vals.size();
    double var_v = 0.0; for (double v : vals) var_v += (v - mean_v) * (v - mean_v);
    var_v /= (double)(vals.size() - 1);
    std::cout << "gamma'g diagnostic: n_interior=" << vals.size()
              << " mean=" << mean_v << " variance=" << var_v << " sd=" << std::sqrt(var_v)
              << " p10=" << quantile_sorted(vals, 0.10) << " p50=" << quantile_sorted(vals, 0.50)
              << " p90=" << quantile_sorted(vals, 0.90) << "\n";
    if (!output_csv.empty()) {
        std::ofstream out(output_csv);
        out << std::setprecision(10) << "firm_idx,mean_gammag\n";
        for (size_t i = 0; i < vals.size(); i++) out << i << "," << vals[i] << "\n";
        out.close();
    }
}

// ============================================================================
// ---- Joint 3D (lambda,delta1,delta2) grid, free=(delta0,eta,gamma[1:9]) --
// ============================================================================
// (2026-09-09) ALL THREE of lambda,delta1,delta2 fixed per grid cell -- only
// 11 free dims (delta0, eta, gamma[1:9]), one fewer than the lambda-grid or
// delta-grid modes (which each leave one more structural parameter free
// alongside gamma). Per the user's own point: gamma's own sub-problem is
// convex (Schennach ELVIS_supplement.pdf p.28) and delta0 is a simple level
// shifter, so this inner problem should be easier/faster than what the
// Nelder-Mead lambda-grid/delta-grid checks measured, not the same or worse.
struct InnerParamsAFixed3 {
    const std::vector<FirmData> *firms;
    double lambda, delta1, delta2;
    int n_burn, n_keep, n_threads;
    uint64_t base_seed;
};

static double inner_obj_A_fixed3(unsigned n, const double *x, double *grad, void *data) {
    (void)n; (void)grad;
    InnerParamsAFixed3 *p = static_cast<InnerParamsAFixed3 *>(data);
    double delta0 = x[0];
    double gamma[D_G_A];
    for (int t = 0; t < D_G_A; t++) gamma[t] = x[1 + t];

    double dvec[D_G_A], Omega[D_G_A * D_G_A];
    compute_dvec_omega_A(*(p->firms), p->lambda, delta0, p->delta1, p->delta2, gamma,
                          p->n_burn, p->n_keep, p->base_seed, p->n_threads, dvec, Omega);
    return cue_objective_A_std(dvec, Omega);
}

struct FitResultAFixed3 {
    double delta0, gamma[D_G_A], Lhat;
    int convergence, iters;
    double Lhat_pass1, wander;
    int iters_pass1, convergence_pass1;
    double point_seconds;
};

static FitResultAFixed3 fit_one_grid_point_A_fixed3(
    const std::vector<FirmData> &firms, double lambda, double delta1, double delta2,
    int n_burn, int n_keep, uint64_t base_seed, int n_threads, double maxtime,
    const double *x0_in, nlopt_algorithm algo = NLOPT_LN_BOBYQA
) {
    const int n_par = 1 + D_G_A;   // delta0, gamma[1:9] -- eta DROPPED 2026-09-10
    InnerParamsAFixed3 params{&firms, lambda, delta1, delta2, n_burn, n_keep, n_threads, base_seed};

    double lower[n_par], upper[n_par], x[n_par];
    lower[0] = -DELTA_BOUND; upper[0] = DELTA_BOUND;
    for (int t = 0; t < D_G_A; t++) { lower[1 + t] = -HUGE_VAL; upper[1 + t] = HUGE_VAL; }
    for (int t = 0; t < n_par; t++) x[t] = x0_in[t];

    auto run_opt = [&](double *xstart) -> FitResultAFixed3 {
        nlopt_opt opt = nlopt_create(algo, n_par);
        nlopt_set_lower_bounds(opt, lower);
        nlopt_set_upper_bounds(opt, upper);
        nlopt_set_min_objective(opt, inner_obj_A_fixed3, &params);
        nlopt_set_xtol_rel(opt, 1e-4);
        nlopt_set_maxeval(opt, 2000);
        nlopt_set_maxtime(opt, maxtime);
        double minf = HUGE_VAL;
        nlopt_result res = nlopt_optimize(opt, xstart, &minf);
        int iters = nlopt_get_numevals(opt);
        nlopt_destroy(opt);
        FitResultAFixed3 r;
        r.delta0 = xstart[0];
        for (int t = 0; t < D_G_A; t++) r.gamma[t] = xstart[1 + t];
        r.Lhat = minf; r.convergence = static_cast<int>(res); r.iters = iters;
        return r;
    };

    auto t_start = std::chrono::steady_clock::now();
    FitResultAFixed3 r1 = run_opt(x);
    double x2[n_par];
    x2[0] = r1.delta0;
    for (int t = 0; t < D_G_A; t++) x2[1 + t] = r1.gamma[t];
    FitResultAFixed3 r2 = run_opt(x2);
    r2.point_seconds = std::chrono::duration<double>(std::chrono::steady_clock::now() - t_start).count();

    r2.Lhat_pass1 = r1.Lhat;
    r2.iters_pass1 = r1.iters;
    r2.convergence_pass1 = r1.convergence;
    double sq = (r2.delta0 - r1.delta0) * (r2.delta0 - r1.delta0);
    for (int t = 0; t < D_G_A; t++) sq += (r2.gamma[t] - r1.gamma[t]) * (r2.gamma[t] - r1.gamma[t]);
    r2.wander = std::sqrt(sq);
    return r2;
}

// Reference/seed point loaded from a previously-solved grid3d output CSV
// (same 23-column format run_grid3d_mode itself writes) -- used as a chained
// seed source instead of always starting every new point from a single fixed
// x0 (2026-09-09: validated on one point that seeding from a nearby already-
// converged fit beats a fixed x0 both on speed, ~42% faster, and quality,
// ~2.9x lower Lhat, for Nelder-Mead specifically).
// eta DROPPED from this format 2026-09-10 -- a seed_csv from before that
// change (14/23-column layout with eta_hat) is NOT compatible with this
// loader; build a fresh reference set instead of reusing an old one, since
// eta's removal also means a very different lambda search range.
struct RefPoint { double lambda, delta1, delta2, delta0, gamma[D_G_A]; };

static std::vector<RefPoint> load_ref_points(const std::string &path) {
    std::vector<RefPoint> refs;
    if (path.empty()) return refs;
    std::ifstream in(path);
    if (!in.is_open()) { std::cerr << "ERROR: could not open seed_csv: " << path << "\n"; std::exit(1); }
    std::string line;
    std::getline(in, line);   // header
    while (std::getline(in, line)) {
        if (line.empty()) continue;
        std::stringstream ss(line);
        std::string tok;
        std::vector<double> v;
        while (std::getline(ss, tok, ',')) v.push_back(std::strtod(tok.c_str(), nullptr));
        if (v.size() < 4 + D_G_A) continue;
        RefPoint rp;
        rp.lambda = v[0]; rp.delta1 = v[1]; rp.delta2 = v[2];
        rp.delta0 = v[3];
        for (int t = 0; t < D_G_A; t++) rp.gamma[t] = v[4 + t];
        refs.push_back(rp);
    }
    return refs;
}

static void run_grid3d_mode(
    const std::vector<FirmData> &firms, const std::vector<std::array<double,3>> &points,
    const double *x0, const std::vector<RefPoint> &ref_points,
    int n_burn, int n_keep, uint64_t base_seed, int n_threads, double maxtime,
    int shard_id, int n_shards, const std::string &output_csv, nlopt_algorithm algo
) {
    std::vector<size_t> my_indices;
    for (size_t idx = 0; idx < points.size(); idx++) if ((int)(idx % n_shards) == shard_id) my_indices.push_back(idx);
    std::cout << "grid3d: " << points.size() << " points total, " << my_indices.size()
              << " assigned to this shard, " << n_threads << " threads/point, "
              << ref_points.size() << " reference seed points loaded, chained nearest-neighbor seeding\n";

    // Normalized squared distance in grid-step units, so no one axis
    // dominates (log10(lambda)'s own step is ~0.33, delta1's ~0.75,
    // delta2's ~0.10 in this grid's spacing) -- a Chebyshev-shell-flavored
    // metric, not raw Euclidean distance in the parameters' native units.
    auto dist2 = [](double lam_a, double d1_a, double d2_a,
                     double lam_b, double d1_b, double d2_b) {
        double dl  = (std::log10(lam_a) - std::log10(lam_b)) / 0.33;
        double dd1 = (d1_a - d1_b) / 0.75;
        double dd2 = (d2_a - d2_b) / 0.10;
        return dl * dl + dd1 * dd1 + dd2 * dd2;
    };

    // Pool of already-solved points this shard can seed a new point from --
    // starts as the caller-supplied reference set (e.g. the existing 27-
    // point grid, common across all shards) and grows with every point THIS
    // shard itself solves (sequential within a shard, no cross-process
    // coordination needed or attempted -- other shards' in-progress results
    // are not visible here, only their own now-solved-once-run-finishes
    // outputs would be, via a future re-run's seed_csv).
    struct SolvedPt { double lambda, delta1, delta2, delta0, gamma[D_G_A]; };
    std::vector<SolvedPt> solved;
    solved.reserve(ref_points.size() + my_indices.size());
    for (auto &rp : ref_points) {
        SolvedPt sp; sp.lambda = rp.lambda; sp.delta1 = rp.delta1; sp.delta2 = rp.delta2;
        sp.delta0 = rp.delta0;
        for (int t = 0; t < D_G_A; t++) sp.gamma[t] = rp.gamma[t];
        solved.push_back(sp);
    }

    std::vector<std::pair<std::array<double,3>, FitResultAFixed3>> results;
    results.reserve(my_indices.size());
    auto t0 = std::chrono::steady_clock::now();
    size_t done = 0;

    // Greedy shell expansion: repeatedly solve whichever remaining point is
    // CLOSEST to any already-solved point (reference set or this shard's own
    // prior results this run), seeding from that neighbor -- points land in
    // an outward-expanding order from the known-good region, each chained
    // from its nearest solved neighbor rather than a single fixed x0.
    std::vector<size_t> remaining = my_indices;
    while (!remaining.empty()) {
        size_t best_pos = 0, best_ref = SIZE_MAX;
        double best_d2 = std::numeric_limits<double>::infinity();
        if (solved.empty()) {
            best_pos = 0;   // nothing to chain from yet -- take any point, seed from x0
        } else {
            for (size_t pos = 0; pos < remaining.size(); pos++) {
                auto pt = points[remaining[pos]];
                for (size_t s = 0; s < solved.size(); s++) {
                    double d2 = dist2(pt[0], pt[1], pt[2], solved[s].lambda, solved[s].delta1, solved[s].delta2);
                    if (d2 < best_d2) { best_d2 = d2; best_pos = pos; best_ref = s; }
                }
            }
        }
        size_t idx = remaining[best_pos];
        remaining.erase(remaining.begin() + best_pos);
        auto pt = points[idx];

        double xstart[1 + D_G_A];
        if (best_ref != SIZE_MAX) {
            xstart[0] = solved[best_ref].delta0;
            for (int t = 0; t < D_G_A; t++) xstart[1 + t] = solved[best_ref].gamma[t];
        } else {
            for (int t = 0; t < 1 + D_G_A; t++) xstart[t] = x0[t];
        }

        FitResultAFixed3 fit = fit_one_grid_point_A_fixed3(
            firms, pt[0], pt[1], pt[2], n_burn, n_keep, base_seed, n_threads, maxtime, xstart, algo);
        results.push_back({pt, fit});

        SolvedPt sp; sp.lambda = pt[0]; sp.delta1 = pt[1]; sp.delta2 = pt[2];
        sp.delta0 = fit.delta0;
        for (int t = 0; t < D_G_A; t++) sp.gamma[t] = fit.gamma[t];
        solved.push_back(sp);

        done++;
        auto elapsed = std::chrono::duration<double>(std::chrono::steady_clock::now() - t0).count();
        std::cout << "  [" << done << "/" << my_indices.size() << "] elapsed=" << elapsed << "s"
                  << "  lambda=" << pt[0] << " d1=" << pt[1] << " d2=" << pt[2]
                  << (best_ref != SIZE_MAX ? " seed=neighbor" : " seed=x0")
                  << " Lhat=" << fit.Lhat << " (pass1=" << fit.Lhat_pass1 << ")"
                  << " conv=" << fit.convergence << " iters=" << fit.iters
                  << " point_seconds=" << fit.point_seconds << "\n" << std::flush;
    }

    std::ofstream out(output_csv);
    if (!out.is_open()) { std::cerr << "ERROR: could not open output_csv for writing: " << output_csv << "\n"; std::exit(1); }
    out << std::setprecision(15);
    out << "lambda,delta1,delta2,delta0_hat,gamma1,gamma2,gamma3,gamma4,gamma5,gamma6,gamma7,gamma8,gamma9,"
           "Lhat,Lhat_pass1,wander,convergence,convergence_pass1,iters,iters_pass1,point_seconds,n\n";
    for (auto &r : results) {
        out << r.first[0] << "," << r.first[1] << "," << r.first[2] << ","
            << r.second.delta0 << ",";
        for (int t = 0; t < D_G_A; t++) out << r.second.gamma[t] << ",";
        out << r.second.Lhat << "," << r.second.Lhat_pass1 << "," << r.second.wander << ","
            << r.second.convergence << "," << r.second.convergence_pass1 << ","
            << r.second.iters << "," << r.second.iters_pass1 << "," << r.second.point_seconds << "," << firms.size() << "\n";
    }
    out.close();
    std::cout << "Saved: " << output_csv << " (" << results.size() << " points)\n";
}

int main(int argc, char **argv) {
    auto opt = parse_cli(argc, argv);
    {   // reject unknown keys (2026-09-30, audit finding 9): a typo used to fall back silently to the default
        static const char *known[] = {"algo","algo2","base_seed","cut","cv_beta","cv_mu_c","delta1_hi","delta1_lo","delta1_offset",
            "delta1_stride","delta2_hi","delta2_lo","delta2_offset","delta2_stride","deltas","drop_rows","gamma","gamma0","input_csv",
            "k_fixed","k_free","k_max","k_min","kink_share","lambda_hi","lambda_lo","lambda_offset","lambda_stride","lambdas",
            "max_shell","maxtime","mode","n_burn","n_delta1","n_delta2","n_keep","n_lambda","n_passes","n_shards","n_threads",
            "output_csv","par","points_csv","qform","rho","rho_D","gamma_init","kappa_fixed","share_u","deltas","cf_target","cf_h","cf_t1_extra","cf_grid","cf_cold","cf_multi","cf_decomp","cf_g10","cf_mresp","cf_op","delta0_fixed","delta1_fixed","delta2_fixed","row6","seed","cluster","sampler","maxeval","init_step","nested","Delta","proposal","mix_umax","inner_start","inner_algo","h_floor","kappa_max","ind_rows","audit_p","audit_group","delta_max","row9_mode","rvals","s_fixed","sa_time","seed_csv","shard_id","theta",
            "threads_per_point","x0"};
        for (const auto &kv : opt) {
            bool ok = false; for (const char *k : known) if (kv.first == k) { ok = true; break; }
            if (!ok) { std::cerr << "Unknown option: " << kv.first << "\n"; return 1; }
        }
    }
    std::string input_csv  = get_opt(opt, "input_csv", "");
    std::string output_csv = get_opt(opt, "output_csv", "");
    double lambda_lo = std::strtod(get_opt(opt, "lambda_lo", "1e-9").c_str(), nullptr);
    double lambda_hi = std::strtod(get_opt(opt, "lambda_hi", "1e-4").c_str(), nullptr);
    int n_lambda      = std::atoi(get_opt(opt, "n_lambda", "10").c_str());
    double delta1_lo = std::strtod(get_opt(opt, "delta1_lo", "-4").c_str(), nullptr);
    double delta1_hi = std::strtod(get_opt(opt, "delta1_hi", "2").c_str(), nullptr);
    int n_delta1      = std::atoi(get_opt(opt, "n_delta1", "10").c_str());
    double delta2_lo = std::strtod(get_opt(opt, "delta2_lo", "-1").c_str(), nullptr);
    double delta2_hi = std::strtod(get_opt(opt, "delta2_hi", "1").c_str(), nullptr);
    int n_delta2      = std::atoi(get_opt(opt, "n_delta2", "10").c_str());
    int n_burn        = std::atoi(get_opt(opt, "n_burn", "50").c_str());
    int n_keep        = std::atoi(get_opt(opt, "n_keep", "200").c_str());
    int n_threads     = std::atoi(get_opt(opt, "n_threads", "10").c_str());   // threads PER grid point's firm loop
    double maxtime    = std::strtod(get_opt(opt, "maxtime", "90").c_str(), nullptr);   // seconds, PER BOBYQA pass (2 passes/point)
    int tpp_override  = std::atoi(get_opt(opt, "threads_per_point", "0").c_str());   // shell mode only; 0 = dynamic default
    int shard_id      = std::atoi(get_opt(opt, "shard_id", "0").c_str());    // this process's shard (0-indexed)
    int n_shards      = std::atoi(get_opt(opt, "n_shards", "1").c_str());    // total concurrent processes
    uint64_t base_seed = static_cast<uint64_t>(std::strtoull(get_opt(opt, "base_seed", "20260907").c_str(), nullptr, 10));
    std::string points_csv = get_opt(opt, "points_csv", "");   // explicit (lambda,delta1,delta2) triples -- see below

    if (input_csv.empty() || output_csv.empty()) {
        std::cerr << "Usage: grid_estimator input_csv=... output_csv=... [lambda_lo=] [lambda_hi=] [n_lambda=] "
                     "[delta1_lo=] [delta1_hi=] [n_delta1=] [delta2_lo=] [delta2_hi=] [n_delta2=] "
                     "[n_burn=] [n_keep=] [n_threads=] [shard_id=] [n_shards=] [base_seed=]\n";
        return 1;
    }
    if (shard_id < 0 || shard_id >= n_shards) {
        std::cerr << "ERROR: shard_id must be in [0, n_shards)\n";
        return 1;
    }
    { // Fail fast on a bad output path BEFORE running the (potentially hours-
      // long) grid, not just at the end -- caught by hand during the smoke
      // test (a bad path of my own making silently "succeeded").
        std::ofstream test_out(output_csv);
        if (!test_out.is_open()) {
            std::cerr << "ERROR: output_csv is not writable: " << output_csv << "\n";
            return 1;
        }
    }

    std::cout << "==== grid_estimator (moment set C) ====\n"
              << "  input_csv  = " << input_csv << "\n"
              << "  output_csv = " << output_csv << "\n"
              << "  lambda     = [" << lambda_lo << ", " << lambda_hi << "], n=" << n_lambda << " (log-spaced)\n"
              << "  delta1     = [" << delta1_lo << ", " << delta1_hi << "], n=" << n_delta1 << "\n"
              << "  delta2     = [" << delta2_lo << ", " << delta2_hi << "], n=" << n_delta2 << "\n"
              << "  n_burn=" << n_burn << " n_keep=" << n_keep << " n_threads=" << n_threads
              << " base_seed=" << base_seed << "\n"
              << "  shard: " << shard_id << " / " << n_shards << "\n"
              << "========================================\n";

    std::vector<FirmData> firms = read_firm_csv(input_csv);
    {   std::string cls = get_opt(opt, "cluster", "none");
        if (cls == "plant") {
            std::map<long, int> ids;
            for (FirmData &f : firms) {
                if (f.plant < 0) { std::cerr << "cluster=plant needs a plant_id column (row_id " << f.row_id << ")\n"; return 1; }
                auto it = ids.find(f.plant); if (it == ids.end()) it = ids.emplace(f.plant, (int)ids.size()).first;
                f.cl = it->second;
            }
            g_ncl = (int)ids.size(); g_cluster_on = true;
            std::cout << "cluster=plant: Omega clustered over " << g_ncl << " plants\n";
        } else if (cls != "none") { std::cerr << "cluster must be none or plant\n"; return 1; }
    }
    std::cout << "Loaded " << firms.size() << " firm-periods ("
              << std::count_if(firms.begin(), firms.end(), [](const FirmData &f) { return f.corner == 1; })
              << " corner) from " << input_csv << "\n";

    std::string mode = get_opt(opt, "mode", "flat");
    {   std::string cut = get_opt(opt, "cut", "ak");
        if (cut == "ak") { g_cut_ak = true; std::cout << "cut=ak: Omega/n, keep eigenvalues > 0, dropped rows removed before eigen (AK2020 objMCcu)\n"; }
        else if (cut == "rel") { g_cut_ak = false; std::cout << "cut=rel: OLD relative eigen-cut (porting error; reproduction of pre-2026-09-30 runs only)\n"; }
        else { std::cerr << "cut must be ak or rel\n"; return 1; } }
    {   // detection function for moment set A's chain (S3, 2026-09-28); default linear = every earlier result
        std::string qform = get_opt(opt, "qform", "linear");
        if (qform == "exp_scale" || qform == "power_scale") {
            g_qform = (qform == "exp_scale") ? 1 : 2;
            for (const FirmData &f : firms)
                if (f.corner == 0 && !(std::isfinite(f.Mbar) && f.Mbar > 0)) {
                    std::cerr << "qform=" << qform << " needs a positive Mbar for every interior firm (row_id " << f.row_id << ")\n";
                    return 1;
                }
            if (g_qform == 1) std::cout << "Detection: qform=exp_scale, q = lambda1*(1-exp(-e/Mbar)); 'lambda' below is lambda1\n";
            else              std::cout << "Detection: qform=power_scale, q = (e/Mbar)^k; 'lambda' below is k\n";
        } else if (qform == "power_kink" || qform == "power_nokink") {
#ifdef KINK
            g_qform = (qform == "power_nokink") ? 5 : 4;
            g_kshare = std::strtod(get_opt(opt, "kink_share", "0.3").c_str(), nullptr);
            g_kmax = std::min(2.9, std::strtod(get_opt(opt, "k_max", "0.99").c_str(), nullptr));
#ifdef KINK_S
            g_kfixed = std::strtod(get_opt(opt, "k_fixed", "-1").c_str(), nullptr);
            g_kfree = get_opt(opt, "k_free", "0") == "1";
            if (g_kfree) { g_kmax = std::min(2.9, std::strtod(get_opt(opt, "k_max", "2.5").c_str(), nullptr));
                           g_kmin = std::max(0.01, std::strtod(get_opt(opt, "k_min", "0.05").c_str(), nullptr));
                           std::cout << "k estimated (k_free=1), bounds [" << g_kmin << ", " << g_kmax << "]\n"; }
            if (!(g_kfixed > 0 && g_kfixed < 2.9)) { std::cerr << "KINK_S build requires k_fixed in (0, 2.9)\n"; return 1; }
            g_sfixed = std::strtod(get_opt(opt, "s_fixed", "-1").c_str(), nullptr);
            {   std::string dr = get_opt(opt, "drop_rows", ""); std::stringstream ss(dr); std::string tok;
                while (std::getline(ss, tok, ',')) if (!tok.empty()) {
                    char *endp = nullptr; long rl = std::strtol(tok.c_str(), &endp, 10);
                    if (endp == tok.c_str() || *endp != '\0') { std::cerr << "drop_rows: bad token '" << tok << "'\n"; return 1; }
                    int r = static_cast<int>(rl);
                    // rows 1, 5, 7 droppable since 2026-09-30 (the corner branch applies the mask too); row 10 pins s
                    if (r < 0 || r >= D_G_A || r == 10) { std::cerr << "drop_rows: row " << r << " not droppable\n"; return 1; }
                    g_dropmask |= (1u << r);
                }
                g_audit_p = std::strtod(get_opt(opt, "audit_p", "-1").c_str(), nullptr);
                g_audit_on = g_audit_p >= 0.0;
                if (g_audit_on && g_qform != 5) { std::cerr << "audit_p requires qform=power_nokink (row 10 is the share row under the kink)\n"; return 1; }
                if (g_audit_on && !(g_audit_p < 1.0)) { std::cerr << "audit_p must be in [0,1)\n"; return 1; }
                if (g_audit_on) {   // review 4: without the kink q <= 1/(1+k), the value at the FOC ceiling
                    const double kk = g_kfree ? g_kmin : g_kfixed, qmax = 1.0 / (1.0 + kk);
                    if (g_audit_p >= qmax) { std::cerr << "audit_p = " << g_audit_p << " >= detection ceiling 1/(1+k) = " << qmax << ": unreachable\n"; return 1; }
                    if (g_audit_p > 0.8 * qmax) std::cout << "WARNING: audit_p = " << g_audit_p << " is within 20% of the detection ceiling " << qmax
                                                          << " -- matching it needs the group near the FOC ceiling; check the CEILING EDGE line\n";
                    if (!g_has_audit_g) { std::cerr << "audit_p needs input column audit_g\n"; return 1; }
                }
                if (g_qform == 5) { g_sfixed = 0.3; if (!g_audit_on) g_dropmask |= (1u << 10); }   // no kink: s inert; row 10 = audit row or dropped
                if (g_audit_on) std::cout << "audit moment: row 10 = audit_g * (q(e) - " << g_audit_p << ")\n";
                if (g_dropmask) std::cout << "drop_rows mask = " << g_dropmask << " (" << dr << ")\n";
            }
            if (g_sfixed > 0 && g_sfixed < 1) std::cout << "KINK_S: s fixed at " << g_sfixed << " (x0's s entry must equal it)\n";
            std::cout << "KINK_S: k fixed at " << g_kfixed << ", share s estimated (start " << g_kshare << "), row [11] = eps * score(kappa)\n";
#endif
            for (const FirmData &f : firms)
                if (f.corner == 0 && !(std::isfinite(f.Mbar) && f.Mbar > 0)) {
                    std::cerr << "qform=power_kink needs a positive Mbar for every interior firm (row_id " << f.row_id << ")\n";
                    return 1;
                }
            if (g_qform == 5) std::cout << "Detection: qform=power_nokink (2026-09-30), q = (e/(kappa*Mbar))^k, support M in (max(0, M* - c_k kappa Mbar), M*]"
                                           " (FOC ceiling as a support restriction, redraw at the floor edge); rows 10 dropped, s inert\n";
            else std::cout << "Detection: qform=power_kink, q = (e/(kappa*Mbar))^k up to the FOC ceiling, flat beyond; 'lambda' below is"
                         " kappa, k estimated; share beyond the kink fixed at " << g_kshare << "\n";
#else
            std::cerr << "qform=power_kink needs the KINK build (grid_estimator_kink)\n"; return 1;
#endif
        } else if (qform == "linear_new") {
            g_qform = 3;
            std::cout << "Detection: qform=linear_new, q = lambda*e (levels) with the new-rows moment path\n";
        } else if (qform != "linear") { std::cerr << "qform must be linear, exp_scale, power_scale or linear_new\n"; return 1; }
        g_sa_time = std::strtod(get_opt(opt, "sa_time", "0").c_str(), nullptr);
        if (g_sa_time > 0) std::cout << "Simulated annealing between Nelder-Mead passes: " << g_sa_time << " s per point\n";
        std::string row6 = get_opt(opt, "row6", "eps_e");
        if (row6 == "eps_psi") {
            if (g_qform == 0) { std::cerr << "row6=eps_psi needs a new-rows qform (exp_scale, power_scale, linear_new)\n"; return 1; }
            g_row6 = 1; std::cout << "Moment set A row [6] = eps*psi\n";
        } else if (row6 != "eps_e") { std::cerr << "row6 must be eps_e or eps_psi\n"; return 1; }
#ifdef TAU_ROW
        if (g_qform == 0 || (mode != "lambdagrid" && mode != "adiag")) {
            std::cerr << "This TAU_ROW build supports only mode=lambdagrid|adiag with qform=exp_scale\n"; return 1;
        }
        for (const FirmData &f : firms)
            if (f.corner == 0 && !std::isfinite(f.ltau_bar)) {
                std::cerr << "TAU_ROW needs a finite ltau_bar for every interior firm (row_id " << f.row_id << ")\n"; return 1;
            }
        std::cout << "Moment set A + row [9] psi*ltau_bar (TAU_ROW build, D_G_A=" << D_G_A << ")\n";
#endif
#ifdef YEAR_FE
        for (const FirmData &f : firms)
            if (f.corner == 0 && (f.yidx < 0 || f.yidx > N_XFE)) {
                std::cerr << "YEAR_FE needs year in 81..91 for every interior firm (row_id " << f.row_id << ")\n"; return 1;
            }
        if (mode == "lambdagrid") {
            std::string ls = get_opt(opt, "lambdas", "");
            if (ls.empty() || ls.find(',') != std::string::npos) {
                std::cerr << "YEAR_FE build: lambdagrid takes exactly ONE lambda per process (global g_d0yr)\n"; return 1;
            }
        }
        std::cout << "Year intercepts delta0_82..91 + rows psi*1{year} (YEAR_FE build, D_G_A=" << D_G_A << ")\n";
#endif
#ifdef KINK
        if (g_qform != 4 && g_qform != 5) { std::cerr << "KINK build: use qform=power_kink or power_nokink\n"; return 1; }
        if (mode == "lambdagrid") {
            std::string ls = get_opt(opt, "lambdas", "");
            if (ls.empty() || ls.find(',') != std::string::npos) {
                std::cerr << "KINK build: lambdagrid takes exactly ONE kappa per process (global g_kpow)\n"; return 1;
            }
        }
#endif
    }
    {   std::string sd = get_opt(opt, "seed", "hash");
        if (sd == "hash") g_seed_hash = true;
        else if (sd == "add") { g_seed_hash = false; std::cout << "seed=add: OLD per-firm seeding base_seed+row_id (reproduction of pre-2026-09-30 runs only)\n"; }
        else { std::cerr << "seed must be hash or add\n"; return 1; } }
    {   std::string sm = get_opt(opt, "sampler", "mh");
        if (sm == "is") { g_sampler_is = true; std::cout << "sampler=is: self-normalized importance sampling on n_keep fixed draws per firm\n"; }
        else if (sm != "mh") { std::cerr << "sampler must be mh or is\n"; return 1; }
        {   std::string pr = get_opt(opt, "proposal", "uniform");
            if (pr == "mix") {
                if (!g_sampler_is) { std::cerr << "proposal=mix requires sampler=is\n"; return 1; }
                if (g_qform != 4 && g_qform != 5) { std::cerr << "proposal=mix requires qform=power_kink or power_nokink\n"; return 1; }
                g_prop_mix = true; g_mix_umax = std::strtod(get_opt(opt, "mix_umax", "25").c_str(), nullptr);
                std::cout << "proposal=mix: uniform in M, log-uniform in M (u up to " << g_mix_umax << ") and, when the support is bounded below, log-uniform in M - lo; 1/2,1/2 or 1/3 each; reweighted\n";
            } else if (pr != "uniform") { std::cerr << "proposal must be uniform or mix\n"; return 1; } }
        {   std::string gi = get_opt(opt, "gamma_init", "zero");
            if (gi == "solve") {
                if (!g_sampler_is) { std::cerr << "gamma_init=solve requires sampler=is\n"; return 1; }
                if (!g_cut_ak) { std::cerr << "gamma_init=solve requires cut=ak\n"; return 1; }
                g_gamma_init_solve = 1; std::cout << "gamma_init=solve: gamma solved alone (L-BFGS) at the start theta before the joint NM\n";
            } else if (gi != "zero") { std::cerr << "gamma_init must be zero or solve\n"; return 1; } }
        if (get_opt(opt, "nested", "0") == "1") {
            if (!g_sampler_is) { std::cerr << "nested=1 requires sampler=is\n"; return 1; }
            if (!g_cut_ak) { std::cerr << "nested=1 requires cut=ak\n"; return 1; }
            {   std::string ia = get_opt(opt, "inner_algo", "lbfgs");
                if (ia == "neldermead") g_inner_nm = 1; else if (ia != "lbfgs") { std::cerr << "inner_algo must be lbfgs or neldermead\n"; return 1; }
                std::cout << "nested inner algorithm: " << ia << "\n"; }
            {   std::string is = get_opt(opt, "inner_start", "fixed");
                if (is == "dual") g_inner_dual = 1; else if (is != "fixed") { std::cerr << "inner_start must be dual or fixed\n"; return 1; } }
            if (g_gamma_init_solve) { std::cerr << "gamma_init=solve applies to the joint NM only; nested=1 already solves gamma at every theta\n"; return 1; }
            g_nested = true; std::cout << "nested=1: outer Nelder-Mead over theta, inner solve over gamma (inner_algo above)\n";
        } }
    {   // dominating measure (Phase 1): rho=uniform (default, old runs) | prop21 (Schennach Prop. 2.1; needs rho_D)
        std::string rho = get_opt(opt, "rho", "uniform");
        if (rho == "prop21") {
            std::string ds = get_opt(opt, "rho_D", ""); std::stringstream ss(ds); std::string tok; int i = 0;
            while (std::getline(ss, tok, ',') && i < D_G_A) g_rhoD[i++] = std::strtod(tok.c_str(), nullptr);
            if (i != D_G_A) { std::cerr << "rho=prop21 needs rho_D with " << D_G_A << " values (mode=rhoD prints it), got " << i << "\n"; return 1; }
            for (int t = 0; t < D_G_A; t++) {
                if (std::isinf(g_rhoD[t]) && !(a_rowmask() & (1u << t))) {   // review 5: inf (row out of the rho penalty) only for bounded indicator rows
                    bool med_row = get_opt(opt, "ind_rows", "epslnm") == "median" && t >= 13 && t < 13 + 9;
#ifdef IND5P
                    if (t >= 13 + 9 && t < 13 + 18) med_row = true;   // share rows (bounded indicators)
#endif
                    if (!med_row) { std::cerr << "rho_D: inf is allowed only on the ind_rows=median rows 13-21 (bounded); row " << t << " is unbounded and inf would make the tilt improper\n"; return 1; } }
                if (!(a_rowmask() & (1u << t)) && !(g_rhoD[t] > 0)) { std::cerr << "rho_D: row " << t << " is live but its D was not computed (0); rerun mode=rhoD with this drop set\n"; return 1; }
                if (!(g_rhoD[t] > 0)) g_rhoD[t] = 1.0;   // dropped row: never used
            }
            g_rho_on = true;
            std::cout << "rho=prop21: drho ~ exp(-||D^-1 (g(M) - g(M*))||^2) x uniform(0, M*]\n";
        } else if (rho != "uniform") { std::cerr << "rho must be uniform or prop21\n"; return 1; }
        else if (g_qform == 5 && mode != "rhoD") { std::cerr << "qform=power_nokink requires rho=prop21 (uniform rho gives an improper tilt for firms whose support reaches M -> 0)\n"; return 1; }
    }
    {   std::string ag = get_opt(opt, "audit_group", "k");   // audit group: k (capital, headline) | v (V, robustness)
        if (ag == "v") { if (!g_has_audit_gv) { std::cerr << "audit_group=v needs input column audit_gv\n"; return 1; }
                         for (FirmData &f : firms) f.audit_g = f.audit_gv; std::cout << "audit group: top 10% of V within industry\n"; }
        else if (ag != "k") { std::cerr << "audit_group must be k or v\n"; return 1; } }
    {   std::string ir = get_opt(opt, "ind_rows", "epslnm");
        if (ir == "eps") g_ind_mode = 1; else if (ir == "median") g_ind_mode = 2; else if (ir == "eps_cw") g_ind_mode = 3; else if (ir == "eps_cwj") g_ind_mode = 4;
        else if (ir != "epslnm") { std::cerr << "ind_rows must be epslnm, eps, eps_cw, eps_cwj or median\n"; return 1; }
#ifndef IND5
        if (ir != "epslnm") { std::cerr << "ind_rows needs the IND5 build\n"; return 1; }
#endif
        if (g_ind_mode == 2 && !g_has_umed) { std::cerr << "ind_rows=median needs input column umed\n"; return 1; }
#ifdef IND5P
        {   g_share_u = std::strtod(get_opt(opt, "share_u", "0.05").c_str(), nullptr);
            if (!(g_share_u > 0)) { std::cerr << "share_u must be positive\n"; return 1; }
            bool live_share = false; for (int t = 13 + N_IND; t < 13 + 2 * N_IND; t++) if (!(g_dropmask & (1u << t))) live_share = true;
            if (live_share && !g_has_pshare) { std::cerr << "IND5P share rows are live but the input has no pshare column (drop rows 22-30 or add it)\n"; return 1; }
            std::cout << "IND5P: share rows 22-30 = (1{u >= " << g_share_u << "} - pshare_j) * 1{j}" << (live_share ? "" : " (all dropped)") << "\n"; }
#else
        if (opt.count("share_u")) { std::cerr << "share_u needs the IND5P build (grid_estimator_ind5p)\n"; return 1; }
#endif
        if (g_ind_mode) std::cout << "industry rows 13+: " << ir << "\n"; }
    {   double hf = std::strtod(get_opt(opt, "h_floor", "1e-6").c_str(), nullptr);
        if (!(hf > 0 && hf < 1)) { std::cerr << "h_floor must be in (0,1)\n"; return 1; }
        g_h_floor_power = hf; if (hf != 1e-6) std::cout << "h_floor (power forms) = " << hf << "\n"; }
    if ((g_nested || g_sampler_is || g_rho_on) && g_qform != 4 && g_qform != 5) {   // review 3: other qforms run a different chain
        std::cerr << "nested=1, sampler=is and rho=prop21 require qform=power_kink or power_nokink\n"; return 1; }
    // data checks for live rows (2026-09-30, audit 7.8)
#ifdef EPSVAR
    if (!(a_rowmask() & (1u << 12)))
        for (const FirmData &f : firms) if (!std::isfinite(f.sig2eps)) { std::cerr << "row 12 live but sig2eps missing (row_id " << f.row_id << ")\n"; return 1; }
#endif
#ifdef IND5
    {   std::vector<int> js; for (const FirmData &f : firms) if (f.corner == 0) js.push_back(f.jidx);
        std::sort(js.begin(), js.end()); js.erase(std::unique(js.begin(), js.end()), js.end());
        if ((int)js.size() != N_IND || js.front() != 0) { std::cerr << "IND5: interior industries = " << js.size() << " (or missing sic_3), N_IND = " << N_IND << "\n"; return 1; } }
#endif
    {   // claims weights w_i = tau_P M*_i / industry mean over interior firms (ind_rows=eps_cw rows; adiag's claims-weighted
        // TARGETED line uses the same weights in every mode). Corner firms keep the weight of their industry's interior mean.
        std::map<int, double> sum_c; std::map<int, int> cnt;
        for (const FirmData &f : firms) if (f.corner == 0) { sum_c[f.sic] += f.tau_rho * f.Mstar; cnt[f.sic]++; }
        for (FirmData &f : firms) if (cnt.count(f.sic) && sum_c[f.sic] > 0) f.cw = f.tau_rho * f.Mstar / (sum_c[f.sic] / cnt[f.sic]);
        if (g_ind_mode == 4) {   // eps_cwj: one constant per industry, c_j = industry mean claims / interior mean claims
            double tot = 0; int nt = 0; for (auto &kv : sum_c) { tot += kv.second; nt += cnt[kv.first]; }
            for (FirmData &f : firms) if (cnt.count(f.sic)) f.cw = (sum_c[f.sic] / cnt[f.sic]) / (tot / nt);
            std::cout << "eps_cwj: c_j = industry mean claims / interior mean:";
            for (auto &kv : sum_c) std::cout << " " << kv.first << ":" << std::setprecision(6) << (kv.second / cnt[kv.first]) / (tot / nt);
            std::cout << "\n"; }
        if (g_ind_mode == 3) {
            std::map<int, double> mx; for (const FirmData &f : firms) if (f.corner == 0) mx[f.sic] = std::max(mx[f.sic], f.cw);
            std::cout << "eps_cw: w = tau_P M* / industry mean (interior); max w by industry:";
            for (auto &kv : mx) std::cout << " " << kv.first << ":" << std::setprecision(4) << kv.second;
            std::cout << std::setprecision(6) << "\n"; }
    }
    if (mode == "adiag" || mode == "rhoD" || mode == "nestedcheck" || mode == "cfprofile") {
        // par=<lambda,delta0,delta1,delta2,gamma1..D_G_A> (13 values in the default build, 14 with TAU_ROW)
        std::string par_str = get_opt(opt, "par", "");
        // YEAR_FE build: par = lambda,delta0,delta1,delta2,d0yr82..91,gamma1..20
        const int NP = 4 + N_XFE + D_G_A;
        double par[4 + N_XFE + D_G_A];
        { std::stringstream ss(par_str); std::string tok; int i = 0;
          while (std::getline(ss, tok, ',') && i < NP) par[i++] = std::strtod(tok.c_str(), nullptr);
          if (i != NP) { std::cerr << "adiag: par must have exactly " << NP << " values, got " << i << "\n"; return 1; } }
#ifdef YEAR_FE
        for (int k = 1; k <= N_XFE; k++) g_d0yr[k] = par[4 + k - 1];
#endif
#ifdef KINK
        g_kpow = par[4];   // par = kappa,delta0,delta1,delta2,k,gamma1..11
#endif
#ifdef KINK_S
        g_kshare = par[5]; // par = kappa,delta0,delta1,delta2,k,s,gamma1..12
#endif
#ifdef KAPPA_FREE
        par[0] = par[6];   // par = kappa0,delta0,delta1,delta2,k,s,kappa_hat,gamma1..; kappa_hat is the one used
#endif
        if (mode == "rhoD") { run_rhoD_mode(firms, par[0], par[1], par[2], par[3], n_keep, base_seed); return 0; }
        if (mode == "cfprofile") {
            if (!g_sampler_is || !g_cut_ak || g_qform != 5 || g_audit_on) { std::cerr << "cfprofile needs sampler=is, cut=ak, qform=power_nokink, no audit row\n"; return 1; }
            for (const FirmData &f : firms) if (f.corner == 1 || !std::isfinite(f.t1) || !std::isfinite(f.pgdp)) {
                std::cerr << "cfprofile: interior firms with t1 and pgdp only (corner firms not supported yet)\n"; return 1; }
            std::vector<double> dl; { std::stringstream ss(get_opt(opt, "deltas", "0")); std::string tok;
                while (std::getline(ss, tok, ',')) if (!tok.empty()) dl.push_back(std::strtod(tok.c_str(), nullptr)); }
            {   const std::string tg = get_opt(opt, "cf_target", "level");
                const char *nm[16] = {"level", "diff_beh", "diff_total", "elast_x", "elast_claims", "overrep", "gap", "true_credit", "loss_t1", "revenue",
                                     "elast_revenue", "mrev", "diff_input", "diff_evasion", "diff_revenue", "mean_q"}; g_cf_target = -1;
                for (int t = 0; t < 16; t++) if (tg == nm[t]) g_cf_target = t;
                if (g_cf_target < 0) { std::cerr << "cf_target must be level, diff_beh, diff_total, elast_x, elast_claims, overrep, gap, true_credit, loss_t1, revenue, elast_revenue, mrev, diff_input, diff_evasion, diff_revenue or mean_q\n"; return 1; }
                g_cf_h = std::strtod(get_opt(opt, "cf_h", "0.01").c_str(), nullptr);
                g_cf_t1_extra = std::strtod(get_opt(opt, "cf_t1_extra", "0").c_str(), nullptr);
                { std::stringstream ss(get_opt(opt, "cf_grid", "")); std::string tok; while (std::getline(ss, tok, ',')) if (!tok.empty()) g_cf_grid.push_back(std::strtod(tok.c_str(), nullptr)); }
                g_cf_cold = get_opt(opt, "cf_cold", "0") == "1"; if (g_cf_cold) std::cout << "cf_cold=1: every profiled gamma solve starts from the operating gamma\n";
                g_cf_multi = get_opt(opt, "cf_multi", "0") == "1";
                g_cf_decomp = get_opt(opt, "cf_decomp", "0") == "1";
                if (g_cf_decomp) std::cout << "cf_decomp=1: read-only split of diff_evasion at +-Delta by current q, operating weights, no profile\n";
                g_cf_mresp = get_opt(opt, "cf_mresp", "0") == "1";
                g_cf_op = get_opt(opt, "cf_op", "1") == "1";
                if (g_cf_multi && !g_cf_op) std::cout << "cf_op=0: no separate operating-gamma start (slot 'operating' in the start diagnostics is the warm start)\n";
                if (g_cf_mresp) std::cout << "cf_mresp=1: true M responds to the purchases rate (two-tax wedge, K and L fixed): M r, t1 r^beta\n";
                if (opt.count("cf_g10")) { g_cf_g10.clear(); std::stringstream ss(get_opt(opt, "cf_g10", "")); std::string tok;
                    while (std::getline(ss, tok, ',')) if (!tok.empty()) g_cf_g10.push_back(std::strtod(tok.c_str(), nullptr)); }
                if (g_cf_multi) { std::cout << "cf_multi=1: starts = operating gamma, warm gamma, best gamma so far, operating gamma with gamma10 in {";
                    for (size_t i = 0; i < g_cf_g10.size(); i++) std::cout << (i ? ", " : "") << g_cf_g10[i]; std::cout << "}\n";
                    if (g_cf_cold) std::cout << "  (cf_cold has no effect with cf_multi=1: the operating and warm starts are both used)\n"; }
                std::cout << "cfprofile target: " << tg << ((g_cf_target == 3 || g_cf_target == 4 || g_cf_target == 10 || g_cf_target == 11) ? " (central difference h = " + std::to_string(g_cf_h) + ")" : "") << "\n"; }
            g_dropmask &= ~(1u << 10);   // row 10 = the credit moment
            g_rhoD[10] = std::numeric_limits<double>::infinity();   // bounded per firm (0 <= credit <= (1+Delta) tau_P M* (1+c_k kappa Mbar/M*)): out of the rho penalty
            run_cfprofile_mode(firms, par, n_keep, base_seed, n_threads, dl, get_opt(opt, "output_csv", "/dev/null"));
            return 0;
        }
        if (mode == "nestedcheck") {   // (1) cached inner L = regular IS objective; (2) analytic gradient vs central FD
            if (!g_sampler_is) { std::cerr << "nestedcheck needs sampler=is\n"; return 1; }
            const double *gam = par + 4 + N_XFE;
            double dvec[D_G_A], Om[D_G_A * D_G_A];
            compute_dvec_omega_A(firms, par[0], par[1], par[2], par[3], gam, n_burn, n_keep, base_seed, n_threads, dvec, Om);
            double Lreg = cue_objective_A_std(dvec, Om);
            NestedCache C; nested_build_cache(C, firms, par[0], par[1], par[2], par[3], n_keep, base_seed, n_threads);
            NestedInner P; P.C = &C; P.firms = &firms; P.n_threads = n_threads;
            for (int t = 0; t < D_G_A; t++) if (!(a_rowmask() & (1u << t))) P.free_idx.push_back(t);
            std::vector<double> gr(P.free_idx.size());
            double g0[D_G_A]; for (int t = 0; t < D_G_A; t++) g0[t] = gam[t];
            double Lnest = nested_L(P, g0, gr.data());
            std::cout << std::setprecision(10) << "Lhat regular (sampler=is) " << Lreg << " | nested cache " << Lnest
                      << " | rel diff " << std::fabs(Lnest - Lreg) / std::fabs(Lreg) << "\n";
            for (size_t q = 0; q < P.free_idx.size(); q++) {
                int t = P.free_idx[q]; double h = 1e-4 * std::max(1.0, std::fabs(g0[t]));
                double gp[D_G_A], gm[D_G_A]; std::copy(g0, g0 + D_G_A, gp); std::copy(g0, g0 + D_G_A, gm); gp[t] += h; gm[t] -= h;
                double fd = (nested_L(P, gp, nullptr) - nested_L(P, gm, nullptr)) / (2 * h);
                std::cout << "  grad row " << t << ": analytic " << gr[q] << "  FD " << fd << "  rel " << std::fabs(gr[q] - fd) / std::max(1e-12, std::fabs(fd)) << "\n";
            }
            {   // dual F: gradient = dbar, checked by central FD
                const unsigned nf = P.free_idx.size(); std::vector<double> xg(nf), gF(nf);
                for (unsigned q = 0; q < nf; q++) xg[q] = g0[P.free_idx[q]];
                nested_F(nf, xg.data(), gF.data(), &P);
                for (unsigned q = 0; q < nf; q++) {
                    double h = 1e-4 * std::max(1.0, std::fabs(xg[q])); std::vector<double> xp = xg, xm = xg; xp[q] += h; xm[q] -= h;
                    double fd = (nested_F(nf, xp.data(), nullptr, &P) - nested_F(nf, xm.data(), nullptr, &P)) / (2 * h);
                    std::cout << "  dual grad row " << P.free_idx[q] << ": analytic " << gF[q] << "  FD " << fd << "\n";
                }
            }
            return 0;
        }
        run_adiag_mode(firms, par[0], par[1], par[2], par[3], par + 4 + N_XFE, n_burn, n_keep, base_seed, n_threads);
        return 0;
    }
    if (mode == "gammagdiag") {
        std::string par_str = get_opt(opt, "par", "");
        if (par_str.empty()) {
            std::cerr << "gammagdiag mode requires par=<14 comma-separated values: delta0,eta,lambda,delta1,delta2,gamma1..9>\n";
            return 1;
        }
        double par[14];
        { std::stringstream ss(par_str); std::string tok; int i = 0;
          while (std::getline(ss, tok, ',') && i < 14) par[i++] = std::strtod(tok.c_str(), nullptr);
          if (i != 14) { std::cerr << "par must have exactly 14 values, got " << i << "\n"; return 1; } }
        std::cout << "Mode: gammagdiag (delta0=" << par[0] << " eta=" << par[1] << " lambda=" << par[2]
                  << " delta1=" << par[3] << " delta2=" << par[4] << ")\n";
        run_gammagdiag_mode(firms, par, n_burn, n_keep, base_seed, output_csv);
        return 0;
    }
    if (mode == "accepttraj") {
        std::string par_str = get_opt(opt, "par", "");
        if (par_str.empty()) {
            std::cerr << "accepttraj mode requires par=<14 comma-separated values: delta0,eta,lambda,delta1,delta2,gamma1..9>\n";
            return 1;
        }
        double par[14];
        { std::stringstream ss(par_str); std::string tok; int i = 0;
          while (std::getline(ss, tok, ',') && i < 14) par[i++] = std::strtod(tok.c_str(), nullptr);
          if (i != 14) { std::cerr << "par must have exactly 14 values, got " << i << "\n"; return 1; } }
        std::cout << "Mode: accepttraj (delta0=" << par[0] << " eta=" << par[1] << " lambda=" << par[2]
                  << " delta1=" << par[3] << " delta2=" << par[4] << ")\n";
        run_accepttraj_mode(firms, par, n_burn, n_keep, base_seed, n_threads, output_csv);
        return 0;
    }
    if (mode == "psitraj") {
        std::string par_str = get_opt(opt, "par", "");
        if (par_str.empty()) {
            std::cerr << "psitraj mode requires par=<14 comma-separated values: delta0,eta,lambda,delta1,delta2,gamma1..9>\n";
            return 1;
        }
        double par[14];
        { std::stringstream ss(par_str); std::string tok; int i = 0;
          while (std::getline(ss, tok, ',') && i < 14) par[i++] = std::strtod(tok.c_str(), nullptr);
          if (i != 14) { std::cerr << "par must have exactly 14 values, got " << i << "\n"; return 1; } }
        std::cout << "Mode: psitraj (delta0=" << par[0] << " eta=" << par[1] << " lambda=" << par[2]
                  << " delta1=" << par[3] << " delta2=" << par[4] << ")\n";
        run_psitraj_mode(firms, par, n_burn, n_keep, base_seed, n_threads, output_csv);
        return 0;
    }
    if (mode == "acceptdiag") {
        // Post-hoc acceptance-rate diagnostic at a single ALREADY-FITTED
        // point -- par=<14 comma-separated values: delta0,eta,lambda,delta1,
        // delta2,gamma1..9>. No optimization; just runs the sampler once.
        std::string par_str = get_opt(opt, "par", "");
        if (par_str.empty()) {
            std::cerr << "acceptdiag mode requires par=<14 comma-separated values: delta0,eta,lambda,delta1,delta2,gamma1..9>\n";
            return 1;
        }
        double par[14];
        { std::stringstream ss(par_str); std::string tok; int i = 0;
          while (std::getline(ss, tok, ',') && i < 14) par[i++] = std::strtod(tok.c_str(), nullptr);
          if (i != 14) { std::cerr << "par must have exactly 14 values, got " << i << "\n"; return 1; } }
        std::cout << "Mode: acceptdiag (delta0=" << par[0] << " eta=" << par[1] << " lambda=" << par[2]
                  << " delta1=" << par[3] << " delta2=" << par[4] << ")\n";
        run_acceptdiag_mode(firms, par, n_burn, n_keep, base_seed, output_csv);
        return 0;
    }
    if (mode == "redrawdiag") {
        // Empirical redraw-rate check for draw_from_rho_checked -- how often
        // does the recursive redraw actually fire, at an ALREADY-FITTED
        // point. No optimization; runs the sampler once with counting on.
        // par=<13 comma-separated values: delta0,lambda,delta1,delta2,
        // gamma1..9> (no eta, matches revenue_baseline/deltagrid/grid3d).
        std::string par_str = get_opt(opt, "par", "");
        if (par_str.empty()) {
            std::cerr << "redrawdiag mode requires par=<13 comma-separated values: delta0,lambda,delta1,delta2,gamma1..9>\n";
            return 1;
        }
        double par[13];
        { std::stringstream ss(par_str); std::string tok; int i = 0;
          while (std::getline(ss, tok, ',') && i < 13) par[i++] = std::strtod(tok.c_str(), nullptr);
          if (i != 13) { std::cerr << "par must have exactly 13 values, got " << i << "\n"; return 1; } }
        std::cout << "Mode: redrawdiag (delta0=" << par[0] << " lambda=" << par[1]
                  << " delta1=" << par[2] << " delta2=" << par[3] << ")\n";
        run_redrawdiag_mode(firms, par, n_burn, n_keep, base_seed, n_threads, output_csv);
        return 0;
    }
    if (mode == "revenue_baseline" || mode == "revgrid" || mode == "revgrid_indep" || mode == "revgrid_fixedtheta" ||
        mode == "grid3d" || mode == "deltagrid" || mode == "shell" || mode == "flat" ||
        mode == "gammagdiag" || mode == "accepttraj" || mode == "psitraj" || mode == "acceptdiag" || mode == "redrawdiag" ||
        mode == "dvecdiag" || mode == "omegadiag") {
        // these modes run firm_chain_R (linear q, uniform rho, MH, no clustering); refuse options they would silently ignore
        if (g_qform >= 4 || g_rho_on || g_sampler_is || g_cluster_on || g_nested) {
            std::cerr << "mode=" << mode << " does not support qform=power_kink/power_nokink, rho=prop21, sampler=is, cluster=plant or nested=1 yet\n"; return 1; }
    }
    if (mode == "revenue_baseline") {
        // Phase 1 "step 1": NO optimization, forward-simulate baseline E[R]
        // at a fixed operating point and a single Delta (default 0) --
        // par=<13 comma-separated values: delta0,lambda,delta1,delta2,
        // gamma1..9> (eta DROPPED 2026-09-10). input_csv must carry t1,pgdp
        // columns (see Code/Deconvolution/1260-stage2-revenue-export.R).
        // output_csv (optional) gets a per-firm row_id,R_nominal,R_real
        // dump; population mean/total always printed to stdout.
        std::string par_str = get_opt(opt, "par", "");
        if (par_str.empty()) {
            std::cerr << "revenue_baseline mode requires par=<13 comma-separated values: delta0,lambda,delta1,delta2,gamma1..9>\n";
            return 1;
        }
        double par[13];
        { std::stringstream ss(par_str); std::string tok; int i = 0;
          while (std::getline(ss, tok, ',') && i < 13) par[i++] = std::strtod(tok.c_str(), nullptr);
          if (i != 13) { std::cerr << "par must have exactly 13 values, got " << i << "\n"; return 1; } }
        double Delta_cf = std::strtod(get_opt(opt, "Delta", "0").c_str(), nullptr);
        double delta0 = par[0], lambda = par[1], delta1 = par[2], delta2 = par[3];
        double gamma[D_G_A];
        for (int t = 0; t < D_G_A; t++) gamma[t] = par[4 + t];
        if (!std::isfinite(firms[0].t1) || !std::isfinite(firms[0].pgdp)) {
            std::cerr << "revenue_baseline mode requires input_csv to carry t1,pgdp columns\n";
            return 1;
        }
        std::cout << "Mode: revenue_baseline (delta0=" << delta0 << " lambda=" << lambda
                  << " delta1=" << delta1 << " delta2=" << delta2 << " Delta=" << Delta_cf << ")\n";
        run_revenue_baseline_mode(firms, lambda, delta0, delta1, delta2, gamma,
                                   n_burn, n_keep, base_seed, n_threads, Delta_cf, output_csv);
        return 0;
    }
    if (mode == "revgrid") {
        // Phase 1 "step 3": joint (Delta,R) grid, FULL profiling of
        // (lambda,delta0,delta1,delta2,gamma[1..10]) at every cell (eta
        // DROPPED 2026-09-10) -- x0=<14 comma-separated values: delta0,
        // lambda,delta1,delta2,gamma1..10>, deltas=<comma-separated Delta
        // values>, rvals=<comma-separated R candidate values, same units as
        // the input_csv's pgdp-deflated revenue -- i.e. REAL, not nominal>.
        // input_csv must carry t1,pgdp (see 1260-stage2-revenue-export.R).
        std::string x0_str = get_opt(opt, "x0", "");
        std::string delta_str = get_opt(opt, "deltas", "");
        std::string rvals_str = get_opt(opt, "rvals", "");
        if (x0_str.empty() || delta_str.empty() || rvals_str.empty()) {
            std::cerr << "revgrid mode requires x0=<14 values: delta0,lambda,delta1,delta2,gamma1..10>, deltas=<comma Delta values>, rvals=<comma R values>\n";
            return 1;
        }
        double x0_rev[14];
        { std::stringstream ss(x0_str); std::string tok; int i = 0;
          while (std::getline(ss, tok, ',') && i < 14) x0_rev[i++] = std::strtod(tok.c_str(), nullptr);
          if (i != 14) { std::cerr << "x0 must have exactly 14 values, got " << i << "\n"; return 1; } }
        std::vector<double> Delta_values, R_values;
        { std::stringstream ss(delta_str); std::string tok; while (std::getline(ss, tok, ',')) Delta_values.push_back(std::strtod(tok.c_str(), nullptr)); }
        { std::stringstream ss(rvals_str); std::string tok; while (std::getline(ss, tok, ',')) R_values.push_back(std::strtod(tok.c_str(), nullptr)); }
        if (!std::isfinite(firms[0].t1) || !std::isfinite(firms[0].pgdp)) {
            std::cerr << "revgrid mode requires input_csv to carry t1,pgdp columns\n";
            return 1;
        }
        std::string algo_str_rg = get_opt(opt, "algo", "neldermead");
        nlopt_algorithm algo_rg = NLOPT_LN_NELDERMEAD;
        if (algo_str_rg == "bobyqa") algo_rg = NLOPT_LN_BOBYQA;
        else if (algo_str_rg != "neldermead") { std::cerr << "algo must be bobyqa or neldermead\n"; return 1; }
        std::cout << "Mode: revgrid, " << Delta_values.size() << " Deltas x " << R_values.size()
                  << " Rs, algo=" << algo_str_rg << "\n";
        run_revenue_grid_mode(firms, Delta_values, R_values, x0_rev, n_burn, n_keep, base_seed, n_threads, maxtime,
                               shard_id, n_shards, output_csv, algo_rg);
        return 0;
    }
    if (mode == "revgrid_indep") {
        // Independent-seeding sibling of revgrid (2026-09-10): every
        // (Delta,R) cell seeded identically from x0 (no chaining at all) --
        // x0's gamma block is meant to be an already-converged anchor fit
        // (solve ONE cell externally first, typically Delta=0 at the
        // central/baseline R candidate, then pass its output in here).
        // Same x0/deltas/rvals CLI contract as revgrid; single process,
        // two-level work-stealing across all cells (no shard_id/n_shards).
        std::string x0_str = get_opt(opt, "x0", "");
        std::string delta_str = get_opt(opt, "deltas", "");
        std::string rvals_str = get_opt(opt, "rvals", "");
        if (x0_str.empty() || delta_str.empty() || rvals_str.empty()) {
            std::cerr << "revgrid_indep mode requires x0=<14 values: delta0,lambda,delta1,delta2,gamma1..10>, deltas=<comma Delta values>, rvals=<comma R values>\n";
            return 1;
        }
        double x0_rev[14];
        { std::stringstream ss(x0_str); std::string tok; int i = 0;
          while (std::getline(ss, tok, ',') && i < 14) x0_rev[i++] = std::strtod(tok.c_str(), nullptr);
          if (i != 14) { std::cerr << "x0 must have exactly 14 values, got " << i << "\n"; return 1; } }
        std::vector<double> Delta_values, R_values;
        { std::stringstream ss(delta_str); std::string tok; while (std::getline(ss, tok, ',')) Delta_values.push_back(std::strtod(tok.c_str(), nullptr)); }
        { std::stringstream ss(rvals_str); std::string tok; while (std::getline(ss, tok, ',')) R_values.push_back(std::strtod(tok.c_str(), nullptr)); }
        if (!std::isfinite(firms[0].t1) || !std::isfinite(firms[0].pgdp)) {
            std::cerr << "revgrid_indep mode requires input_csv to carry t1,pgdp columns\n";
            return 1;
        }
        std::string algo_str_rgi = get_opt(opt, "algo", "neldermead");
        nlopt_algorithm algo_rgi = NLOPT_LN_NELDERMEAD;
        if (algo_str_rgi == "bobyqa") algo_rgi = NLOPT_LN_BOBYQA;
        else if (algo_str_rgi != "neldermead") { std::cerr << "algo must be bobyqa or neldermead\n"; return 1; }
        // row9_mode (2026-09-14, added to let revgrid_indep use the same
        // control-variate-adjusted R moment as revgrid_fixedtheta -- this
        // mode previously only ever used the raw R moment): raw (default),
        // cv (needs cv_beta=/cv_mu_c=), loss (cv_beta/cv_mu_c unused).
        std::string row9_str_rgi = get_opt(opt, "row9_mode", "raw");
        int row9_mode_rgi = 0;
        if (row9_str_rgi == "cv") row9_mode_rgi = 1;
        else if (row9_str_rgi == "loss") row9_mode_rgi = 2;
        else if (row9_str_rgi == "theory") row9_mode_rgi = 3;
        else if (row9_str_rgi != "raw") { std::cerr << "row9_mode must be raw, cv, loss, or theory\n"; return 1; }
        double cv_beta_rgi = std::strtod(get_opt(opt, "cv_beta", "0").c_str(), nullptr);
        double cv_mu_c_rgi = std::strtod(get_opt(opt, "cv_mu_c", "0").c_str(), nullptr);
        if (row9_mode_rgi == 1 && (get_opt(opt, "cv_beta", "").empty() || get_opt(opt, "cv_mu_c", "").empty())) {
            std::cerr << "row9_mode=cv requires cv_beta= and cv_mu_c=\n"; return 1;
        }
        std::cout << "Mode: revgrid_indep, " << Delta_values.size() << " Deltas x " << R_values.size()
                  << " Rs, algo=" << algo_str_rgi << ", row9_mode=" << row9_str_rgi
                  << " (cv_beta=" << cv_beta_rgi << " cv_mu_c=" << cv_mu_c_rgi << ")\n";
        run_revenue_grid_indep_mode(firms, Delta_values, R_values, x0_rev, n_burn, n_keep, base_seed, n_threads, maxtime,
                                     output_csv, algo_rgi, row9_mode_rgi, cv_beta_rgi, cv_mu_c_rgi);
        return 0;
    }
    if (mode == "revgrid_fixedtheta") {
        // Theta_smooth=(lambda,delta0,delta1,delta2) held COMPLETELY FIXED
        // (2026-09-12) -- not merely seeded there, never touched by NLopt at
        // all. Only gamma (all 10) is optimized per cell. theta=<4 values:
        // delta0,lambda,delta1,delta2>, gamma0=<10 values: initial gamma
        // seed, same for every cell -- no chaining>, deltas=, rvals=.
        std::string theta_str = get_opt(opt, "theta", "");
        std::string gamma0_str = get_opt(opt, "gamma0", "");
        std::string delta_str = get_opt(opt, "deltas", "");
        std::string rvals_str = get_opt(opt, "rvals", "");
        if (theta_str.empty() || gamma0_str.empty() || delta_str.empty() || rvals_str.empty()) {
            std::cerr << "revgrid_fixedtheta mode requires theta=<4 values: delta0,lambda,delta1,delta2>, gamma0=<10 values>, deltas=<comma Delta values>, rvals=<comma R values>\n";
            return 1;
        }
        double theta[4];
        { std::stringstream ss(theta_str); std::string tok; int i = 0;
          while (std::getline(ss, tok, ',') && i < 4) theta[i++] = std::strtod(tok.c_str(), nullptr);
          if (i != 4) { std::cerr << "theta must have exactly 4 values, got " << i << "\n"; return 1; } }
        double gamma0[D_G_R];
        { std::stringstream ss(gamma0_str); std::string tok; int i = 0;
          while (std::getline(ss, tok, ',') && i < D_G_R) gamma0[i++] = std::strtod(tok.c_str(), nullptr);
          if (i != D_G_R) { std::cerr << "gamma0 must have exactly " << D_G_R << " values, got " << i << "\n"; return 1; } }
        std::vector<double> Delta_values, R_values;
        { std::stringstream ss(delta_str); std::string tok; while (std::getline(ss, tok, ',')) Delta_values.push_back(std::strtod(tok.c_str(), nullptr)); }
        { std::stringstream ss(rvals_str); std::string tok; while (std::getline(ss, tok, ',')) R_values.push_back(std::strtod(tok.c_str(), nullptr)); }
        if (!std::isfinite(firms[0].t1) || !std::isfinite(firms[0].pgdp)) {
            std::cerr << "revgrid_fixedtheta mode requires input_csv to carry t1,pgdp columns\n";
            return 1;
        }
        std::string algo_str_rgf = get_opt(opt, "algo", "neldermead");
        nlopt_algorithm algo_rgf = NLOPT_LN_NELDERMEAD;
        if (algo_str_rgf == "bobyqa") algo_rgf = NLOPT_LN_BOBYQA;
        else if (algo_str_rgf != "neldermead") { std::cerr << "algo must be bobyqa or neldermead\n"; return 1; }
        // row9_mode (2026-09-12): raw (default, original R-moment), cv
        // (control-variate-adjusted R, needs cv_beta=/cv_mu_c=), loss
        // (revenue loss due to uncaught evasion only, cv_beta/cv_mu_c unused).
        std::string row9_str = get_opt(opt, "row9_mode", "raw");
        int row9_mode = 0;
        if (row9_str == "cv") row9_mode = 1;
        else if (row9_str == "loss") row9_mode = 2;
        else if (row9_str == "theory") row9_mode = 3;
        else if (row9_str != "raw") { std::cerr << "row9_mode must be raw, cv, loss, or theory\n"; return 1; }
        double cv_beta = std::strtod(get_opt(opt, "cv_beta", "0").c_str(), nullptr);
        double cv_mu_c = std::strtod(get_opt(opt, "cv_mu_c", "0").c_str(), nullptr);
        if (row9_mode == 1 && (get_opt(opt, "cv_beta", "").empty() || get_opt(opt, "cv_mu_c", "").empty())) {
            std::cerr << "row9_mode=cv requires cv_beta= and cv_mu_c=\n"; return 1;
        }
        std::cout << "Mode: revgrid_fixedtheta, " << Delta_values.size() << " Deltas x " << R_values.size()
                  << " Rs, algo=" << algo_str_rgf << ", row9_mode=" << row9_str
                  << " (cv_beta=" << cv_beta << " cv_mu_c=" << cv_mu_c << ")\n";
        run_revenue_grid_fixedtheta_mode(firms, Delta_values, R_values,
                                          theta[0], theta[1], theta[2], theta[3], gamma0,
                                          n_burn, n_keep, base_seed, n_threads, maxtime, output_csv, algo_rgf,
                                          row9_mode, cv_beta, cv_mu_c);
        return 0;
    }
    if (mode == "dvecdiag") {
        // Read-only: dump the full 10-row dvec + per-row SE at a GIVEN fixed
        // (theta,gamma,Delta,R) -- no NLopt call at all. theta=<4 values:
        // delta0,lambda,delta1,delta2>, gamma=<10 values>, deltas=, rvals=.
        std::string theta_str = get_opt(opt, "theta", "");
        std::string gamma_str = get_opt(opt, "gamma", "");
        std::string delta_str = get_opt(opt, "deltas", "");
        std::string rvals_str = get_opt(opt, "rvals", "");
        if (theta_str.empty() || gamma_str.empty() || delta_str.empty() || rvals_str.empty()) {
            std::cerr << "dvecdiag mode requires theta=<4 values: delta0,lambda,delta1,delta2>, gamma=<10 values>, deltas=<comma Delta values>, rvals=<comma R values>\n";
            return 1;
        }
        double theta_d[4];
        { std::stringstream ss(theta_str); std::string tok; int i = 0;
          while (std::getline(ss, tok, ',') && i < 4) theta_d[i++] = std::strtod(tok.c_str(), nullptr);
          if (i != 4) { std::cerr << "theta must have exactly 4 values, got " << i << "\n"; return 1; } }
        double gamma_d[D_G_R];
        { std::stringstream ss(gamma_str); std::string tok; int i = 0;
          while (std::getline(ss, tok, ',') && i < D_G_R) gamma_d[i++] = std::strtod(tok.c_str(), nullptr);
          if (i != D_G_R) { std::cerr << "gamma must have exactly " << D_G_R << " values, got " << i << "\n"; return 1; } }
        std::vector<double> Delta_values_dd, R_values_dd;
        { std::stringstream ss(delta_str); std::string tok; while (std::getline(ss, tok, ',')) Delta_values_dd.push_back(std::strtod(tok.c_str(), nullptr)); }
        { std::stringstream ss(rvals_str); std::string tok; while (std::getline(ss, tok, ',')) R_values_dd.push_back(std::strtod(tok.c_str(), nullptr)); }
        if (!std::isfinite(firms[0].t1) || !std::isfinite(firms[0].pgdp)) {
            std::cerr << "dvecdiag mode requires input_csv to carry t1,pgdp columns\n";
            return 1;
        }
        std::string row9_str_dd = get_opt(opt, "row9_mode", "raw");
        int row9_mode_dd = 0;
        if (row9_str_dd == "cv") row9_mode_dd = 1;
        else if (row9_str_dd == "loss") row9_mode_dd = 2;
        else if (row9_str_dd == "theory") row9_mode_dd = 3;
        else if (row9_str_dd != "raw") { std::cerr << "row9_mode must be raw, cv, loss, or theory\n"; return 1; }
        double cv_beta_dd = std::strtod(get_opt(opt, "cv_beta", "0").c_str(), nullptr);
        double cv_mu_c_dd = std::strtod(get_opt(opt, "cv_mu_c", "0").c_str(), nullptr);
        std::cout << "Mode: dvecdiag, " << Delta_values_dd.size() << " Deltas x " << R_values_dd.size()
                  << " Rs, row9_mode=" << row9_str_dd << "\n";
        run_dvecdiag_mode(firms, Delta_values_dd, R_values_dd,
                           theta_d[0], theta_d[1], theta_d[2], theta_d[3], gamma_d,
                           n_burn, n_keep, base_seed, n_threads, output_csv,
                           row9_mode_dd, cv_beta_dd, cv_mu_c_dd);
        return 0;
    }
    if (mode == "omegadiag") {
        // Read-only (2026-09-18): full eigen-spectrum of Omega + truncation
        // accounting at a GIVEN fixed (theta,gamma,Delta,R) -- same CLI
        // contract as dvecdiag (theta=, gamma=, deltas=, rvals=, row9_mode=).
        std::string theta_str = get_opt(opt, "theta", "");
        std::string gamma_str = get_opt(opt, "gamma", "");
        std::string delta_str = get_opt(opt, "deltas", "");
        std::string rvals_str = get_opt(opt, "rvals", "");
        if (theta_str.empty() || gamma_str.empty() || delta_str.empty() || rvals_str.empty()) {
            std::cerr << "omegadiag mode requires theta=<4 values: delta0,lambda,delta1,delta2>, gamma=<10 values>, deltas=<comma Delta values>, rvals=<comma R values>\n";
            return 1;
        }
        double theta_o[4];
        { std::stringstream ss(theta_str); std::string tok; int i = 0;
          while (std::getline(ss, tok, ',') && i < 4) theta_o[i++] = std::strtod(tok.c_str(), nullptr);
          if (i != 4) { std::cerr << "theta must have exactly 4 values, got " << i << "\n"; return 1; } }
        double gamma_o[D_G_R];
        { std::stringstream ss(gamma_str); std::string tok; int i = 0;
          while (std::getline(ss, tok, ',') && i < D_G_R) gamma_o[i++] = std::strtod(tok.c_str(), nullptr);
          if (i != D_G_R) { std::cerr << "gamma must have exactly " << D_G_R << " values, got " << i << "\n"; return 1; } }
        std::vector<double> Delta_values_o, R_values_o;
        { std::stringstream ss(delta_str); std::string tok; while (std::getline(ss, tok, ',')) Delta_values_o.push_back(std::strtod(tok.c_str(), nullptr)); }
        { std::stringstream ss(rvals_str); std::string tok; while (std::getline(ss, tok, ',')) R_values_o.push_back(std::strtod(tok.c_str(), nullptr)); }
        if (!std::isfinite(firms[0].t1) || !std::isfinite(firms[0].pgdp)) {
            std::cerr << "omegadiag mode requires input_csv to carry t1,pgdp columns\n";
            return 1;
        }
        std::string row9_str_o = get_opt(opt, "row9_mode", "raw");
        int row9_mode_o = 0;
        if (row9_str_o == "cv") row9_mode_o = 1;
        else if (row9_str_o == "loss") row9_mode_o = 2;
        else if (row9_str_o == "theory") row9_mode_o = 3;
        else if (row9_str_o != "raw") { std::cerr << "row9_mode must be raw, cv, loss, or theory\n"; return 1; }
        double cv_beta_o = std::strtod(get_opt(opt, "cv_beta", "0").c_str(), nullptr);
        double cv_mu_c_o = std::strtod(get_opt(opt, "cv_mu_c", "0").c_str(), nullptr);
        std::cout << "Mode: omegadiag, " << Delta_values_o.size() << " Deltas x " << R_values_o.size()
                  << " Rs, row9_mode=" << row9_str_o << "\n";
        run_omegadiag_mode(firms, Delta_values_o, R_values_o,
                            theta_o[0], theta_o[1], theta_o[2], theta_o[3], gamma_o,
                            n_burn, n_keep, base_seed, n_threads, output_csv,
                            row9_mode_o, cv_beta_o, cv_mu_c_o);
        return 0;
    }
    if (mode == "grid3d") {
        // Joint (lambda,delta1,delta2) grid -- points_csv has columns
        // lambda,delta1,delta2; x0=<10 comma-separated values:
        // delta0,gamma1..9> (eta DROPPED 2026-09-10).
        std::string x0_str = get_opt(opt, "x0", "");
        if (points_csv.empty() || x0_str.empty()) {
            std::cerr << "grid3d mode requires points_csv=<lambda,delta1,delta2 triples CSV> and x0=<10 comma-separated values: delta0,gamma1..9>\n";
            return 1;
        }
        double x0[10];
        { std::stringstream ss(x0_str); std::string tok; int i = 0;
          while (std::getline(ss, tok, ',') && i < 10) x0[i++] = std::strtod(tok.c_str(), nullptr);
          if (i != 10) { std::cerr << "x0 must have exactly 10 values, got " << i << "\n"; return 1; } }

        std::vector<std::array<double,3>> points3;
        std::ifstream pf3(points_csv);
        if (!pf3) { std::cerr << "Cannot open points_csv: " << points_csv << "\n"; return 1; }
        std::string phdr3; std::getline(pf3, phdr3);   // header: lambda,delta1,delta2
        std::string pline3;
        while (std::getline(pf3, pline3)) {
            if (pline3.empty()) continue;
            std::stringstream ss(pline3); std::string tok; std::vector<double> v;
            while (std::getline(ss, tok, ',')) v.push_back(std::strtod(tok.c_str(), nullptr));
            points3.push_back({v[0], v[1], v[2]});
        }
        std::string algo_str3 = get_opt(opt, "algo", "bobyqa");
        nlopt_algorithm algo3 = NLOPT_LN_BOBYQA;
        if (algo_str3 == "neldermead") algo3 = NLOPT_LN_NELDERMEAD;
        else if (algo_str3 != "bobyqa") { std::cerr << "algo must be bobyqa or neldermead\n"; return 1; }

        // seed_csv (2026-09-09): optional reference set of already-solved
        // grid3d points (same output format this mode itself writes) to
        // chain new points from their nearest neighbor instead of a single
        // fixed x0 -- see run_grid3d_mode's own header comment. Omit to fall
        // back to the original independent-from-x0 behavior for the first
        // point per shard (later points still chain off each other).
        std::string seed_csv = get_opt(opt, "seed_csv", "");
        std::vector<RefPoint> ref_points = load_ref_points(seed_csv);

        std::cout << "Mode: grid3d, " << points3.size() << " (lambda,delta1,delta2) points from " << points_csv
                  << ", algo=" << algo_str3
                  << ", seed_csv=" << (seed_csv.empty() ? "(none)" : seed_csv)
                  << " (" << ref_points.size() << " ref points)\n";
        run_grid3d_mode(firms, points3, x0, ref_points, n_burn, n_keep, base_seed, n_threads, maxtime,
                         shard_id, n_shards, output_csv, algo3);
        return 0;
    }
    if (mode == "deltagrid") {
        // Moment set A (9 rows), (delta1,delta2) grid, lambda a free/profiled
        // nuisance parameter -- see run_deltagrid_mode's own header comment.
        std::string x0_str = get_opt(opt, "x0", "");
        double lambda_lo2  = std::strtod(get_opt(opt, "lambda_lo", "1e-8").c_str(), nullptr);
        double lambda_hi2  = std::strtod(get_opt(opt, "lambda_hi", "1e-4").c_str(), nullptr);
        if (points_csv.empty() || x0_str.empty()) {
            std::cerr << "deltagrid mode requires points_csv=<delta1,delta2 pairs CSV> and x0=<11 comma-separated values: delta0,lambda,gamma1..9>\n";
            return 1;
        }
        double x0[11];
        { std::stringstream ss(x0_str); std::string tok; int i = 0;
          while (std::getline(ss, tok, ',') && i < 11) x0[i++] = std::strtod(tok.c_str(), nullptr);
          if (i != 11) { std::cerr << "x0 must have exactly 11 values, got " << i << "\n"; return 1; } }

        std::vector<std::pair<double,double>> points;
        std::ifstream pf(points_csv);
        if (!pf) { std::cerr << "Cannot open points_csv: " << points_csv << "\n"; return 1; }
        std::string phdr; std::getline(pf, phdr);   // header: delta1,delta2
        std::string pline;
        while (std::getline(pf, pline)) {
            if (pline.empty()) continue;
            std::stringstream ss(pline); std::string tok; std::vector<double> v;
            while (std::getline(ss, tok, ',')) v.push_back(std::strtod(tok.c_str(), nullptr));
            points.push_back({v[0], v[1]});
        }
        std::string algo_str = get_opt(opt, "algo", "bobyqa");
        nlopt_algorithm algo = NLOPT_LN_BOBYQA;
        if (algo_str == "neldermead") algo = NLOPT_LN_NELDERMEAD;
        else if (algo_str != "bobyqa") { std::cerr << "algo must be bobyqa or neldermead\n"; return 1; }
        std::cout << "Mode: deltagrid, " << points.size() << " (delta1,delta2) points from " << points_csv
                  << ", lambda bounds=[" << lambda_lo2 << "," << lambda_hi2 << "], algo=" << algo_str << "\n";
        run_deltagrid_mode(firms, points, x0, lambda_lo2, lambda_hi2, n_burn, n_keep, base_seed, n_threads, maxtime,
                            shard_id, n_shards, output_csv, algo);
        return 0;
    }
    if (mode == "lambdagrid") {
        // Moment set A (9 rows), lambda-only grid, (delta0,delta1,delta2,
        // gamma[1:9]) all free/profiled -- see run_lambdagrid_mode's own
        // header comment. x0=<12 comma-separated values: delta0,delta1,
        // delta2,gamma1..9>. Grid: log-spaced over [lambda_lo,lambda_hi]
        // (top-level CLI options, same as every other mode's lambda range),
        // n_lambda points, unless lambdas=<comma-separated values> is given
        // directly.
        std::string x0_str = get_opt(opt, "x0", "");
        if (x0_str.empty()) {
            std::cerr << "lambdagrid mode requires x0=<12 comma-separated values: delta0,delta1,delta2,gamma1..9>\n";
            return 1;
        }
        const int NX0 = 3 + N_XFE + D_G_A;   // delta0,delta1,delta2,[year intercepts],gamma[1..D_G_A] (12 / 13 / 33)
        double x0[3 + N_XFE + D_G_A];
        { std::stringstream ss(x0_str); std::string tok; int i = 0;
          while (std::getline(ss, tok, ',') && i < NX0) x0[i++] = std::strtod(tok.c_str(), nullptr);
          if (i != NX0) { std::cerr << "x0 must have exactly " << NX0 << " values, got " << i << "\n"; return 1; } }

        std::string lambdas_str = get_opt(opt, "lambdas", "");
        std::vector<double> lambdas;
        if (!lambdas_str.empty()) {
            std::stringstream ss(lambdas_str); std::string tok;
            while (std::getline(ss, tok, ',')) lambdas.push_back(std::strtod(tok.c_str(), nullptr));
        } else {
            lambdas = log_space(lambda_lo, lambda_hi, n_lambda);
        }
        std::string algo_str4 = get_opt(opt, "algo", "neldermead");
        nlopt_algorithm algo4 = NLOPT_LN_NELDERMEAD;
        if (algo_str4 == "bobyqa") algo4 = NLOPT_LN_BOBYQA;
        else if (algo_str4 != "neldermead") { std::cerr << "algo must be bobyqa or neldermead\n"; return 1; }
        {   std::string a2 = get_opt(opt, "algo2", "");
            if (a2 == "bobyqa") g_algo2 = NLOPT_LN_BOBYQA;
            else if (a2 == "lbfgs") g_algo2 = NLOPT_LD_LBFGS;
            else if (a2 == "neldermead") g_algo2 = NLOPT_LN_NELDERMEAD;
            else if (!a2.empty()) { std::cerr << "algo2 must be bobyqa, neldermead or lbfgs\n"; return 1; }
            if (g_algo2 >= 0) std::cout << "pass 2 algorithm: " << a2 << "\n"; }
        g_npasses = std::max(1, std::atoi(get_opt(opt, "n_passes", "2").c_str()));   // 1 allowed in nested mode only
        if (g_npasses == 1 && get_opt(opt, "nested", "0") != "1") { g_npasses = 2; std::cout << "n_passes=1 applies to nested mode only; running 2 passes\n"; }
        g_maxeval = std::atoi(get_opt(opt, "maxeval", "-1").c_str());
        DELTA_BOUND = std::strtod(get_opt(opt, "delta_max", "60").c_str(), nullptr);
        if (opt.count("kappa_fixed")) {
#ifndef KAPPA_FREE
            std::cerr << "kappa_fixed needs a KAPPA_FREE build (kappa is already fixed via lambdas= here)\n"; return 1;
#endif
            g_kappa_fix = std::strtod(opt["kappa_fixed"].c_str(), nullptr);
            if (!(g_kappa_fix > 0)) { std::cerr << "kappa_fixed must be positive\n"; return 1; }
            std::cout << "kappa_fixed = " << g_kappa_fix << " (pinned)\n"; }
        {   const char *nm[3] = {"delta0_fixed", "delta1_fixed", "delta2_fixed"};
            for (int t = 0; t < 3; t++) if (opt.count(nm[t])) { g_dfix[t] = std::strtod(opt[nm[t]].c_str(), nullptr);
                if (!std::isfinite(g_dfix[t]) || std::fabs(g_dfix[t]) > DELTA_BOUND) { std::cerr << nm[t] << " must be finite and inside +/-delta_max\n"; return 1; }
                std::cout << nm[t] << " = " << g_dfix[t] << " (pinned)\n"; } }
        if (!(DELTA_BOUND > 0)) { std::cerr << "delta_max must be positive\n"; return 1; }
        if (DELTA_BOUND != 60.0) std::cout << "delta bounds: +/-" << DELTA_BOUND << "\n";
        g_kappa_max = std::strtod(get_opt(opt, "kappa_max", "5").c_str(), nullptr);
        if (!(g_kappa_max > 0.02)) { std::cerr << "kappa_max must exceed 0.02\n"; return 1; }
        {   std::string is = get_opt(opt, "init_step", "auto");
            if (is == "nlopt") g_init_step_auto = false; else if (is != "auto") { std::cerr << "init_step must be auto or nlopt\n"; return 1; } }
        if (g_npasses != 2) std::cout << "optimizer passes: " << g_npasses << "\n";
        std::cout << "Mode: lambdagrid, " << lambdas.size() << " lambda points, algo=" << algo_str4 << "\n";
        // Two-level work-stealing (2026-09-10): single process, no
        // shard_id/n_shards needed -- n_threads is now the TOTAL thread
        // budget, split across concurrent point-groups internally. Run
        // directly (no run_grid_shards.sh wrapper) even for a 1-point call.
#ifdef KAPPA_FREE
        for (double l : lambdas)   // review 4: an out-of-bounds kappa start made NLopt return -2 and still wrote Lhat = inf
            if (!(l >= 0.02 && l <= g_kappa_max)) { std::cerr << "lambdas (kappa start) " << l << " outside [0.02, kappa_max = " << g_kappa_max << "]\n"; return 1; }
#endif
        if (!g_nested && (opt.count("inner_algo") || opt.count("inner_start"))) std::cout << "note: inner_algo/inner_start apply to nested=1 only; ignored\n";
        if (!g_prop_mix && opt.count("mix_umax")) std::cout << "note: mix_umax applies to proposal=mix only; ignored\n";
        run_lambdagrid_mode(firms, lambdas, x0, n_burn, n_keep, base_seed, n_threads, maxtime,
                             output_csv, algo4);
        return 0;
    }
    if (mode == "shell") {
        int max_shell = std::atoi(get_opt(opt, "max_shell", "1").c_str());
        std::vector<double> lambdas = log_space(lambda_lo, lambda_hi, n_lambda);
        std::vector<double> delta1s = lin_space(delta1_lo, delta1_hi, n_delta1);
        std::vector<double> delta2s = lin_space(delta2_lo, delta2_hi, n_delta2);
        std::cout << "Mode: shell (max_shell=" << max_shell << ")\n";
        run_shell_mode(firms, lambdas, delta1s, delta2s, max_shell, n_burn, n_keep, base_seed, n_threads, maxtime, output_csv, tpp_override);
        return 0;
    }

    std::vector<GridPoint> grid;

    if (!points_csv.empty()) {
        // Explicit-points mode (2026-09-07): read (lambda,delta1,delta2)
        // triples directly from a CSV built by the R-side selection script
        // (Code/Deconvolution/1227-stage2-grid-point-selection.R), instead
        // of constructing a lambda_lo/hi/n_lambda-style lattice here.
        // Deliberately does NOT re-derive which points these are from
        // indices into the reference grid -- R hands over exact numeric
        // values, this file just evaluates them, so there is no cross-
        // language indexing scheme to keep in sync (a real correctness risk
        // an index-based handoff would have had).
        std::ifstream pf(points_csv);
        if (!pf) { std::cerr << "Cannot open points_csv: " << points_csv << "\n"; return 1; }
        std::string phdr; std::getline(pf, phdr);   // header: lambda,delta1,delta2
        std::string pline;
        while (std::getline(pf, pline)) {
            if (pline.empty()) continue;
            std::stringstream ss(pline); std::string tok; std::vector<double> v;
            while (std::getline(ss, tok, ',')) v.push_back(std::strtod(tok.c_str(), nullptr));
            grid.push_back({v[0], v[1], v[2]});
        }
        std::cout << "Explicit points: " << grid.size() << " (from " << points_csv << ")\n";
    } else {
        std::vector<double> lambdas = log_space(lambda_lo, lambda_hi, n_lambda);
        std::vector<double> delta1s = lin_space(delta1_lo, delta1_hi, n_delta1);
        std::vector<double> delta2s = lin_space(delta2_lo, delta2_hi, n_delta2);

        // Sub-lattice selection: select every `*_stride`-th value starting at
        // index `*_offset` from each axis's FULL n-point array, then take the
        // cartesian product of the SELECTED sub-arrays. Superseded by
        // points_csv (progressive random sampling) as the primary mechanism
        // going forward, but kept -- it's simple and still useful for a
        // quick structured scan when that's what's actually wanted.
        auto select_stride = [](const std::vector<double> &v, int stride, int offset) {
            std::vector<double> out;
            for (int i = offset; i < (int)v.size(); i += stride) out.push_back(v[i]);
            return out;
        };
        int lambda_stride = std::atoi(get_opt(opt, "lambda_stride", "1").c_str());
        int lambda_offset = std::atoi(get_opt(opt, "lambda_offset", "0").c_str());
        int delta1_stride = std::atoi(get_opt(opt, "delta1_stride", "1").c_str());
        int delta1_offset = std::atoi(get_opt(opt, "delta1_offset", "0").c_str());
        int delta2_stride = std::atoi(get_opt(opt, "delta2_stride", "1").c_str());
        int delta2_offset = std::atoi(get_opt(opt, "delta2_offset", "0").c_str());
        std::vector<double> lambdas_sel = select_stride(lambdas, lambda_stride, lambda_offset);
        std::vector<double> delta1s_sel = select_stride(delta1s, delta1_stride, delta1_offset);
        std::vector<double> delta2s_sel = select_stride(delta2s, delta2_stride, delta2_offset);
        std::cout << "Sub-lattice: " << lambdas_sel.size() << " lambda x " << delta1s_sel.size()
                  << " delta1 x " << delta2s_sel.size() << " delta2 (stride/offset: lambda="
                  << lambda_stride << "/" << lambda_offset << ", delta1=" << delta1_stride << "/" << delta1_offset
                  << ", delta2=" << delta2_stride << "/" << delta2_offset << ")\n";

        grid.reserve(lambdas_sel.size() * delta1s_sel.size() * delta2s_sel.size());
        for (double lam : lambdas_sel) for (double d1 : delta1s_sel) for (double d2 : delta2s_sel) grid.push_back({lam, d1, d2});
    }

    // This process's share of the grid: every index with idx % n_shards ==
    // shard_id -- an interleaved (not contiguous-block) split, so uneven
    // per-point cost (e.g. slow vs. fast regions of the grid) is spread
    // roughly evenly across shards rather than one shard getting unlucky
    // with a contiguous slow region.
    std::vector<size_t> my_indices;
    for (size_t idx = 0; idx < grid.size(); idx++) if ((int)(idx % n_shards) == shard_id) my_indices.push_back(idx);

    std::cout << "Grid: " << grid.size() << " points total, " << my_indices.size() << " assigned to this shard, "
              << n_threads << " threads/point\n";

    std::vector<GridResult> results;
    results.reserve(my_indices.size());
    auto t0 = std::chrono::steady_clock::now();
    size_t done = 0;

    for (size_t idx : my_indices) {
        GridPoint gp = grid[idx];
        FitResult fit = fit_one_grid_point(firms, gp.lambda, gp.delta1, gp.delta2, n_burn, n_keep, base_seed, n_threads, maxtime);
        results.push_back({gp, fit});
        done++;
        auto elapsed = std::chrono::duration<double>(std::chrono::steady_clock::now() - t0).count();
        std::cout << "  [" << done << "/" << my_indices.size() << "] elapsed=" << elapsed << "s"
                  << "  lambda=" << gp.lambda << " d1=" << gp.delta1 << " d2=" << gp.delta2
                  << " Lhat=" << fit.Lhat << " conv=" << fit.convergence << " iters=" << fit.iters
                  << "\n" << std::flush;
    }

    std::ofstream out(output_csv);
    if (!out.is_open()) {
        // Silent failure here would be nasty: a full-scale run could "finish"
        // (print "Saved: ...") without ever having written a byte -- caught
        // by hand during the smoke test (a bad path of my own making), fixed
        // so it can never happen unnoticed again.
        std::cerr << "ERROR: could not open output_csv for writing: " << output_csv << "\n";
        return 1;
    }
    out << std::setprecision(15);
    out << "lambda,delta1,delta2,delta0_hat,eta_hat,gamma1,gamma2,gamma3,gamma4,gamma5,gamma6,gamma7,Lhat,convergence,iters,n\n";
    for (auto &r : results) {
        out << r.gp.lambda << "," << r.gp.delta1 << "," << r.gp.delta2 << ","
            << r.fit.delta0 << "," << r.fit.eta << ",";
        for (int t = 0; t < D_G_C; t++) out << r.fit.gamma[t] << ",";
        out << r.fit.Lhat << "," << r.fit.convergence << "," << r.fit.iters << "," << firms.size() << "\n";
    }
    out.close();
    std::cout << "Saved: " << output_csv << "\n";
    return 0;
}
