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
static const double DELTA_BOUND = 60.0;   // same box used throughout this project's stage-2 code

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
};

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

    std::mt19937_64 rng(base_seed + static_cast<uint64_t>(f.row_id));
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
                D_G_C, n, 1.0 / (n - 1), Xc.data(), n, 0.0, Omega, D_G_C);
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
        if (w[k] > 1e-8 * max_eig) {
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
        double minf;
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
        out.push_back(d);
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
static const int D_G_A = 9;
typedef std::array<double, D_G_A> GVecA;

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
        return;
    }

    std::mt19937_64 rng(base_seed + static_cast<uint64_t>(f.row_id));
    std::uniform_real_distribution<double> unif(0.0, 1.0);   // MH accept/reject only -- the M-proposal draw has its own internal distribution inside draw_from_rho_checked

    GVecA g_current, g_try, g_run;
    double M_current = draw_from_rho_checked(rng, f.Mstar, lambda);
    moment_g_A_one(M_current, f.Mstar, f.V, f.Wt, f.tau_rho, f.beta, lambda, delta0, delta1, delta2, g_current);
    g_run.fill(0.0);

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

    std::mt19937_64 rng(base_seed + static_cast<uint64_t>(f.row_id));
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

    std::mt19937_64 rng(base_seed + static_cast<uint64_t>(f.row_id));
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
                D_G_R, n, 1.0 / (n - 1), Xc.data(), n, 0.0, Omega, D_G_R);
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
        if (w[k] > 1e-8 * max_eig) {
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
        double minf;
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
        double minf;
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

    cblas_dsyrk(CblasColMajor, CblasUpper, CblasTrans,
                D_G_A, n, 1.0 / (n - 1), Xc.data(), n, 0.0, Omega, D_G_A);
    for (int i = 0; i < D_G_A; i++)
        for (int j = i + 1; j < D_G_A; j++)
            Omega[j + i * D_G_A] = Omega[i + j * D_G_A];
}

static double cue_objective_A_std(const double dvec[D_G_A], const double Omega_in[D_G_A * D_G_A]) {
    double A[D_G_A * D_G_A];
    std::copy(Omega_in, Omega_in + D_G_A * D_G_A, A);

    double w[D_G_A];
    __CLPK_integer n = D_G_A, lda = D_G_A, il = 1, iu = D_G_A, m, ldz = D_G_A, info;
    double vl = 0, vu = 0, abstol = 1e-10;
    __CLPK_integer lwork = -1, liwork = -1, iwork_query;
    double work_query;
    double Z[D_G_A * D_G_A];
    __CLPK_integer isuppz[2 * D_G_A];

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
        if (w[k] > 1e-8 * max_eig) {
            double d2 = 0.0;
            for (int i = 0; i < D_G_A; i++) d2 += Z[i + k * D_G_A] * dvec[i];
            obj += 0.5 * d2 * d2 / w[k];
        }
    }
    return obj;
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
        double minf;
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
    double gamma[D_G_A];
    for (int t = 0; t < D_G_A; t++) gamma[t] = x[3 + t];

    double dvec[D_G_A], Omega[D_G_A * D_G_A];
    compute_dvec_omega_A(*(p->firms), p->lambda, delta0, delta1, delta2, gamma,
                          p->n_burn, p->n_keep, p->base_seed, p->n_threads, dvec, Omega);
    return cue_objective_A_std(dvec, Omega);
}

struct FitResultAFixedLambda {
    double delta0, delta1, delta2, gamma[D_G_A], Lhat;
    int convergence, iters;
    double Lhat_pass1, wander;
    int iters_pass1, convergence_pass1;
    double point_seconds;
};

static FitResultAFixedLambda fit_one_grid_point_A_fixedLambda(
    const std::vector<FirmData> &firms, double lambda,
    int n_burn, int n_keep, uint64_t base_seed, int n_threads, double maxtime,
    const double *x0_in, nlopt_algorithm algo = NLOPT_LN_NELDERMEAD
) {
    const int n_par = 3 + D_G_A;   // delta0, delta1, delta2, gamma[1:9]
    InnerParamsAFixedLambda params{&firms, lambda, n_burn, n_keep, n_threads, base_seed};

    double lower[n_par], upper[n_par], x[n_par];
    lower[0] = -DELTA_BOUND; upper[0] = DELTA_BOUND;
    lower[1] = -DELTA_BOUND; upper[1] = DELTA_BOUND;
    lower[2] = -DELTA_BOUND; upper[2] = DELTA_BOUND;
    for (int t = 0; t < D_G_A; t++) { lower[3 + t] = -HUGE_VAL; upper[3 + t] = HUGE_VAL; }
    for (int t = 0; t < n_par; t++) x[t] = x0_in[t];

    auto run_opt = [&](double *xstart) -> FitResultAFixedLambda {
        nlopt_opt opt = nlopt_create(algo, n_par);
        nlopt_set_lower_bounds(opt, lower);
        nlopt_set_upper_bounds(opt, upper);
        nlopt_set_min_objective(opt, inner_obj_A_fixedLambda, &params);
        nlopt_set_xtol_rel(opt, 1e-4);
        nlopt_set_maxeval(opt, 2000);
        nlopt_set_maxtime(opt, maxtime);
        double minf;
        nlopt_result res = nlopt_optimize(opt, xstart, &minf);
        int iters = nlopt_get_numevals(opt);
        nlopt_destroy(opt);
        FitResultAFixedLambda r;
        r.delta0 = xstart[0]; r.delta1 = xstart[1]; r.delta2 = xstart[2];
        for (int t = 0; t < D_G_A; t++) r.gamma[t] = xstart[3 + t];
        r.Lhat = minf; r.convergence = static_cast<int>(res); r.iters = iters;
        return r;
    };

    auto t_start = std::chrono::steady_clock::now();
    FitResultAFixedLambda r1 = run_opt(x);
    double x2[n_par];
    x2[0] = r1.delta0; x2[1] = r1.delta1; x2[2] = r1.delta2;
    for (int t = 0; t < D_G_A; t++) x2[3 + t] = r1.gamma[t];
    FitResultAFixedLambda r2 = run_opt(x2);
    r2.point_seconds = std::chrono::duration<double>(std::chrono::steady_clock::now() - t_start).count();

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
    out << "lambda,delta0_hat,delta1_hat,delta2_hat,gamma1,gamma2,gamma3,gamma4,gamma5,gamma6,gamma7,gamma8,gamma9,"
           "Lhat,Lhat_pass1,wander,convergence,convergence_pass1,iters,iters_pass1,point_seconds,n\n";
    for (auto &r : results) {
        out << r.first << ","
            << r.second.delta0 << "," << r.second.delta1 << "," << r.second.delta2 << ",";
        for (int t = 0; t < D_G_A; t++) out << r.second.gamma[t] << ",";
        out << r.second.Lhat << "," << r.second.Lhat_pass1 << "," << r.second.wander << ","
            << r.second.convergence << "," << r.second.convergence_pass1 << ","
            << r.second.iters << "," << r.second.iters_pass1 << "," << r.second.point_seconds << "," << firms.size() << "\n";
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

    std::mt19937_64 rng(base_seed + static_cast<uint64_t>(f.row_id));
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

    std::mt19937_64 rng(base_seed + static_cast<uint64_t>(f.row_id));
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

            std::mt19937_64 rng(base_seed + static_cast<uint64_t>(f.row_id));
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

            std::mt19937_64 rng(base_seed + static_cast<uint64_t>(f.row_id));
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

    std::mt19937_64 rng(base_seed + static_cast<uint64_t>(f.row_id));
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
        double minf;
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
    std::cout << "Loaded " << firms.size() << " firm-periods ("
              << std::count_if(firms.begin(), firms.end(), [](const FirmData &f) { return f.corner == 1; })
              << " corner) from " << input_csv << "\n";

    std::string mode = get_opt(opt, "mode", "flat");
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
        else if (row9_str_rgi != "raw") { std::cerr << "row9_mode must be raw, cv, or loss\n"; return 1; }
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
        else if (row9_str != "raw") { std::cerr << "row9_mode must be raw, cv, or loss\n"; return 1; }
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
        else if (row9_str_dd != "raw") { std::cerr << "row9_mode must be raw, cv, or loss\n"; return 1; }
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
        double x0[12];
        { std::stringstream ss(x0_str); std::string tok; int i = 0;
          while (std::getline(ss, tok, ',') && i < 12) x0[i++] = std::strtod(tok.c_str(), nullptr);
          if (i != 12) { std::cerr << "x0 must have exactly 12 values, got " << i << "\n"; return 1; } }

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
        std::cout << "Mode: lambdagrid, " << lambdas.size() << " lambda points, algo=" << algo_str4 << "\n";
        // Two-level work-stealing (2026-09-10): single process, no
        // shard_id/n_shards needed -- n_threads is now the TOTAL thread
        // budget, split across concurrent point-groups internally. Run
        // directly (no run_grid_shards.sh wrapper) even for a 1-point call.
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
