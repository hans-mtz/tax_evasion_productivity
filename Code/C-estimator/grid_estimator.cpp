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
