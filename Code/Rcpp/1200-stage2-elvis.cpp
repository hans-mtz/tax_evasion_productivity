// Stage-2 ELVIS: evasion-FOC moment recast in the latent true-materials level
// M (Paper/sections/9999-elvis.qmd, "Applying ELVIS to stage 2 alone").
//
// Structural maps (M is the ONE unobservable; everything else is firm data
// or theta=(lambda,delta0,delta1,delta2,sigma2_psi), beta_hat fixed from
// stage 1):
//   e(M)       = M*_it - M
//   eps(M)     = ln(M*_it/M) - cal_V_it
//   omega(M)   = tilde_cal_W_it - (1-beta)*eps(M)
//   h(e)       = ln(tau_rho_it) + ln(1 - 2*lambda*e)      [linear q(e), domain e<1/(2*lambda)]
//   psi(M)     = h(e(M)) - delta0 + delta1*omega(M) - delta2*omega(M)^2
//   eps_c(M)   = eps(M) - eps_mu1_it   [demeaned by this firm's own (sic_3,ins) CORP target]
//
// Moment vector g(M, Z_i, theta), 15 rows, finalized 2026-08-28 (CLAUDE.md,
// "Stage-2 moment system, finalized") -- replaces the earlier 7-row version
// that used psi*lag_k/psi*lag_l/psi*(cross-instrument) spare-instrument
// interactions. Those were dropped: (i) lagging costs observations, (ii) the
// closed-form materials-FOC inversion (1-beta)*ln(M) = omega + alpha_K*k +
// alpha_L*l + const shows M, given omega, IS (up to that affine map) a
// deterministic function of (k,l) alone -- so psi*ln(M)/psi*(ln M)^2 already
// subsume the "M net of omega" content those instruments were standing in
// for, more directly ("true inputs", not a proxy).
//
//   [0]  psi
//   [1]  eps*psi                          -- eps _|_ psi (raw eps, not demeaned, matches original)
//   [2]  om*psi                           -- psi _|_ omega
//   [3]  eps*om                           -- eps _|_ omega (stage-1-inherited spec check)
//   [4]  psi*om^2                         -- extends [2] to the omega^2 term in the U-shaped cost
//   [5]  psi*ln(M)                        -- psi _|_ true inputs (replaces psi*lag_k/lag_l)
//   [6]  psi*(ln M)^2
//   [7]  eps_c                            -- E[eps_c]=0: does unincorp eps(M) center like CORP's own (sic_3,ins) eps?
//   [8]  eps_c^2 - eps_var_it             -- variance-matching against the CORP (sic_3,ins) target
//   [9]  eps_c^3 - eps_mu3_it             -- 3rd-central-moment-matching, same target family
//   [10] psi*eps_c^2                      -- does psi track eps's local dispersion?
//   [11] psi^2 - sigma2_psi               -- defines NEW nuisance parameter sigma2_psi (psi's own dispersion)
//   [12] (psi^2-sigma2_psi)*om            -- heteroskedasticity check: psi's dispersion vs omega
//   [13] (psi^2-sigma2_psi)*om^2
//   [14] (psi^2-sigma2_psi)*eps_c         -- heteroskedasticity check: psi's dispersion vs eps
//
// Two rows considered and DROPPED as redundant before finalizing (see
// CLAUDE.md): psi*e (e is the evasion FOC's own deterministic, monotonic
// solution given omega and psi -- "psi _|_ e" isn't a structural claim, it's
// asking whether a policy function's output is independent of the shock
// that determines it); psi*(eps^2-eps_var_it) alongside psi*eps_c^2 --
// algebraically identical in population given E[psi]=0 is already enforced
// by row [0] (the two differ only by -eps_var_it*psi, whose expectation is
// exactly 0 at any point where row [0] holds), so only [10] is kept.
//
// eps_mu1/eps_var/eps_mu3 are PER-FIRM inputs (matched to that firm's own
// sic_3+ins via an R-side join against Code/Products/1206-stage2-eps-
// targets.RData before calling this function) -- same pattern as `beta`,
// which is already per-firm/per-industry. These are FIXED external targets
// computed once from CORP epsilon (directly observed, e=0 exactly for CORP,
// no deconvolution needed) -- NOT re-estimated inside the tilt/gamma search.
// Per-industry, not pooled: CORP epsilon's sd ranges 0.17-0.63 across sic_3
// (pooled 0.42), Bartlett's test K^2=920, p<2.2e-16 -- pooling would
// misspecify rows [7]-[10],[14] for most industries. Mean (eps_mu1) is
// exactly 0 within every industry already (first_stage_panel fits beta
// separately per sic_3, so the CORP residual's mean is forced to 0 by that
// regression's own normal equations) -- kept as a general per-firm input
// rather than hardcoded 0 for robustness/generality.
//
// rho(M|Z) = Uniform(M*_it - 1/(2*lambda), M*_it): a plain bounded interval
// (not a polytope), so no hit-and-run is needed -- cached Uniform(0,1) draws
// are rescaled to this lambda-dependent support directly (draw_from_rho).
//
// The Metropolis independence sampler below targets the entropy-maximizing
// tilt mu*(M|Z;theta,gamma) = rho(M|Z)*exp(gamma'g(M,Z,theta)) / Z(gamma)
// (9999-elvis.qmd, eq-elvis-mu-star): since the proposal IS rho itself, the
// acceptance ratio collapses to log(U) < gamma'(g_try - g_current) -- no rho
// density value or normalizing integral ever needs to be computed (Schennach
// 2014's own remark on why Metropolis is the natural sampler here).
//
// ---- corner firms (tau_P_it == 0), added 2026-08-27, updated 2026-08-28 ---
// For firm-periods with a reported tax_rho == 0, the evasion-FOC benefit
// MB(e) = tau_rho*(1-2*lambda*e) is IDENTICALLY 0 for every e (not just at
// e=0), while MC(e) = exp{delta0-delta1*om+delta2*om^2+psi} is a positive
// constant in e (cost linear in e) -- so e*=0 strictly dominates any e>0 for
// ANY theta. M is therefore a known point (M=Mstar, not latent) for these
// firms: no MCMC sampling, no tilting, and -- importantly -- no informative
// value for psi either (the FOC-corner inequality on psi has RHS=ln(0)=-Inf
// at tau_rho=0, satisfied unconditionally regardless of theta). So every
// PSI-CARRYING row ([0],[1],[2],[4],[5],[6],[10],[11],[12],[13],[14]) is set
// to exactly 0 for corner firms -- not "satisfied at 0", just excluded/
// uninformative -- while the rows that don't involve psi at all ([3] eps*om,
// [7]-[9] the eps-only CORP-target checks) ARE computed for real: eps(M) is
// still well-defined at M=Mstar (independent of tau_rho, lambda, or psi
// entirely), so these rows carry genuine information about corner firms too.
//
// ---- per-firm reseeded RNG + burn-in, added 2026-08-27 ---------------------
// Checked against both reference ELVIS implementations before writing this
// (Schennach's own GAUSS code, Lit-Papers/ELVIS_code/elvisutil.g's avg_mom
// proc; Aguiar-Kashaev 2020's Julia/CUDA gchaincu!,
// TopicsDecisionMaking/ReplicationAK/Appendix_B/cudafunctions/cuda_chainfun.jl)
// -- both do two things this file was missing:
//   (i) reseed the RNG deterministically PER INDIVIDUAL before generating
//       that individual's chain (her `rndseed(myseed+i)`), so the ENTIRE
//       random sequence -- proposals AND the accept/reject draw -- is
//       IDENTICAL for firm i across every (theta,gamma) evaluation.
//   (ii) discard an initial burn-in/equilibration phase before averaging
//       starts (her `rep[1]` steps, r<=0; AK2020's `repn[1]`, same r<=0
//       pattern) -- the chain's initial draw comes from the UNTILTED base
//       measure rho, not the tilted target mu*, so averaging from step 0
//       biases the tilted average toward that untilted start.
// Both are done in C++ directly (std::mt19937_64, seeded per firm as
// BASE_SEED + row_id) rather than by passing an R-generated matrix in.
//
// ---- RcppParallel, added 2026-08-28 ----------------------------------------
// Each firm's chain was ALREADY fully independent of every other firm's --
// its own local std::mt19937_64, own local moment buffers, writes only to
// its own row of Ghat -- so this required no algorithm changes, only
// restructuring the per-firm loop body into a Worker::operator() dispatched
// via RcppParallel::parallelFor. Superseded an earlier R-level
// parallel::mclapply approach (chunk dat in R, fork, rbind) that WORKED and
// was verified bit-identical, but re-forks fresh child processes on EVERY
// single call -- measured overhead ~8.65ms/call in this project's actual
// session (isolated with a trivial no-op mclapply workload), on top of
// result-serialization cost, against calls that can be requested hundreds of
// times per BOBYQA lambda-grid-point. RcppParallel's TBB-backed thread pool
// is created once and persists across calls from the same R session, paying
// that setup cost once instead of on every objective evaluation -- measured
// ~4.9x speedup at 8 cores under mclapply vs a theoretical ~8x ceiling; the
// gap IS that per-call fork/serialize tax. One correctness point that
// doesn't arise with fork-based parallelism: Rcpp:: objects (NumericVector
// etc.) touch R's memory allocator, which is NOT safe to call from worker
// threads -- so all per-firm scratch buffers below use std::array<double,
// D_G> (stack-allocated, no R API involved) rather than Rcpp::NumericVector,
// and all R-facing inputs are wrapped in RcppParallel's RVector/RMatrix
// (thread-safe raw-pointer accessors over the same underlying memory) before
// entering the parallel region. Verified bit-identical against the prior
// (mclapply-parallelized and serial) versions before replacing them.

// [[Rcpp::depends(RcppParallel)]]
// Shared includes + structural maps (e_of_M, eps_of_M, omega_of_M, h_of_e,
// draw_from_rho, draw_from_rho_eta) moved to 1200-stage2-elvis-common.h
// (2026-08-31) when moment set B was split into its own file
// (1200-stage2-elvis-B.cpp) -- see that header's own comment for why.
#include "1200-stage2-elvis-common.h"

static const int D_G = 15;
typedef std::array<double, D_G> GVec;

// ---- moment vector for one firm-period at one candidate M ----
// (internal helper, thread-safe: plain doubles in, std::array out, no R API)
static void moment_g_one(
    double M, double Mstar, double V, double Wt, double tau_rho,
    double beta, double lambda, double delta0, double delta1, double delta2, double sigma2_psi,
    double eps_mu1_i, double eps_var_i, double eps_mu3_i,
    GVec& g_out
) {
    double e     = e_of_M(M, Mstar);
    double eps   = eps_of_M(M, Mstar, V);
    double om    = omega_of_M(M, Mstar, V, Wt, beta);
    double psi   = h_of_e(e, tau_rho, lambda) - delta0 + delta1 * om - delta2 * om * om;
    double eps_c = eps - eps_mu1_i;
    double lnM   = std::log(M);
    double psi2c = psi * psi - sigma2_psi;

    g_out[0]  = psi;
    g_out[1]  = eps * psi;
    g_out[2]  = om * psi;
    g_out[3]  = eps * om;
    g_out[4]  = psi * om * om;
    g_out[5]  = psi * lnM;
    g_out[6]  = psi * lnM * lnM;
    g_out[7]  = eps_c;
    g_out[8]  = eps_c * eps_c - eps_var_i;
    g_out[9]  = eps_c * eps_c * eps_c - eps_mu3_i;
    g_out[10] = psi * eps_c * eps_c;
    g_out[11] = psi2c;
    g_out[12] = psi2c * om;
    g_out[13] = psi2c * om * om;
    g_out[14] = psi2c * eps_c;
}

// ---- RcppParallel Worker: one firm's whole chain per operator() iteration --
// Every input is wrapped in RVector/RMatrix (thread-safe raw-pointer reads
// over the SAME underlying R memory, no copy) rather than passed as
// Rcpp::NumericVector -- Rcpp:: accessors are not guaranteed safe from
// worker threads. Ghat is written via RMatrix too (thread-safe: each firm
// writes only its own row, so there's no cross-thread write contention even
// though all threads share the one output matrix).
struct TiltedMomentWorker : public Worker {
    const RVector<double>  Mstar, V, Wt, tau_rho, eps_mu1, eps_var, eps_mu3, beta, gamma;
    const RVector<int>     row_id, corner;
    const double lambda, delta0, delta1, delta2, sigma2_psi;
    const int n_burn, n_keep, base_seed;
    RMatrix<double> Ghat;

    TiltedMomentWorker(
        const NumericVector& Mstar_, const NumericVector& V_, const NumericVector& Wt_, const NumericVector& tau_rho_,
        const NumericVector& eps_mu1_, const NumericVector& eps_var_, const NumericVector& eps_mu3_,
        const IntegerVector& row_id_, const NumericVector& beta_,
        double lambda_, double delta0_, double delta1_, double delta2_, double sigma2_psi_,
        const NumericVector& gamma_, const IntegerVector& corner_,
        int n_burn_, int n_keep_, int base_seed_,
        NumericMatrix& Ghat_
    ) : Mstar(Mstar_), V(V_), Wt(Wt_), tau_rho(tau_rho_),
        eps_mu1(eps_mu1_), eps_var(eps_var_), eps_mu3(eps_mu3_), beta(beta_), gamma(gamma_),
        row_id(row_id_), corner(corner_),
        lambda(lambda_), delta0(delta0_), delta1(delta1_), delta2(delta2_), sigma2_psi(sigma2_psi_),
        n_burn(n_burn_), n_keep(n_keep_), base_seed(base_seed_),
        Ghat(Ghat_) {}

    void operator()(std::size_t begin, std::size_t end) {
        for (std::size_t ii = begin; ii < end; ii++) {
            int i = static_cast<int>(ii);

            if (corner[i] == 1) {
                // M is a known point (Mstar) -- no sampling, no tilting, no gamma
                // dependence. Psi-carrying rows are uninformative (0); eps*om and
                // the eps-only CORP-target rows are computed for real since
                // eps(M) is well-defined at M=Mstar regardless of tau_rho/lambda.
                double eps_pt   = eps_of_M(Mstar[i], Mstar[i], V[i]);
                double om_pt    = omega_of_M(Mstar[i], Mstar[i], V[i], Wt[i], beta[i]);
                double eps_c_pt = eps_pt - eps_mu1[i];
                for (int t = 0; t < D_G; t++) Ghat(i, t) = 0.0;
                Ghat(i, 3) = eps_pt * om_pt;
                Ghat(i, 7) = eps_c_pt;
                Ghat(i, 8) = eps_c_pt * eps_c_pt - eps_var[i];
                Ghat(i, 9) = eps_c_pt * eps_c_pt * eps_c_pt - eps_mu3[i];
                continue;
            }

            // Deterministic per-firm reseed (Schennach's rndseed(myseed+i)): the
            // WHOLE random sequence below is identical for this firm at ANY
            // (theta,gamma), since it depends only on base_seed+row_id[i] --
            // and now, identical regardless of which thread runs it too.
            std::mt19937_64 rng(static_cast<uint64_t>(base_seed) + static_cast<uint64_t>(row_id[i]));
            std::uniform_real_distribution<double> unif(0.0, 1.0);

            GVec g_current, g_try, g_run;
            double M_current = draw_from_rho(unif(rng), Mstar[i], lambda);
            moment_g_one(M_current, Mstar[i], V[i], Wt[i], tau_rho[i],
                         beta[i], lambda, delta0, delta1, delta2, sigma2_psi,
                         eps_mu1[i], eps_var[i], eps_mu3[i], g_current);

            g_run.fill(0.0);

            // r ranges over the WHOLE chain (burn-in + averaging): only
            // accumulate into g_run once r>0, i.e. after n_burn steps have run.
            for (int r = -n_burn + 1; r <= n_keep; r++) {
                double M_try = draw_from_rho(unif(rng), Mstar[i], lambda);
                moment_g_one(M_try, Mstar[i], V[i], Wt[i], tau_rho[i],
                             beta[i], lambda, delta0, delta1, delta2, sigma2_psi,
                             eps_mu1[i], eps_var[i], eps_mu3[i], g_try);

                double log_ratio = 0.0;
                for (int t = 0; t < D_G; t++) log_ratio += gamma[t] * (g_try[t] - g_current[t]);

                if (std::log(unif(rng)) < log_ratio) {
                    g_current = g_try;
                }
                if (r > 0) {
                    for (int t = 0; t < D_G; t++) g_run[t] += g_current[t] / n_keep;
                }
            }
            for (int t = 0; t < D_G; t++) Ghat(i, t) = g_run[t];
        }
    }
};

// ---- main entry point: dispatches the per-firm loop across a persistent ---
// TBB thread pool via RcppParallel::parallelFor. Thread count is controlled
// from R via RcppParallel::setThreadOptions(numThreads=...) -- not passed as
// an argument here, since it's a process-global TBB scheduler setting, not
// something meaningful to vary within a single call.
//
// [[Rcpp::export]]
NumericMatrix mh_tilted_average_cpp(
    NumericVector Mstar, NumericVector V, NumericVector Wt, NumericVector tau_rho,
    NumericVector eps_mu1, NumericVector eps_var, NumericVector eps_mu3,  // per-firm CORP (sic_3,ins) targets
    IntegerVector row_id,      // per-firm id used to seed that firm's RNG stream: seed = BASE_SEED + row_id[i]
    NumericVector beta,        // per-firm beta (industry-specific, from stage2_data$beta)
    double lambda, double delta0, double delta1, double delta2, double sigma2_psi,
    NumericVector gamma,
    IntegerVector corner,      // 1 = known e=0 (tau_P_it==0) firm, 0 = ordinary latent-M firm
    int n_burn, int n_keep,    // burn-in (discarded) and averaging (kept) chain lengths
    int base_seed = 20260827
) {
    int n = Mstar.size();

    if (gamma.size() != D_G) stop("gamma length must equal 15");
    if (corner.size() != n) stop("corner length must equal n");
    if (row_id.size() != n) stop("row_id length must equal n");

    NumericMatrix Ghat(n, D_G);

    TiltedMomentWorker worker(
        Mstar, V, Wt, tau_rho, eps_mu1, eps_var, eps_mu3, row_id, beta,
        lambda, delta0, delta1, delta2, sigma2_psi, gamma, corner,
        n_burn, n_keep, base_seed, Ghat
    );

    RcppParallel::parallelFor(0, n, worker);

    return Ghat;
}

// ---- Moment sets A and B, added 2026-08-29 ---------------------------------
// Replace the 15-moment system above for new estimation runs (kept alive
// only for comparison/backward reference -- see CLAUDE.md's 2026-08-29
// "gamma-counting confirmed + moment-set A/B" entry for the full derivation).
// Two corrections motivated this redesign, both verified against Schennach's
// own paper (ELVIS.pdf pp.348-350 Example 1.5, p.372 Example 1.5 Continued)
// and her GAUSS code (Lit-Papers/ELVIS_code/elvisutil.g/elvisexample.g)
// before writing any of this:
//   (i) gamma's dimension is NOT counted against the order condition -- it's
//       a Lagrange multiplier mechanically sized to d_g (Theorem 2.1) and
//       solved jointly with nuisance parameters, not an extra structural
//       unknown competing for identifying variation. Her own Example 1.5
//       goes from 5 moments/1 parameter to 27 moments/4 parameters (3 of
//       them nuisance) -- gamma is never on the "parameter" side of that
//       count in either version, and elvisexample.g's `nuisgam` vector
//       (nuisance stacked with gam, jointly profiled via amoeba) confirms
//       this mechanically. So d_g_A=8 vs 3 smooth params, d_g_B=14+J vs
//       3+J smooth params are the real order-condition comparisons -- gamma
//       (8, or 14+J) is unbounded machinery, not counted here.
//   (ii) gamma should NOT be box-constrained -- Schennach explicitly notes
//       (ELVIS.pdf p.359) that solutions at gamma->infinity are expected,
//       corresponding to a degenerate limiting conditional distribution of
//       M, and AK2020's implementation uses +-infinity bounds throughout.
//       mh_tilted_average_A_cpp/B_cpp therefore accept gamma with NO
//       internal clamping; the R driver passes +-Inf bounds to BOBYQA
//       directly (confirmed nloptr::bobyqa handles this correctly).
//
// ---- Moment set A: relative-evasion floor eta, added 2026-08-29 -----------
// M could otherwise be pushed arbitrarily close to 0 (M=0 <=> the firm truly
// used no materials, which cannot coexist with the positive output every
// manufacturing firm in this data reports) -- and nothing in the CUE
// objective actually penalizes this: eps=ln(Mstar/M)-V and lnM=log(M) both
// DIVERGE as M->0, but rather than making Lhat worse, the resulting inflated
// variance in those rows gets DOWN-WEIGHTED by cue_objective's eigenvalue
// truncation of cov(Ghat) -- CUE mutes high-variance directions, it doesn't
// penalize them, so a tilt that dumps mass near M=0 is not automatically
// disfavored. Fixed by adding eta in [0,1) as a genuine theta_smooth member
// (jointly optimized with delta0-2 via BOBYQA, box-bounded, NOT part of
// gamma/g()) that floors the sampler's support at M>=eta*Mstar
// (draw_from_rho_eta above) -- a RELATIVE cap on evasion (e<=(1-eta)*Mstar),
// chosen over an absolute currency-level floor because materials
// expenditure spans orders of magnitude across firms in this data, matching
// this model's existing preference for relative/scale-invariant evasion
// measures (u=ln(Mstar/M) elsewhere in CLAUDE.md). eta=0 recovers the
// pre-existing (2026-08-29-earlier) M>=0 floor exactly, so the data is free
// to reveal no additional restriction is needed. Implemented as a sampler
// restriction, not a gamma-weighted moment row (AK2020 Theorem 4 precedent,
// already used elsewhere in this project for the analogous "unincorp evades
// positively" inequality) -- guarantees M>=eta*Mstar for EVERY draw, not
// just in aggregate expectation, which a moment-based E[M-U_2]=0 device
// would not. D_G_A stays 8; eta adds one column to theta_smooth only.
//
// ---- Moment set A: 8 moments, theta_smooth=(delta0,delta1,delta2,eta) ----
//   [0] psi
//   [1] eps                              -- E[eps]=0 in the aggregate (eps's
//                                            industry-conditional mean is
//                                            already forced to 0 by stage 1's
//                                            own per-industry OLS normal
//                                            equations, so no per-industry
//                                            demeaning/targets are needed
//                                            here, unlike the old 15-moment
//                                            system's eps_c machinery)
//   [2] psi*lnM                          -- psi _|_ true inputs
//   [3] psi*om                           -- psi _|_ omega
//   [4] psi*om^2                         -- extends [3] to the omega^2 term
//   [5] eps*lnM                          -- eps _|_ true inputs
//   [6] h_prime*eps                      -- score moment for lambda (2026-09-05,
//                                            replaces the old eps*e row: not
//                                            especially lambda-informative and
//                                            redundant with [5]/[7]); h_prime =
//                                            d(h)/d(lambda), see h_prime_of_e's
//                                            own header comment for why eps is
//                                            a valid (non-tautological)
//                                            partner, unlike omega
//   [7] eps*om                           -- eps _|_ omega (stage-1-inherited
//                                            spec check)
//
// ---- Moment set B: A's 8 rows + 7 more, adding ONE genuinely new jointly-
// estimated nuisance block (mu_m,j, industry mean of ln M) plus two
// EXTERNALLY, cheaply computed per-industry anchors for omega (mu_omega,j,
// sigma_omega,j -- NOT jointly estimated: unlike ln M, omega has a directly
// observed proxy via tilde_cal_W = omega + (1-beta)*eps, so these are built
// once from data in Code/Deconvolution/1207-stage2-omega-targets.R, exactly
// analogous to the CORP-based eps targets, just built from the unincorp
// sample itself since omega's distribution is NOT assumed firm-type
// invariant -- see that file's header for the construction). mu_m,j, by
// contrast, has no such external proxy (ln M is the ELVIS latent itself)
// and MUST be jointly estimated -- confirmed in-session that a single
// POOLED row would underidentify the J separate mu_m,j values (many
// combinations zero out one pooled average), so it gets one
// industry-indicator-weighted row per industry instead (a firm's row is 0
// unless it belongs to that industry) -- exactly identified for that
// sub-block (J parameters, J rows), consistent with the rest of the system.
//
//   [8 .. 8+J-1]  (lnM - mu_m[j]) * 1(firm's industry == j), j=0..J-1
//                                        -- defines mu_m[0..J-1]; a firm's
//                                           OWN industry row is real, all
//                                           other industries' rows are 0 for
//                                           that firm (not "satisfied at 0",
//                                           genuinely uninformative for a
//                                           firm outside that industry)
//   [8+J+0] om - mu_omega                -- om _|_ nothing new: E[om]=target
//   [8+J+1] om^2 - sigma_omega           -- variance-matching vs the target
//   [8+J+2] psi*(om - mu_omega)          -- psi _|_ omega, industry-net
//   [8+J+3] psi*(om^2 - sigma_omega)     -- heteroskedasticity check
//   [8+J+4] psi*(lnM - mu_m[j])          -- psi _|_ true inputs, industry-net
//                                           (over-identifying check on mu_m
//                                           once pinned down by [8..8+J-1])
//   [8+J+5] psi*eps*om                   -- joint 3-way independence check
//                                           (raw, no per-industry anchor --
//                                           valid because eps's own
//                                           industry-conditional mean is
//                                           already ~0 by the firm-type-
//                                           invariance assumption used
//                                           throughout, not just in
//                                           aggregate, so this factors
//                                           through E[eps|j]=0 for every j)
//   [8+J+6] h_prime*(lnM - mu_m[j])      -- score moment for lambda, materials-
//                                           FOC side (2026-09-05) -- same
//                                           validity argument as [8+J+4]
//                                           (psi*lnM_c), see h_prime_of_e's
//                                           own comment and CLAUDE.md
//
// d_g for moment set B is now 8+J+7 (was 8+J+6 before the h_prime row was
// added, 2026-09-05).
//
// Rows [8+J+2..5] and [8+J+4] use a per-industry ANCHOR (mu_omega,
// sigma_omega, or the current mu_m guess) multiplied by a RAW quantity from
// elsewhere (psi) -- valid regardless of psi's own between-industry
// heterogeneity by the law-of-total-expectation argument already
// established for the old system's rows [10],[12]-[14] (CLAUDE.md,
// 2026-08-29): E[psi*(X-mu_X,j)] = E_j[Cov(psi,X|j)] exactly, since
// E[X-mu_X,j|j]=0 by construction of the anchor -- the between-industry
// term that contaminated the REJECTED E[eM]-E[e]E[M] moment cannot arise
// here because that moment used E[e]*E[M] (raw pooled means), not a
// per-observation per-industry demeaning.
//
// Corner firms (tau_P_it==0): same convention as the 15-moment system --
// M=Mstar known, psi-carrying rows are 0 (uninformative), non-psi rows
// ([1] eps, [5] eps*lnM, [7] eps*om, the firm's own industry-indicator row
// [8..8+J-1], [8+J+0] om-mu_omega, [8+J+1] om^2-sigma_omega) are computed
// for real since none of them depend on tau_rho/lambda/psi.
//
// ---- eta (relative-evasion floor) extended to B, 2026-08-29 (same day) ----
// Originally judged unnecessary for B on the theory that mu_m,j's own
// per-industry mean-matching already disciplines the average recovered ln M
// away from a pathological M->0 collapse -- but that's a MEAN-level guard,
// not a per-draw floor, and A's smoke tests showed the eta mechanism is
// cheap and (at the fitted optimum) barely binds anyway (eta~0 throughout
// A's whole grid) -- so there's little cost and a real safety-margin gain to
// applying the same sampler restriction here too, for consistency. Uses
// draw_from_rho_eta exactly as in A; mu_m,j's own bounds/box are unaffected.

static const int D_G_A = 8;
typedef std::array<double, D_G_A> GVecA;

// [[Rcpp::export]]
double psi_of_M_A(double M, double Mstar, double V, double Wt, double tau_rho,
                   double beta, double lambda, double delta0, double delta1, double delta2) {
    double e  = e_of_M(M, Mstar);
    double om = omega_of_M(M, Mstar, V, Wt, beta);
    return h_of_e(e, tau_rho, lambda) - delta0 + delta1 * om - delta2 * om * om;
}

static void moment_g_A_one(
    double M, double Mstar, double V, double Wt, double tau_rho,
    double beta, double lambda, double delta0, double delta1, double delta2,
    GVecA& g_out
) {
    double e     = e_of_M(M, Mstar);
    double eps   = eps_of_M(M, Mstar, V);
    double om    = omega_of_M(M, Mstar, V, Wt, beta);
    double psi   = h_of_e(e, tau_rho, lambda) - delta0 + delta1 * om - delta2 * om * om;
    double lnM   = std::log(M);
    double hprime = h_prime_of_e(e, lambda);

    g_out[0] = psi;
    g_out[1] = eps;
    g_out[2] = psi * lnM;
    g_out[3] = psi * om;
    g_out[4] = psi * om * om;
    g_out[5] = eps * lnM;
    g_out[6] = hprime * eps;
    g_out[7] = eps * om;
}

struct TiltedMomentWorkerA : public Worker {
    const RVector<double>  Mstar, V, Wt, tau_rho, beta, gamma;
    const RVector<int>     row_id, corner;
    const double lambda, delta0, delta1, delta2, eta;
    const int n_burn, n_keep, base_seed;
    RMatrix<double> Ghat;

    TiltedMomentWorkerA(
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
                for (int t = 0; t < D_G_A; t++) Ghat(i, t) = 0.0;
                Ghat(i, 1) = eps_pt;
                Ghat(i, 5) = eps_pt * lnM_pt;
                Ghat(i, 7) = eps_pt * om_pt;
                continue;
            }

            std::mt19937_64 rng(static_cast<uint64_t>(base_seed) + static_cast<uint64_t>(row_id[i]));
            std::uniform_real_distribution<double> unif(0.0, 1.0);

            GVecA g_current, g_try, g_run;
            double M_current = draw_from_rho_eta(unif(rng), Mstar[i], lambda, eta);
            moment_g_A_one(M_current, Mstar[i], V[i], Wt[i], tau_rho[i],
                           beta[i], lambda, delta0, delta1, delta2, g_current);

            g_run.fill(0.0);

            for (int r = -n_burn + 1; r <= n_keep; r++) {
                double M_try = draw_from_rho_eta(unif(rng), Mstar[i], lambda, eta);
                moment_g_A_one(M_try, Mstar[i], V[i], Wt[i], tau_rho[i],
                               beta[i], lambda, delta0, delta1, delta2, g_try);

                double log_ratio = 0.0;
                for (int t = 0; t < D_G_A; t++) log_ratio += gamma[t] * (g_try[t] - g_current[t]);

                if (std::log(unif(rng)) < log_ratio) {
                    g_current = g_try;
                }
                if (r > 0) {
                    for (int t = 0; t < D_G_A; t++) g_run[t] += g_current[t] / n_keep;
                }
            }
            for (int t = 0; t < D_G_A; t++) Ghat(i, t) = g_run[t];
        }
    }
};

// [[Rcpp::export]]
NumericMatrix mh_tilted_average_A_cpp(
    NumericVector Mstar, NumericVector V, NumericVector Wt, NumericVector tau_rho,
    IntegerVector row_id, NumericVector beta,
    double lambda, double delta0, double delta1, double delta2, double eta,
    NumericVector gamma,
    IntegerVector corner,
    int n_burn, int n_keep,
    int base_seed = 20260829
) {
    int n = Mstar.size();
    if (gamma.size() != D_G_A) stop("gamma length must equal 8 (moment set A)");
    if (corner.size() != n) stop("corner length must equal n");
    if (row_id.size() != n) stop("row_id length must equal n");
    if (eta < 0.0 || eta >= 1.0) stop("eta must be in [0,1)");

    NumericMatrix Ghat(n, D_G_A);
    TiltedMomentWorkerA worker(
        Mstar, V, Wt, tau_rho, beta, gamma, row_id, corner,
        lambda, delta0, delta1, delta2, eta, n_burn, n_keep, base_seed, Ghat
    );
    RcppParallel::parallelFor(0, n, worker);
    return Ghat;
}

// ---- Auxiliary-parameter diagnostic: E[e] and Med[e], moment set A -------
// (2026-08-31) Schennach's own device (her E[U] worked example, ELVIS.pdf
// p.1250-analog in the JEL survey -- see CLAUDE.md's 2026-08-25 citation-
// correction entry) applied to e(M) instead of a generic U: since e(M) is
// already computed inside TiltedMomentWorkerA's chain (via moment_g_A_one),
// evaluating its tilted average is a POST-HOC auxiliary-parameter step at an
// ALREADY-FITTED (theta_hat, gamma_hat) -- no re-optimization, same chain
// mechanics (independence-sampler MH: M_try is drawn i.i.d. from rho every
// step regardless of the current state, so the acceptance test only decides
// which g/e gets carried forward and averaged -- M_current itself, like in
// TiltedMomentWorkerA, is never needed after the first draw).
//
// E[e]-mu_e=0 (Schennach's linear device) is a running per-firm mean, free.
// A literal "Med[e]-theta_e=0" is NOT itself a valid moment (there is no
// expectation of a median) -- the moment that actually identifies a median
// is the indicator/quantile-GMM one, E[1(e<=theta_e)-0.5]=0 (Koenker's
// check-function FOC); solving E[e-theta]=0 for theta always recovers the
// MEAN regardless of the label. Practically this means pooling actual
// (subsampled, thinned) post-burn-in e draws across firms and taking their
// empirical median, rather than any per-firm running statistic -- pools
// n_pool evenly-spaced draws per firm (not the full n_keep, to bound
// output size: n x n_pool, not n x n_keep) into a flat vector for the
// caller to summarize (R's own median()) as the AGGREGATE, population-level
// median of e across all firms -- matching build_lambda_grid's own
// "median firm" framing (a population quantile of M*), not a per-firm
// conditional median.
struct TiltedEDiagWorkerA : public Worker {
    const RVector<double>  Mstar, V, Wt, tau_rho, beta, gamma;
    const RVector<int>     row_id, corner;
    const double lambda, delta0, delta1, delta2, eta;
    const int n_burn, n_keep, n_pool, base_seed;
    RVector<double> e_mean_out;
    RMatrix<double> e_pool_out;

    TiltedEDiagWorkerA(
        const NumericVector& Mstar_, const NumericVector& V_, const NumericVector& Wt_, const NumericVector& tau_rho_,
        const NumericVector& beta_, const NumericVector& gamma_,
        const IntegerVector& row_id_, const IntegerVector& corner_,
        double lambda_, double delta0_, double delta1_, double delta2_, double eta_,
        int n_burn_, int n_keep_, int n_pool_, int base_seed_,
        NumericVector& e_mean_out_, NumericMatrix& e_pool_out_
    ) : Mstar(Mstar_), V(V_), Wt(Wt_), tau_rho(tau_rho_), beta(beta_), gamma(gamma_),
        row_id(row_id_), corner(corner_),
        lambda(lambda_), delta0(delta0_), delta1(delta1_), delta2(delta2_), eta(eta_),
        n_burn(n_burn_), n_keep(n_keep_), n_pool(n_pool_), base_seed(base_seed_),
        e_mean_out(e_mean_out_), e_pool_out(e_pool_out_) {}

    void operator()(std::size_t begin, std::size_t end) {
        int pool_stride = std::max(1, n_keep / n_pool);
        for (std::size_t ii = begin; ii < end; ii++) {
            int i = static_cast<int>(ii);

            if (corner[i] == 1) {
                // structural non-evader: e=0 exactly, every pooled slot is 0 too
                e_mean_out[i] = 0.0;
                for (int p = 0; p < n_pool; p++) e_pool_out(i, p) = 0.0;
                continue;
            }

            std::mt19937_64 rng(static_cast<uint64_t>(base_seed) + static_cast<uint64_t>(row_id[i]));
            std::uniform_real_distribution<double> unif(0.0, 1.0);

            GVecA g_current, g_try;
            double M_current = draw_from_rho_eta(unif(rng), Mstar[i], lambda, eta);
            moment_g_A_one(M_current, Mstar[i], V[i], Wt[i], tau_rho[i],
                           beta[i], lambda, delta0, delta1, delta2, g_current);
            double e_current = e_of_M(M_current, Mstar[i]);

            double e_sum = 0.0;
            int pool_idx = 0;

            for (int r = -n_burn + 1; r <= n_keep; r++) {
                double M_try = draw_from_rho_eta(unif(rng), Mstar[i], lambda, eta);
                moment_g_A_one(M_try, Mstar[i], V[i], Wt[i], tau_rho[i],
                               beta[i], lambda, delta0, delta1, delta2, g_try);
                double e_try = e_of_M(M_try, Mstar[i]);

                double log_ratio = 0.0;
                for (int t = 0; t < D_G_A; t++) log_ratio += gamma[t] * (g_try[t] - g_current[t]);

                if (std::log(unif(rng)) < log_ratio) {
                    g_current = g_try;
                    e_current = e_try;
                }
                if (r > 0) {
                    e_sum += e_current / n_keep;
                    if ((r - 1) % pool_stride == 0 && pool_idx < n_pool) {
                        e_pool_out(i, pool_idx) = e_current;
                        pool_idx++;
                    }
                }
            }
            e_mean_out[i] = e_sum;
            while (pool_idx < n_pool) { e_pool_out(i, pool_idx) = e_current; pool_idx++; }
        }
    }
};

// [[Rcpp::export]]
List tilted_e_diag_A_cpp(
    NumericVector Mstar, NumericVector V, NumericVector Wt, NumericVector tau_rho,
    IntegerVector row_id, NumericVector beta,
    double lambda, double delta0, double delta1, double delta2, double eta,
    NumericVector gamma,
    IntegerVector corner,
    int n_burn, int n_keep, int n_pool = 20,
    int base_seed = 20260831
) {
    int n = Mstar.size();
    if (gamma.size() != D_G_A) stop("gamma length must equal 8 (moment set A)");
    if (corner.size() != n) stop("corner length must equal n");
    if (row_id.size() != n) stop("row_id length must equal n");
    if (eta < 0.0 || eta >= 1.0) stop("eta must be in [0,1)");

    NumericVector e_mean(n);
    NumericMatrix e_pool(n, n_pool);
    TiltedEDiagWorkerA worker(
        Mstar, V, Wt, tau_rho, beta, gamma, row_id, corner,
        lambda, delta0, delta1, delta2, eta, n_burn, n_keep, n_pool, base_seed,
        e_mean, e_pool
    );
    RcppParallel::parallelFor(0, n, worker);
    return List::create(Named("e_mean") = e_mean, Named("e_pool") = e_pool);
}

// ---- Auxiliary-parameter diagnostic: pooled e AND omega, moment set A ----
// (2026-09-01) A new function, not a modification of TiltedEDiagWorkerA/
// tilted_e_diag_A_cpp above -- zero risk to that already-validated code.
// Same chain mechanics, same post-hoc-at-fitted-(theta,gamma) logic, just
// also tracking omega_current alongside e_current so both can be pooled in
// ONE pass (no reason to run the chain twice). Requested to compare
// omega_star=delta1/(2*delta2) against the ACTUAL tilted omega distribution
// (is it near the median firm, or a tail firm?), and to get e's upper-tail
// percentiles (p80/p90/p95/p99) for a detection-probability check beyond
// just the mean/median already in 1213.
struct TiltedEOmegaDiagWorkerA : public Worker {
    const RVector<double>  Mstar, V, Wt, tau_rho, beta, gamma;
    const RVector<int>     row_id, corner;
    const double lambda, delta0, delta1, delta2, eta;
    const int n_burn, n_keep, n_pool, base_seed;
    RMatrix<double> e_pool_out, omega_pool_out;

    TiltedEOmegaDiagWorkerA(
        const NumericVector& Mstar_, const NumericVector& V_, const NumericVector& Wt_, const NumericVector& tau_rho_,
        const NumericVector& beta_, const NumericVector& gamma_,
        const IntegerVector& row_id_, const IntegerVector& corner_,
        double lambda_, double delta0_, double delta1_, double delta2_, double eta_,
        int n_burn_, int n_keep_, int n_pool_, int base_seed_,
        NumericMatrix& e_pool_out_, NumericMatrix& omega_pool_out_
    ) : Mstar(Mstar_), V(V_), Wt(Wt_), tau_rho(tau_rho_), beta(beta_), gamma(gamma_),
        row_id(row_id_), corner(corner_),
        lambda(lambda_), delta0(delta0_), delta1(delta1_), delta2(delta2_), eta(eta_),
        n_burn(n_burn_), n_keep(n_keep_), n_pool(n_pool_), base_seed(base_seed_),
        e_pool_out(e_pool_out_), omega_pool_out(omega_pool_out_) {}

    void operator()(std::size_t begin, std::size_t end) {
        int pool_stride = std::max(1, n_keep / n_pool);
        for (std::size_t ii = begin; ii < end; ii++) {
            int i = static_cast<int>(ii);

            if (corner[i] == 1) {
                // structural non-evader: M=Mstar known exactly, e=0, omega deterministic
                double om_pt = omega_of_M(Mstar[i], Mstar[i], V[i], Wt[i], beta[i]);
                for (int p = 0; p < n_pool; p++) { e_pool_out(i, p) = 0.0; omega_pool_out(i, p) = om_pt; }
                continue;
            }

            std::mt19937_64 rng(static_cast<uint64_t>(base_seed) + static_cast<uint64_t>(row_id[i]));
            std::uniform_real_distribution<double> unif(0.0, 1.0);

            GVecA g_current, g_try;
            double M_current = draw_from_rho_eta(unif(rng), Mstar[i], lambda, eta);
            moment_g_A_one(M_current, Mstar[i], V[i], Wt[i], tau_rho[i],
                           beta[i], lambda, delta0, delta1, delta2, g_current);
            double e_current  = e_of_M(M_current, Mstar[i]);
            double om_current = omega_of_M(M_current, Mstar[i], V[i], Wt[i], beta[i]);

            int pool_idx = 0;

            for (int r = -n_burn + 1; r <= n_keep; r++) {
                double M_try = draw_from_rho_eta(unif(rng), Mstar[i], lambda, eta);
                moment_g_A_one(M_try, Mstar[i], V[i], Wt[i], tau_rho[i],
                               beta[i], lambda, delta0, delta1, delta2, g_try);
                double e_try  = e_of_M(M_try, Mstar[i]);
                double om_try = omega_of_M(M_try, Mstar[i], V[i], Wt[i], beta[i]);

                double log_ratio = 0.0;
                for (int t = 0; t < D_G_A; t++) log_ratio += gamma[t] * (g_try[t] - g_current[t]);

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
List tilted_e_omega_diag_A_cpp(
    NumericVector Mstar, NumericVector V, NumericVector Wt, NumericVector tau_rho,
    IntegerVector row_id, NumericVector beta,
    double lambda, double delta0, double delta1, double delta2, double eta,
    NumericVector gamma,
    IntegerVector corner,
    int n_burn, int n_keep, int n_pool = 20,
    int base_seed = 20260901
) {
    int n = Mstar.size();
    if (gamma.size() != D_G_A) stop("gamma length must equal 8 (moment set A)");
    if (corner.size() != n) stop("corner length must equal n");
    if (row_id.size() != n) stop("row_id length must equal n");
    if (eta < 0.0 || eta >= 1.0) stop("eta must be in [0,1)");

    NumericMatrix e_pool(n, n_pool), omega_pool(n, n_pool);
    TiltedEOmegaDiagWorkerA worker(
        Mstar, V, Wt, tau_rho, beta, gamma, row_id, corner,
        lambda, delta0, delta1, delta2, eta, n_burn, n_keep, n_pool, base_seed,
        e_pool, omega_pool
    );
    RcppParallel::parallelFor(0, n, worker);
    return List::create(Named("e_pool") = e_pool, Named("omega_pool") = omega_pool);
}

// ---- Moment set B moved to 1200-stage2-elvis-B.cpp (2026-08-31) -------
// -- moment_g_B_one, TiltedMomentWorkerB, mh_tilted_average_B_cpp all live
// there now, so that file alone can take on an Accelerate/CBLAS dependency
// without requiring it here too. See that file's header comment.

// ---- diagnostic: per-firm TILTED-AVERAGE omega(M), added 2026-08-29 -------
// Separate function, does NOT touch mh_tilted_average_cpp or its Worker --
// zero risk to the live estimator. Runs the EXACT SAME per-firm Metropolis
// chain (same RNG seeding, same rho, same tilt/acceptance rule using the
// SAME 15-dim g(M) and gamma) so the reported omega distribution reflects
// what the real estimation is actually drawing under a given (theta,gamma)
// -- not a proxy from CORP firms, which is invalid for omega specifically
// (unlike epsilon, the AR(1) innovation eta is NOT assumed independent of
// corp/non-corp status, so CORP's omega distribution doesn't generalize to
// the unincorp estimation sample). Returns, per firm: tilted-average omega,
// omega^2 (for a per-firm variance proxy), e, and M -- diagnostic only, not
// part of any moment condition. Serial (not RcppParallel) -- this is an
// occasional diagnostic call, not something run hundreds of times inside
// BOBYQA, so the thread-pool setup isn't worth the added complexity here.
//
// [[Rcpp::export]]
NumericMatrix tilted_omega_diag_cpp(
    NumericVector Mstar, NumericVector V, NumericVector Wt, NumericVector tau_rho,
    NumericVector eps_mu1, NumericVector eps_var, NumericVector eps_mu3,
    IntegerVector row_id, NumericVector beta,
    double lambda, double delta0, double delta1, double delta2, double sigma2_psi,
    NumericVector gamma,
    IntegerVector corner,
    int n_burn, int n_keep,
    int base_seed = 20260827
) {
    int n = Mstar.size();
    if (gamma.size() != D_G) stop("gamma length must equal 15");
    if (corner.size() != n) stop("corner length must equal n");
    if (row_id.size() != n) stop("row_id length must equal n");

    NumericMatrix out(n, 4);
    colnames(out) = CharacterVector::create("omega_mean", "omega2_mean", "e_mean", "M_mean");

    for (int i = 0; i < n; i++) {
        if (corner[i] == 1) {
            double om_pt = omega_of_M(Mstar[i], Mstar[i], V[i], Wt[i], beta[i]);
            out(i, 0) = om_pt; out(i, 1) = om_pt * om_pt; out(i, 2) = 0.0; out(i, 3) = Mstar[i];
            continue;
        }

        std::mt19937_64 rng(static_cast<uint64_t>(base_seed) + static_cast<uint64_t>(row_id[i]));
        std::uniform_real_distribution<double> unif(0.0, 1.0);

        GVec g_current, g_try;
        double M_current = draw_from_rho(unif(rng), Mstar[i], lambda);
        moment_g_one(M_current, Mstar[i], V[i], Wt[i], tau_rho[i],
                     beta[i], lambda, delta0, delta1, delta2, sigma2_psi,
                     eps_mu1[i], eps_var[i], eps_mu3[i], g_current);

        double om_sum = 0.0, om2_sum = 0.0, e_sum = 0.0, M_sum = 0.0;

        for (int r = -n_burn + 1; r <= n_keep; r++) {
            double M_try = draw_from_rho(unif(rng), Mstar[i], lambda);
            moment_g_one(M_try, Mstar[i], V[i], Wt[i], tau_rho[i],
                         beta[i], lambda, delta0, delta1, delta2, sigma2_psi,
                         eps_mu1[i], eps_var[i], eps_mu3[i], g_try);

            double log_ratio = 0.0;
            for (int t = 0; t < D_G; t++) log_ratio += gamma[t] * (g_try[t] - g_current[t]);

            if (std::log(unif(rng)) < log_ratio) {
                g_current = g_try;
                M_current = M_try;
            }
            if (r > 0) {
                double om_c = omega_of_M(M_current, Mstar[i], V[i], Wt[i], beta[i]);
                double e_c  = e_of_M(M_current, Mstar[i]);
                om_sum  += om_c / n_keep;
                om2_sum += om_c * om_c / n_keep;
                e_sum   += e_c / n_keep;
                M_sum   += M_current / n_keep;
            }
        }
        out(i, 0) = om_sum; out(i, 1) = om2_sum; out(i, 2) = e_sum; out(i, 3) = M_sum;
    }
    return out;
}
