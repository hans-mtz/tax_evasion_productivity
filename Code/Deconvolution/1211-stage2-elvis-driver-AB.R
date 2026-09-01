## Stage-2 ELVIS driver, moment sets A and B (2026-08-29) -------------------
## Supersedes 1210-stage2-elvis-driver.R's 15-moment system for new runs
## (that file/system is kept for comparison, not deleted). Two corrections
## motivated this redesign -- both verified against Schennach (2014) directly
## (ELVIS.pdf pp.348-350, p.372) and her GAUSS reference code
## (Lit-Papers/ELVIS_code/) before writing anything -- see
## Code/Rcpp/1200-stage2-elvis.cpp's "Moment sets A and B" header comment for
## the full derivation and CLAUDE.md's 2026-08-29 entry for the chat record:
##   (i) gamma's dimension is NOT counted against the order condition (it's a
##       Lagrange multiplier mechanically sized to d_g, solved jointly with
##       nuisance params -- not a structural unknown). Real order condition:
##       d_theta_smooth (incl. nuisance) vs d_g.
##   (ii) gamma should be UNBOUNDED, not box-constrained -- Schennach
##       (ELVIS.pdf p.359) and AK2020 both expect/allow gamma->infinity
##       (degenerate limiting conditional distribution of M is a valid
##       solution, not a numerical failure). Confirmed nloptr::bobyqa handles
##       lower=-Inf/upper=Inf correctly (toy quadratic test, session log).
##
## Moment set A (8 moments, theta_smooth=(delta0,delta1,delta2)):
##   psi, eps, psi*lnM, psi*om, psi*om^2, eps*lnM, eps*e, eps*om
## Moment set B (A's 8 + 7 more, theta_smooth=(delta0,delta1,delta2,mu_m[1:J])):
##   one industry-indicator row per industry defining mu_m[j] (jointly
##   estimated -- ln M has no external proxy, unlike omega), plus
##   om-mu_omega, om^2-sigma_omega, psi*(om-mu_omega), psi*(om^2-sigma_omega),
##   psi*(lnM-mu_m[j]), psi*eps*om -- mu_omega/sigma_omega are EXTERNALLY,
##   cheaply computed per-industry targets (1207-stage2-omega-targets.R), not
##   jointly estimated, since tilde_cal_W=omega+(1-beta)*eps is directly
##   observed for unincorp firms too.
##
## Two-pass BOBYQA refinement (AK2020's own practice): run once from
## par_init, then re-run warm-started from the first result, SAME (unbounded
## gamma) bounds -- not narrowed -- as a cheap convergence check. Both
## results are reported; the second (refined) is what's used downstream.

library(tidyverse)
library(Rcpp)
library(nloptr)
library(RcppParallel)

source("Code/Deconvolution/utils-cli.R")
sourceCpp("Code/Rcpp/1200-stage2-elvis.cpp")
load("Code/Products/1200-stage2-data.RData")            # stage2_data
load("Code/Products/1205-stage2-warmstart.RData")       # warmstart_lag_m, warmstart_lag2W (delta0-2 reused; sigma2_psi unused here)
load("Code/Products/1207-stage2-omega-targets.RData")   # omega_targets (moment set B only)

## %% Build the estimation sample for one ins --------------------------------
## (no eps-target join here -- neither A nor B uses per-industry eps
## centering; that was specific to the superseded 15-moment system)

build_run_sample <- function(ins_choice, data = stage2_data, corner_mode = c("drop", "include_zero")) {
    corner_mode <- match.arg(corner_mode)

    base <- data %>% dplyr::filter(ins == ins_choice, !corp)

    interior <- base %>%
        dplyr::filter(is.finite(sales_tax_rate_purchases), sales_tax_rate_purchases > 0) %>%
        mutate(corner = 0L)

    out <- if (corner_mode == "drop") {
        interior
    } else {
        corner_obs <- base %>%
            dplyr::filter(is.finite(sales_tax_rate_purchases), sales_tax_rate_purchases == 0) %>%
            mutate(corner = 1L)
        bind_rows(interior, corner_obs)
    }

    out %>%
        dplyr::select(plant, year, sic_3, M_star, cal_V, tilde_cal_W, sales_tax_rate_purchases, beta, corner) %>%
        dplyr::left_join(
            dplyr::filter(omega_targets, ins == ins_choice) %>% dplyr::select(sic_3, mu_omega, sigma_omega),
            by = "sic_3"
        )
}

## %% Industry indexing for moment set B (0-based, fixed once per run) ------

add_industry_idx <- function(run_sample) {
    sic_levels <- sort(unique(run_sample$sic_3))
    run_sample$industry_idx <- match(run_sample$sic_3, sic_levels) - 1L
    attr(run_sample, "sic_levels") <- sic_levels
    run_sample
}

## %% CUE objective, both moment sets -----------------------------------

cue_objective_common <- function(Ghat) {
    dvec  <- colMeans(Ghat)
    Omega <- cov(Ghat)
    eig   <- eigen(Omega, symmetric = TRUE)
    pos   <- eig$values > 1e-8 * max(eig$values)
    A     <- eig$vectors[, pos, drop = FALSE]
    d2    <- as.numeric(t(A) %*% dvec)
    ## A's columns are Omega's own eigenvectors, so t(A) %*% Omega %*% A is
    ## exactly diagonal (=diag(eig$values[pos])) by orthonormality -- verified
    ## numerically to ~1e-15. solve()-ing that diagonal matrix was redundant;
    ## dividing by the eigenvalues directly is the same generalized inverse,
    ## just without the wasted matrix build + inversion (2026-08-31).
    0.5 * sum(d2^2 / eig$values[pos])
}

## eta (2026-08-29): relative-evasion floor, M>=eta*Mstar i.e. e<=(1-eta)*Mstar,
## added as a genuine theta_smooth member (NOT part of gamma/g() -- an AK2020
## Theorem-4-style sampler restriction, see the .cpp file's header for why).
## Guards against the CUE objective's eigenvalue-truncated weighting muting,
## rather than penalizing, a tilt that pushes M towards 0 (where eps/lnM
## diverge) -- economically, M=0 cannot coexist with the positive output
## every firm in this data reports. eta=0 recovers the pre-existing (M>=0
## only) behavior exactly, so the data can reveal no extra floor is needed.
cue_objective_A <- function(par, lambda, dat, n_burn, n_keep) {
    delta0 <- par[1]; delta1 <- par[2]; delta2 <- par[3]; eta <- par[4]
    gamma  <- par[-(1:4)]
    Ghat <- mh_tilted_average_A_cpp(
        dat$M_star, dat$cal_V, dat$tilde_cal_W, dat$sales_tax_rate_purchases,
        dat$.row_id, dat$beta, lambda, delta0, delta1, delta2, eta, gamma, dat$corner,
        n_burn, n_keep
    )
    cue_objective_common(Ghat)
}

## par = (delta0,delta1,delta2,eta, mu_m[1:J], gamma[1:(8+J+6)]) -- eta added
## 2026-08-29 (same day), mirroring A: same sampler-restriction mechanism
## (draw_from_rho_eta), not a gamma-weighted row -- see the .cpp file's "eta
## extended to B" header note for why this was added despite mu_m,j already
## providing a mean-level guard.
cue_objective_B <- function(par, lambda, dat, J, n_burn, n_keep) {
    delta0 <- par[1]; delta1 <- par[2]; delta2 <- par[3]; eta <- par[4]
    mu_m   <- par[5:(4 + J)]
    gamma  <- par[-(1:(4 + J))]
    Ghat <- mh_tilted_average_B_cpp(
        dat$M_star, dat$cal_V, dat$tilde_cal_W, dat$sales_tax_rate_purchases,
        dat$beta, dat$mu_omega, dat$sigma_omega,
        dat$.row_id, dat$corner, dat$industry_idx,
        lambda, delta0, delta1, delta2, eta, gamma, mu_m,
        n_burn, n_keep
    )
    cue_objective_common(Ghat)
}

## %% Bounds ------------------------------------------------------------
## gamma: UNBOUNDED (+-Inf) for both moment sets, per point (ii) above.
## delta0-2: kept at a finite box (real structural parameters, not tilting
## machinery) -- same +-20 used throughout this project's stage-2 code.
## mu_m,j (set B only): M<=M* always (e>=0 structurally) => ln(M)<=ln(M*)
## pointwise for every firm => E[lnM|j]<=E[lnM*|j] exactly -- so the upper
## bound IS the mean itself (tiny epsilon below for numerical safety), no
## slack above it makes sense. Bounded per-industry as [mean(log M*|j)-8,
## mean(log M*|j)-0.01] -- generous slack on the low side (M can be much
## smaller than M* under heavy evasion), essentially none on the high side
## since that's a hard structural ceiling, not an estimation-noise buffer
## (corrected 2026-08-29, same day -- the original "+1 above the mean" had
## this backwards and let a few industries' mu_m,j sit somewhere they
## structurally cannot be).

## Widened 2026-08-29 from 20 to 60 (same day): B's quantile scan showed
## delta0 pinned exactly at -20 (and delta1/delta2 near +-20) for lag_m at
## every lambda past the smallest -- not real optima, wherever the box wall
## stopped BOBYQA. Confined to large lambda (already the uninteresting/
## plateau region of Lhat, not the small-lambda candidate optimum), and only
## lag_m, not lag_2_cal_W -- but widened regardless so large-lambda B results
## aren't silently box-constrained.
DELTA_BOUND <- 60

## No feasibility filter on M_star vs. lambda (removed 2026-08-29): with
## draw_from_rho's support now correctly truncated at M>=0 (see
## Code/Rcpp/1200-stage2-elvis.cpp), every firm has a valid, nonempty support
## at every lambda -- there is no longer any firm that legitimately needs to
## be dropped for feasibility. Previously, dropping firms with
## M_star<1/(2*lambda) instead of truncating their support caused sample size
## to swing sharply and non-monotonically with lambda (confirmed in-session:
## a >2x difference in n_used between corner_mode="drop" and "include_zero"
## at the smallest lambda grid point, entirely an artifact of the interior
## sample shrinking, not of how many corner firms were added).

## Diagnostic (2026-08-29, "keep an eye on it" per the user): fraction of
## non-corner firms at a given lambda whose NATURAL FOC-domain floor
## (Mstar-1/(2*lambda)) is already <=0 -- i.e. where the physical/eta floor,
## not the FOC-domain ceiling, is what actually binds their support. Purely
## descriptive (independent of eta's fitted value); large fractions flag a
## lambda where a big chunk of the sample is being pushed toward the
## boundary regime this session's fix/floor is about.
boundary_fraction <- function(dat, lambda) {
    interior <- dat$corner == 0
    if (!any(interior)) return(NA_real_)
    mean(dat$M_star[interior] <= 1 / (2 * lambda))
}

fit_one_lambda_A <- function(lambda, run_sample, n_burn, n_keep, par_init) {
    dat <- run_sample
    n_dropped <- 0L
    p_boundary <- boundary_fraction(dat, lambda)

    obj <- function(par) cue_objective_A(par, lambda, dat, n_burn, n_keep)
    lower <- c(rep(-DELTA_BOUND, 3), 0,     rep(-Inf, 8))
    upper <- c(rep( DELTA_BOUND, 3), 0.999, rep( Inf, 8))

    res1 <- nloptr::bobyqa(x0 = par_init, fn = obj, lower = lower, upper = upper)
    res2 <- nloptr::bobyqa(x0 = res1$par, fn = obj, lower = lower, upper = upper)   # AK2020-style refinement pass

    list(lambda = lambda, value = res2$value, par = res2$par, n = nrow(dat), n_dropped = n_dropped,
         p_boundary = p_boundary,
         convergence = res2$convergence, iter = res2$iter, message = res2$message,
         value1 = res1$value, delta_value = res2$value - res1$value,
         max_abs_gamma = max(abs(res2$par[-(1:4)])))
}

## maxeval (2026-08-29): nloptr::bobyqa's default (1000) is dimension-
## INDEPENDENT -- fine for A's 12-dim inner problem (converges in ~150-250
## evals in practice), but B's inner problem is 4+J+(8+J+6) ~ 75-dim, and
## every B smoke test hit conv=5 (the maxeval cap, not real convergence) at
## every single lambda point. Raised here, not for A (which doesn't need
## it) -- a modest bump first (not jumping straight to something drastic),
## to see whether that alone is enough before spending more compute.
fit_one_lambda_B <- function(lambda, run_sample, J, mu_m_lower, mu_m_upper, n_burn, n_keep, par_init,
                              maxeval = 1000) {
    dat <- run_sample
    n_dropped <- 0L
    p_boundary <- boundary_fraction(dat, lambda)

    d_g <- 8 + J + 6
    obj <- function(par) cue_objective_B(par, lambda, dat, J, n_burn, n_keep)
    lower <- c(rep(-DELTA_BOUND, 3), 0,     mu_m_lower, rep(-Inf, d_g))
    upper <- c(rep( DELTA_BOUND, 3), 0.999, mu_m_upper, rep( Inf, d_g))

    res1 <- nloptr::bobyqa(x0 = par_init, fn = obj, lower = lower, upper = upper, control = list(maxeval = maxeval))
    res2 <- nloptr::bobyqa(x0 = res1$par, fn = obj, lower = lower, upper = upper, control = list(maxeval = maxeval))

    gamma_idx <- (4 + J + 1):(4 + J + d_g)
    list(lambda = lambda, value = res2$value, par = res2$par, n = nrow(dat), n_dropped = n_dropped,
         p_boundary = p_boundary,
         convergence = res2$convergence, iter = res2$iter, message = res2$message,
         value1 = res1$value, delta_value = res2$value - res1$value,
         max_abs_gamma = max(abs(res2$par[gamma_idx])))
}

## %% Lambda grid: two mechanisms -----------------------------------------
## (1) quantile-based (unchanged from 1210) -- cheap first-pass scan across
##     the plausible range, but badly uneven in lambda-space itself: M_star
##     is heavy-tailed, so quantiles barely move through most of the
##     distribution and then explode near the top -- a probability range as
##     wide as [0.9999,0.9] maps to a >300x range in lambda, unevenly spaced
##     (confirmed in-session, 2026-08-29). Good for finding a region of
##     interest, not for reading the SHAPE of Lhat(lambda) once found.
## (2) explicit/linear -- an arithmetically-equally-spaced grid in lambda
##     itself (Schennach's own convention for her theta grid, `theta=theta+
##     0.1`), for refining around a region already identified by (1). Passed
##     directly via the lambda_grid CLI arg (comma-separated raw values),
##     bypassing build_lambda_grid/quantiles entirely when supplied.

# build_lambda_grid <- function(run_sample, probs = c(0.9, 0.75, 0.5, 0.25, 0.1)) {
#     e_max <- quantile(run_sample$M_star, probs = probs)
#     1 / (2 * e_max)
# }

build_lambda_grid <- function(run_sample, probs = seq(0.01, 0.5, length.out = 5), ref_stat = 0.5) {
## The most aggressive tax evader stops below 1/(2lambda), max(e) < 1/(2lambda)
## since I do not observe e, but M^*=M+e, since M^* has heavy tails, I would look at the median
## at the median, I do not expect firms to be evading at the audit triggering threshold
## Thus, I expec Med(e)*lambda < 0.5, thus for a probability of detection p at the median, lambda < p/Med(e)
## I left ref_stat as a parameter in case I want to change from median to an upper quantile
## Safety check: for linear prob of detection, max p=0.5
    if (max(probs) > 0.5) {
        warning("max(probs) should be <= 0.5 for linear prob of detection, only probs <= 0.5 were evaluated")
    }
    probs <- probs[probs <= 0.5]
    probs / quantile(run_sample$M_star, probs = ref_stat)[1] 
}

## %% Local-minimum bracketing for the linear refinement grid ---------------
## Given a completed (sorted) lambda grid and its Lhat values, find the
## argmin and bracket it with its immediate neighbors -- interior case uses
## the neighbors directly; an EDGE-case minimum (argmin at either end of the
## tested grid, meaning the true minimum may lie further out) extends past
## that edge by the SAME MULTIPLICATIVE ratio as the adjacent gap, not the
## same additive gap -- our lambda grids are built log/ratio-spaced (heavy-
## tailed M_star), so an additive extension at the small-lambda end can go
## negative or land absurdly close to zero; a ratio-based extension stays on
## the same natural scale. Returns a list(lo, hi, i_min) -- the caller builds
## the actual arithmetically-equally-spaced grid from (lo, hi).
# bracket_local_min <- function(lambda_grid, Lhat) {
#     stopifnot(length(lambda_grid) == length(Lhat), all(diff(lambda_grid) > 0))
#     ok <- is.finite(Lhat)
#     lambda_grid <- lambda_grid[ok]; Lhat <- Lhat[ok]
#     n <- length(lambda_grid)
#     i_min <- which.min(Lhat)

#     lo <- if (i_min == 1) {
#         ratio <- lambda_grid[2] / lambda_grid[1]
#         lambda_grid[1] / ratio
#     } else {
#         lambda_grid[i_min - 1]
#     }
#     hi <- if (i_min == n) {
#         ratio <- lambda_grid[n] / lambda_grid[n - 1]
#         lambda_grid[n] * ratio
#     } else {
#         lambda_grid[i_min + 1]
#     }
#     list(lo = lo, hi = hi, i_min = i_min, lambda_at_min = lambda_grid[i_min])
# }

bracket_local_min <- function(lambda_grid, Lhat, run_sample, ref_stat = 0.5) {
    ## Build bracket in terms of probability space, rather than lambda space.
    ## Inverts build_lambda_grid()'s own transform (lambda = probs /
    ## quantile(M_star, ref_stat)), so ref_stat here must match whatever
    ## ref_stat built lambda_grid in the first place -- both default to the
    ## median (0.5), consistent with build_lambda_grid()'s own default.
    ##
    ## 2026-08-31 fix: was `min(median(run_sample$M_star), 1)`, which always
    ## evaluates to 1 given this data's scale (median(M_star)~13,780 >> 1) --
    ## a stray cap that silently broke the probability-space conversion
    ## (probs_median ended up equal to lambda_grid itself, not recovered
    ## probabilities), producing a NEGATIVE lambda in the edge-case branch
    ## (measured -5.92e-10 on a toy edge-case test before this fix). Verified
    ## not yet wired into run_stage2_elvis_AB (no call sites), so no live
    ## results were affected.
    stopifnot(length(lambda_grid) == length(Lhat), all(diff(lambda_grid) > 0))
    ok <- is.finite(Lhat)
    lambda_grid <- lambda_grid[ok]; Lhat <- Lhat[ok]
    n <- length(lambda_grid)
    i_min <- which.min(Lhat)
    probs_median <- quantile(run_sample$M_star, probs = ref_stat)[[1]] * lambda_grid
    ## Lower edge: additive extension in probability space can still cross
    ## p=0 (e.g. build_lambda_grid()'s own default probs=seq(0.01,0.5,...)
    ## extends to -0.1125 additively, verified in-session) -- same failure
    ## mode the lambda-space version was already rejected for, one level up.
    ## Ratio-based extension (mirroring the superseded lambda-space version
    ## just below, ported into probability space) guarantees p_lo>0 since
    ## probs_median is strictly positive by construction.
    lo <- if (i_min == 1) {
        ratio <- probs_median[2] / probs_median[1]
        p_lo  <- probs_median[1] / ratio
        build_lambda_grid(run_sample, probs = p_lo, ref_stat = ref_stat)
    } else {
        lambda_grid[i_min - 1]
    }
    ## Upper edge: an even more likely case in practice than the lower edge
    ## -- this project's own real grid runs have repeatedly shown Lhat still
    ## falling at the largest lambda tried (CLAUDE.md, 2026-08-27/28 entries).
    ## Additive extension can push p above 0.5 (the model's own FOC ceiling);
    ## build_lambda_grid()'s own filter (probs[probs<=0.5]) would then return
    ## an EMPTY vector for a single out-of-range scalar, not just a wrong
    ## number -- breaking hi entirely. Ratio extension, capped at 0.5 (a
    ## legitimate boundary to extend to -- p=0.5 itself stays valid, only
    ## p>0.5 is filtered), keeps p_hi always in (0,0.5].
    hi <- if (i_min == n) {
        ratio <- probs_median[n] / probs_median[n - 1]
        p_hi  <- min(probs_median[n] * ratio, 0.5)
        build_lambda_grid(run_sample, probs = p_hi, ref_stat = ref_stat)
    } else {
        lambda_grid[i_min + 1]
    }
    list(lo = lo, hi = hi, i_min = i_min, lambda_at_min = lambda_grid[i_min])
}

## Combine brackets across multiple runs (e.g. both ins choices) that will
## share ONE linear refinement grid -- widest union of the individual
## brackets, so neither run's local minimum falls outside the refined range.
build_linear_refinement <- function(brackets, n_points = 10) {
    lo <- min(sapply(brackets, `[[`, "lo"))
    hi <- max(sapply(brackets, `[[`, "hi"))
    seq(lo, hi, length.out = n_points)
}

## %% Run for one ins choice, one moment_set ----------------------------

run_stage2_elvis_AB <- function(ins_choice, moment_set = c("A", "B"), warmstart,
                                 n_burn = 50, n_keep = 500,
                                 grid_probs = c(0.5, 0.25, 0.1, 0.05, 0.01),
                                 lambda_grid_override = NULL,   # explicit lambda values; NULL = quantile-based
                                 corner_mode = c("drop", "include_zero"),
                                 maxeval_b = 1000,
                                 refine = TRUE, refine_points = 10) {
    moment_set  <- match.arg(moment_set)
    corner_mode <- match.arg(corner_mode)

    run_sample <- build_run_sample(ins_choice, corner_mode = corner_mode) %>%
        mutate(.row_id = dplyr::row_number())
    if (moment_set == "B") run_sample <- add_industry_idx(run_sample)
    lambda_grid <- if (!is.null(lambda_grid_override)) {
        sort(lambda_grid_override)
    } else {
        sort(build_lambda_grid(dplyr::filter(run_sample, corner == 0), grid_probs))
    }

    delta_init <- c(warmstart$par[["delta0"]], warmstart$par[["delta1"]], warmstart$par[["delta2"]])

    ## %% Quantile-grid pass, then an automatic linear-refinement pass ------
    ## (2026-08-31: wires in bracket_local_min/build_linear_refinement, fixed
    ## same day -- see their own header comments). Only runs when the caller
    ## didn't already supply an explicit lambda_grid_override -- an explicit
    ## grid IS itself meant to be the refined/manual phase 2 (see the
    ## build_lambda_grid header comment on the two-phase design), so
    ## auto-refining on top of it would refine a refinement.

    if (moment_set == "A") {
        cat(sprintf("[%s, A, corner_mode=%s] n=%d (%d corner), n_burn=%d, n_keep=%d, lambda grid: %s\n",
                    ins_choice, corner_mode, nrow(run_sample), sum(run_sample$corner), n_burn, n_keep,
                    paste(signif(lambda_grid, 3), collapse = ", ")))
        par_init0 <- c(delta_init, 0, rep(0, 8))   # eta0=0: no extra floor beyond M>=0 until the data asks for one
        run_grid_A <- function(grid, par_init) {
            out <- vector("list", length(grid))
            for (i in seq_along(grid)) {
                out[[i]] <- fit_one_lambda_A(grid[i], run_sample, n_burn, n_keep, par_init)
                if (!is.na(out[[i]]$value)) par_init <- out[[i]]$par
            }
            list(fits = out, par_init = par_init)
        }
        res1 <- run_grid_A(lambda_grid, par_init0)
        fits <- res1$fits
        for (i in seq_along(fits)) fits[[i]]$phase <- "quantile"
        Lhat <- sapply(fits, `[[`, "value")
        if (refine && is.null(lambda_grid_override) && sum(is.finite(Lhat)) >= 2) {
            ## corner==0 to match the sample build_lambda_grid used when
            ## constructing lambda_grid itself (corner firms are structural
            ## non-evaders, shouldn't calibrate the M_star reference quantile)
            bracket <- bracket_local_min(lambda_grid, Lhat, dplyr::filter(run_sample, corner == 0))
            refine_grid <- sort(build_linear_refinement(list(bracket), n_points = refine_points))
            cat(sprintf("[%s, A, corner_mode=%s] linear refinement around lambda_at_min=%.3g: grid %s\n",
                        ins_choice, corner_mode, bracket$lambda_at_min, paste(signif(refine_grid, 3), collapse = ", ")))
            ## Warm-start from the fit AT the argmin (matched by lambda value,
            ## not position -- robust to any NA/dropped fits upstream), not
            ## res1$par_init (the LAST quantile-grid point, which can be far
            ## from the refined region -- e.g. argmin at the low end, last
            ## point at the high end). 2026-08-31: caught after a real-scale
            ## run showed this mismatch dragging the whole refined sequence
            ## into a much worse local optimum for lag_m specifically.
            argmin_idx <- which(lambda_grid == bracket$lambda_at_min)[1]
            par_at_argmin <- fits[[argmin_idx]]$par
            res2 <- run_grid_A(refine_grid, if (!is.null(par_at_argmin) && !anyNA(par_at_argmin)) par_at_argmin else res1$par_init)
            for (i in seq_along(res2$fits)) res2$fits[[i]]$phase <- "linear_refine"
            fits <- c(fits, res2$fits)
        }
    } else {
        J <- length(attr(run_sample, "sic_levels"))
        # Computed ONCE from the full run_sample (every industry present is guaranteed
        # to appear here, unlike any one lambda-point's feasibility-filtered subset) so
        # the init and the bounds are built from the same source and can never conflict
        # (2026-08-29 bug: a per-lambda-filtered fallback bound reused DELTA_BOUND=20,
        # the wrong scale for ln(M*) -- nominal materials in COP routinely exceeds e^20).
        logMstar_by_j <- run_sample %>%
            group_by(industry_idx) %>%
            summarise(mu = mean(log(M_star)), .groups = "drop") %>%
            arrange(industry_idx)
        mu_m_init  <- logMstar_by_j$mu - 0.1
        mu_m_lower <- logMstar_by_j$mu - 8
        # Fixed 2026-08-29 (same day): the upper bound was WRONG, not just tight.
        # M<=M* always (e>=0 structurally), so ln(M)<=ln(M*) pointwise for every
        # firm, hence E[lnM|j]<=E[lnM*|j] exactly -- there is no room for a "+1
        # slack above the mean" on the high side; the true ceiling IS the mean
        # itself. Earlier "+1" reasoning ("allowance for estimation noise") had
        # this backwards. A few industries' fitted mu_m,j pinned at that
        # (wrongly loose) +1 bound in the first B smoke test -- not necessarily
        # evidence of misspecification, just evidence the box let them go
        # somewhere they structurally shouldn't.
        mu_m_upper <- logMstar_by_j$mu - 0.01
        cat(sprintf("[%s, B, corner_mode=%s] n=%d (%d corner), J=%d industries, n_burn=%d, n_keep=%d, maxeval_b=%d, lambda grid: %s\n",
                    ins_choice, corner_mode, nrow(run_sample), sum(run_sample$corner), J, n_burn, n_keep, maxeval_b,
                    paste(signif(lambda_grid, 3), collapse = ", ")))
        par_init0 <- c(delta_init, 0, mu_m_init, rep(0, 8 + J + 6))   # eta0=0, same rationale as A
        run_grid_B <- function(grid, par_init) {
            out <- vector("list", length(grid))
            for (i in seq_along(grid)) {
                out[[i]] <- fit_one_lambda_B(grid[i], run_sample, J, mu_m_lower, mu_m_upper, n_burn, n_keep, par_init,
                                              maxeval = maxeval_b)
                if (!is.na(out[[i]]$value)) par_init <- out[[i]]$par
            }
            list(fits = out, par_init = par_init)
        }
        res1 <- run_grid_B(lambda_grid, par_init0)
        fits <- res1$fits
        for (i in seq_along(fits)) fits[[i]]$phase <- "quantile"
        Lhat <- sapply(fits, `[[`, "value")
        if (refine && is.null(lambda_grid_override) && sum(is.finite(Lhat)) >= 2) {
            ## corner==0 to match the sample build_lambda_grid used when
            ## constructing lambda_grid itself (corner firms are structural
            ## non-evaders, shouldn't calibrate the M_star reference quantile)
            bracket <- bracket_local_min(lambda_grid, Lhat, dplyr::filter(run_sample, corner == 0))
            refine_grid <- sort(build_linear_refinement(list(bracket), n_points = refine_points))
            cat(sprintf("[%s, B, corner_mode=%s] linear refinement around lambda_at_min=%.3g: grid %s\n",
                        ins_choice, corner_mode, bracket$lambda_at_min, paste(signif(refine_grid, 3), collapse = ", ")))
            ## Warm-start from the fit AT the argmin, not res1$par_init (the
            ## LAST quantile-grid point) -- see the matching comment in the
            ## moment-set-A branch above for why (2026-08-31).
            argmin_idx <- which(lambda_grid == bracket$lambda_at_min)[1]
            par_at_argmin <- fits[[argmin_idx]]$par
            res2 <- run_grid_B(refine_grid, if (!is.null(par_at_argmin) && !anyNA(par_at_argmin)) par_at_argmin else res1$par_init)
            for (i in seq_along(res2$fits)) res2$fits[[i]]$phase <- "linear_refine"
            fits <- c(fits, res2$fits)
        }
    }

    for (f in fits) {
        eta_str <- if (!is.na(f$value)) sprintf(", eta=%.4g", f$par[4]) else ""
        cat(sprintf("  [%s] lambda=%.3g: n_used=%d (dropped %d), p_boundary=%.3g, Lhat=%s (pass1=%s, delta=%s), conv=%s, iter=%s, max|gamma|=%s%s\n",
                     f$phase, f$lambda, f$n, f$n_dropped, ifelse(is.na(f$p_boundary), NA, f$p_boundary),
                     ifelse(is.na(f$value), "NA (too few obs)", signif(f$value, 4)),
                     ifelse(is.null(f$value1) || is.na(f$value1), "NA", signif(f$value1, 4)),
                     ifelse(is.null(f$delta_value) || is.na(f$delta_value), "NA", signif(f$delta_value, 3)),
                     f$convergence, f$iter,
                     ifelse(is.na(f$max_abs_gamma), "NA", signif(f$max_abs_gamma, 4)),
                     eta_str))
    }
    fits
}

## %% One config per invocation -----------------------------------------

WARMSTARTS <- list(lag_m = warmstart_lag_m, lag_2_cal_W = warmstart_lag2W)

DEFAULTS <- list(
    ins         = "lag_m",                 # "lag_m" | "lag_2_cal_W"
    moment_set  = "A",                     # "A" | "B"
    corner_mode = "include_zero",          # "drop" | "include_zero"
    n_burn      = 50,
    n_keep      = 500,
    grid_probs  = seq(0.01, 0.5, length.out = 5),
    lambda_grid = numeric(0),   # explicit, arithmetically-spaced lambda values; overrides grid_probs when non-empty
    n_cores     = 1,
    maxeval_b   = 1000,  # moment set B only; A converges fine at nloptr::bobyqa's own default
    refine        = TRUE,  # auto linear-refinement pass around the quantile grid's argmin (bracket_local_min/build_linear_refinement)
    refine_points = 10     # build_linear_refinement()'s n_points
)
opt <- parse_cli_args(DEFAULTS)
opt$n_cores <- as.integer(opt$n_cores)
opt$maxeval_b <- as.integer(opt$maxeval_b)
opt$refine_points <- as.integer(opt$refine_points)
stopifnot(
    opt$ins %in% names(WARMSTARTS),
    opt$moment_set %in% c("A", "B"),
    opt$corner_mode %in% c("drop", "include_zero")
)
log_run_header("1211-stage2-elvis-driver-AB.R", opt)
RcppParallel::setThreadOptions(numThreads = opt$n_cores)

fits <- run_stage2_elvis_AB(
    opt$ins, opt$moment_set, WARMSTARTS[[opt$ins]],
    n_burn = opt$n_burn, n_keep = opt$n_keep,
    grid_probs = opt$grid_probs,
    lambda_grid_override = if (length(opt$lambda_grid) > 0) opt$lambda_grid else NULL,
    corner_mode = opt$corner_mode,
    maxeval_b = opt$maxeval_b,
    refine = opt$refine, refine_points = opt$refine_points
)

tag <- sprintf("%s-%s-%s-nburn%d-nkeep%d-maxevalb%d", opt$ins, opt$moment_set, opt$corner_mode, opt$n_burn, opt$n_keep, opt$maxeval_b)
out_file <- sprintf("Code/Products/1211-stage2-elvis-AB-%s.RData", tag)
save(fits, opt, file = out_file)
cat(sprintf("Saved: %s\n", out_file))
