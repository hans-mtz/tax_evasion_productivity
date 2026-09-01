// Shared structural maps and includes for the stage-2 ELVIS Rcpp files
// (2026-08-31) -- extracted from 1200-stage2-elvis.cpp when moment set B was
// split into its own file (1200-stage2-elvis-B.cpp) so that B's file could
// take on an Accelerate/CBLAS dependency without forcing the main file (old
// 15-moment system + moment set A, both Accelerate-free and already
// validated) to require it too. Included by both files; each becomes its
// own separate shared object via sourceCpp(), so there is no ODR/multiple-
// definition risk across them -- these are marked inline purely as good
// header practice, not because it's load-bearing here.
//
// NOT exported to R from here: e_of_M/eps_of_M/etc. used to carry
// individual // [[Rcpp::export]] markers in the original file, but nothing
// in Code/Deconvolution/*.R calls them directly (checked before moving) --
// only other C++ code (the worker structs) uses them. Moving them into a
// header means they're no longer directly R-callable; a low-risk change
// since nothing depended on that.
#ifndef STAGE2_ELVIS_COMMON_H
#define STAGE2_ELVIS_COMMON_H

#include <Rcpp.h>
#include <RcppParallel.h>
#include <random>
#include <array>
#include <algorithm>
using namespace Rcpp;
using namespace RcppParallel;

inline double e_of_M(double M, double Mstar) {
    return Mstar - M;
}

inline double eps_of_M(double M, double Mstar, double V) {
    return std::log(Mstar / M) - V;
}

inline double omega_of_M(double M, double Mstar, double V, double Wt, double beta) {
    return Wt - (1.0 - beta) * eps_of_M(M, Mstar, V);
}

inline double h_of_e(double e, double tau_rho, double lambda) {
    return std::log(tau_rho) + std::log(1.0 - 2.0 * lambda * e);
}

inline double draw_from_rho(double u01, double Mstar, double lambda) {
    // u01=0 -> Mstar ; u01=1 -> max(0, Mstar - 1/(2*lambda))
    //
    // Floored at 0, fixed 2026-08-29: the FOC-domain constraint e<1/(2*lambda)
    // (needed for h(e)=ln(tau_rho)+ln(1-2*lambda*e) to be defined) gives the
    // lower bound Mstar-1/(2*lambda), but that constraint is only THE binding
    // one when it's tighter than the physical constraint M>=0 (equivalently
    // e<=Mstar, since you cannot overreport more materials than you have room
    // for). When Mstar<1/(2*lambda), M>=0 is the tighter constraint, and the
    // correct support is (0, Mstar), not (Mstar-1/(2*lambda), Mstar) [which
    // would extend below 0]. Previously firms with Mstar<1/(2*lambda) were
    // dropped ENTIRELY by a per-lambda feasibility filter in the R driver
    // instead of having their support truncated -- that filter has been
    // removed (Code/Deconvolution/1211-stage2-elvis-driver-AB.R) since no firm
    // ever actually needs to be excluded once the true (possibly narrower)
    // support is used. This was the mechanism behind sample size shrinking
    // sharply at small lambda grid points, confirmed in-session before this
    // fix (a firm's inclusion should not depend on lambda at all).
    double lo = std::max(0.0, Mstar - 1.0 / (2.0 * lambda));
    return Mstar - u01 * (Mstar - lo);
}

inline double draw_from_rho_eta(double u01, double Mstar, double lambda, double eta) {
    // Same as draw_from_rho, but floors the support at eta*Mstar instead of 0
    // (2026-08-29, moment set A only -- see Code/Rcpp/1200-stage2-elvis.cpp's
    // "Moment set A: relative-evasion floor" header below for the economic
    // argument). eta in [0,1): M>=eta*Mstar, equivalently e<=(1-eta)*Mstar --
    // a RELATIVE cap on evasion (unitless, scale-consistent across firms of
    // very different size), not an absolute currency-level floor. eta=0
    // recovers draw_from_rho exactly (no additional restriction beyond M>=0).
    // Implemented as a SUPPORT restriction (AK2020 Theorem 4: an inequality
    // moment never needs its own gamma component, enforced on the sampler
    // instead), not a gamma-weighted moment row -- eta is a smooth,
    // box-bounded member of theta_smooth, jointly optimized with
    // (delta0,delta1,delta2) via BOBYQA, but does not add a row to g().
    double lo = std::max(eta * Mstar, Mstar - 1.0 / (2.0 * lambda));
    return Mstar - u01 * (Mstar - lo);
}

#endif // STAGE2_ELVIS_COMMON_H
