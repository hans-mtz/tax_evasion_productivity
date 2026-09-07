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

// Score moment for lambda (2026-09-05): d(h)/d(lambda) for the linear-q
// h_of_e above. The existing moment system already has this structure for
// every OTHER smooth parameter without it being named as such -- psi =
// d(psi)/d(delta0)*psi, psi*om = d(psi)/d(delta1)*psi, psi*om^2 =
// -d(psi)/d(delta2)*psi are already the Schennach-style score moments for
// delta0,delta1,delta2. h_prime*eps closes the same gap for lambda: eps is
// NOT an argument of e's own defining FOC the way psi/omega are (so this
// isn't the "psi*e" tautology, rejected 2026-08-28 -- eps _|_ (psi,omega) is
// already maintained, which implies eps _|_ e, hence eps _|_ h_prime(e)).
// See CLAUDE.md's 2026-09-05 "Moment system upgrade" entry for the full
// derivation, including which analogous moments were considered and
// rejected (h_prime*omega -- tautological, omega IS an argument of e's own
// FOC).
inline double h_prime_of_e(double e, double lambda) {
    double denom = 1.0 - 2.0 * lambda * e;
    return -2.0 * e / denom;
}

// Concave/exponential q=1-exp(-lambda*e) variant (2026-09-05), added to test
// whether the linear-q result (lambda pinned near 0, apparently by a few
// extreme-e firms whose ceiling 2*lambda*e->1 makes h(e) blow up) survives
// under a functional form whose own ceiling sits at TWICE the lambda*e value
// (lambda*e<1, not <1/2). From the FOC MB(e)=tau_rho*(1-(q+q'e)) with
// q'=lambda*exp(-lambda*e): q+q'e = 1-exp(-lambda*e)*(1-lambda*e), so
// MB(e)=tau_rho*exp(-lambda*e)*(1-lambda*e), h(e)=ln(MB/tau_rho) below.
// Verified (CLAUDE.md, same date): d h/d lambda = -e[1+1/(1-lambda*e)] here,
// vs -2e/(1-2*lambda*e) for the linear form -- both equal -2e as lambda*e->0
// (identical sensitivity for typical/small-e firms), but this one stays
// FINITE (-3e) at linear's own blow-up point lambda*e=1/2, exploding only at
// lambda*e=1 instead.
inline double h_of_e_concave(double e, double tau_rho, double lambda) {
    return std::log(tau_rho) - lambda * e + std::log(1.0 - lambda * e);
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

// Same role as draw_from_rho_eta, but for h_of_e_concave: ceiling is e<1/lambda
// (not e<1/(2*lambda)), matching h_of_e_concave's own domain restriction.
inline double draw_from_rho_eta_concave(double u01, double Mstar, double lambda, double eta) {
    double lo = std::max(eta * Mstar, Mstar - 1.0 / lambda);
    return Mstar - u01 * (Mstar - lo);
}

#endif // STAGE2_ELVIS_COMMON_H
