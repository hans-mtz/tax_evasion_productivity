// Standalone MSL estimator, Version A, parametric f_psi (2026-09-18)
// ------------------------------------------------------------------
// Spec: Paper/sections/9999-msl-implementation.qmd. Cast in EPSILON-space
// (per-firm Gauss-Legendre quadrature over eps). Deliberately shares NOTHING
// with Code/Rcpp/1200-stage2-elvis-common.h, which is cast in M-space (ELVIS's
// single unobservable is the true materials level M; here it is eps).
//
// Likelihood contribution of firm i (interior branch + e*=0 branch):
//   L_i = int_{D_i(lambda)} f_psi(psi(eps)) * |h'(e)|*M* * f_omega(omega(eps)) * f_eps(eps) d eps
//         + f_eps(-V) * f_omega(omega_0) * [1 - F_psi(h(0) - C(omega_0))]
// with e(eps)=M*(1-exp(-V-eps)), omega(eps)=Wt-(1-beta)eps,
//      psi(eps)=ln(tau rho)+ln(1-2 lambda e)-d0+d1 omega-d2 omega^2,
//      |h'(e)|=2 lambda/(1-2 lambda e), omega_0=Wt+(1-beta)V,
//      C(omega)=d0-d1 omega+d2 omega^2, h(0)=ln(tau rho).
// f_psi = N(0, sigma^2); f_omega, f_eps normal with per-firm (mu, sd) columns.
//
// Input CSV (header required, columns by name): V,Wt,Mst,taur,beta,mu_eps,
// sd_eps,mu_om,sd_om. Reference implementation in R:
// Code/Deconvolution/1400-msl-sim-harness.R (exports the same CSV plus its
// per-firm log-likelihoods; this program must reproduce them).
//
// Usage:
//   msl_estimator data=<csv> mode=eval par=l,d0,d1,d2,logsig [corner=1] [out=<csv>]
//   msl_estimator data=<csv> mode=fit  start=l,d0,d1,d2,logsig [corner=1] [maxeval=2000]
//   optional: K=150 kwid=6 threads=<n> quad=1|0  (1: s=ln(1-2 lambda e) quadrature, default; 0: legacy eps quadrature)

#include <nlopt.h>

#include <algorithm>
#include <atomic>
#include <chrono>
#include <cmath>
#include <cstdio>
#include <cstdlib>
#include <fstream>
#include <iostream>
#include <map>
#include <sstream>
#include <string>
#include <thread>
#include <vector>

struct Firm {
    double V, Wt, Mst, ltr, bo, mu_eps, sd_eps, mu_om, sd_om;
};

static const double INV_SQRT_2PI = 0.3989422804014327;
static const double INF = std::numeric_limits<double>::infinity();

static inline double npdf(double x, double mu, double sd) {
    const double z = (x - mu) / sd;
    return INV_SQRT_2PI * std::exp(-0.5 * z * z) / sd;
}
static inline double upper_tail(double x) { return 0.5 * std::erfc(x / std::sqrt(2.0)); }  // 1 - Phi(x)

// Gauss-Legendre nodes/weights on [-1,1] (Newton iteration on P_n).
static void gauleg(int n, std::vector<double> &x, std::vector<double> &w) {
    x.assign(n, 0.0);
    w.assign(n, 0.0);
    const int m = (n + 1) / 2;
    for (int i = 0; i < m; ++i) {
        double z = std::cos(M_PI * (i + 0.75) / (n + 0.5)), z1, pp;
        do {
            double p1 = 1.0, p2 = 0.0;
            for (int j = 0; j < n; ++j) {
                const double p3 = p2;
                p2 = p1;
                p1 = ((2.0 * j + 1.0) * z * p2 - j * p3) / (j + 1.0);
            }
            pp = n * (z * p1 - p2) / (z * z - 1.0);
            z1 = z;
            z = z1 - p1 / pp;
        } while (std::fabs(z - z1) > 1e-15);
        x[i] = -z;
        x[n - 1 - i] = z;
        w[i] = 2.0 / ((1.0 - z * z) * pp * pp);
        w[n - 1 - i] = w[i];
    }
}

struct Model {
    std::vector<Firm> firms;
    std::vector<double> gx, gw;
    int K = 150;
    double kwid = 6.0;
    bool corner = true;
    int threads = 1;
    int quad = 1;   // 1: integrate over s=ln(1-2 lambda e) (smooth, default); 0: over eps (legacy)
};

// par = (lambda, delta0, delta1, delta2, log sigma_psi). Returns log L_i.
static double firm_ll(const Firm &f, const double *par, const Model &M) {
    const double lam = par[0], d0 = par[1], d1 = par[2], d2 = par[3], sp = std::exp(par[4]);
    const double lo = std::max(-f.V, f.mu_eps - M.kwid * f.sd_eps);
    const double emax = (2.0 * lam * f.Mst > 1.0)
                            ? -f.V - std::log(std::max(1.0 - 1.0 / (2.0 * lam * f.Mst), 1e-300))
                            : INF;
    const double hi = std::min(emax, f.mu_eps + M.kwid * f.sd_eps);

    double Lint = 0.0;
    if (hi > lo && M.quad == 0) {
        const double half = 0.5 * (hi - lo);
        for (int k = 0; k < M.K; ++k) {
            const double eps = lo + half * (M.gx[k] + 1.0);
            const double e = f.Mst * (1.0 - std::exp(-f.V - eps));
            const double den = 1.0 - 2.0 * lam * e;
            if (den <= 1e-12) continue;
            const double om = f.Wt - f.bo * eps;
            const double psi = f.ltr + std::log(den) - d0 + d1 * om - d2 * om * om;
            const double integrand = npdf(psi, 0.0, sp) * (2.0 * lam * f.Mst / den) *
                                     npdf(om, f.mu_om, f.sd_om) * npdf(eps, f.mu_eps, f.sd_eps);
            Lint += integrand * half * M.gw[k];
        }
    } else if (hi > lo) {
        // Smooth reparametrization: s = ln(1 - 2 lambda e), e = M*(1-exp(-V-eps)).
        // |d eps| = den ds / (2 lambda (M*-e)) and J = 2 lambda M*/den, so the
        // interior term is  int f_psi(psi(s)) * M*/(M*-e) * f_omega f_eps ds,
        // with psi = ln(tau rho) + s - C(omega): Gaussian in s, no spike at the ceiling.
        const double e_lo = f.Mst * (1.0 - std::exp(-f.V - lo));
        const double e_hi = f.Mst * (1.0 - std::exp(-f.V - hi));
        const double s_top = std::log(std::max(1.0 - 2.0 * lam * e_lo, 1e-300));   // ~0 at e=0
        const double s_bot = std::log(std::max(1.0 - 2.0 * lam * e_hi, 1e-300));
        // Where psi is non-negligible: psi = ltr + s - C(omega(eps)), C quadratic in omega.
        double cmin = INF, cmax = -INF;
        for (int j = 0; j <= 8; ++j) {
            const double eps = lo + (hi - lo) * j / 8.0, om = f.Wt - f.bo * eps;
            const double C = d0 - d1 * om + d2 * om * om;
            cmin = std::min(cmin, C); cmax = std::max(cmax, C);
        }
        const double s_lo = std::max(s_bot, cmin - f.ltr - 10.0 * sp - 1.0);
        const double s_hi = std::min(s_top, cmax - f.ltr + 10.0 * sp + 1.0);
        if (s_hi > s_lo) {
            const double half = 0.5 * (s_hi - s_lo);
            for (int k = 0; k < M.K; ++k) {
                const double sv = s_lo + half * (M.gx[k] + 1.0);
                const double den = std::exp(sv);
                const double e = (1.0 - den) / (2.0 * lam);
                if (e >= f.Mst) continue;
                const double eps = -f.V - std::log(1.0 - e / f.Mst);
                const double om = f.Wt - f.bo * eps;
                const double psi = f.ltr + sv - d0 + d1 * om - d2 * om * om;
                const double integrand = npdf(psi, 0.0, sp) * (f.Mst / (f.Mst - e)) *
                                         npdf(om, f.mu_om, f.sd_om) * npdf(eps, f.mu_eps, f.sd_eps);
                Lint += integrand * half * M.gw[k];
            }
        }
    }

    double Lcor = 0.0;
    if (M.corner) {
        const double om0 = f.Wt + f.bo * f.V;
        const double psi0 = f.ltr - (d0 - d1 * om0 + d2 * om0 * om0);
        Lcor = npdf(-f.V, f.mu_eps, f.sd_eps) * npdf(om0, f.mu_om, f.sd_om) * upper_tail(psi0 / sp);
    }
    return std::log(std::max(Lint + Lcor, 1e-300));
}

// Total -logL, threaded over fixed-size firm chunks; chunk sums are added in
// chunk order so the result does not depend on thread scheduling.
static double neg_loglik(const double *par, const Model &M, std::vector<double> *per_firm = nullptr) {
    const size_t n = M.firms.size();
    if (par[0] <= 0.0) return 1e10 * static_cast<double>(n);
    const size_t chunk = 256, nchunk = (n + chunk - 1) / chunk;
    std::vector<double> csum(nchunk, 0.0);
    if (per_firm) per_firm->assign(n, 0.0);
    std::atomic<size_t> next(0);
    auto worker = [&]() {
        for (;;) {
            const size_t c = next.fetch_add(1);
            if (c >= nchunk) break;
            double s = 0.0;
            const size_t a = c * chunk, b = std::min(n, a + chunk);
            for (size_t i = a; i < b; ++i) {
                const double l = firm_ll(M.firms[i], par, M);
                if (per_firm) (*per_firm)[i] = l;
                s += l;
            }
            csum[c] = s;
        }
    };
    std::vector<std::thread> pool;
    for (int t = 1; t < M.threads; ++t) pool.emplace_back(worker);
    worker();
    for (auto &th : pool) th.join();
    double total = 0.0;
    for (double s : csum) total += s;
    return -total;
}

// Fit works in log(lambda): real-data lambda is ~1e-7-1e-6 next to O(1) delta's,
// a scale gap Nelder-Mead handles poorly in levels.
static double nlopt_obj_loglam(unsigned n, const double *x, double *grad, void *data) {
    (void)grad;
    double p[5];
    for (unsigned i = 0; i < n; ++i) p[i] = x[i];
    p[0] = std::exp(x[0]);
    return neg_loglik(p, *static_cast<Model *>(data));
}

static std::vector<double> parse_vec(const std::string &s) {
    std::vector<double> v;
    std::stringstream ss(s);
    std::string tok;
    while (std::getline(ss, tok, ',')) v.push_back(std::atof(tok.c_str()));
    return v;
}

static bool load_csv(const std::string &path, std::vector<Firm> &out) {
    std::ifstream in(path);
    if (!in) return false;
    std::string line;
    std::getline(in, line);
    std::map<std::string, int> col;
    {
        std::stringstream ss(line);
        std::string tok;
        int j = 0;
        while (std::getline(ss, tok, ',')) {
            tok.erase(std::remove(tok.begin(), tok.end(), '"'), tok.end());
            col[tok] = j++;
        }
    }
    const char *need[] = {"V", "Wt", "Mst", "taur", "beta", "mu_eps", "sd_eps", "mu_om", "sd_om"};
    for (const char *nm : need)
        if (!col.count(nm)) {
            std::fprintf(stderr, "missing column %s\n", nm);
            return false;
        }
    while (std::getline(in, line)) {
        if (line.empty()) continue;
        std::vector<double> v;
        std::stringstream ss(line);
        std::string tok;
        while (std::getline(ss, tok, ',')) v.push_back(std::atof(tok.c_str()));
        Firm f;
        f.V = v[col["V"]];
        f.Wt = v[col["Wt"]];
        f.Mst = v[col["Mst"]];
        f.ltr = std::log(v[col["taur"]]);
        f.bo = 1.0 - v[col["beta"]];
        f.mu_eps = v[col["mu_eps"]];
        f.sd_eps = v[col["sd_eps"]];
        f.mu_om = v[col["mu_om"]];
        f.sd_om = v[col["sd_om"]];
        out.push_back(f);
    }
    return true;
}

int main(int argc, char **argv) {
    std::map<std::string, std::string> a = {{"mode", "eval"}, {"K", "150"}, {"kwid", "6"},
                                            {"corner", "1"}, {"maxeval", "2000"}, {"threads", ""}, {"quad", "1"}};
    for (int i = 1; i < argc; ++i) {
        std::string s = argv[i];
        const size_t eq = s.find('=');
        if (eq == std::string::npos) {
            std::fprintf(stderr, "bad arg %s\n", s.c_str());
            return 1;
        }
        a[s.substr(0, eq)] = s.substr(eq + 1);
    }
    if (!a.count("data")) {
        std::fprintf(stderr, "data=<csv> required\n");
        return 1;
    }
    Model M;
    if (!load_csv(a["data"], M.firms)) return 1;
    M.K = std::atoi(a["K"].c_str());
    M.kwid = std::atof(a["kwid"].c_str());
    M.corner = std::atoi(a["corner"].c_str()) != 0;
    M.quad = std::atoi(a["quad"].c_str());
    M.threads = a["threads"].empty() ? std::max(1u, std::thread::hardware_concurrency())
                                     : std::atoi(a["threads"].c_str());
    gauleg(M.K, M.gx, M.gw);
    std::printf("n=%zu K=%d kwid=%g corner=%d threads=%d\n", M.firms.size(), M.K, M.kwid,
                M.corner ? 1 : 0, M.threads);

    if (a["mode"] == "eval") {
        const std::vector<double> p = parse_vec(a["par"]);
        if (p.size() != 5) {
            std::fprintf(stderr, "par needs 5 values\n");
            return 1;
        }
        std::vector<double> pf;
        const double nll = neg_loglik(p.data(), M, &pf);
        std::printf("-logL = %.8f\n", nll);
        if (a.count("out")) {
            std::ofstream o(a["out"]);
            o << "ll\n";
            o.precision(17);
            for (double l : pf) o << l << "\n";
        }
        return 0;
    }

    if (a["mode"] == "fit") {
        std::vector<double> x = parse_vec(a["start"]);
        if (x.size() != 5) {
            std::fprintf(stderr, "start needs 5 values\n");
            return 1;
        }
        x[0] = std::log(x[0]);   // optimize log(lambda); reported back in levels below
        const double lower[5] = {std::log(1e-9), -60, -60, -60, -6};
        const double upper[5] = {0.0, 60, 60, 60, 3};
        const auto t0 = std::chrono::steady_clock::now();
        double fval = 0.0;
        for (int pass = 0; pass < 2; ++pass) {
            nlopt_opt opt = nlopt_create(NLOPT_LN_NELDERMEAD, 5);
            nlopt_set_lower_bounds(opt, lower);
            nlopt_set_upper_bounds(opt, upper);
            nlopt_set_min_objective(opt, nlopt_obj_loglam, &M);
            nlopt_set_xtol_rel(opt, 1e-9);
            nlopt_set_ftol_rel(opt, 1e-12);
            nlopt_set_maxeval(opt, std::atoi(a["maxeval"].c_str()));
            const nlopt_result rc = nlopt_optimize(opt, x.data(), &fval);
            std::printf("pass %d: rc=%d -logL=%.6f\n", pass + 1, static_cast<int>(rc), fval);
            nlopt_destroy(opt);
        }
        const double secs =
            std::chrono::duration<double>(std::chrono::steady_clock::now() - t0).count();
        std::printf("lambda=%.6e delta0=%.6f delta1=%.6f delta2=%.6f sigma_psi=%.6f  (%.1fs)\n",
                    std::exp(x[0]), x[1], x[2], x[3], std::exp(x[4]), secs);
        return 0;
    }
    std::fprintf(stderr, "unknown mode\n");
    return 1;
}
