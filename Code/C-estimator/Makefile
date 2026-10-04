# Standalone (lambda,delta1,delta2) grid estimator build (2026-09-07).
# Links against the native-arm64 Homebrew NLopt (installed to /opt/homebrew
# this session -- the only pre-existing nlopt on this machine was an x86_64
# build under /usr/local, incompatible with this arm64 Mac mini's native
# toolchain) and Accelerate (system framework, symmetric eigendecomposition
# via dsyevr + covariance via cblas_dsyrk -- same routines
# Code/Rcpp/1200-stage2-elvis-B.cpp already validated for the same shape of
# problem, just called directly here instead of through R/Rcpp).

CXX      := clang++
CXXFLAGS := -O3 -std=c++17 -Wall -Wno-deprecated-declarations
INCLUDES := -I/opt/homebrew/include
LDFLAGS  := -L/opt/homebrew/lib -lnlopt -framework Accelerate -lpthread

# Homebrew prefix: /opt/homebrew on Apple Silicon (Mac mini), /usr/local on Intel (MacBook).
ifeq ($(shell uname -m),x86_64)
INCLUDES := -I/usr/local/include
LDFLAGS  := -L/usr/local/lib -lnlopt -framework Accelerate -lpthread
endif

SRC := grid_estimator.cpp ../Rcpp/1200-stage2-elvis-common.h

# One target per binary, so the flags that built each one are on record (2026-09-30, audit finding 8).
#   grid_estimator_eps_ak2 : kinked power q, 13-row system (EPSVAR); kappa fixed via lambdas=
#   grid_estimator_kf3     : same, kappa estimated (KAPPA_FREE)
#   grid_estimator_ind5k   : + eps*lnM by industry (IND5, 22 rows); kappa fixed via lambdas=, k_free=1 available
#   grid_estimator_ind5b   : IND5 with kappa estimated
#   grid_estimator_ind5b_cf3: ind5b flags, current source (cfprofile cf_target + cf_cold), 2026-10-03
#   grid_estimator_ind5b_cf4: + cf_target=overrep (E[M*/M] - 1, static), 2026-10-03
#   grid_estimator_ind5b_cw: same source as ind5b, built 2026-10-03 with ind_rows=eps_cw (claims-weighted eps by industry)
#   grid_estimator_s2      : CURRENT stage-2 binary (2026-09-30): KINK_S + EPSVAR + KAPPA_FREE with all options
#                            (qform=power_nokink, rho=prop21, sampler=is, cluster=plant, nested=1, seed=hash)
STAGE2 := grid_estimator_eps_ak2 grid_estimator_kf3 grid_estimator_ind5k grid_estimator_ind5b grid_estimator_s2

all: grid_estimator $(STAGE2)

grid_estimator: $(SRC)
	$(CXX) $(CXXFLAGS) $(INCLUDES) grid_estimator.cpp -o $@ $(LDFLAGS)
grid_estimator_eps_ak2: $(SRC)
	$(CXX) $(CXXFLAGS) $(INCLUDES) -DKINK_S -DEPSVAR grid_estimator.cpp -o $@ $(LDFLAGS)
grid_estimator_kf3: $(SRC)
	$(CXX) $(CXXFLAGS) $(INCLUDES) -DKINK_S -DEPSVAR -DKAPPA_FREE grid_estimator.cpp -o $@ $(LDFLAGS)
grid_estimator_ind5k: $(SRC)
	$(CXX) $(CXXFLAGS) $(INCLUDES) -DKINK_S -DEPSVAR -DIND5 grid_estimator.cpp -o $@ $(LDFLAGS)
grid_estimator_ind5b: $(SRC)
	$(CXX) $(CXXFLAGS) $(INCLUDES) -DKINK_S -DEPSVAR -DKAPPA_FREE -DIND5 grid_estimator.cpp -o $@ $(LDFLAGS)

grid_estimator_ind5b_cw: $(SRC)
	$(CXX) $(CXXFLAGS) $(INCLUDES) -DKINK_S -DEPSVAR -DKAPPA_FREE -DIND5 grid_estimator.cpp -o $@ $(LDFLAGS)

grid_estimator_ind5b_cf3: $(SRC)
	$(CXX) $(CXXFLAGS) $(INCLUDES) -DKINK_S -DEPSVAR -DKAPPA_FREE -DIND5 grid_estimator.cpp -o $@ $(LDFLAGS)

grid_estimator_ind5b_cf4: $(SRC)
	$(CXX) $(CXXFLAGS) $(INCLUDES) -DKINK_S -DEPSVAR -DKAPPA_FREE -DIND5 grid_estimator.cpp -o $@ $(LDFLAGS)

grid_estimator_ind5p: $(SRC)
	$(CXX) $(CXXFLAGS) $(INCLUDES) -DKINK_S -DEPSVAR -DKAPPA_FREE -DIND5 -DIND5P grid_estimator.cpp -o $@ $(LDFLAGS)

grid_estimator_s2: $(SRC)
	$(CXX) $(CXXFLAGS) $(INCLUDES) -DKINK_S -DEPSVAR -DKAPPA_FREE grid_estimator.cpp -o $@ $(LDFLAGS)

clean:
	rm -f grid_estimator $(STAGE2)

.PHONY: all clean
