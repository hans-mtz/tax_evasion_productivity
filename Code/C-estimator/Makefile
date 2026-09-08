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

all: grid_estimator

grid_estimator: grid_estimator.cpp ../Rcpp/1200-stage2-elvis-common.h
	$(CXX) $(CXXFLAGS) $(INCLUDES) grid_estimator.cpp -o grid_estimator $(LDFLAGS)

clean:
	rm -f grid_estimator

.PHONY: all clean
