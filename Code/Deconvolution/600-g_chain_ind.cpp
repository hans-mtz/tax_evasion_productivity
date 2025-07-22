
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
DataFrame g_chain_ind_cpp(NumericVector theta, DataFrame data, NumericVector eps_sim, double bbeta) {
    double delta_0 = pow(1.4, theta[0]) / (1 + pow(1.4, theta[0]));
    double delta_1 = pow(1.4, theta[1]) / (1 + pow(1.4, theta[1]));

    NumericVector W = data["W"];
    IntegerVector id = data["id"];
    NumericVector W_squig = W - (1 - bbeta) * eps_sim;

    NumericVector lag_W_squig(W_squig.size(), NA_REAL);
    for (int i = 1; i < W_squig.size(); i++) {
        if (id[i] == id[i - 1]) {
            lag_W_squig[i] = W_squig[i - 1];
        }
    }

    NumericVector resid = W_squig - delta_0 - delta_1 * lag_W_squig;
    NumericVector Z1(W.size(), 1.0);
    NumericVector Z2 = lag_W_squig;

    std::map<int, std::pair<double, double>> moments_map;
    for (int i = 0; i < id.size(); i++) {
        if (!NumericVector::is_na(resid[i])) {
            moments_map[id[i]].first += Z1[i] * resid[i];
            moments_map[id[i]].second += Z2[i] * resid[i];
        }
    }

    IntegerVector unique_ids = unique(id);
    NumericVector Z1_resid(unique_ids.size());
    NumericVector Z2_resid(unique_ids.size());

    for (int i = 0; i < unique_ids.size(); i++) {
        int current_id = unique_ids[i];
        Z1_resid[i] = moments_map[current_id].first;
        Z2_resid[i] = moments_map[current_id].second;
    }

    return DataFrame::create(
        Named("id") = unique_ids,
        Named("Z1_resid") = Z1_resid,
        Named("Z2_resid") = Z2_resid
    );
}

