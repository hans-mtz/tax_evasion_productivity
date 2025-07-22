#include <Rcpp.h>
using namespace Rcpp;

// Helper function to calculate delta
double calculate_delta(double theta) {
    return std::pow(1.4, theta) / (1 + std::pow(1.4, theta));
}

// [[Rcpp::export]]
DataFrame g_chain_ind_cpp(NumericVector theta, DataFrame data, NumericVector eps_sim, double bbeta) {
    double delta_0 = calculate_delta(theta[0]);
    double delta_1 = calculate_delta(theta[1]);

    NumericVector W = data["W"];
    NumericVector id = data["id"];
    int n = W.size();

    NumericVector W_squig(n), lag_W_squig(n), resid(n), Z1(n), Z2(n);

    for (int i = 0; i < n; ++i) {
        W_squig[i] = W[i] - (1 - bbeta) * eps_sim[i];
        if (i > 0 && id[i] == id[i - 1]) {
            lag_W_squig[i] = W_squig[i - 1];
        } else {
            lag_W_squig[i] = NA_REAL;
        }
        resid[i] = W_squig[i] - delta_0 - delta_1 * lag_W_squig[i];
        Z1[i] = 1;
        Z2[i] = lag_W_squig[i];
    }

    NumericVector unique_ids = unique(id);
    int n_ids = unique_ids.size();
    NumericVector Z1_resid(n_ids), Z2_resid(n_ids);

    for (int j = 0; j < n_ids; ++j) {
        double sum_Z1_resid = 0, sum_Z2_resid = 0;
        int count = 0;
        for (int i = 0; i < n; ++i) {
            if (id[i] == unique_ids[j] && !NumericVector::is_na(resid[i])) {
                sum_Z1_resid += Z1[i] * resid[i];
                sum_Z2_resid += Z2[i] * resid[i];
                count++;
            }
        }
        Z1_resid[j] = sum_Z1_resid / count;
        Z2_resid[j] = sum_Z2_resid / count;
    }

    return DataFrame::create(
        Named("id") = unique_ids,
        Named("Z1_resid") = Z1_resid,
        Named("Z2_resid") = Z2_resid
    );
}

// [[Rcpp::export]]
NumericVector g_ind_gamma_cpp(NumericVector theta, NumericVector ggamma, DataFrame data, NumericVector eps_sim, double bbeta) {
    double delta_0 = calculate_delta(theta[0]);
    double delta_1 = calculate_delta(theta[1]);

    NumericVector W = data["W"];
    NumericVector id = data["id"];
    int n = W.size();

    NumericVector W_squig(n), lag_W_squig(n), resid(n), Z1(n), Z2(n);

    for (int i = 0; i < n; ++i) {
        W_squig[i] = W[i] - (1 - bbeta) * eps_sim[i];
        if (i > 0 && id[i] == id[i - 1]) {
            lag_W_squig[i] = W_squig[i - 1];
        } else {
            lag_W_squig[i] = NA_REAL;
        }
        resid[i] = W_squig[i] - delta_0 - delta_1 * lag_W_squig[i];
        Z1[i] = 1;
        Z2[i] = lag_W_squig[i];
    }

    NumericVector unique_ids = unique(id);
    int n_ids = unique_ids.size();
    NumericVector mom(n_ids);

    for (int j = 0; j < n_ids; ++j) {
        double sum_Z1_resid = 0, sum_Z2_resid = 0;
        int count = 0;
        for (int i = 0; i < n; ++i) {
            if (id[i] == unique_ids[j] && !NumericVector::is_na(resid[i])) {
                sum_Z1_resid += Z1[i] * resid[i];
                sum_Z2_resid += Z2[i] * resid[i];
                count++;
            }
        }
        double Z1_resid = sum_Z1_resid / count;
        double Z2_resid = sum_Z2_resid / count;
        mom[j] = ggamma[0] * std::pow(Z1_resid, 2) + ggamma[1] * std::pow(Z2_resid, 2);
    }

    return mom;
}


// [[Rcpp::export]]
List get_mcmc_chain_ind_cpp(NumericVector delta, NumericVector ggamma, int n_sims, int burn, DataFrame ar1_data, DataFrame eps_sim_dt, double bbeta) {
    NumericVector eps_current = eps_sim_dt["sim_1"];
    NumericVector g_chain(2, 0.0);
    double acceptance_rate = 0.0;
    // printf("eps_current: %f\n", eps_current[0]);
    // printf("delta: %f\n", delta[0]);
    // printf("ggamma: %f\n", ggamma[0]);
    // printf("bbeta: %f\n", bbeta);
    // printf("n_sims: %d\n", n_sims);
    // printf("burn: %d\n", burn);
    // printf("ar1_data size: %d\n", ar1_data.size());
    // printf("eps_sim_dt size: %d\n", eps_sim_dt.size());
    for (int i = 1; i < n_sims; ++i) {
        NumericVector aux = eps_sim_dt[std::string("sim_") + std::to_string(i)];
        // printf("aux: %f\n", aux[0]);
        NumericVector eps_propose = eps_current + aux;
        // printf("eps_propose: %f\n", eps_propose[0]);
        double logtrydensity = mean(g_ind_gamma_cpp(delta, ggamma, ar1_data, eps_current, bbeta)) -
                               mean(g_ind_gamma_cpp(delta, ggamma, ar1_data, eps_propose, bbeta));
        // printf("logtrydensity: %f\n", logtrydensity);
        bool choose = log(R::runif(0, 1)) < logtrydensity;

        if (choose) {
            eps_current = eps_propose;
            acceptance_rate += 1.0;
        }

        if (i > burn) {
            DataFrame g_chain_ind_result = g_chain_ind_cpp(delta, ar1_data, eps_current, bbeta);
            NumericVector ids = g_chain_ind_result["id"];
            NumericVector Z1_resid = g_chain_ind_result["Z1_resid"];
            NumericVector Z2_resid = g_chain_ind_result["Z2_resid"];


            int m = ids.size();
            for (int j = 0; j < m; ++j) {
                // int id = ids[j];
                g_chain[0] += ggamma[0] * Z1_resid[j];
                g_chain[1] += ggamma[1] * Z2_resid[j];
            }
        }
    }

    return List::create(
        Named("g_chain") = g_chain / n_sims,
        Named("acceptance_rate") = acceptance_rate / n_sims
    );
}

DataFrame filter_dataframe(DataFrame df, LogicalVector condition) {
    int nRows = df.nrows();
    int nCols = df.size();
    if (nRows != condition.size()) {
      stop("Condition vector must have the same length as the number of rows in the data frame.");
    }
  
    // Get column names
    CharacterVector col_names = df.names();
    
    // Create a list to store the filtered columns
    List filtered_cols;
    // filtered_cols.names() = col_names;
    
    // Iterate through the columns of the data frame
    for (int i = 0; i < nCols; ++i) {
      NumericVector column = df[i];
      filtered_cols.push_back(column[condition]); // Initialize with NA
      // Filter the column based on the condition
      // switch (TYPEOF(column)) {
      // case LGLSXP: {
      //   LogicalVector col = as<LogicalVector>(column);
      //   LogicalVector filtered_col;
      //   for(int j = 0; j < nRows; ++j){
      //     if(condition[j]){
      //       filtered_col.push_back(col[j]);
      //     }
      //   }
      //   filtered_cols[col_names[i]] = filtered_col;
      //   break;
      // }
      // case INTSXP: {
      //   IntegerVector col = as<IntegerVector>(column);
      //   IntegerVector filtered_col;
      //   for(int j = 0; j < nRows; ++j){
      //     if(condition[j]){
      //       filtered_col.push_back(col[j]);
      //     }
      //   }
      //   filtered_cols[col_names[i]] = filtered_col;
      //   break;
      // }
      // case REALSXP: {
      //   NumericVector col = as<NumericVector>(column);
      //   NumericVector filtered_col;
      //   for(int j = 0  < nRows; ++j){
      //     if(condition[j]){
      //       filtered_col.push_back(col[j]);
      //     }
      //   }
      //   filtered_cols[col_names[i]] = filtered_col;
      //   break;
      // }
      // case STRSXP: {
      //   CharacterVector col = as<CharacterVector>(column);
      //   CharacterVector filtered_col;
      //   for(int j = 0; j < nRows; ++j){
      //     if(condition[j]){
      //       filtered_col.push_back(col[j]);
      //     }
      //   }
      //   filtered_cols[col_names[i]] = filtered_col;
      //   break;
      // }
      // default:
      //   throw std::runtime_error("Unsupported column type");
      // }
    }
    filtered_cols.names() = col_names;
    return DataFrame(filtered_cols);
  }
  

// [[Rcpp::export]]
List get_mcmc_chain_cpp(NumericVector delta, NumericVector ggamma, int n_individuals, int burn, int n_sims, DataFrame ar1_data, DataFrame eps_sim_dt, double bbeta) {
    // NumericVector delta = theta[Range(0, 1)];
    // NumericVector ggamma = theta[Range(2, 3)];

    List mcmc_chain_list(n_individuals);

    // Environment dplyr = Environment::namespace_env("dplyr");
    // Function filter = dplyr["filter"];

    for (int i = 0; i < n_individuals; ++i) {
        // Filtrar datos usando dplyr::filter
        LogicalVector select_ar1 = as<NumericVector>(ar1_data["id"]) == (i + 1);
        LogicalVector select_eps_sim = as<NumericVector>(eps_sim_dt["id"]) == (i + 1);

        DataFrame ar1_data_i = filter_dataframe(ar1_data, select_ar1);
        DataFrame eps_sim_dt_i = filter_dataframe(eps_sim_dt, select_eps_sim);
        // DataFrame ar1_data_i = filter(ar1_data, _["..."]= ar1_data["id"]== (i + 1));
        // DataFrame eps_sim_dt_i = filter(eps_sim_dt, _["..."]= eps_sim_dt["id"]== (i + 1));

        // Llamar a la función get_mcmc_chain_ind
        mcmc_chain_list[i] = get_mcmc_chain_ind_cpp(delta, ggamma, n_sims, burn, ar1_data_i, eps_sim_dt_i, bbeta)["g_chain"];
    }

    // Combinar resultados usando dplyr::bind_rows
    // Function bind_rows = dplyr["bind_rows"];
    // DataFrame mcmc_chain_dt = bind_rows(mcmc_chain_list);

    // // Calcular colMeans
    // Environment base = Environment::base_env();
    // Function colMeans = base["colMeans"];
    // NumericVector g = colMeans(mcmc_chain_dt);

    // // Calcular el resultado final
    // double mom = std::pow(g[0], 2) + std::pow(g[1], 2);
    // return NumericVector::create(mom);
    return mcmc_chain_list;
}

// #include <Rcpp.h>
// using namespace Rcpp;

// Auxiliary function to filter a DataFrame based on a condition



// // [[Rcpp::export]]
// NumericVector obj_mcmc_ar1_par(NumericVector theta, int n_individuals, int mc_cores, int burn, int n_sims, DataFrame ar1_data, DataFrame eps_sim_dt, double bbeta) {
//     NumericVector delta = theta[Range(0, 1)];
//     NumericVector ggamma = theta[Range(2, 3)];

//     List mcmc_chain_list(n_individuals);

//     Environment dplyr = Environment::namespace_env("dplyr");
//     Function filter = dplyr["filter"];

//     for (int i = 0; i < n_individuals; ++i) {
//         LogicalVector select_ar1 = as<NumericVector>(ar1_data["id"]) == (i + 1);
//         LogicalVector select_eps_sim = as<NumericVector>(eps_sim_dt["id"]) == (i + 1);
//         DataFrame ar1_data_i = filter_dataframe(ar1_data, select_ar1);
//         DataFrame eps_sim_dt_i = filter_dataframe(eps_sim_dt, select_eps_sim);
//         mcmc_chain_list[i] = get_mcmc_chain_ind(delta, ggamma, n_sims, burn, ar1_data_i, eps_sim_dt_i, bbeta);
//     }

//     Environment dplyr = Environment::namespace_env("dplyr");
//     Function bind_rows = dplyr["bind_rows"];
//     DataFrame mcmc_chain_dt = bind_rows(mcmc_chain_list);
//     NumericVector g = colMeans(mcmc_chain_dt);

//     double mom = std::pow(g[0], 2) + std::pow(g[1], 2);
//     return NumericVector::create(mom);
// }
