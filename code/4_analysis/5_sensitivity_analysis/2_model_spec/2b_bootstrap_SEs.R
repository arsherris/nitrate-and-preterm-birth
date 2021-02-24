## bootstrapped standard errors
source('code/5_analysis/0_model_fxns/4_run_analysis_fxn.R')
source('code/5_analysis/3_sensitivity/1_model_spec/5_bootstrap_SE_functions.R')
#load("data_processed/births_exposure/nitrate_sibs_crossover_2020-07-17.RData") 

data <- sibs_discordant
n_iters <- 500
outcome <- "prem_20_to_31"
set_size <- 50000

iters_store <- tibble(beta_med = numeric(n_iters),
                      beta_high = numeric(n_iters))


Sys.time()

for(i in 1:n_iters) {
  
  print(paste("iteration", i))
  
  # generate bootstrap sample
  setDT(data, key = "mat_id")
  boot_sample <- data[.(sample(unique(mat_id), replace = T, size = set_size))]
 
  # run model with bootstrap sample
  mod_iter <- broom::tidy(clog_adj_fast(data = boot_sample, outcome = outcome))

  # store model output
  iters_store$beta_med[i] <- mod_iter$estimate[mod_iter$term == "exp_catmedium"]
  iters_store$beta_high[i] <- mod_iter$estimate[mod_iter$term == "exp_cathigh"]

}

Sys.time()
boot_store_early_50k <- iters_store

write_output(boot_store_early_50k, "output/results/sensitivity/2_model_spec/bootstrap_ses/")

outcome <- "prem_32_to_36"

iters_store <- tibble(beta_med = numeric(n_iters),
                      beta_high = numeric(n_iters))


Sys.time()

for(i in 1:n_iters) {
  
  Sys.time()
  print(paste("iteration", i))
  
  # generate bootstrap sample
  setDT(data, key = "mat_id")
  boot_sample <- data[.(sample(unique(mat_id), replace = T, size = set_size))]
  
  # run model with bootstrap sample
  mod_iter <- broom::tidy(clog_adj_fast(data = boot_sample, outcome = outcome))
  
  # store model output
  iters_store$beta_med[i] <- mod_iter$estimate[mod_iter$term == "exp_catmedium"]
  iters_store$beta_high[i] <- mod_iter$estimate[mod_iter$term == "exp_cathigh"]
  
}

Sys.time()

boot_store_late_50k <- iters_store

write_output(boot_store_late_50k, "output/results/sensitivity/2_model_spec/bootstrap_ses/")


## load old data

boot_store_early_3 <- read_csv("output/results/sensitivity/2_model_spec/bootstrap_ses/Nitrate_boot_store_early_40kmat_2020-09-02.csv")
boot_store_early_4 <- read_csv("output/results/sensitivity/2_model_spec/bootstrap_ses/Nitrate_boot_store_early_40kmat_3_2020-09-02.csv")
boot_store_early_1 <- boot_store_early

boot_store_early <- boot_store_early_50k
  #bind_rows(boot_store_early_1, boot_store_early_2, boot_store_early_3)
                              #boot_store_early_4)

hist(boot_store_early$beta_med, breaks = 20)

# store boot results in table
boot_tbl_early <- tibble(outcome = "prem_20_to_31",
                       exposure_cat = c("Medium", "High"),
                       estimate = double(2),
                       boot_estimate =double(2),
                       lower = double(2),
                       upper = double(2))

  beta_boot_med <- mean(boot_store_early$beta_med)
  lower_med <- quantile(boot_store_early$beta_med, seq(0, 1, .025))[2]
  upper_med <- quantile(boot_store_early$beta_med, seq(0, 1, .025))[40]
  
  
  beta_boot_high <- mean(boot_store_early$beta_high)
  lower_high <- quantile(boot_store_early$beta_high, seq(0, 1, .025))[2]
  upper_high <- quantile(boot_store_early$beta_high, seq(0, 1, .025))[40]
  
  boot_tbl_early$boot_estimate[1] <- exp(beta_boot_med)
  boot_tbl_early$lower[1] <- exp(lower_med)
  boot_tbl_early$upper[1] <- exp(upper_med)
  
  boot_tbl_early$boot_estimate[2] <- exp(beta_boot_high)
  boot_tbl_early$lower[2] <- exp(lower_high)
  boot_tbl_early$upper[2] <- exp(upper_high)
  

write_csv(boot_tbl_early, "output/results/sensitivity/2_model_spec/bootstrap_ses/boot_tbl_early_50k.csv")


## late ptb

## load old data

boot_store_late_1 <- read_csv("output/results/sensitivity/2_model_spec/bootstrap_ses/Nitrate_boot_store_late_2021-01-10.csv")
boot_store_late_2 <- read_csv("output/results/sensitivity/2_model_spec/bootstrap_ses/Nitrate_boot_store_late_2_2021-01-11.csv")
boot_store_late_3 <- read_csv("output/results/sensitivity/2_model_spec/bootstrap_ses/Nitrate_boot_store_late_40kmat_2020-09-02.csv")

boot_store_late <- boot_store_late_50k #bind_rows(boot_store_late_1, boot_store_late_2, boot_store_late_3)

hist(boot_store_late_1$beta_med, breaks = 20)
hist(boot_store_late_2$beta_med, breaks = 20)
hist(boot_store_late$beta_med, breaks = 20)

# store boot results in table
boot_tbl_late <- tibble(outcome = "prem_32_to_36",
                        exposure_cat = c("Medium", "High"),
                        estimate = double(2),
                        boot_estimate =double(2),
                        lower = double(2),
                        upper = double(2))

beta_boot_med <- mean(boot_store_late$beta_med)
lower_med <- quantile(boot_store_late$beta_med, seq(0, 1, .025))[2]
upper_med <- quantile(boot_store_late$beta_med, seq(0, 1, .025))[40]


beta_boot_high <- mean(boot_store_late$beta_high)
lower_high <- quantile(boot_store_late$beta_high, seq(0, 1, .025))[2]
upper_high <- quantile(boot_store_late$beta_high, seq(0, 1, .025))[40]

boot_tbl_late$boot_estimate[1] <- exp(beta_boot_med)
boot_tbl_late$lower[1] <- exp(lower_med)
boot_tbl_late$upper[1] <- exp(upper_med)

boot_tbl_late$boot_estimate[2] <- exp(beta_boot_high)
boot_tbl_late$lower[2] <- exp(lower_high)
boot_tbl_late$upper[2] <- exp(upper_high)

write_output(boot_tbl_late, "output/results/sensitivity/2_model_spec/bootstrap_ses/")

