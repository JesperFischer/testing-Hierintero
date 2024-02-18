

pacman::p_load(cmdstanr, tidyverse,posterior, bayesplot, tidybayes, furrr,bridgesampling, rstan, brms, faux,LRO.utilities,reticulate)
print("runnning")
Run_poweranalysis = function(subjects, trials, effectsize_alpha,effectsize_beta){
  pacman::p_load(cmdstanr, tidyverse,posterior, bayesplot, tidybayes, furrr,bridgesampling, rstan, brms, faux,LRO.utilities,reticulate)
  
  
  
  source(here::here("Power analysis","R scripts", "Run poweranalysis.R"))
  source(here::here("Power analysis","R scripts", "Simulate agents.R"))
  
  
  
  subjects = subjects
  trials = trials
  effect_size_alpha = effectsize_alpha
  effect_size_beta = effectsize_beta
  
  
  if(effect_size_alpha>0){
    mu_cond = effect_size_alpha*11
  }else if(effect_size_alpha<0){
    mu_cond = -effect_size_alpha*11
  }else{
    mu_cond = 0
  }
  sd_cond = 1
  
  
  
  if(effect_size_beta>0){
    mu_cond_beta = effect_size_beta*2
  }else if(effect_size_beta<0){
    mu_cond_beta = -effect_size_beta*2
  }else{
    mu_cond_beta = 0
  }
  sd_cond_beta = 1
  
  
  parameter = "both"
  replicate = 1:2
  
  parameters = expand.grid(subjects = subjects,
                           trials = trials,
                           mu_cond = mu_cond,
                           sd_cond = sd_cond,
                           mu_cond_beta = mu_cond_beta,
                           sd_cond_beta = sd_cond_beta,
                           parameter = parameter,
                           effect_size_alpha = effect_size_alpha,
                           effect_size_beta = effect_size_beta,
                           replicate = replicate) %>% 
    mutate(id = 1:nrow(.))
  
  data_list <- split(parameters, parameters$id)
  
  
  results = list()
  i = 1
  for(i in 1:length(data_list)){
    print(i)
    tryCatch({
      testing <- power_analysis(data_list[[i]])
      results[[i]] <- testing
    }, error = function(e) {
      cat("Error in iteration", i, ":", conditionMessage(e), "\n")
      results[[i]] <- "Error"
    })
    
  }
  
  directory = here::here("Power analysis","Power analysis results",paste0(parameter),paste0("results_","subjects=",subjects,"_trials=",trials))
  
  if(!dir.exists(directory)){
    dir.create(directory)
  }
  
  saveRDS(results,here::here(directory,paste0("results_","subjects=",subjects,"_trials=",trials,"_effectsize_alpha=",effect_size_alpha,"_effectsize_beta=",effect_size_beta,".rds")))
}



if (length(commandArgs(trailingOnly = TRUE)) > 0) {
  # If arguments are provided, use them
  subjects <- as.numeric(commandArgs(trailingOnly = TRUE)[1])
  trials <- as.numeric(commandArgs(trailingOnly = TRUE)[2])
  effectsize_alpha <- as.numeric(commandArgs(trailingOnly = TRUE)[3])
  effectsize_beta <- as.numeric(commandArgs(trailingOnly = TRUE)[4])
  
} else {
  # If no arguments are provided, use default values
  subjects <- 0  # Set your default value for subjects
  trials <- 0   # Set your default value for trials
  effectsize_alpha <- 0  # Set your default value for effect size
  effectsize_beta <- 0  # Set your default value for effect size
  
}


print("runnning")
print("both")

print(subjects)
print(trials)
print(effectsize_alpha)
print(effectsize_beta)

# Call your function with the extracted arguments
Run_poweranalysis(subjects, trials, effectsize_alpha,effectsize_beta)


