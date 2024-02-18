#run all models
pacman::p_load(cmdstanr, tidyverse,posterior, bayesplot, tidybayes, furrr)

run_model = function(model){
  print(model)
  
  pacman::p_load(cmdstanr, tidyverse,posterior, bayesplot, tidybayes, furrr)
  
  source(here::here("Fitting population","R scripts","Recover_data.R"))
  
  raw_hrd = read.csv(here::here("Fitting population","Data","raw_hrd.csv"))
  
  df = prep_data(raw_hrd) %>% filter(Modality == "Intero")
  
  df = df %>% group_by(s,x) %>% summarize(yn = sum(y), n = n()) 
  
  
  if(model != "Weibull"){
    datastan = list(Y = df$yn,
                    N = nrow(df),
                    npx = df$n,
                    S = length(unique(df$s)),
                    S_id = df$s,
                    X = matrix(c(rep(1,nrow(df)), df$x), ncol = 2, nrow = nrow(df)))

  }else if(model == "Weibull"){

    datastan = list(Y = df$yn,
                    N = nrow(df),
                    npx = df$n,
                    S = length(unique(df$s)),
                    S_id = df$s,
                    X = matrix(c(rep(1,nrow(df)), exp(df$x)), ncol = 2, nrow = nrow(df)))

  }
  
  
  # mod_norm_prior = cmdstanr::cmdstan_model(here::here("Fitting population","Stan models",model,paste0(model,"_mixed_prior.stan")))
  # 
  # fit_norm <- mod_norm_prior$sample(
  #   data = datastan,
  #   iter_sampling = 2000,
  #   iter_warmup = 2000,
  #   chains = 4,
  #   parallel_chains = 4,
  #   refresh = 100,
  #   adapt_delta = 0.99,
  #   max_treedepth = 15
  # )
  # 
  # if(!dir.exists(here::here("Fitting population","Saved models"))){
  #   dir.create(here::here("Fitting population","Saved models"))
  # }
  # 
  # if(!dir.exists(here::here("Fitting population","Saved models","Priors"))){
  #   dir.create(here::here("Fitting population","Saved models","Priors"))
  # }
  # if(!dir.exists(here::here("Fitting population","Saved models","Nopriors"))){
  #   dir.create(here::here("Fitting population","Saved models","Nopriors"))
  # }
  # 
  # fit_norm$save_object(file.path(here::here("Fitting population", "Saved models", "Priors"), paste0(model, ".RDS")))
  # 
  
  mod_norm_noprior = cmdstanr::cmdstan_model(here::here("Fitting population","Stan models",model,paste0(model,"_mixed_noprior.stan")))

  fit_norm <- mod_norm_noprior$sample(
    data = datastan,
    iter_sampling = 2000,
    iter_warmup = 2000,
    chains = 4,
    parallel_chains = 4,
    refresh = 100,
    adapt_delta = 0.99,
    max_treedepth = 15
  )

  fit_norm$save_object(file.path(here::here("Fitting population", "Saved models", "Nopriors"), paste0(model, ".RDS")))
  


}


if (commandArgs(trailingOnly = TRUE) %>% length() > 0) {
  # If so, run the model with the provided argument
  model_argument <- commandArgs(trailingOnly = TRUE)[1]
  run_model(model_argument)
} else {
  # If not, run the model with a default argument
  default_model <- "Normal"  # Set your default model here
  run_model(default_model)
}

