
#run all models
pacman::p_load(cmdstanr, tidyverse,posterior, bayesplot, tidybayes, furrr)


run_rt_model = function(model){
  
  print(model)
  
  pacman::p_load(cmdstanr, tidyverse,posterior, bayesplot, tidybayes, furrr)
  source("/home/jespere/Hierarchical-Interoception/R scripts/recover_data.R")
  
  
  
  ######## RT models
  
  hrd_data = recover_data2()
  
  df =  hrd_data %>% filter(rt > 0.1 & y != 2) %>% mutate(rt = as.numeric(rt)) %>% drop_na()
  
  if(model != "Weibull"){
    
    datastan = list(Y = df$y,
                    N = nrow(df),
                    npx = df$n,
                    RT = df$rt,
                    min_RT = df %>% group_by(s) %>% summarize(min_RT = min(rt)) %>% .$min_RT,
                    S = length(unique(df$s)),
                    S_id = df$s,
                    X = matrix(c(rep(1,nrow(df)), df$x), ncol = 2, nrow = nrow(df)))
    
    
  }else if(model == "Weibull"){
    
    datastan = list(Y = df$y,
                    N = nrow(df),
                    npx = df$n,
                    RT = df$rt,
                    min_RT = df %>% group_by(s) %>% summarize(min_RT = min(rt)) %>% .$min_RT,
                    S = length(unique(df$s)),
                    S_id = df$s,
                    X = matrix(c(rep(1,nrow(df)), exp(df$x)), ncol = 2, nrow = nrow(df)))
    
  }
  

  # mod_norm_rt_prior = cmdstanr::cmdstan_model(here::here("Stan",model,paste0(model,"_mixed_rt_prior.stan")),stanc_options = list("O1"))
  # 
  # fit_norm <- mod_norm_rt_prior$sample(
  #   data = datastan,
  #   iter_sampling = 4000,
  #   iter_warmup = 2000,
  #   chains = 4,
  #   init = 0,
  #   parallel_chains = 4,
  #   refresh = 100,
  #   adapt_delta = 0.80,
  #   max_treedepth = 15
  # )
  # 
  # 
  # fit_norm$save_object(paste0("/home/jespere/Hierarchical-Interoception/saved models/priors/",model,"_rt",".RDS"))
  # 

  
  mod_norm_rt_noprior = cmdstanr::cmdstan_model(here::here("Stan",model,paste0(model,"_mixed_rt_noprior.stan")),stanc_options = list("O1"))
  
  fit_norm <- mod_norm_rt_noprior$sample(
    data = datastan,
    iter_sampling = 4000,
    iter_warmup = 2000,
    chains = 4,
    init = 0,
    parallel_chains = 4,
    refresh = 100,
    adapt_delta = 0.80,
    max_treedepth = 15
  )
  
  fit_norm$save_object(paste0("/home/jespere/Hierarchical-Interoception/saved models/nopriors/",model,"_rt",".RDS"))
  
}


if (commandArgs(trailingOnly = TRUE) %>% length() > 0) {
  # If so, run the model with the provided argument
  model_argument <- commandArgs(trailingOnly = TRUE)[1]
  run_rt_model(model_argument)
} else {
  # If not, run the model with a default argument
  default_model <- "Normal"  # Set your default model here
  run_rt_model(default_model)
}