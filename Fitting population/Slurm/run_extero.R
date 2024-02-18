#run all models
pacman::p_load(cmdstanr, tidyverse,posterior, bayesplot, tidybayes, furrr)

run_model = function(){

  pacman::p_load(cmdstanr, tidyverse,posterior, bayesplot, tidybayes, furrr)
  
  source(here::here("Fitting population","R scripts", "Recover_data.R"))
  
  raw_hrd = read.csv(here::here("Fitting population","Data","raw_hrd.csv"))
  
  hrd_data = prep_data(raw_hrd)
  
  hrd_data = hrd_data %>% mutate(nTrials = nTrials+1,Modality = ifelse(Modality == "Extero",1,ifelse(Modality == "Intero",0,NA)))
  

  df = hrd_data
  
  df = df %>% group_by(Modality,s,x) %>% summarize(yn = sum(y), n = n())

  
  datastan = list(Y = df %>% arrange(Modality,s) %>% .$yn,
                  N = nrow(df),
                  npx = df %>% arrange(Modality,s) %>% .$n,
                  S = length(unique(df$s)),
                  S_id = df %>% arrange(Modality,s) %>% .$s,
                  X = df %>% arrange(Modality,s) %>% .$x,
                  condition = df%>% group_by(Modality,s) %>% .$Modality
                  )
#
#
#
  model = "Normal"
#   mod_norm_noprior = cmdstanr::cmdstan_model(here::here("Fitting population","Stan models",model,"Extero",paste0("Extero_priors.stan")),stanc_options = list("O1"))
#   
# #
#   fit_norm <- mod_norm_prior$sample(
#     data = datastan,
#     iter_sampling = 2000,
#     iter_warmup = 2000,
#     chains = 4,
#     parallel_chains = 4,
#     refresh = 100,
#     adapt_delta = 0.99,
#     max_treedepth = 15
#   )
# 
#   if(!dir.exists(here::here("Fitting population","Saved models","Extero"))){
#     dir.create(here::here("Fitting population","Saved models","Extero"))
#   }
#  #fit_norm$save_object("/home/jespere/Hierarchical-Interoception/saved models/Extero/Extero_priors.RDS")
#  fit_norm$save_object(file.path(here::here("Fitting population", "Saved models","Extero","Extero_priors.RDS")))
#  
print("No! Priors")
  
mod_norm_noprior = cmdstanr::cmdstan_model(here::here("Fitting population","Stan models",model,"Extero",paste0("Extero_nopriors.stan")))

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

#fit_norm$save_object("/home/jespere/Hierarchical-Interoception/saved models/Extero/Extero_nopriors.RDS")
fit_norm$save_object(file.path(here::here("Fitting population", "Saved models","Extero","Extero_nopriors.RDS")))


  ######## RT models
  # 
  # hrd_data = recover_data2_extero()
  # 
  # df =  hrd_data %>% filter(rt > 0.1 & y != 2) %>% mutate(rt = as.numeric(rt)) %>% drop_na() %>% arrange(s, Modality)
  # 
  # 
  #   
  # 
  # datastan = list(Y = df %>% arrange(Modality) %>% .$y,
  #                 N = nrow(df),
  #                 npx = df %>% arrange(Modality) %>% .$n,
  #                 RT = df %>% arrange(Modality) %>% .$rt,
  #                 min_RT = df %>% group_by(s) %>% summarize(min_RT = min(rt)) %>% .$min_RT,
  #                 S = length(unique(df$s)),
  #                 S_id = df %>% arrange(Modality) %>% .$s,
  #                 X = matrix(c(rep(1,nrow(df)),
  #                              df %>% arrange(Modality) %>% .$x,
  #                              df %>% arrange(Modality) %>% mutate(Modality = ifelse(Modality == "Extero",1,0)) %>% .$Modality),
  #                            ncol = 3,
  #                            nrow = nrow(df)))
  # 
  # 
  #   
  # model = "Normal"
  # 
  # mod_norm_rt_prior = cmdstanr::cmdstan_model(here::here("Stan",model,"Extero",paste0("Extero_rt_priors.stan")),stanc_options = list("O1"))
  # 
  # fit_norm <- mod_norm_rt_prior$sample(
  #   data = datastan,
  #   iter_sampling = 4000,
  #   iter_warmup = 2000,
  #   chains = 4,
  #   init = 0,
  #   parallel_chains = 4,
  #   refresh = 10,
  #   adapt_delta = 0.80,
  #   max_treedepth = 15
  # )
  # 
  # 
  # fit_norm$save_object(paste0("/home/jespere/Hierarchical-Interoception/saved models/Extero/Extero_priors_rt",".RDS"))
  
  # 
  # 
  # mod_norm_rt_noprior = cmdstanr::cmdstan_model(here::here("Stan",model,paste0(model,"_mixed_rt_noprior.stan")),stanc_options = list("O1"))
  # 
  # fit_norm <- mod_norm_rt_noprior$sample(
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
  # fit_norm$save_object(paste0("/home/jespere/Hierarchical-Interoception/saved models/nopriors/",model,"_rt",".RDS"))
  # 
  
}

run_model()
