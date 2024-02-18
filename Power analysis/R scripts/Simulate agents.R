erf <- function(x) 2 * pnorm(x * sqrt(2)) - 1


get_things_2con = function(parameters){
  
  alpha = array(NA,dim = c(parameters$subjects, parameters$ncon))
  cond = array(NA,dim = c(parameters$subjects, parameters$ncon))
  cond_beta = array(NA,dim = c(parameters$subjects, parameters$ncon))
  
  beta = array(NA,dim = c(parameters$subjects, parameters$ncon))
  gamma = array(NA,dim = c(parameters$subjects, parameters$ncon))
  lambda = array(NA,dim = c(parameters$subjects, parameters$ncon))
  
  library(faux)
  
  alphacor = 0
  
  while(alphacor < 0.3 | alphacor > 0.7){
    alphas = rnorm_multi(n = parameters$subjects, 
                         mu = rep(parameters$mu_alpha, parameters$ncon),
                         sd = rep(parameters$sd_alpha, parameters$ncon),
                         r = c(parameters$correlation_alpha), 
                         varnames = c("alpha_1", "alpha_2"),
                         empirical = FALSE)
    alphacor = cor.test(alphas[,1],alphas[,2])$estimate
    
  }
  
  betacor = -1
  while(betacor < 0 | betacor > 0.5){  
    betas = exp(rnorm_multi(n = parameters$subjects, 
                            mu = rep(parameters$mu_beta, parameters$ncon),
                            sd = rep(parameters$sd_beta, parameters$ncon),
                            r = c(parameters$correlation_beta), 
                            varnames = c("beta_1", "beta_2"),
                            empirical = FALSE))
    
    betacor = cor.test(betas[,1],betas[,2])$estimate
    
  }
  
  
  
  df = data.frame()
  #linear increase in condition from condition 1 to 2 to 3 to 4 with slope of cond_alpha and beta
  for(s in 1:(parameters$subjects)){
    cond[s,] = rnorm(parameters$ncon,parameters$mu_cond_alpha, parameters$sd_cond_alpha)
    cond_beta[s,] = rnorm(parameters$ncon,parameters$mu_cond_beta, parameters$sd_cond_beta)
    
    for (c in 1:(parameters$ncon)){
      
      alpha[s,c] = alphas[s,c]+(c-1)*cond[s,c]
      
      beta[s,c] = betas[s,c]+(c-1)*cond_beta[s,c]
      
      #beta[s,c] = rnorm(1,parameters$mu_beta+c1*parameters$mu_cond_beta,parameters$sd_beta)
      
      #gamma[s,c] = brms::inv_logit_scaled(rnorm(1,parameters$mu_gamma,parameters$sd_gamma))
      
      lambda[s,c] = brms::inv_logit_scaled(rnorm(1,parameters$mu_lambda,parameters$sd_lambda)) / 2
      
    }
  }
  
  lapse = data.frame(lambda) %>% mutate(participant_id = 1:parameters$subjects) %>% pivot_longer(c("X1","X2"), names_to = "sessions",values_to = "lapse")
  alpha = data.frame(alpha) %>% mutate(participant_id = 1:parameters$subjects) %>% pivot_longer(c("X1","X2"), names_to = "sessions",values_to = "alpha")
  beta = data.frame(beta) %>% mutate(participant_id = 1:parameters$subjects) %>% pivot_longer(c("X1","X2"), names_to = "sessions",values_to = "beta") 
  
  parameters2 = inner_join(inner_join(lapse,alpha,by = join_by(sessions, participant_id)),beta,by = join_by(sessions, participant_id))%>% mutate(id = 1:nrow(.), sessions = ifelse(sessions == "X1", 1,ifelse(sessions == "X2",2,NA)))
  
  return(parameters2)
}

get_psi_stim = function(parameters){
  
  python_script <- here::here("Power analysis","Python scripts","PSI.py")
  
  alpha = parameters$alpha
  
  beta = parameters$beta
  
  lapse = parameters$lapse
  
  trials = as.integer(parameters$trials)
  
  ids = as.integer(parameters$participant_id)
  
  subjects = max(as.integer(unique(parameters$participant_id)))
  
  sessions = max(as.integer(unique(parameters$sessions)))
  
  library(reticulate)
  
  # Use reticulate to run the Python script with arguments
  
  source_python(python_script, convert = FALSE)
  
  d = get_stim(lapse, alpha, beta, ids, trials, subjects, sessions)
  
  dd = reticulate::py_to_r(d)
  
  d = data.frame(lapse = dd$lapse, alpha = dd$alpha, beta = dd$beta, participant_id = unlist(dd$participant_id),
                 trials = unlist(dd$trials), subs = unlist(dd$subs), X = unlist(dd$X), resp = unlist(dd$resp), sessions = unlist(dd$sessions))
  
  directory = paste0("effect_size_alpha = " , round(parameters$alpha_power,2)[1],
                     " effect_size_beta = ", round(parameters$beta_power,2)[1],
                     " trials = ", parameters$trials[1],
                     " subjects = ", length(unique(parameters$participant_id))[1],
                     " random id = ", round(rnorm(1,0,1000),2),".csv")
  
  write.csv(d,here::here("Power analysis","Preliminary datasets",directory))
  
  return(d)
  
}
