power_analysis = function(parameters){
  source(here::here("Power analysis","R scripts", "Simulate agents.R"))
  
  if(parameters$parameter == "alpha"){
    change_in_sd_cohens_alpha = 0
    lapse = "NaN"
    while(!((change_in_sd_cohens_alpha < (parameters$effect_size+0.005)) & (change_in_sd_cohens_alpha > (parameters$effect_size-0.005)) & lapse != "NaN")){
      a = get_things_2con(parameters = data.frame(subjects = parameters$subjects,
                                                  ncon = 2,
                                                  mu_cond_alpha = parameters$mu_cond,
                                                  sd_cond_alpha = parameters$sd_cond,
                                                  mu_cond_beta = parameters$mu_cond_beta,
                                                  sd_cond_beta = parameters$sd_cond_beta,
                                                  mu_alpha = rnorm(1,-8.81,0.50),
                                                  sd_alpha = rnorm(1,11.19,0.38),
                                                  mu_beta = rnorm(1,2.25, 0.02),
                                                  sd_beta = rnorm(1, 0.25, 0.02),
                                                  mu_lambda = rnorm(1,-5.58,0.77),
                                                  sd_lambda = rnorm(1,2.77,0.45),
                                                  correlation_alpha = 0.5,
                                                  correlation_beta = 0.34
      ))
      
      betacor = cor.test(a %>% filter(sessions == 1) %>% .$beta, a %>% filter(sessions == 2) %>% .$beta)
      
      effectsizedata_beta = a %>% group_by(sessions) %>% summarize(mean = mean(beta), sd = sd(beta))
      
      
      change_in_beta = (effectsizedata_beta[1,2]-effectsizedata_beta[2,2])[[1]]
      change_in_sd_cohens_beta = ((effectsizedata_beta[1,2]-effectsizedata_beta[2,2])/(sqrt((effectsizedata_beta[1,3]^2 + effectsizedata_beta[2,3]^2)/2)))[[1]]
      
      
      effectsizedata_alpha = a %>% group_by(sessions) %>% summarize(mean = mean(alpha), sd = sd(alpha))
      
      change_in_alpha = (effectsizedata_alpha[1,2]-effectsizedata_alpha[2,2])[[1]]
      change_in_sd_cohens_alpha = -((effectsizedata_alpha[1,2]-effectsizedata_alpha[2,2])/(sqrt((effectsizedata_alpha[1,3]^2 + effectsizedata_alpha[2,3]^2)/2)))[[1]]
      
      alphacor = cor.test(a %>% filter(sessions == 1) %>% .$alpha, a %>% filter(sessions == 2) %>% .$alpha)
      
      
      lapse = a$lapse[1]
    }
  }else if(parameters$parameter == "beta"){
    change_in_sd_cohens_beta = 0
    lapse = "NaN"
    while(!((change_in_sd_cohens_beta < (parameters$effect_size+0.005)) & (change_in_sd_cohens_beta > (parameters$effect_size-0.005)) & lapse != "NaN")){
      a = get_things_2con(parameters = data.frame(subjects = parameters$subjects,
                                                  ncon = 2,
                                                  mu_cond_alpha = parameters$mu_cond,
                                                  sd_cond_alpha = parameters$sd_cond,
                                                  mu_cond_beta = parameters$mu_cond_beta,
                                                  sd_cond_beta = parameters$sd_cond_beta,
                                                  mu_alpha = rnorm(1,-8.81,0.50),
                                                  sd_alpha = rnorm(1,11.19,0.38),
                                                  mu_beta = rnorm(1,2.25, 0.02),
                                                  sd_beta = rnorm(1, 0.25, 0.02),
                                                  mu_lambda = rnorm(1,-5.58,0.77),
                                                  sd_lambda = rnorm(1,2.77,0.45),
                                                  correlation_alpha = 0.5,
                                                  correlation_beta = 0.34
      ))
      betacor = cor.test(a %>% filter(sessions == 1) %>% .$beta, a %>% filter(sessions == 2) %>% .$beta)
      
      effectsizedata_beta = a %>% group_by(sessions) %>% summarize(mean = mean(beta), sd = sd(beta))
      
      
      change_in_beta = (effectsizedata_beta[2,2]-effectsizedata_beta[1,2])[[1]]
      change_in_sd_cohens_beta = ((effectsizedata_beta[2,2]-effectsizedata_beta[1,2])/(sqrt((effectsizedata_beta[1,3]^2 + effectsizedata_beta[2,3]^2)/2)))[[1]]
      
      
      effectsizedata_alpha = a %>% group_by(sessions) %>% summarize(mean = mean(alpha), sd = sd(alpha))
      
      change_in_alpha = (effectsizedata_alpha[1,2]-effectsizedata_alpha[2,2])[[1]]
      change_in_sd_cohens_alpha = -((effectsizedata_alpha[1,2]-effectsizedata_alpha[2,2])/(sqrt((effectsizedata_alpha[1,3]^2 + effectsizedata_alpha[2,3]^2)/2)))[[1]]
      
      alphacor = cor.test(a %>% filter(sessions == 1) %>% .$alpha, a %>% filter(sessions == 2) %>% .$alpha)
      
      
      lapse = a$lapse[1]
    }
  }else if (parameters$parameter == "both"){
    change_in_sd_cohens_beta = 0
    change_in_sd_cohens_alpha = 0
    
    lapse = "NaN"
    while(!((change_in_sd_cohens_beta < (parameters$effect_size_beta+0.005)) & (change_in_sd_cohens_beta > (parameters$effect_size_beta-0.005)) &
            (change_in_sd_cohens_alpha < (parameters$effect_size_alpha+0.005)) & (change_in_sd_cohens_alpha > (parameters$effect_size_alpha-0.005)) & lapse != "NaN")){
      

      a = get_things_2con(parameters = data.frame(subjects = parameters$subjects,
                                                  ncon = 2,
                                                  mu_cond_alpha = parameters$mu_cond,
                                                  sd_cond_alpha = parameters$sd_cond,
                                                  mu_cond_beta = parameters$mu_cond_beta,
                                                  sd_cond_beta = parameters$sd_cond_beta,
                                                  mu_alpha = rnorm(1,-8.81,0.50),
                                                  sd_alpha = rnorm(1,11.19,0.38),
                                                  mu_beta = rnorm(1,2.25, 0.02),
                                                  sd_beta = rnorm(1, 0.25, 0.02),
                                                  mu_lambda = rnorm(1,-5.58,0.77),
                                                  sd_lambda = rnorm(1,2.77,0.45),
                                                  correlation_alpha = 0.5,
                                                  correlation_beta = 0.34
      ))
      betacor = cor.test(a %>% filter(sessions == 1) %>% .$beta, a %>% filter(sessions == 2) %>% .$beta)
      
      effectsizedata_beta = a %>% group_by(sessions) %>% summarize(mean = mean(beta), sd = sd(beta))
      
      
      change_in_beta = (effectsizedata_beta[2,2]-effectsizedata_beta[1,2])[[1]]
      change_in_sd_cohens_beta = ((effectsizedata_beta[2,2]-effectsizedata_beta[1,2])/(sqrt((effectsizedata_beta[1,3]^2 + effectsizedata_beta[2,3]^2)/2)))[[1]]
      
      
      effectsizedata_alpha = a %>% group_by(sessions) %>% summarize(mean = mean(alpha), sd = sd(alpha))
      
      change_in_alpha = (effectsizedata_alpha[1,2]-effectsizedata_alpha[2,2])[[1]]
      change_in_sd_cohens_alpha = -((effectsizedata_alpha[1,2]-effectsizedata_alpha[2,2])/(sqrt((effectsizedata_alpha[1,3]^2 + effectsizedata_alpha[2,3]^2)/2)))[[1]]
      
      alphacor = cor.test(a %>% filter(sessions == 1) %>% .$alpha, a %>% filter(sessions == 2) %>% .$alpha)
      
      
      lapse = a$lapse[1]  
    }
    }else{
        print("wrong parameter input")
    }
  
  print("done finding parameter values")
  
  #getting Psi stimuli
  df = a %>% arrange(sessions,participant_id) %>% mutate(trials = parameters$trials)
  
  df$alpha_power = change_in_sd_cohens_alpha
  df$beta_power = change_in_sd_cohens_beta
  
  ########################################################################################################### GETTING PSI!
  data = get_psi_stim(df)
  ###########################################################################################################
  
  #wrangling the data
  
  data = inner_join(df,data %>% dplyr::select(resp,X,participant_id,sessions), by = c("participant_id","sessions"))
  
  #reformatting and fitting in stan
  ses_times_par = length(unique(data$participant_id))*length(unique(data$sessions))
  
  
  xs = array(NA, dim = c(ses_times_par,data$trials[1]))
  for(i in 1:data$trials[1]){
    xs[,i] = data %>% arrange(sessions,participant_id) %>% mutate(trial = rep(1:data$trials[1],ses_times_par)) %>% filter(trial == i) %>% .$X
    
  }
  
  
  datastan = list(Y = data %>% arrange(sessions,participant_id) %>% .$resp,
                  T = nrow(data %>% filter(sessions == 1 & participant_id == 1)),
                  P = length(unique(data$participant_id)),
                  S = length(unique(data$sessions)),
                  X = xs,
                  condition1 = c(rep(0,length(unique(data$participant_id))),
                                 rep(1,length(unique(data$participant_id)))))
  
  
  mod_norm_prior = cmdstanr::cmdstan_model(here::here("Power analysis","Stan models","Simulations","Within subject design.stan"),stanc_options = list("O1"))
  
  
  #fitting
  fit_norm <- mod_norm_prior$sample(
    data = datastan,
    iter_sampling = 2000,
    iter_warmup = 2000,
    chains = 4,
    parallel_chains = 4,
    refresh = 100,
    adapt_delta = 0.8,
    max_treedepth = 12
  )
  
  diags = data.frame(fit_norm$diagnostic_summary())
  
  
  
  alpha_draws = as_draws_df(fit_norm$draws("mu_g[4]")) %>% .$`mu_g[4]`
  beta_draws = as_draws_df(fit_norm$draws("mu_g[5]")) %>% .$`mu_g[5]`
  
  p_zero_alpha = sum(alpha_draws<0)/length(alpha_draws)
  

  if(parameters$parameter == "both"){
    if(parameters$effect_size_beta > 0){
      p_zero_beta = sum(beta_draws<0)/length(beta_draws)
    }else if(parameters$effect_size_beta < 0){
      p_zero_beta = sum(beta_draws>0)/length(beta_draws)
    }else{
      p_zero_beta = sum(beta_draws<0)/length(beta_draws)
    }
  }
  if(parameters$parameter == "beta"){
    if(parameters$effect_size > 0){
      p_zero_beta = sum(beta_draws<0)/length(beta_draws)
    }else if(parameters$effect_size < 0){
      p_zero_beta = sum(beta_draws>0)/length(beta_draws)
    }else{
      p_zero_beta = sum(beta_draws<0)/length(beta_draws)
    }
  }
  if(parameters$parameter == "alpha"){
    p_zero_beta = sum(beta_draws<0)/length(beta_draws)
  }
    
  alphas = as_draws_df(fit_norm$summary("alphas")) %>% mutate(real_values = df$alpha, parameter = "alpha")
  betas = as_draws_df(fit_norm$summary("betas"))%>% mutate(real_values = df$beta, parameter = "beta")
  
  indi_estimates = rbind(alphas,betas)
  
  
  #saving
  difs = data.frame(fit_norm$summary(c("mu_g[1]","mu_g[2]","mu_g[3]","mu_g[4]","mu_g[5]"))) %>% mutate(iter = parameters$id)%>% 
    mutate(effectsize_alpha = change_in_sd_cohens_alpha,
           effectsize_beta = change_in_sd_cohens_beta,
           subjects = parameters$subjects,
           trials = parameters$trials,
           sim_alphacor = alphacor$estimate,
           sim_betacor = betacor$estimate,
           divergences = mean(diags$num_divergent),
           treedepths = mean(diags$num_max_treedepth),
           p_alpha = p_zero_alpha,
           p_beta = p_zero_beta)
  
  
  
  
  return(list(difs,indi_estimates))
  
}
