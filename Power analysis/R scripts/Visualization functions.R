get_df = function(subjects,trials,parameter){
  files = list.files(here::here("Power analysis","Power analysis results",parameter), recursive = T)
  
  realfiles = files[grepl(paste0("trials=",trials),files)]
  
  realfiles = realfiles[grepl(paste0("subjects=",subjects),realfiles)]
  
  df = data.frame()
  for(i in 1:length(realfiles)){
    effectsize = str_sub(realfiles[i], nchar(realfiles[i])-6,nchar(realfiles[i])-4)
    result = readRDS(here::here("Power analysis","Power analysis results", parameter,realfiles[i]))
    tester = map_dfr(result, 1) %>% mutate(effectsize = as.numeric(effectsize), parameters = parameter)
    df = rbind(df,tester)
  }
  return(df)
}


get_full_df = function(parameter){
  dirs = list.files(here::here("Power analysis","Power analysis results",parameter))
  
  matches <- str_match(dirs, "results_subjects=(\\d+)_trials=(\\d+)")
  
  # Convert matched values to numeric
  subjects <- as.numeric(matches[, 2])
  trials <- as.numeric(matches[, 3])
  
  
  df_list <- lapply(seq(length(dirs)), function(i) {
    get_df(subjects[i], trials[i], parameter)
  })
  return(do.call(rbind, df_list))
  
}



fit_results = function(df,parameter, ntrials, signi_level = 0.05, desired_power = 0.8){
  ## modeling
  alpha = signi_level
  
  p_zero = paste0("p_",parameter)
  
  group_change = paste0("effectsize_",parameter)
  
  if(parameter == "alpha"){
    
    ff = df %>% filter(variable == "mu_g[4]") %>% 
      mutate(significant = ifelse(get(p_zero)<alpha, 1, 0),
             subjects = as.factor(subjects),
             trials = as.factor(trials)) %>% 
      select(group_change, significant, subjects, trials) %>% 
      rename(group_change = group_change)
  }
  
  
  
  if(parameter == "beta"){
    
    ff = df %>% filter(variable == "mu_g[5]") %>% 
      mutate(significant = ifelse(get(p_zero)<alpha, 1, 0),
             subjects = as.factor(subjects),
             trials = as.factor(trials)) %>% 
      select(group_change, significant, subjects, trials) %>% 
      rename(group_change = group_change)
    
  }
  
  ff = ff %>% filter(subjects != 40, trials != 150)
  
  ff$subjects = droplevels(ff$subjects)
  ff$trials = droplevels(ff$trials)
  
  
  
  ff = ff %>% arrange(trials,subjects)
  
  
  levels_factor1 <- levels(ff$subjects)
  levels_factor2 <- levels(ff$trials)
  
  # Create a matrix with all combinations
  TnS <- matrix(NA, nrow = nrow(ff), ncol = length(levels_factor1) * length(levels_factor2))
  
  
  col_names <- outer(levels_factor1, levels_factor2, FUN = function(x, y) paste(x, y, sep = "_"))
  colnames(TnS) <- col_names
  
  # Fill the matrix with the combinations
  for (i in seq_along(levels_factor1)) {
    for (j in seq_along(levels_factor2)) {
      TnS[, (i - 1) * length(levels_factor2) + j] <- as.numeric(ff$subjects == levels_factor1[i] & ff$trials == levels_factor2[j])
    }
  }
  
  
  
  standata = list(x = ff$group_change,
                  N = nrow(ff),
                  nT = length(unique(ff$trials)),
                  nS = length(unique(ff$subjects)),
                  TnS = as.matrix(TnS),
                  y = ff$significant)
  
  
  mod = cmdstanr::cmdstan_model(here::here("Power analysis","Stan models","Visualization","indpendent trials and subject effects.stan"),stanc_options = list("O1"))
  
  
  fit_cond_cmd <- mod$sample(
    data = standata, 
    chains = 4, 
    iter_warmup = 1000,
    iter_sampling = 1000,
    parallel_chains = 4,
    adapt_delta = 0.80,
    max_treedepth = 12
  )
  
  
  #looking at the parameter values of this psychometric to gain infomation on these
  
  combinations <- expand.grid(levels_factor2,levels_factor1)
  
  ddalpha = data.frame(fit_cond_cmd$summary("alphas")) %>% 
    mutate(new_column = paste(combinations$Var1, combinations$Var2, sep = "_")) %>% 
    separate(new_column, c("trials", "subjects"), sep = "_", convert = TRUE) %>% 
    mutate(trials = as.factor(trials), subjects = as.factor(subjects), parameter = "alpha")
  
  
  ddbeta = data.frame(fit_cond_cmd$summary("betas")) %>% 
    mutate(new_column = paste(combinations$Var1, combinations$Var2, sep = "_")) %>% 
    separate(new_column, c("trials", "subjects"), sep = "_", convert = TRUE) %>% 
    mutate(trials = as.factor(trials), subjects = as.factor(subjects), parameter = "beta",
           mean = exp(mean), q5 = exp(q5), q95 = exp(q95))
  
  
  
  rbind(ddalpha,ddbeta) %>% ggplot()+
    geom_pointrange(aes(x = subjects, y = mean, ymin = q5, ymax = q95, col = trials), width = 0.2, position=position_dodge(width=0.3))+
    facet_wrap(~parameter)
  
  
  #psychometric fits
  rbind(ddalpha,ddbeta) %>% select(mean,parameter,trials,subjects) %>% 
    pivot_wider(names_from = c("parameter"), values_from = c("mean")) %>% rowwise() %>% 
    mutate(xs = list(seq(0,3,by = 0.1)), ys = list(pnorm(seq(0,3,by = 0.1), mean = alpha, sd = beta))) %>% 
    unnest() %>% ggplot(aes(x = xs, y = ys, col = interaction(trials,subjects)))+geom_line()
  
  
  ff = ff %>% filter(trials == ntrials)
  
  standata = list(x = ff$group_change,
                  N = nrow(ff),
                  Subs = as.numeric(as.character(ff$subjects)),
                  Trials = ff$trials,
                  y = ff$significant)
  
  
  mod = cmdstanr::cmdstan_model(here::here("Power analysis","Stan models","Visualization","exponential decay with asymptote.stan"),stanc_options = list("O1"))
  
  #mod = cmdstanr::cmdstan_model("Power analysis","Stan models","Visualization","exponential decay without asymptote.stan"))
    
  
  fit <- mod$sample(
    data = standata, 
    chains = 4, 
    iter_warmup = 1000,
    iter_sampling = 1000,
    parallel_chains = 4,
    adapt_delta = 0.80,
    max_treedepth = 12
  )
  
  
  exp_decay = function(x,int,expo,asym){
    
    return(int * exp(-expo * x) + asym)
    
  }
  
  
  get_decay_line = function(fit,parameter, nsubs){
    
    params = fit$summary(c(paste0("int_",parameter),paste0("expo_",parameter),paste0("asym_",parameter)))
    
    params = params %>% select(variable,mean) %>% pivot_wider(names_from = variable, values_from = mean)
    
    params = params %>% 
      mutate(x = list(5:nsubs), y = list(exp_decay(5:nsubs,get(paste0("int_",parameter)), get(paste0("expo_",parameter)), get(paste0("asym_",parameter))))) %>% 
      unnest() %>% 
      mutate(parameter = parameter) %>% 
      dplyr::select(x,y,parameter)
    
  }
  
  
  dcalpha = get_decay_line(fit,"alpha",nsubs = max(df_alpha$subjects))
  dcbeta = get_decay_line(fit,"beta",nsubs = max(df_alpha$subjects))
  
  parameters = rbind(ddalpha,ddbeta) %>% filter(trials == ntrials) %>% 
    mutate(subjects = as.numeric(as.character(subjects))) %>% 
    ggplot()+
    geom_pointrange(aes(x = subjects, y = mean, ymin = q5, ymax = q95, col = trials), width = 0.2, position=position_dodge(width=0.3))+
    facet_wrap(~parameter, scales = "free")+
    geom_line(data = rbind(dcalpha,dcbeta), aes(x = x, y = y))
  
  
  # next we might that the model fitted individually to the significance on differing trials and subjects and 
  # get the desired power for each subject / trial combination and  plot this
  
  # effect sizes at power = power
  power = desired_power
  
  effectsizeplot = rbind(ddalpha,ddbeta) %>% filter(trials != 150) %>% select(mean,parameter,trials,subjects) %>% 
    pivot_wider(names_from = c("parameter"), values_from = c("mean")) %>% rowwise() %>% 
    mutate(effectsize_at_power = qnorm(power,alpha,beta)) %>% ggplot()+
    geom_point(aes(x = subjects, y = effectsize_at_power,col = trials), width = 0.2, position=position_dodge(width=0.3))+
    ylab(paste0("effect size at power = ", power))+theme_classic()+coord_cartesian(ylim = c(0,3.5))
  
  
  return(list(parameters, effectsizeplot))
  
  
}