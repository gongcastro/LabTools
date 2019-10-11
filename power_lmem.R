# Power analysis for Linear Mixed-Effects Models
# Gonzalo Garcia-Castro, gonzalo.garciadecastro@upf.edu
# Center for Brain and Cognition, Pompeu Fabra University

power_lmem <- function(
  
  #### set parameteres ####################################
  alpha        = 0.05, # significance criterion
  d            = 0.50, # Cohen's d
  mean_control = 0.00,
  sd           = 2.5,
  subjects     = 20,   # number of subjects
  trials       = 80,   # number of trials
  conditions   = 2,    # levels of factor of interest (n_trials should be multiple of n_conditions)
  simulations  = 100   # number of simulated datasets
){
  
  #### load packages ######################################
  require(dplyr)    # for manipulating variables
  require(tidyr)    # for reshaping datasets
  require(ggplot2)  # for visualising data
  require(optimx)   # for using different opmitisers
  require(car)      # for performing ANOVAS
  require(lmerTest) # for p-values in LMEM
  require(purrr)    # for working with several datasets in paralell
  
  #### simulate datasets ##################################
  mean_experimental <- abs(-d*(sqrt((2*(subjects-1)*sd^2)/(2*subjects-2))) + mean_control)
  
  data <- map(.x = vector("list", simulations),
              .f = ~data.frame(
                subject   = rep(paste0("subj", 1:subjects), each = trials),
                trial     = rep(1:trials, times = subjects),
                condition = factor(rep(paste0("condition", 1:conditions), each = trials/2, times = subjects),
                                   levels = c("condition1", "condition2"))
              )
  ) %>%
    map(., ~mutate(.,
                   response = rnorm(
                     n    = subjects*trials,
                     mean = case_when(condition == "condition1" ~ mean_control, TRUE ~ mean_experimental),
                     sd   = sd
                   )
    )
    ) %>%
    set_names(paste0("simulation", 1:simulations))
  
  #### fit models #########################################
  models <- map(
    .x = data,
    .f = ~lmerTest::lmer(
      response ~ condition + (1+condition|subject),
      control = lmerControl(optimizer ='bobyqa', optCtrl=list(method='nlminb')),
      REML = FALSE,
      data = .x
    )
  )
  
  #### ANOVA ##############################################
  anovas <- map(
    .x = models,
    .f = ~anova(.x, ddf = "Satterthwaite")
  )
  
  #### calculate power ####################################
  power <- map(
    .x = anovas,
    "Pr(>F)"
  ) %>%
    bind_rows(.id = "simulation") %>%
    gather("simulation", "p") %>%
    mutate(sig = p <= alpha) %>%
    summarise(power = mean(sig)) %>%
    pull(power)
  
  #### output #############################################
  print(paste0("Power to detect a ",
               abs(mean_control-mean_experimental),
               " (Cohen's d = ", round(d, 2),
               ") difference between conditions is ",
               round(power, 3)*100,
               "%"))
  
  #### visualise data ######################################
  data %>%
    bind_rows(.id = "simulation") %>%
    group_by(simulation, subject, condition) %>%
    summarise(response = mean(response)) %>%
    ungroup() %>%
    group_by(simulation, condition) %>%
    summarise(response = mean(response)) %>%
    ggplot(., aes(condition, response)) +
    geom_violin() +
    geom_point(position = position_jitter(width = 0.1), alpha = 0.5) +
    stat_summary(fun.data = mean_se, geom = "pointrange", colour = "red", size = 1)

}




