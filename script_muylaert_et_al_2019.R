# Script Muylaert et al. (2019)
  
  ####################################
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  ls()
  rm(list = ls())
  gc() 
  options(digits=7, scipen=999)
  memory.limit(size= 1.75e13)
  # packages
  require(dplyr)
  require(INLA)
  require(coefINLA)
  # data
  load("data_9316.rda") # data
  load("g_models.rda")  # graph
  # casos 
  data_9316 %>% group_by(year) %>% dplyr::summarise(sum(cases)) %>% as.data.frame() 
  # casos transformados em zero e um
  data_9316 %>% group_by(year) %>% dplyr::summarise(sum(casesbin)) %>% as.data.frame()   
  
  ##########################################
  # Model with spatial and temporal effects#
  ##########################################
  
  d <- data_9316[data_9316$variable %in% 2000:2014 ,]  
  # 2000-2014 is the period with less zeros
  
  f_mb <-  casesbin ~  scale(s_hosts) + 
    scale(rural_workers_approxExtrap) +
    scale(mplu) +
    scale(t) +
    scale(pfor) +
    scale(pasture) +
    scale(sug) + 
    scale(peu) +
    scale(mz) +
    f(re_u, model="besag", graph= g, adjust.for.con.comp= FALSE,
      constr= TRUE, scale.model= TRUE) + 
    f(year, model = "rw2")
  
  mb <- inla(formula = f_mb,
             family = "binomial",
             data = d,
             verbose = TRUE,
             control.compute = list(config = TRUE, dic = TRUE, cpo=TRUE))
  
  save(mb, file = "mb.rda") 
  png(file = "coefINLA_mb.png")
  coefINLA(mb, intercept=FALSE)
  dev.off()
  #####################################################