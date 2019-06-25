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
 # Subsetting data 
d_0016 <- data_9316[data_9316$variable %in% 2000:2016,] # for binomial
# Creating df from positive counts
d_pos_0016 <- subset(d_0016, cases > 0 ) # for truncated poisson

# Set data from 2015 and 2016 to NA for binomial models
d_0016[d_0016$variable %in% 2015:2016, "casesbin"] <- NA
# Set count data from 2015 and 2016 to NA
d_pos_0016[d_pos_0016$variable %in% 2015:2016, "cases"] <- NA 

# binomial
f_mb <-  casesbin ~ scale(s_hosts) + 
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

mb_NA1516 <- inla(formula = f_mb,
                        family = "binomial",
                        data = d_0016,
                        verbose = TRUE,
                        control.compute = list(config = TRUE, dic = TRUE, cpo=TRUE))

# Save model
save(mb_NA1516, file = "mb_NA1516.rda") 

require(coefINLA)
coefINLA(mb_NA1516, intercept = TRUE, "Greys") +
  labs(title = "Posterior distributions",
       subtitle = "with 95% credible intervals") +
  ggsave(filename = "coefINLA_mb_NA1516.png", dpi = 600,
         width = 18, height = 14, units= "cm")


# Truncated Poisson model
# a very negative and fixed hyperpar approximates pi i to zero 
Theta_neg_fixed <-  list(hyper = list(theta = list(initial = -10, fixed = TRUE)))

f_mpos <-  cases ~  scale(s_hosts) + 
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

mpos_NA1516 <- inla(formula = f_mpos,
             family = "zeroinflatedpoisson0",
             control.family = list(list(Theta_neg_fixed)),
             data = d_pos_0016,
             verbose = TRUE,
             control.compute = list(config = TRUE, dic = TRUE, cpo = TRUE))

# Improving model
mpos_NA1516_improved <- inla.cpo(mpos_NA1516)

# Save model
save(mpos_NA1516, file = "mb_NA1516.rda")
save(mpos_NA1516_improved, file = "mb_NA1516_improved.rda")

# Coef plot
coefINLA(mpos_NA1516, intercept = TRUE, "Greys") +
  labs(title = "Posterior distributions",
       subtitle = "with 95% credible intervals") +
  ggsave(filename = "coefINLA_mpos_NA1516.png", dpi = 600,
         width = 18, height = 14, units= "cm")
