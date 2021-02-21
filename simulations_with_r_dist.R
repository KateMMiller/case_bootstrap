library(tidyverse)
library(forestNETNarch)
source("custom_rdist_from_QAQC.R")

reg_4yr <- joinRegenData(park = "all", from = 2016, to = 2019, speciesType = "native", canopyForm = "canopy") %>% 
           group_by(Unit_Code, Plot_Name, Year) %>% 
           summarize(seed_den = sum(seed.den),
                     sap_den = sum(sap.den),
                     stock_den = sum(stock))

length(unique(reg_4yr$Plot_Name)) - nrow(reg_4yr) # no dups
head(reg_4yr)
ncycles = 3
tot_inc = 0.5 # Overall % increase or by cycle
tot_inc = 0 # Just add noice
effect = tot_inc#/(ncycles-1) #50% increase over 3 cycles

reg_4yr <- reg_4yr %>% mutate(seed_den_c2 = seed_den + effect*seed_den + r_seed(1)*seed_den ,
                              sap_den_c2 = sap_den + effect*sap_den + r_sap(1)*sap_den,
                              stock_den_c2 = stock_den + effect*stock_den + r_stock(1)*stock_den,
                              seed_den_c3 = seed_den + effect*seed_den_c2 + r_seed(1)*seed_den,
                              sap_den_c3 = sap_den + effect*sap_den_c2 + r_sap(1)*sap_den,
                              stock_den_c3 = stock_den + effect*stock_den_c2 + r_stock(1)*stock_den)

reg_seed_long <- reg_4yr %>% select(Unit_Code, Plot_Name, seed_den, seed_den_c2, seed_den_c3) %>% 
                 pivot_longer(cols = c(seed_den, seed_den_c2, seed_den_c3), 
                              names_to = "cycle", values_to = "seed_den") %>% 
                 mutate(cycle_num = case_when(cycle == "seed_den" ~ 1,
                                              cycle == "seed_den_c2" ~ 2,
                                              cycle == "seed_den_c3" ~ 3))

test50 <- lm(seed_den ~ cycle_num, data = reg_seed_long)
summary(test50)

new_test <- data.frame(cycle_num = c(1,2,3))
predict(test50, newdata = new_test)

fake <- ggplot(data = reg_seed_long, aes(x = cycle_num, y = seed_den, color = Unit_Code))+
  geom_point(alpha = 0.4)+ 
  geom_line(aes(group = Plot_Name), alpha = 0.2)+ 
  theme_FHM()+
  ylab("fake seedling dens.")+
  #stat_smooth(method = 'lm', formula = y~x, se = FALSE, col = 'black')+
  facet_wrap(~Unit_Code, scales = "free")+
  theme(legend.position = 'none')


reg_3cyc <- joinRegenData(park = "all", from = 2008, to = 2019, speciesType = "native", canopyForm = "canopy") %>% 
  group_by(Unit_Code, Plot_Name, Year) %>% 
  summarize(seed_den = sum(seed.den),
            sap_den = sum(sap.den),
            stock_den = sum(stock)) %>% 
  mutate(cycle = case_when(Year < 2012 ~ 1,
                           between(Year, 2012, 2015) ~ 2,
                           Year > 2015 ~ 3))

real <- ggplot(data = reg_3cyc, aes(x = cycle, y = seed_den, color = Unit_Code))+
  geom_point(alpha = 0.4)+ 
  geom_line(aes(group = Plot_Name), alpha = 0.2)+ 
  theme_FHM()+
  ylab("real seedling dens.")+
  #stat_smooth(method = 'lm', formula = y~x, se = FALSE, col = 'black')+
  facet_wrap(~Unit_Code, scales = "free")+
  theme(legend.position = 'none')

# Need to control the scales between fake and real for better check
cowplot::plot_grid(fake, real, ncol = 2)
?plot_grid
