library(tidyverse)
library(forestNETNarch)
source("custom_rdist_from_QAQC.R")

reg_4yr <- joinRegenData(park = "all", from = 2008, to = 2011, speciesType = "native", canopyForm = "canopy") %>% 
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
                              seed_den_c3 = seed_den + effect*seed_den_c2 + r_seed(1)*seed_den_c2,
                              sap_den_c3 = sap_den + effect*sap_den_c2 + r_sap(1)*sap_den_c2,
                              stock_den_c3 = stock_den + effect*stock_den_c2 + r_stock(1)*stock_den_c2)

reg_seed_long <- reg_4yr %>% select(Unit_Code, Plot_Name, seed_den, seed_den_c2, seed_den_c3) %>% 
                 pivot_longer(cols = c(seed_den, seed_den_c2, seed_den_c3), 
                              names_to = "cycle", values_to = "seed_den") %>% 
                 mutate(cycle = case_when(cycle == "seed_den" ~ 1,
                                              cycle == "seed_den_c2" ~ 2,
                                              cycle == "seed_den_c3" ~ 3))

test <- lm(seed_den ~ cycle, data = reg_seed_long)
summary(test)

new_test <- data.frame(cycle = c(1,2,3))
predict(test, newdata = new_test)

head(reg_seed_long)
head(reg_3cyc)

reg_seed_long$type = "Fake"
reg_3cyc$type = 'Real'

comb_seeds <- rbind(reg_3cyc[,c("Unit_Code", "Plot_Name", "cycle", "type", "seed_den")], 
                    reg_seed_long[,c("Unit_Code", "Plot_Name", "cycle", "type", "seed_den")])

comp_plots <- function(park){
p <- ggplot(data = comb_seeds %>% filter(Unit_Code == park), 
       aes(x = cycle, y = seed_den, color = type, group = type))+
  geom_point(alpha = 0.2)+
  geom_line(aes(x = cycle, y = seed_den, group = interaction(type, Plot_Name)),alpha = 0.2)+
  #geom_smooth(method = 'lm', formula = y ~ x, se = F)+
  theme_FHM()+
  scale_color_manual(values = c("#818181", "ForestGreen"))+
  scale_x_continuous(breaks = c(1,2,3), limits = c(0.9, 3.1))+
  facet_wrap(~type)+ theme(legend.position = "none", 
                           plot.title = element_text(hjust = 0.5, size = 10))+
  labs(y = "Seedling density", x = NULL, title = park)
return(p)
}

comps <- map(unique(comb_seeds$Unit_Code), ~comp_plots(.x)) %>% set_names(unique(comb_seeds$Unit_Code))

gridExtra::grid.arrange(grobs = comps, ncol = 2)

# ENDED HERE

#--------------------
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
