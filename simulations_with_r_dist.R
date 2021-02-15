library(tidyverse)
library(forestNETN)
source("custom_rdist_from_QAQC.R")


reg_4yr <- joinRegenData(park = "all", from = 2016, to = 2019, speciesType = "native", canopyForm = "canopy") %>% 
           group_by(Unit_Code, Plot_Name, Year) %>% 
           summarize(seed_den = sum(seed.den),
                     sap_den = sum(sap.den),
                     stock_den = sum(stock))

length(unique(reg_4yr$Plot_Name)) - nrow(reg_4yr) # no dups
head(reg_4yr)

effect <- 0.5/2 

reg_4yr <- reg_4yr %>% mutate(seed_den_r = r_seed(1),
                              sap_den_r = r_sap(1),
                              stock_den_r = r_sap(1),
                              seed_den_c2 = seed_den + effect*seed_den + r_seed(1) ,
                              sap_den_c2 = sap_den + effect*sap_den + r_sap(1),
                              stock_den_c2 = stock_den + effect*stock_den + r_stock(1),
                              seed_den_c3 = seed_den + 2*effect*seed_den + r_seed(1) ,
                              sap_den_c3 = sap_den + 2*effect*sap_den + r_sap(1),
                              stock_den_c3 = stock_den + 2*effect*stock_den + r_stock(1))

head(reg_4yr)

# cols <- c("seed_den_c2", "seed_den_c3")
# reg_4yr[, cols][reg_4yr[, cols] < 0] <- 0

reg_seed_long <- reg_4yr %>% select(Unit_Code, Plot_Name, seed_den, seed_den_c2, seed_den_c3) %>% 
                 pivot_longer(cols = c(seed_den, seed_den_c2, seed_den_c3), 
                              names_to = "cycle", values_to = "seed_den") %>% 
                 mutate(cycle_num = case_when(cycle == "seed_den" ~ 1,
                                              cycle == "seed_den_c2" ~ 2,
                                              cycle == "seed_den_c3" ~ 3))

test50 <- lm(seed_den ~ cycle_num, data = reg_seed_long)
summary(test50)

ggplot(data = reg_seed_long, aes(x = cycle, y = seed_den, group = Plot_Name, color = Unit_Code))+
  geom_point()+ geom_line()+
  facet_wrap(~Unit_Code, scales = "free")

ggplot(data = reg_4yr, aes(x = sap_den, y = sap_den_new20, group = Unit_Code, color = Unit_Code))+
  geom_point()+ geom_smooth()+
  facet_wrap(~Unit_Code, scales = "free")

ggplot(data = reg_4yr, aes(x = stock_den, y = stock_den_new20, group = Unit_Code, color = Unit_Code))+
  geom_point()+ geom_smooth()+
  facet_wrap(~Unit_Code, scales = "free")

test20b <- lm(sap_den_new20 ~ sap_den, data = reg_4yr)
summary(test20b)
