#---------------------
# Estimate sd for simulation using QAQC data 
#---------------------
library(tidyverse)
library(forestNETN)
library(pdqr)
#library(viridis)
options(scipen = 100, digits = 6)

importData()

# Omitted 2006, because only sampled 1 microplot that year
all_evs <- joinLocEvent(park = 'all', from = 2007, to = 2019, QAQC = TRUE) %>%                         
  mutate(Event = paste(Plot_Name, Year, sep = "-"))

evs_q <- all_evs %>% filter(Event_QAQC == TRUE) %>% 
  select(Event_ID, Event, Plot_Name, Year) %>% arrange(Year, Plot_Name) %>% 
  rename(Event_IDq = Event_ID)

evs_comb <- left_join(evs_q, all_evs[all_evs$Event_QAQC == FALSE, c("Event_ID", "Event", "Plot_Name", "Year")], 
                      by = c("Event", "Plot_Name", "Year")) %>% filter(!is.na(Event_ID))

qaqc_list <- sort(unique(evs_comb$Event))
qaqc_list

#-------------------
# Regeneration Data
reg1 <- joinRegenData(from = 2007, to = 2019, QAQC = TRUE, speciesType = "native", canopyForm = "canopy") %>% 
  mutate(Event = paste(Plot_Name, Year, sep = "-")) %>% filter(Event %in% qaqc_list)

names(reg1)

reg_sum <- reg1 %>% group_by(Event_ID, Event, Plot_Name, Unit_Code, Year, Event_QAQC) %>%
  summarize(seed_den = sum(seed.den, na.rm = T),
            sap_den = sum(sap.den, na.rm = T),
            stock = sum(stock, na.rm = T), 
            .groups = 'drop') %>% arrange(Event, Event_QAQC)

head(reg_sum)

reg_wide <- reg_sum %>% pivot_wider(id_cols = c(Event, Plot_Name, Unit_Code, Year), 
                                    names_from = Event_QAQC, 
                                    names_glue = "{.value}_{ifelse(Event_QAQC == TRUE, 'q', 'c')}",
                                    values_from = c(seed_den, sap_den, stock))

reg_wide <- reg_wide %>% mutate(seed_den_diff = seed_den_c - seed_den_q,
                                sap_den_diff = sap_den_c - sap_den_q,
                                stock_diff = stock_c - stock_q)

reg_wide_yr <- reg_wide %>% group_by(Year) %>% summarize(seed_dif_mean = mean(seed_den_diff),
                                                         sap_dif_mean = mean(sap_den_diff),
                                                         stock_dif_mean = mean(stock_diff))

ggplot(reg_wide_yr, aes(x = Year, y = seed_dif_mean)) + geom_point() + theme_FHM()
ggplot(reg_wide_yr, aes(x = Year, y = sap_dif_mean)) + geom_point() + theme_FHM()
ggplot(reg_wide_yr, aes(x = Year, y = stock_dif_mean)) + geom_point() + theme_FHM()

reg_wide_yr
# Create my own density functions to generate random values from
r_seed <- new_r(reg_wide$seed_den_diff, type = "continuous")
r_sap <- new_r(reg_wide$sap_den_diff, type = "continuous")
r_stock <- new_r(reg_wide$stock_diff, type = "continuous")

reg_sd <- reg_wide %>% 
  summarize(
    seed_den_mean = mean(seed_den_diff),
    sap_den_mean = mean(sap_den_diff),
    stock_den_mean = mean(stock_diff),
    seed_den_sd = sd(seed_den_diff),
    sap_den_sd = sd(sap_den_diff),
    stock_sd = sd(stock_diff))

reg_sd

# Overall and compare with diff. distributions
ggplot(reg_wide, aes(x = seed_den_diff))+ 
  geom_density(fill = "#A8CF9C", col = "#A8CF9C", alpha = 0.5) +
   stat_function(fun = dnorm, geom = 'density', n = nrow(reg_wide), 
     args = list(mean = 0, sd = reg_sd$seed_den_sd),
                  col = "#a2a2a2", fill = "#a2a2a2", alpha = 0.6)+
  stat_function(fun = dt, geom = 'density', 
                args = list(df = nrow(reg_wide)-1), col = '#CFA29C', fill = '#CFA29C', alpha = 0.6)+
  theme_FHM() + labs(x = "C - Q diff Seedling Dens.")


ggplot(reg_wide, aes(x = sap_den_diff))+ 
  geom_density(fill = "#A8CF9C", col = "#A8CF9C", alpha = 0.5) +
  stat_function(fun = dnorm, geom = 'density', n = nrow(reg_wide), 
                args = list(mean = 0, sd = reg_sd$sap_den_sd),
                col = "#a2a2a2", fill = "#a2a2a2", alpha = 0.6)+ #grey
  stat_function(fun = dt, geom = 'density', col = '#CFA29C', fill = '#CFA29C', alpha = 0.6,
                args = list(df = nrow(reg_wide)-1))+ #red
  theme_FHM() + labs(x = "C - Q diff Sapling Dens.")



# Looking at dist. by year
ggplot(reg_wide, aes(x = seed_den_diff, color = factor(Year), fill = factor(Year)))+ 
  geom_density() + 
  scale_fill_viridis(discrete = T, option = "D", alpha = 0.5)+
  scale_color_viridis(discrete = T, option = "D")+
  theme_FHM() + labs(x = "C - Q diff Seedling Dens.")

ggplot(reg_wide, aes(x = sap_den_diff, color = factor(Year), fill = factor(Year)))+ 
  geom_density() + 
  scale_fill_viridis(discrete = T, option = "D", alpha = 0.5)+
  scale_color_viridis(discrete = T, option = "D")+
  theme_FHM() + labs(x = "C - Q diff Sapling Dens.")

ggplot(reg_wide, aes(x = stock_diff, color = factor(Year), fill = factor(Year)))+ 
  geom_density() + 
  scale_fill_viridis(discrete = T, option = "D", alpha = 0.5)+
  scale_color_viridis(discrete = T, option = "D")+
  theme_FHM() + labs(x = "C - Q diff Stocking")

# Looking at dist. in ACAD
ggplot(reg_wide %>% filter(Unit_Code == "ACAD"), 
       aes(x = seed_den_diff))+ 
  geom_density() + 
  theme_FHM() + labs(x = "C - Q diff Seedling Dens.")

ggplot(reg_wide %>% filter(Unit_Code == "ACAD"), 
       aes(x = sap_den_diff))+ 
  geom_density() + 
  theme_FHM() + labs(x = "C - Q diff Sapling Dens.")

ggplot(reg_wide %>% filter(Unit_Code == "ACAD"), 
       aes(x = stock_diff))+ 
  geom_density() + 
  theme_FHM() + labs(x = "C - Q diff Stocking")



reg_sd_year <- reg_wide %>% group_by(Year) %>%  
  summarize(seed_den_mean = mean(seed_den_diff),
            sap_den_mean = mean(sap_den_diff),
            stock_den_mean = mean(stock_diff),
            seed_den_sd = sd(seed_den_diff),
            sap_den_sd = sd(sap_den_diff),
            stock_sd = sd(stock_diff))

reg_sd_park <- reg_wide %>% group_by(Unit_Code) %>%  
  summarize(seed_den_mean = mean(seed_den_diff),
            sap_den_mean = mean(sap_den_diff),
            stock_den_mean = mean(stock_diff),
            seed_den_sd = sd(seed_den_diff),
            sap_den_sd = sd(sap_den_diff),
            stock_sd = sd(stock_diff))


