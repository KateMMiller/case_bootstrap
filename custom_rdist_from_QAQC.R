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

# Create density functions to generate random values from
r_seed <- new_r(reg_wide$seed_den_diff, type = "continuous")
r_sap <- new_r(reg_wide$sap_den_diff, type = "continuous")
r_stock <- new_r(reg_wide$stock_diff, type = "continuous")

