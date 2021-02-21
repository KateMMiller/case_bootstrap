library(tidyverse)
library(forestNETNarch)
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

# There's a tendency for the first sample to find fewer seedlings than the qaqc sample, causing 
# more negative diff than positive. To get a better, more balanced error structure, I'm going to 
# randomize the order of the group before taking the difference.

rand_diff <- function(df, col1, col2){
  col_name <- paste0(substr(col1, 1, nchar(col1)-2), "_pctdiff")
  samp_fun <- function(){sample(c(col1, col2), 2, replace = FALSE)}
  samp1 <- lapply(1:nrow(df), function(x){df[x, samp_fun()]})
  pctdiff <- lapply(1:nrow(df), function(x){
    ((samp1[[x]][2] + 0.1) - (samp1[[x]][1] + 0.1))/(samp1[[x]][1]+0.1)}) %>% 
    unlist() %>% data.frame()
  colnames(pctdiff) = col_name
  df2 <- cbind(df, pctdiff)
  return(df2)
}

list1 <- c("seed_den_c", "sap_den_c", "stock_c")
list2 <- c("seed_den_q", "sap_den_q", "stock_q")

for(i in seq_along(list1)){
  col1 <- list1[i]
  col2 <- list2[i]
  reg_wide2 <- rand_diff(reg_wide, col1, col2)
}


head(reg_wide2)

summary(lm(seed_den_pctdiff ~ seed_den_c, data = reg_wide2)) # slope non-sign
cor(abs(reg_wide2$seed_den_pctdiff), reg_wide2$seed_den_c) # slightly neg. corr

summary(lm(sap_den_pctdiff ~ sap_den_c, data = reg_wide2)) # slope non-sign
cor(abs(reg_wide2$sap_den_pctdiff), reg_wide2$sap_den_c) # slightly neg. corr

summary(lm(stock_pctdiff ~ stock_c, data = reg_wide2)) # slope non-sign
cor(abs(reg_wide2$stock_pctdiff), reg_wide2$stock_c) # slightly neg. corr

# Slightly negative correlation indicates that when there's a lot of 
# seed/saps, we all find them. When there are few, more variance

# Create density functions to generate random values from. These are based on
# percent difference, so need to multiple the new r variable * x.
r_seed <- new_r(reg_wide$seed_den_pctdiff, type = "continuous") 
r_seed2 <- new_r(sample(reg_wide$seed_den_pctdiff, 10000, replace = T), type = "continuous") 
?new_r
r_sap <- new_r(reg_wide$sap_den_pctdiff, type = "continuous")
r_stock <- new_r(reg_wide$stock_pctdiff, type = "continuous")

plot(density(r_seed(10000))) # I prefer r_seed b/c smoother
plot(density(r_seed2(10000)))

plot(density(r_sap(10000)))
plot(density(r_stock(10000)))

