library(tidyverse)
#library(broom)
library(stringr)
library(forestNETNarch)

importData()

regdf <- joinRegenData(park = "MABI", from = 2008, to = 2018,
                       speciesType = 'native', canopyForm = 'canopy') %>%
         mutate(cycle = case_when(Year %in% c(2008:2011) ~ 1,
                               Year %in% c(2012:2015) ~ 2,
                               Year %in% c(2016:2019) ~ 3))

regdf2 <- regdf %>% group_by(Plot_Name, Year, cycle) %>%
  summarize(seed_den = sum(seed.den),
            sap_den = sum(sap.den),
            stock = sum(stock),
            .groups = "drop") %>%
  mutate(Year = Year - (min(Year)))

df = regdf2
y = "stock"
sample = TRUE

# Quick and dirty case_bootstrap using lm with Plot_Name as factor, then
# taking mean of plot slopes and intercepts 
case_boot <- function(df, y, sample){
  plots <- data.frame(Plot_Name = unique(df$Plot_Name))
  n <- nrow(plots)

  samp <- if(sample == TRUE){
            data.frame(Plot_Name =
                       sample(plots$Plot_Name, n, replace = TRUE))
           } else {data.frame(plots)} %>% 
          arrange(Plot_Name)

  samp$case <- as.factor(str_pad(rownames(samp),
                nchar(n),
                side ="left",
                pad = 0))

  df_samp <- merge(samp, df, by = c("Plot_Name"),
               all.x = T, all.y = F)

  df_samp$y <- df_samp[,y]

mod <- lm(y ~ Year * case, data = df_samp)

coefs <- data.frame(coef = mod$coefficients,
                    name = names(mod$coefficients))
coefs$type <- ifelse(grepl("Year", coefs$name), "slope", "int")
coefs$type2 <- ifelse(grepl("case|Year:", coefs$name),
                 paste0(coefs$type),
                 paste0("base", coefs$type))

base_int <- coefs$coef[coefs$type2 == "baseint"]
base_slope <- coefs$coef[coefs$type2 == "baseslope"]
coefs <- coefs %>% mutate(coef_corr =
                            case_when(grepl("base", coefs$type2) ~ coef,
                                      type2 == "int" ~ base_int + coef,
                                      type2 == "slope" ~ base_slope + coef)
                                                )
mod_out <- data.frame(int   = mean(coefs$coef_corr[coefs$type == "int"]),
                      slope = mean(coefs$coef_corr[coefs$type == "slope"]))

return(mod_out)
}

rep = 1000
stock_mod <- case_boot(regdf2, "stock", sample = FALSE)
stock_boot <- purrr::map_df(seq_len(rep),
                            ~case_boot(regdf2, "stock",
                                       sample = TRUE))
stock_boot_final <- data.frame(int = mean(stock_boot$int),
                               slope = mean(stock_boot$slope),
                               u95_int = quantile(stock_boot$int, probs = 0.975),
                               l95_int = quantile(stock_boot$int, probs = 0.025),
                               u95_slope = quantile(stock_boot$slope, probs = 0.975),
                               l95_slope = quantile(stock_boot$slope, probs = 0.025),
                               row.names = NULL)
stock_boot_final

ggplot(regdf2, aes(x = cycle, y = stock, group = Plot_Name,
                   color = Plot_Name))+
  geom_point()+
  geom_line()
