#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ALEJANDRO MELGUIZO
# DATE: 5/13/26
# TOPIC: data viz project final
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)
library(viridis)

setwd("~/Desktop/Econometrics/Econometrics Final Project")

df1 <- read.csv('dataviz_final_clean.csv')

ggplot(df1,
       aes(
         x = years_since_immig,
         y = ln_inctot,
         color = citizen_cleaner
       )) +
  facet_wrap(~citizen_cleaner) +
  geom_point(alpha = 0.2,
             shape = 17,
             size = 0.7) +
  geom_smooth(se = FALSE,
              linewidth = 0.5,
              color = 'black') +
  scale_x_continuous(breaks = seq(0, 65, 10)) +
  scale_color_manual(values = c("red", "cornflowerblue", 'orange')) +
  labs(title = 'Relationship between Years Since Immigration and Personal Income',
       subtitle = 'CPS IPUMS',
       caption = 'source: https://cps.ipums.org/',
       x = "Years Since Immigration",
       y = "Personal Income (log of income)"
  ) +
  guides(color = 'none') +
  theme_minimal()

ggsave('income_immigration_relationship.png', width = 7, height = 5, units = 'in', dpi = 300)