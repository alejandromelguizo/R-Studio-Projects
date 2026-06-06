#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ALEJANDRO MELGUIZO
# DATE: 5/20/26
# TOPIC: blog post data viz code
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)
library(ggridges)
library(ggalluvial)

#~~~~~~~~~~~~~~~~~~~~

setwd("~/Desktop/Econometrics/Econometrics Final Project") 
#called econometrics because im also doing a project on the same data set for that class

df <- read.csv('econometrics_v_10.csv')

options(scipen = 9999999)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#CLEANING:
plotdata <- df

#1#
plotdata$ln_wage <- log(df$incwage_clean)

plotdata <- plotdata %>% mutate(citizen_cleaner = case_when(
  citizen_clean == 0 ~ 'Non-Citizen',
  citizen_clean == 2 ~ 'Naturalized Citizen',
  citizen_clean == 1 ~ 'Born Citizen'
))

#2#
plotdata <- plotdata %>% mutate(inctot_percentile = case_when(
  inctot_clean <= 11466 ~ 'Bottom 25%',
  inctot_clean > 11466 & inctot_clean <= 21120 ~ '25th - 50th',
  inctot_clean > 21120 & inctot_clean <= 38016 ~ '50th - 75th',
  inctot_clean > 38016 ~ 'Top 25%'
))

plotdata <- plotdata %>% mutate(sex_str = case_when(
  SEX == 2 ~ 'Female',
  SEX == 1 ~ 'Male',
  TRUE ~ 'NA'
))

plotdata_alluv <- plotdata %>%
  group_by(sex_str, inctot_percentile, citizen_cleaner) %>% count()

plotdata_alluv$inctot_percentile <- factor(plotdata_alluv$inctot_percentile,
                                           levels = c("Top 25%",
                                                     "50th - 75th",
                                                     "25th - 50th",
                                                     "Bottom 25%"))



#3#
bpl_labels <- c(
  "0" = "North America",
  "1" = "Central America",
  "2" = "Caribbean",
  "3" = "South America",
  "4" = "Europe",
  "5" = "Asia / Middle East",
  "6" = "Africa",
  "7" = "Oceania"
)

educ_labels <- c(
  "0" = "< High School",
  "1" = "High School",
  "2" = "Some College",
  "3" = "Bachelor's",
  "4" = "Graduate"
)

heatmap_df <- df %>%
  filter(!is.na(bpl_detail), !is.na(educ_clean)) %>%
  group_by(bpl_detail, educ_clean) %>%
  summarise(mean_wage = mean(incwage_clean / 0.531, na.rm = TRUE),
            n = n(), .groups = "drop") %>%
  mutate(
    bpl_label  = bpl_labels[as.character(bpl_detail)],
    educ_label = educ_labels[as.character(educ_clean)]
  )

heatmap_df$educ_label <- factor(heatmap_df$educ_label,
                                levels = educ_labels)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#VISUALIZATIONS:

#~~~~~~~~~~
#1#: scatter plot of log INCTOT (2000 CPI USD) on years since immigration
   # faceted by citizenship status

ggplot(plotdata,
       aes(
         x = years_since_immig,
         y = ln_wage,
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
  scale_color_manual(values = c("#C9859A", "#7F90C4", '#D49C55')) +
  labs(title = 'Relationship between Years Since Immigration and Wages',
       subtitle = 'Inflation-adjusted to 1999 dollars  |  Ages 25–65  |  Immigrants only',
       caption = 'https://cps.ipums.org/',
       x = "Years Since Immigration",
       y = "Log of wages"
  ) +
  guides(color = 'none') +
  theme_minimal()

ggsave('income_immigration_relationship.png', width = 7, height = 5, units = 'in', dpi = 300)

#~~~~~~~~~~
#2# Alluvial of Sex, immigration status, and income percentiles

ggplot(plotdata_alluv, 
       aes(
         y = n/1000,
         axis1 = sex_str,
         axis2 = citizen_cleaner,
         axis3 = inctot_percentile
       )) +
  geom_alluvium(aes(
    fill = citizen_cleaner),
    ) +
  scale_fill_manual(values = c(
    "Born Citizen" = "#C9859A",
    "Naturalized Citizen" = "#7F90C4",
    "Non-Citizen" = "#D49C55"
  )) +
  geom_stratum(alpha = 0.9,
               fill = "#D0CDF9", 
               color = 'white',
               size = 0.2) +
  geom_text(stat = 'stratum',
            aes(label = after_stat(stratum)), 
            size = 1.5) +
  scale_x_discrete(limits = c("Gender", "Citizenship Status", "Income Percentiles"),
                   expand = c(0.15, 0.15)) +
  theme_minimal() +
  guides(fill = guide_legend(title = "Citzenship Status:",
                             title.position = "left")
         ) + 
  theme(
    legend.position = "top",
    legend.text = element_text(size = 5),
    legend.title = element_text(size = 5),
    panel.grid.major = element_blank(),
    plot.title = element_text(size = 14, face = "plain"), 
    axis.text = element_text(size = 9, color = "gray50")
  ) +
  labs(
    title = "Immigrant Income Distribution by Gender and Citizenship",
    subtitle = "",
    x = NULL,
    y = "Count (in thousands)",
    caption = 'https://cps.ipums.org/'
  ) 

#ggsave('alluv_income_immigration_relationship.png', width = 7, height = 5, units = 'in', dpi = 300)

#~~~~~~~~~~
#3# Heatmap comparing wages, educational attainment, and region of birth

ggplot(heatmap_df, aes(x = educ_label, y = bpl_label, fill = mean_wage)) +
  geom_tile(color = "white", linewidth = 0.4) +
  geom_text(aes(label = scales::dollar(mean_wage, scale = 1e-3,
                                       suffix = "k", accuracy = 1)),
            size = 3, color = "white", fontface = "bold") +
  scale_fill_gradientn(
    colors = c("#2C2C6C", "#4B6FB5", "#7F90C4", "#F4C97A", "#D49C55"),
    labels = scales::dollar_format(scale = 1e-3, suffix = "k"),
    name = "Mean Wage\n(2024 dollars)",
    breaks = c(25000, 75000, 125000)
  ) +
  labs(
    title = "Mean Wage by Education and Region of Origin",
    subtitle = "Inflation-adjusted to 2024 dollars  |  Ages 25–65  |  Immigrants only",
    x = "Education Level",
    y = "Region of Birth",
    caption = "https://cps.ipums.org/"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "plain", size = 14),
    plot.subtitle = element_text(size = 10, color = "gray50"),
    axis.text.x = element_text(size = 7, vjust = 0.5),
    axis.text.y = element_text(size = 7),
    panel.grid = element_blank(),
    legend.key.width = unit(1.2, "cm")
  )

ggsave('heatmap_income_immigration_relationship.png', width = 7, height = 5, units = 'in', dpi = 300)
