#Preliminary extraction data analysis

#clean the environment
rm(list=ls())

library(janitor)
library(here)
library(tidyverse)
library(tidyr)
library(ggplot2)

##read in data
concentrations_raw <- read_csv(here("summer_2025/data", "eDNA_extractions.csv")) |>
  clean_names()

#add mroning/afternoon column

# Create Time variable based on the number after site
concentrations_raw$Time <- ifelse(grepl("1[A-Za-z]?$", concentrations_raw$sample_code), "Morning",
                  ifelse(grepl("2[A-Za-z]?$", concentrations_raw$sample_code), "Afternoon", NA))

concentrations_raw$Time <- factor(concentrations_raw$Time, levels = c("Morning", "Afternoon"))
concentrations_raw$collection_site <- factor(concentrations_raw$collection_site)

concentrations_raw$collection_site <- as.factor(concentrations_raw$collection_site)
concentrations_raw$Time <- as.factor(concentrations_raw$Time)

#ANOVA
anova_model <- aov(dna_yield ~ collection_site * Time, data = concentrations_raw)
summary(anova_model)


#check assumptions
par(mfrow = c(1, 2))
plot(anova_model, which = 1)  # Residuals vs fitted (homogeneity)
plot(anova_model, which = 2)  # Q-Q plot (normality)



#Tukeys
TukeyHSD(anova_model)


#plot
ggplot(concentrations_raw, aes(x = Time, y = dna_yield, fill = Time)) +
  geom_boxplot() +
  facet_wrap(~ collection_site) +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "black") +
  labs(
    x = "Time of Day",
    y = "DNA Yield",
    title = "DNA Yield by Site and Time of Day"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    legend.position = "none"
  )



