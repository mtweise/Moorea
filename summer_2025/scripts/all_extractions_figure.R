#new figure with all extractions


#clean the environment
rm(list=ls())

library(janitor)
library(here)
library(tidyverse)
library(tidyr)
library(ggplot2)
library(dplyr)
library(car)
library(ggplot2)
library(dplyr)
library(ggpubr)
library(WRS2)
library(stringr)
library(rstatix)
library(sandwich)

##read in data
concentrations_raw <- read_csv(here("summer_2025/data", "eDNA_extractions_all.csv")) |>
  clean_names()

#add morning/afternoon column
concentrations_clean <- concentrations_raw %>%
  mutate(Time = ifelse(grepl("1[A-Za-z]?$", sample_code), "Morning",
                       ifelse(grepl("2[A-Za-z]?$", sample_code), "Afternoon", NA))) %>%
  select(sample_code, collection_site, dna_yield, Time) %>%
  mutate(
    Time = factor(Time, levels = c("Morning", "Afternoon")),
    collection_site = factor(collection_site)
  )

#remove NAs
concentrations_clean2 <- na.omit(concentrations_clean)

#remove FW
#concentrations_clean3 <- concentrations_clean2 %>%
#  filter(collection_site != "FW")


concentrations_clean3 <- concentrations_clean2 %>%
  mutate(collection_site = str_trim(as.character(collection_site))) 


#check ANOVA assumptions (chris levene's test)
#null hypo: equal variances

leveneTest(dna_yield ~ collection_site * Time, data = concentrations_clean3)
#conclude that there are unequal variances so we must do a welch's ANOVA


#Welch's anova (heterodastically robust)
welch_anova_test(dna_yield ~ collection_site * Time, data = concentrations_clean3)

#There is a statistically significant difference in mean DNA yield among the groups,
# accounting for unequal variances.


#Kruskal–Wallis may not work for 2-factor design?
#could transform data


#data transformation
#concentrations_clean_transformed <- concentrations_clean2 %>%
#  mutate(
#    log_yield  = log1p(dna_yield),
#    sqrt_yield = sqrt(dna_yield)
#  )

#leveneTest(log_yield ~ collection_site * Time, data = concentrations_clean_transformed)
#leveneTest(sqrt_yield ~ collection_site * Time, data = concentrations_clean_transformed)

#doesn't really help 



#robust_model <- t2way(dna_yield ~ collection_site * Time,
#                      data = concentrations_clean3)

#robust_model
#why wont this work?



lm_model <- lm(dna_yield ~ collection_site * Time, data = concentrations_clean3)

Anova(lm_model, type = 3, white.adjust = TRUE)


######

#pairwise_t_test(
#  concentrations_clean3,
#  dna_yield ~ Time,
#  group.by = "collection_site",
#  p.adjust.method = "bonferroni")
#why wont this work?



# Split by site and run pairwise t-tests
pairwise_results <- concentrations_clean3 %>%
  group_by(collection_site) %>%             # group manually
  pairwise_t_test(
    dna_yield ~ Time,
    p.adjust.method = "bonferroni"
  )

pairwise_results


######################
#plot


pairwise_results1.5 <- pairwise_results %>%
  # keep group1/group2 for ggpubr
  # facet column to match collection_site
  rename(facet = collection_site) %>%
  # position bars slightly above each site's max
  group_by(facet) %>%
  mutate(
    y.position = max(concentrations_clean3$dna_yield[concentrations_clean3$collection_site == facet]) + 0.05
  ) %>%
  ungroup()

fig1.5 <- ggplot(concentrations_clean3, aes(x = Time, y = dna_yield, fill = Time)) +
  geom_boxplot() +
  facet_wrap(~ collection_site) +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "black") +
  scale_fill_manual(values = c("Morning" = "lightblue", "Afternoon" = "lightyellow")) +
  labs(
    x = "Time of Day",
    y = "DNA Yield",
    title = "DNA Yield by Site and Time of Day"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    legend.position = "none",
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14)
  ) +
  stat_pvalue_manual(
    pairwise_results1.5,
    label = "p.adj.signif",
    x = "group1",
    xend = "group2",
    y.position = "y.position",
    tip.length = 0.03,
    inherit.aes = FALSE,
    facet = "facet"  # matches facet_wrap
  )

print(fig1.5)


pairwise_results1.6 <- pairwise_results %>%
  rename(facet = collection_site) %>%
  rowwise() %>%  # operate row by row
  mutate(
    y.position = max(
      concentrations_clean3$dna_yield[
        concentrations_clean3$collection_site == facet &
          concentrations_clean3$Time %in% c(group1, group2)
      ]
    ) + 0.05
  ) %>%
  ungroup()


fig1.6 <- ggplot(concentrations_clean3, aes(x = Time, y = dna_yield, fill = Time)) +
  geom_boxplot() +
  facet_wrap(~ collection_site) +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "black") +
  scale_fill_manual(values = c("Morning" = "lightblue", "Afternoon" = "lightyellow")) +
  labs(
    x = "Time of Day",
    y = "DNA Yield",
    title = "DNA Yield by Site and Time of Day"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    legend.position = "none",
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14)
  ) +
  stat_pvalue_manual(
    pairwise_results1.6,
    label = "p.adj.signif",
    x = "group1",
    xend = "group2",
    y.position = "y.position",
    tip.length = 0.03,
    inherit.aes = FALSE,
    facet = "facet"  # matches facet_wrap
  )

print(fig1.6)
#same issue



library(ggpubr)
library(dplyr)

# Compute y.position per comparison (just above the taller box in the two groups)
pairwise_results_plot1 <- pairwise_results %>%
  rowwise() %>%
  mutate(
    y.position = max(
      concentrations_clean3$dna_yield[
        concentrations_clean3$collection_site == collection_site &
          concentrations_clean3$Time %in% c(group1, group2)
      ]
    ) + 0.05
  ) %>%
  ungroup()

# Plot
fig5 <- ggplot(concentrations_clean3, aes(x = Time, y = dna_yield, fill = Time)) +
  geom_boxplot() +
  facet_wrap(~ collection_site) +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "black") +
  scale_fill_manual(values = c("Morning" = "lightblue", "Afternoon" = "lightyellow")) +
  labs(
    x = "Time of Day",
    y = "DNA Yield",
    title = "DNA Yield by Site and Time of Day"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    legend.position = "none",
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14)
  ) +
  stat_pvalue_manual(
    pairwise_results_plot1,
    label = "p.adj.signif",
    x = "group1",
    y.position = "y.position",
    tip.length = 0.03,
    inherit.aes = FALSE
  )

print(fig5)



fig6 <- ggplot(concentrations_clean3, aes(x = Time, y = dna_yield, fill = Time)) +
  geom_boxplot() +
  facet_wrap(~ collection_site) +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "black") +
  scale_fill_manual(values = c("Morning" = "lightblue", "Afternoon" = "lightyellow")) +
  labs(
    x = "Time of Day",
    y = "DNA Yield",
    title = "DNA Yield by Site and Time of Day"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    legend.position = "none",
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14)
  ) +
  stat_pvalue_manual(
    pairwise_results_plot1,
    label = "p.adj.signif",    # stars
    x = "group1",              # start of bracket
    xend = "group2",           # end of bracket
    y.position = "y.position",
    tip.length = 0.03,
    inherit.aes = FALSE
  )
print(fig6)



fig7 <- ggplot(concentrations_clean3, aes(x = Time, y = dna_yield, fill = Time)) +
  geom_boxplot() +
  facet_wrap(~ collection_site) +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "black") +
  scale_fill_manual(values = c("Morning" = "lightblue", "Afternoon" = "lightyellow")) +
  labs(
    x = "Time of Day",
    y = "DNA Yield",
    title = "DNA Yield by Site and Time of Day"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    legend.position = "none",
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14)
  ) +
  stat_pvalue_manual(
    pairwise_results_plot1,
    label = "p.adj.signif",    # stars
    x = "group1",              # start of bracket
    y.position = "y.position",
    tip.length = 0.03,
    inherit.aes = FALSE
  )
print(fig7)


###############

concentrations_clean3.5 <- concentrations_clean3 %>%
  mutate(Time = factor(Time, levels = c("Morning", "Afternoon")))

pairwise_results_plot1.5 <- pairwise_results_plot1 %>%
  mutate(
    group1 = factor(group1, levels = c("Morning", "Afternoon")),
    group2 = factor(group2, levels = c("Morning", "Afternoon"))
  )

pairwise_results_plot1.5 <- pairwise_results_plot1.5 %>%
  rowwise() %>%
  mutate(
    y.position = max(
      concentrations_clean3.5$dna_yield[
        concentrations_clean3.5$collection_site == collection_site &
          concentrations_clean3$Time %in% c(group1, group2)
      ]
    ) + 0.05
  ) %>%
  ungroup()



fig8 <- ggplot(concentrations_clean3.5, aes(x = Time, y = dna_yield, fill = Time)) +
  geom_boxplot() +
  facet_wrap(~ collection_site) +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "black") +
  scale_fill_manual(values = c("Morning" = "lightblue", "Afternoon" = "lightyellow")) +
  stat_pvalue_manual(
    pairwise_results_plot1.5,
    x = "group1",
    y.position = "y.position",
    label = "p.adj.signif",
    tip.length = 0.03,
    inherit.aes = FALSE
  )
print(fig8)




# Make sure Time is a factor
concentrations_clean3.5 <- concentrations_clean3 %>%
  mutate(Time = factor(Time, levels = c("Morning", "Afternoon")))

# Make sure pairwise_results has group1, group2, p.adj.signif, collection_site
pairwise_results_plot <- pairwise_results %>%
  rowwise() %>%
  mutate(
    y.position = max(
      concentrations_clean3.5$dna_yield[
        concentrations_clean3.5$collection_site == collection_site &
          concentrations_clean3.5$Time %in% c(group1, group2)
      ]
    ) + 0.05
  ) %>%
  ungroup()

# Plot with brackets
fig9 <- ggplot(concentrations_clean3.5, aes(x = Time, y = dna_yield, fill = Time)) +
  geom_boxplot() +
  facet_wrap(~ collection_site) +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "black") +
  scale_fill_manual(values = c("Morning" = "lightblue", "Afternoon" = "lightyellow")) +
  stat_pvalue_manual(
    pairwise_results_plot,
    label = "p.adj.signif",
    x = "group1",
    xend = "group2",       # ← THIS is now recognized internally
    y.position = "y.position",
    tip.length = 0.03,
    inherit.aes = FALSE
  )
print(fig9)


library(ggplot2)
library(dplyr)
library(ggpubr)

# --- Assign numeric x-axis per site ---
site_positions <- concentrations_clean3 %>%
  distinct(collection_site) %>%
  arrange(collection_site) %>%
  mutate(site_num = row_number()) %>%
  deframe()  # returns named vector for mapping

# Map site names to numeric positions
concentrations_clean3 <- concentrations_clean3 %>%
  mutate(
    site_num = site_positions[collection_site],
    time_offset = ifelse(Time == "Morning", -0.2, 0.2),
    x_pos = site_num + time_offset
  )

# --- Prepare pairwise results with numeric positions ---
pairwise_results_plot <- pairwise_results %>%
  # Ensure the column names match
  rename(facet = collection_site) %>%
  rowwise() %>%
  mutate(
    xmin = site_positions[facet] - 0.2,      # Morning position
    xmax = site_positions[facet] + 0.2,      # Afternoon position
    y.position = max(concentrations_clean3$dna_yield[concentrations_clean3$collection_site == facet]) + 0.05,
    label = p.adj.signif                       # stars from rstatix
  ) %>%
  ungroup()

# --- Plot ---
fig_final <- ggplot(concentrations_clean3, aes(x = x_pos, y = dna_yield, fill = Time, group = interaction(collection_site, Time))) +
  geom_boxplot(width = 0.3) +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "black") +
  
  # Add significance brackets
  geom_segment(data = pairwise_results_plot,
               aes(x = xmin, xend = xmax, y = y.position, yend = y.position),
               inherit.aes = FALSE) +
  geom_text(data = pairwise_results_plot,
            aes(x = (xmin + xmax)/2, y = y.position, label = label),
            inherit.aes = FALSE,
            vjust = -0.5,
            size = 6) +
  
  # X-axis labels (numeric positions to site names)
  scale_x_continuous(breaks = site_positions, labels = names(site_positions)) +
  scale_fill_manual(values = c("Morning" = "lightblue", "Afternoon" = "lightyellow")) +
  
  labs(
    x = "Site",
    y = "DNA Yield",
    title = "DNA Yield by Site and Time of Day"
  ) +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.position = "none"
  )

fig_final

####################
#no NS bracket and data points jittered

# --- Numeric x-axis for sites ---
site_positions <- concentrations_clean3 %>%
  distinct(collection_site) %>%
  arrange(collection_site) %>%
  mutate(site_num = row_number()) %>%
  deframe()

concentrations_clean3 <- concentrations_clean3 %>%
  mutate(
    site_num = site_positions[collection_site],
    time_offset = ifelse(Time == "Morning", -0.2, 0.2),
    x_pos = site_num + time_offset
  )

# --- Prepare pairwise results with numeric positions ---
pairwise_results_sig <- pairwise_results %>%
  rename(facet = collection_site) %>%
  rowwise() %>%
  mutate(
    xmin = site_positions[facet] - 0.2,
    xmax = site_positions[facet] + 0.2,
    y.position = max(concentrations_clean3$dna_yield[concentrations_clean3$collection_site == facet]) + 0.05,
    label = p.adj.signif
  ) %>%
  ungroup() %>%
  filter(label != "ns")  # keep only significant comparisons

# --- Plot ---
fig_jittered <- ggplot(concentrations_clean3, aes(x = x_pos, y = dna_yield, fill = Time, group = interaction(collection_site, Time))) +
  
  # Boxplots without outlier dots
  geom_boxplot(width = 0.3, outlier.shape = NA) +
  
  # Jittered points
  geom_point(position = position_jitter(width = 0.1), size = 1, alpha = 0.7, color="gray50") +
  
  # Mean points
  stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "black") +
  
  # Significance brackets only for significant comparisons
  geom_segment(data = pairwise_results_sig,
               aes(x = xmin, xend = xmax, y = y.position, yend = y.position),
               inherit.aes = FALSE) +
  geom_text(data = pairwise_results_sig,
            aes(x = (xmin + xmax)/2, y = y.position, label = label),
            inherit.aes = FALSE,
            vjust = -0.5,
            size = 6) +
  
  # X-axis labels
  scale_x_continuous(breaks = site_positions, labels = names(site_positions)) +
  scale_fill_manual(values = c("Morning" = "lightblue", "Afternoon" = "lightyellow")) +
  
  labs(
    x = "Site",
    y = "DNA Yield (ng/uL)",
    title = "DNA Yield by Site and Time of Day"
  ) +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.position = "none"
  )

fig_jittered
#ggsave(here("summer_2025/figures", "extractions_all.jpg"), fig_jittered, dpi=500,
#                    width=10, height=7, unit="in")



##################
#plot without FW

concentrations_noFW <- concentrations_clean3 %>%
  dplyr::filter(collection_site != "FW")


fig_jittered2 <- ggplot(
  concentrations_noFW,
  aes(
    x = x_pos,
    y = dna_yield,
    fill = Time,
    group = interaction(collection_site, Time)
  )
) +
  geom_boxplot(width = 0.3, outlier.shape = NA) +
  geom_point(position = position_jitter(width = 0.1),
             size = 1, alpha = 0.7, color = "gray50") +
  stat_summary(fun = mean, geom = "point",
               shape = 20, size = 3, color = "black") +
  geom_segment(
    data = pairwise_results_sig %>% dplyr::filter(facet != "FW"),
    aes(x = xmin, xend = xmax, y = y.position, yend = y.position),
    inherit.aes = FALSE
  ) +
  geom_text(
    data = pairwise_results_sig %>% dplyr::filter(facet != "FW"),
    aes(x = (xmin + xmax)/2, y = y.position, label = label),
    inherit.aes = FALSE,
    vjust = -0.5,
    size = 6
  ) +
  scale_x_continuous(
    breaks = site_positions[names(site_positions) != "FW"],
    labels = names(site_positions[names(site_positions) != "FW"])
  ) +
  scale_fill_manual(values = c("Morning" = "lightblue",
                               "Afternoon" = "lightyellow")) +
  labs(
    x = "Site",
    y = "DNA Yield (ng/uL)",
    title = "DNA Yield by Site and Time of Day"
  ) +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.position = "none"
  )


fig_jittered2
#ggsave(here("summer_2025/figures", "extractions_Bsites.jpg"), fig_jittered2, dpi=500,
#                           width=10, height=7, unit="in")




























