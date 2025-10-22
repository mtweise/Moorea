#Preliminary extraction data analysis

#clean the environment
rm(list=ls())

library(janitor)
library(here)
library(tidyverse)
library(tidyr)
library(ggplot2)
library(dplyr)
library(car)

##read in data
concentrations_raw <- read_csv(here("summer_2025/data", "eDNA_extractions.csv")) |>
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


#check ANOVA assumptions (chris levene's test)

leveneTest(dna_yield ~ collection_site * Time, data = concentrations_clean)

#ANOVA
anova_model <- aov(dna_yield ~ collection_site * Time, data = concentrations_clean)
summary(anova_model)


#check assumptions (chat gpt)
par(mfrow = c(1, 2))
plot(anova_model, which = 1)  # Residuals vs fitted (homogeneity)
plot(anova_model, which = 2)  # Q-Q plot (normality)




#Tukeys
TukeyHSD(anova_model)


# plot
committee_meeting_fig <- ggplot(concentrations_clean %>% dplyr::filter(!is.na(Time), !is.na(dna_yield)), 
       aes(x = Time, y = dna_yield, fill = Time)) +
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
    axis.title = element_text(size = 16),   # X and Y axis titles
    axis.text = element_text(size = 14)     # Tick labels
  )

#ggsave(here("summer_2025/figures", "committee_fig.jpg"), committee_meeting_fig, dpi=500,
#       width=10, height=5, unit="in")


###fix significance display please and thank you 
library(ggplot2)
library(dplyr)
library(ggpubr)

# Clean max values table
max_vals <- concentrations_clean %>%
  filter(!is.na(collection_site)) %>%
  group_by(collection_site) %>%
  summarise(y_max = max(dna_yield, na.rm = TRUE)) %>%
  ungroup()

# Create significance label dataframe
sig_labels <- data.frame(
  collection_site = c("B32", "B35", "B01"),
  group1 = c("Morning", "Morning", "Morning"),
  group2 = c("Afternoon", "Afternoon", "Afternoon"),
  p.adj = c(0.3679, 0.0397, 0.0050)
)

# Add significance asterisks
sig_labels <- sig_labels %>%
  mutate(
    label = cut(
      p.adj,
      breaks = c(-Inf, 0.001, 0.01, 0.05, Inf),
      labels = c("***", "**", "*", "ns")
    )
  ) %>%
  left_join(max_vals, by = "collection_site") %>%
  mutate(
    y.position = y_max * 1.1,      # position slightly above the max
    xmin = "Morning",              # start of significance bar
    xmax = "Afternoon",            # end of significance bar
    Time = "Morning"               # dummy variable to fix ggplot error
  )

# Plot
committee_meeting_fig2 <- ggplot(
  concentrations_clean %>% filter(!is.na(Time), !is.na(dna_yield)),
  aes(x = Time, y = dna_yield, fill = Time)
) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "black") +
  stat_pvalue_manual(
    sig_labels,
    label = "label",
    y.position = "y.position",
    xmin = "xmin",
    xmax = "xmax",
    tip.length = 0.01,
    size = 6
  ) +
  facet_wrap(~ collection_site, scales = "fixed") +
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
  )

committee_meeting_fig2

#fix order

# Reorder main data
concentrations_clean$collection_site <- factor(
  concentrations_clean$collection_site,
  levels = c("B32", "B35", "B01")
)

# Reorder sig_labels
sig_labels$collection_site <- factor(
  sig_labels$collection_site,
  levels = c("B32", "B35", "B01")
)

committee_meeting_fig3 <- ggplot(
  concentrations_clean %>% filter(!is.na(Time), !is.na(dna_yield)),
  aes(x = Time, y = dna_yield, fill = Time)
) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "black") +
  stat_pvalue_manual(
    sig_labels,
    label = "label",
    y.position = "y.position",
    xmin = "xmin",
    xmax = "xmax",
    tip.length = 0.01,
    size = 6
  ) +
  facet_wrap(~ collection_site, scales = "fixed") +   # consistent y-axis
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
  )

committee_meeting_fig3
ggsave(here("summer_2025/figures", "committee_fig3.jpg"), committee_meeting_fig3, dpi=500,
              width=10, height=5, unit="in")


#####add all the tukey's info

# Map site names to numeric x positions for proper plotting
site_levels <- c("B32", "B35", "B01")
site_positions <- setNames(1:3, site_levels)

tukey_sites <- data.frame(
  group1 = c("B32", "B35", "B35"),
  group2 = c("B01", "B01", "B32"),
  p.adj = c(0.0454, 0.2296, 0.0008875)
)

# Add labels
tukey_sites$label <- cut(
  tukey_sites$p.adj,
  breaks = c(-Inf, 0.001, 0.01, 0.05, Inf),
  labels = c("***", "**", "*", "ns")
)

# Add plotting positions
y_max_overall <- max(concentrations_clean$dna_yield, na.rm = TRUE)
tukey_sites$y.position <- y_max_overall * 1.3   # slightly above all boxes
tukey_sites$xmin <- site_positions[tukey_sites$group1]
tukey_sites$xmax <- site_positions[tukey_sites$group2]

# Mark these as not tied to any facet
tukey_sites$collection_site <- "all_sites"  # dummy facet



# Make sure within-site sig_labels collection_site is factor
sig_labels$collection_site <- factor(sig_labels$collection_site,
                                     levels = c("B32", "B35", "B01"))

# Combine
all_sig <- bind_rows(
  sig_labels,
  tukey_sites
)



committee_meeting_fig4 <- ggplot(
  concentrations_clean %>% filter(!is.na(Time), !is.na(dna_yield)),
  aes(x = Time, y = dna_yield, fill = Time)
) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "black") +
  # Within-site Time comparisons
  stat_pvalue_manual(
    sig_labels,
    label = "label",
    y.position = "y.position",
    xmin = "xmin",
    xmax = "xmax",
    tip.length = 0.01,
    size = 6
  ) +
  facet_wrap(~ collection_site, scales = "fixed") +
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
  # Add Tukey site comparisons manually
  geom_segment(
    data = tukey_sites,
    aes(x = xmin, xend = xmax, y = y.position, yend = y.position),
    inherit.aes = FALSE
  ) +
  geom_text(
    data = tukey_sites,
    aes(x = (xmin + xmax)/2, y = y.position, label = label),
    inherit.aes = FALSE,
    vjust = -0.5,
    size = 6
  )
#################


#########




################ here we go again



######im losing steam

library(ggplot2)
library(dplyr)
library(ggpubr)

# --- Numeric x-axis for dodging Time within sites ---
site_positions <- c(B32 = 1, B35 = 2, B01 = 3)

concentrations_clean <- concentrations_clean %>%
  mutate(
    site_num = site_positions[collection_site],
    time_offset = ifelse(Time == "Morning", -0.2, 0.2),
    x_pos = site_num + time_offset
  )

# --- Within-site Time comparisons ---
sig_labels <- sig_labels %>%
  mutate(
    xmin = site_positions[collection_site] - 0.2,
    xmax = site_positions[collection_site] + 0.2,
    y.position = y_max * 1.05  # adjust as needed
  )

# --- Between-site Tukey comparisons ---
tukey_sites <- data.frame(
  group1 = c("B32", "B35", "B35"),
  group2 = c("B01", "B01", "B32"),
  p.adj = c(0.0454, 0.2296, 0.0008875)
)

tukey_sites$label <- cut(
  tukey_sites$p.adj,
  breaks = c(-Inf, 0.001, 0.01, 0.05, Inf),
  labels = c("***", "**", "*", "ns")
)

y_max_overall <- max(concentrations_clean$dna_yield, na.rm = TRUE)
tukey_sites <- tukey_sites %>%
  mutate(
    xmin = site_positions[group1],
    xmax = site_positions[group2],
    y.position = y_max_overall * 1.25  # position above within-site bars
  )

# --- Final Plot ---
committee_meeting_fig_final3 <- ggplot(
  concentrations_clean,
  aes(x = x_pos, y = dna_yield, fill = Time, group = interaction(collection_site, Time))
) +
  geom_boxplot(width = 0.3) +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "black") +
  
  # Within-site Time comparisons
  stat_pvalue_manual(sig_labels,
                     label = "label",
                     xmin = "xmin",
                     xmax = "xmax",
                     y.position = "y.position",
                     tip.length = 0.01,
                     size = 6) +
  
  # Between-site Tukey comparisons
  geom_segment(data = tukey_sites,
               aes(x = xmin, xend = xmax, y = y.position, yend = y.position),
               inherit.aes = FALSE) +
  geom_text(data = tukey_sites,
            aes(x = (xmin + xmax)/2, y = y.position, label = label),
            inherit.aes = FALSE,
            vjust = -0.5,
            size = 6) +
  
  # X-axis labeling
  scale_x_continuous(breaks = 1:3, labels = c("B32", "B35", "B01")) +
  scale_fill_manual(values = c("Morning" = "lightblue", "Afternoon" = "lightyellow")) +
  
  labs(x = "Site", y = "DNA Yield", title = "DNA Yield by Site and Time of Day") +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.position = "none"
  )


committee_meeting_fig_final3


#fig 4

library(ggplot2)
library(dplyr)
library(ggpubr)

# --- Numeric x-axis for dodging Time within sites ---
site_positions <- c(B32 = 1, B35 = 2, B01 = 3)

concentrations_clean <- concentrations_clean %>%
  mutate(
    site_num = site_positions[collection_site],
    time_offset = ifelse(Time == "Morning", -0.2, 0.2),
    x_pos = site_num + time_offset
  )

# --- Within-site Time comparisons ---
sig_labels <- sig_labels %>%
  mutate(
    xmin = site_positions[collection_site] - 0.2,
    xmax = site_positions[collection_site] + 0.2,
    y.position = y_max * 1.05  # adjust as needed
  )

# --- Between-site Tukey comparisons ---
tukey_sites <- data.frame(
  group1 = c("B32", "B35", "B35"),
  group2 = c("B01", "B01", "B32"),
  p.adj = c(0.0454, 0.2296, 0.0008875)
)

tukey_sites$label <- cut(
  tukey_sites$p.adj,
  breaks = c(-Inf, 0.001, 0.01, 0.05, Inf),
  labels = c("***", "**", "*", "ns")
)

y_max_overall <- max(concentrations_clean$dna_yield, na.rm = TRUE)

# --- Offset Tukey comparisons to prevent overlap ---
tukey_sites <- tukey_sites %>%
  mutate(
    xmin = site_positions[group1],
    xmax = site_positions[group2],
    y.position = y_max_overall * c(1.25, 1.35, 1.45)  # stack the comparisons
  )

# --- Final Plot ---
committee_meeting_fig_final4 <- ggplot(
  concentrations_clean,
  aes(x = x_pos, y = dna_yield, fill = Time, group = interaction(collection_site, Time))
) +
  geom_boxplot(width = 0.3) +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "black") +
  
  # Within-site Time comparisons
  stat_pvalue_manual(sig_labels,
                     label = "label",
                     xmin = "xmin",
                     xmax = "xmax",
                     y.position = "y.position",
                     tip.length = 0.01,
                     size = 6) +
  
  # Between-site Tukey comparisons (stacked)
  geom_segment(data = tukey_sites,
               aes(x = xmin, xend = xmax, y = y.position, yend = y.position),
               inherit.aes = FALSE) +
  geom_text(data = tukey_sites,
            aes(x = (xmin + xmax)/2, y = y.position, label = label),
            inherit.aes = FALSE,
            vjust = -0.5,
            size = 6) +
  
  # X-axis labeling
  scale_x_continuous(breaks = 1:3, labels = c("B32", "B35", "B01")) +
  scale_fill_manual(values = c("Morning" = "lightblue", "Afternoon" = "lightyellow")) +
  
  labs(x = "Site", y = "DNA concentration (ng/ul)", title = "DNA Yield by Site and Time of Day") +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.position = "none"
  )

committee_meeting_fig_final4
ggsave(here("summer_2025/figures", "committee_fig_final4.jpg"), committee_meeting_fig_final4, dpi=500,
              width=10, height=7, unit="in")
