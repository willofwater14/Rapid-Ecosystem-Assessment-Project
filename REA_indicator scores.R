library(tidyverse)
library(readxl)
library(ggplot2)
library(gt)
library(ggrepel)

# Load dataset
df_rea <- read_xlsx( "data_deposit/OA_Coral Reef REA Data Template.xlsx",
                     sheet = "Coral_Community", na = "NA")

# Create wide dataframe
site_summary <- df_rea %>%
  select(Site, Latitude, Longitude, Indicator_score, Average_score) %>%
  group_by(Site, Latitude, Longitude, Indicator_score) %>%
  summarise(
    Mean_score = round(mean(Average_score, na.rm = TRUE)),
    .groups = "drop") %>%
  pivot_wider(
    names_from = Indicator_score,
    values_from = Mean_score) %>%
  select(Site, Latitude, Longitude, LCC, FMA, SD, SC) %>% 
  mutate(CHI = rowMeans(across(c(LCC, FMA, SD, SC)), na.rm = TRUE))

# Filter master/reference sites
master_sites <- df_rea %>%
  filter(grepl("^AMP", Site)) %>% 
  mutate(
    Date = as.Date(Date),
    Year = format(Date, "%Y"),
    Year = ifelse(Year == "2626", "2026", Year)
  ) %>%
  group_by(Year, Site, Indicator_score) %>% 
  summarise(
    Mean_score = mean(Average_score, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = Indicator_score,
    values_from = Mean_score
  ) %>% 
  select(Year, Site, LCC, FMA, SD, SC)

summary_rea <- master_sites %>% 
  pivot_longer(
    cols = c(LCC, FMA, SD, SC),
    names_to = "Indicator_score",
    values_to = "Mean_score"
  ) %>% 
  mutate(
    Mean_score = round(Mean_score))

# write.csv(site_summary, "C:/Users/Wilberto Castillo/OneDrive/Documents/REA_Ocean Accounts/REA_heatmap.csv", row.names = FALSE)

# Normalizing detailed AGRRA indicator calculation to match Rapid Assessment score scale

site_lcc <- benthic_cover_presence_lcc %>%
  select(Year, Site, LCC = Coral_Cover_Site) %>%
  distinct() %>% 
  mutate(
    LCC_score = case_when(LCC <= 10 ~ 1, LCC <= 25 ~ 2, LCC <= 45 ~ 3, LCC <= 60 ~ 4, LCC > 60 ~ 5))
site_fma <- benthic_cover_presence_fma %>%
  select(Year, Site, FMA = Algae_Cover_Site) %>%
  distinct() %>% 
  mutate(
    FMA_score = case_when(FMA <= 10 ~ 5, FMA <= 25 ~ 4, FMA <= 45 ~ 3, FMA <= 60 ~ 2, FMA > 60 ~ 1))
site_cd <- coral_community_cd %>%
  select(Year, Site, Diversity, Richness) %>%
  distinct() %>% 
  mutate(
    SD_score = case_when(Richness <= 2  ~ 1, Richness <= 5  ~ 2, Richness <= 10 ~ 3, Richness <= 15 ~ 4, Richness > 15  ~ 5))
site_relief <- fish_relief %>%
  select(Year,Site,Relief = Relief_Site) %>%
  distinct() %>% 
  mutate(
    SC_index = pmin(Relief / 150,1),
    SC_score = case_when(SC_index <= 0.2 ~ 1, SC_index <= 0.4 ~ 2, SC_index <= 0.6 ~ 3, SC_index <= 0.8 ~ 4, SC_index <= 1.0 ~ 5))


# Combine all indicators ---------------------------

site_indicator_values <- site_lcc %>%
  left_join(site_fma, by = c("Year", "Site")) %>%
  left_join(site_cd, by = c("Year", "Site")) %>%
  left_join(site_relief, by = c("Year", "Site")) %>%
  select(Year, Site, LCC_score, FMA_score, SD_score, SC_score) %>%
  distinct() %>%
  group_by(Year, Site) %>%
  ungroup() %>%
  rename(LCC = LCC_score, FMA = FMA_score, SD = SD_score, SC = SC_score)
summary_agrra <- site_indicator_values %>% 
  pivot_longer(
    cols = c(LCC, FMA, SD, SC),
    names_to = "Indicator_score",
    values_to = "Mean_score"
  )

# Slicing and binding data frame to create table

right <- slice(site_summary,1:18)
middle <- slice(site_summary, 19:36)
left <- slice(site_summary, 37:54)
names(left) <- paste0(names(left),"_2")
names(middle) <- paste0(names(middle),"_3")
site_summary_wrapped <- bind_cols(right, middle, left) 

# Color scheme vector for scores

normal_palette <- c("1" = "#d73027", "2" = "#fc8d59", "3" = "#fee08b", "4" = "#91cf60", "5" = "#1a9850")
reverse_palette <- c("1" = "#1a9850", "2" = "#91cf60", "3" = "#fee08b", "4" = "#fc8d59", "5" = "#d73027")

Summary_table <- site_summary_wrapped %>% 
  gt() %>% 
  tab_header(
    title = "Rapid Reef Assessment Scores",
    subtitle = "San Pedro, Ambergris Caye") %>% 
  tab_style(style = cell_text(color = "black"), locations = cells_body(columns = everything())) %>% 
  tab_options(table.background.color = "#f5f5f5") %>% 
  cols_align(align = "center", columns = everything()) %>% 
  cols_label(Site_2 = "Site", Longitude_2 = "Longitude", Latitude_2 = "Latitude", LCC_2 = "LCC", FMA_2 = "FMA",
             SD_2 = "SD", SC_2 = "SC", Site_3 = "Site", Longitude_3 = "Longitude", Latitude_3 = "Latitude",
             LCC_3 = "LCC", FMA_3 = "FMA", SD_3 = "SD", SC_3 = "SC") %>% 
  data_color(columns = c(LCC, SD, SC, LCC_2, SD_2, SC_2, LCC_3, SD_3, SC_3), palette = normal_palette, domain = c(1,2,3,4,5)) %>% 
  data_color(columns = c(FMA, FMA_2, FMA_3), palette = reverse_palette, domain = c(1,2,3,4,5))
  
gt::gtsave(Summary_table, "summary_table.html")

# comparison test (Generate permanent data frame for site_indicator_value)

site_indicator_long <- site_indicator_values %>%
  pivot_longer(
    cols = c(LCC, FMA, SD, SC),
    names_to = "Indicator_score",
    values_to = "Mean_score"
  )

# Testing testing 

summary_rea_clean <- summary_rea %>%
  filter(grepl("^AMP", Site)) %>%
  mutate(
    Site = gsub("^AMP-", "", Site),
    Site = recode(Site,"BZ1076B" = "1076B"))
summary_agrra_clean <- summary_agrra %>%
  mutate(
    Site = trimws(Site)
  )
comparison_df <- summary_rea_clean %>%
  select(Site, Indicator_score, Mean_score) %>%
  rename(Rapid_score = Mean_score) %>%
  inner_join(
    summary_agrra_clean %>%
      select(Site, Indicator_score, Mean_score) %>%
      rename(AGRRA_score = Mean_score),
    by = c("Site", "Indicator_score")
  )
# Spearman comparison for REA score in relation to AGRRA

spearman_result <- cor.test(
  comparison_df$AGRRA_score,
  comparison_df$Rapid_score,
  method = "spearman"
)

spearman_r <- round(spearman_result$estimate, 3)
spearman_p <- signif(spearman_result$p.value, 3)

comparison_df <- comparison_df %>%
  mutate(Score_Difference = Rapid_score - AGRRA_score)

mean_diff <- mean(comparison_df$Score_Difference, na.rm = TRUE)

score_diffrence_plot <- ggplot(comparison_df,
       aes(x = Indicator_score,
           y = Score_Difference,
           fill = Indicator_score)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.5) +
  geom_hline(yintercept = mean_diff, linetype = "dashed") +
  annotate("text", x = 2.5, y = 0.2, label = paste0("Mean difference = ", round(mean_diff,2)), size = 4) +
  labs(
    x = "Indicator",
    y = "REA Score - AGRRA Score") +
  theme_classic(base_size = 14)

# Pearson comparison of AGRRA vs REA scores at the AMP (master) sites, per indicator

pearson_result <- cor.test(
  comparison_df$AGRRA_score,
  comparison_df$Rapid_score,
  method = "pearson")
r_value <- round(pearson_result$estimate, 3)

# Pearson r per indicator (NA when an indicator has < 3 sites or no variation in scores)
pearson_by_indicator <- comparison_df %>%
  group_by(Indicator_score) %>%
  summarise(
    n = n(),
    testable = n >= 3 & sd(AGRRA_score) > 0 & sd(Rapid_score) > 0,
    r = if (testable) cor.test(AGRRA_score, Rapid_score, method = "pearson")$estimate else NA_real_,
    p = if (testable) cor.test(AGRRA_score, Rapid_score, method = "pearson")$p.value else NA_real_,
    .groups = "drop") %>%
  mutate(
    label = ifelse(is.na(r),
                   paste0("Pearson r = NA\nn = ", n),
                   paste0("Pearson r = ", round(r, 2), ", p = ", signif(p, 2), "\nn = ", n)))
print(pearson_by_indicator)

Indicator_agreement_plot <- ggplot(comparison_df,
       aes(x = AGRRA_score, y = Rapid_score, color = Indicator_score)) +
  geom_point(size = 4, alpha = 0.9) +
  geom_text_repel(aes(label = Site), size = 3.5, max.overlaps = 30) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red", linewidth = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "blue", linewidth = 0.6) +
  geom_text(data = pearson_by_indicator,
            aes(x = 1, y = 5, label = label),
            inherit.aes = FALSE, hjust = 0, vjust = 1, size = 3.5) +
  facet_wrap(~Indicator_score) +
  scale_x_continuous(limits = c(1, 5), breaks = 1:5) +
  scale_y_continuous(limits = c(1, 5), breaks = 1:5) +
  scale_color_manual(values = c("LCC" = "darkgreen", "FMA" = "orange", "SD"  = "purple","SC"  = "magenta") ) +
  labs(
    x = "AGRRA Score",
    y = "REA Score") +
  theme_classic(base_size = 14) +
  theme(legend.position = "none")

# kruksal-wallis test to determine if there is a sig dif between Surveyors

Surveyor_scores <- df_rea %>% 
  filter(Indicator_score != "OM") %>% 
  select(Date, Site, Indicator_score, ANLI, ELAL, NOMC, WICA) %>% 
  pivot_longer(
    cols = c("ANLI", "ELAL", "NOMC", "WICA"),
    names_to = "Surveyor",
    values_to = "Score"
  )
kruskal.test(
  Score ~ Surveyor,
  data = Surveyor_scores)

kruskal_result <- Surveyor_scores %>%
  group_by(Indicator_score) %>%
  summarise(
    P_value = kruskal.test(Score ~ Surveyor)$p.value)
kruskal_result

anova_result <- Surveyor_scores %>%
  group_by(Indicator_score) %>%
  summarise(
    P_value = summary(aov(Score ~ Surveyor))[[1]][["Pr(>F)"]][1],
    .groups = "drop")
print(anova_result)

Surveyor_variability_plot <- ggplot(Surveyor_scores,
       aes(x = Surveyor, y = Score, fill = Surveyor)) +
  geom_boxplot( alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.4, size = 2) +
  facet_wrap(~Indicator_score) +
  geom_text(data = kruskal_result,
   aes(x = 2.5, y = 5.4,label = paste0("Kruskal p = ", signif(P_value, 3))), 
   inherit.aes = FALSE, size = 3.5 ) +
  labs(
    x = "Surveyor",
    y = "REA Score") +
  theme_classic(base_size = 14)

# Data visualization outputs

ggsave( "outputs/Indicator_agreement.png", 
        Indicator_agreement_plot, width = 11, height = 7, dpi = 300)
ggsave("outputs/Surveyor_variabilty.png",
       Surveyor_variability_plot, width = 11, height = 7, dpi = 300)
ggsave("outputs/Score_difference.png",
       score_diffrence_plot, width = 11, height = 7, dpi = 300)
