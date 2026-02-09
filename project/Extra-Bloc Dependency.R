library(dplyr)
library(data.table)
library(tidyr)
library(ggplot2)

trade_data <- fread("trade_final.csv")

bloc_membership <- fread("bloc_membership.csv")

# Precise HS code to critical sector mapping
critical_sector_map <- data.frame(
  product_section = sprintf("%02d", 1:99),
  critical_sector = c(
    rep("Food & Agriculture", 24),      # HS 17-24 (Sugars, cocoa, food preparations)
    rep("Manufacturing Inputs", 2),    # HS 25-26 (Salt, earths, ores)
    rep("Energy", 1),                  # HS 27 (Mineral fuels, oils)
    rep("Manufacturing Inputs", 2),    # HS 28-29 (Inorganic chemicals)
    rep("Pharmaceuticals", 1),         # HS 30 (Pharmaceuticals)
    rep("Manufacturing Inputs", 10),   # HS 31-40 (Fertilizers, tanning, dyes, etc.)
    rep("Electronics/Machinery", 9),   # HS 41-49 (Leather, wood, pulp, textiles)
    rep("Textiles", 10),               # HS 50-59 (Textiles and clothing)
    rep("Manufacturing Inputs", 8),    # HS 60-67 (Footwear, headgear, etc.)
    rep("Metals", 16),                 # HS 68-83 (Stone, glass, metals)
    rep("Electronics/Machinery", 2),   # HS 84-85 (Machinery/electrical)
    rep("Transport", 4),               # HS 86-89 (Vehicles/transport)
    rep("Other", 10)                   # HS 90-99 (Instruments, arms, art, etc.)
  )
)

# Manual adjustments for specific cases
critical_sector_map <- critical_sector_map %>%
  mutate(critical_sector = case_when(
    product_section == "16" ~ "Food & Agriculture",  # Preparations of meat/fish
    product_section == "27" ~ "Energy",             # Mineral fuels/oils
    product_section == "30" ~ "Pharmaceuticals",    # Pharmaceuticals
    product_section %in% sprintf("%02d", 84:85) ~ "Electronics/Machinery",
    product_section %in% sprintf("%02d", 86:89) ~ "Transport",
    TRUE ~ critical_sector
  ))

# Add product_section and merge critical sector info
trade_data$product_section <- substr(sprintf("%02d", trade_data$product), 1, 2)
trade_data <- trade_data %>%
  left_join(critical_sector_map, by = "product_section")

# Remove existing imp_bloc if it exists
if("imp_bloc" %in% names(trade_data)) {
  trade_data <- trade_data %>% select(-imp_bloc)
}

# Merge bloc membership and handle non-bloc countries
trade_data <- trade_data %>%
  left_join(bloc_membership, by = c("imp" = "iso3")) %>%
  rename(imp_bloc = bloc) %>%
  mutate(imp_bloc = ifelse(is.na(imp_bloc), "non-bloc", imp_bloc))

# Filter for critical sectors only
critical_sectors <- c("Energy", "Food & Agriculture", "Pharmaceuticals",
                      "Electronics/Machinery", "Transport", 
                      "Manufacturing Inputs", "Metals", "Textiles")

trade_data_critical <- trade_data %>%
  filter(critical_sector %in% critical_sectors)

# Aggregate imports data
imports <- trade_data_critical %>%
  filter(trade_type %in% c("intra", "extra")) %>%
  group_by(imp, imp_bloc, critical_sector, trade_type) %>%
  summarise(import_value = sum(value, na.rm = TRUE), .groups = "drop")

# Pivot to wide format
imports_wide <- imports %>%
  pivot_wider(
    names_from = trade_type,
    values_from = import_value,
    values_fill = 0
  ) %>%
  mutate(
    total_imports = intra + extra,
    dependency = case_when(
      imp_bloc == "non-bloc" ~ 100,
      TRUE ~ 100 * extra / total_imports
    )
  )
View(imports_wide)
# Filter clean data for ANOVA (excluding non-bloc)
anova_data <- imports_wide %>%
  filter(!is.na(dependency), imp_bloc != "non-bloc")

# one-way ANOVA (bloc)
anova_bloc <- aov(dependency ~ imp_bloc, data = anova_data)
summary(anova_bloc)

# Tukey HSD test
tukey_result <- TukeyHSD(anova_bloc)
print(tukey_result)

# one-way ANOVA (sector)
anova_sect <- aov(dependency ~ critical_sector, data = anova_data)
summary(anova_sect)

# Tukey HSD test
tukey_result <- TukeyHSD(anova_sect)
print(tukey_result)

# dependency by bloc
ggplot(anova_data, aes(x = reorder(imp_bloc, dependency), 
                       y = dependency,
                       color = imp_bloc)) +
  geom_boxplot() +
  labs(title = "Extra-Bloc Dependency by Trade Bloc",
       x = "Trade Bloc",
       y = "% Crit. Imports from Non-Members") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Heatmap of mean dependencies
dependency_means <- anova_data %>%
  group_by(imp_bloc, critical_sector) %>%
  summarise(mean_dependency = mean(dependency, na.rm = TRUE))

ggplot(dependency_means, aes(x = imp_bloc, y = critical_sector, fill = mean_dependency)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(mean_dependency, 1)), color = "white", size = 3.5) +
  labs(title = "% Mean Extra-Bloc Dependency",
       x = "Trade Bloc",
       y = "Critical Sector",
       fill = "% Mean") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

