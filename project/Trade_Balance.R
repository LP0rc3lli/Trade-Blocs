library(data.table)
trade_data <- fread("trade_final.csv")

# intra-bloc exports
intra_exports <- trade_data %>%
  filter(trade_type == "intra", !is.na(exp_bloc)) %>%
  group_by(exp) %>%
  summarise(intra_exports = sum(value, na.rm = TRUE))

# intra-bloc imports
intra_imports <- trade_data %>%
  filter(trade_type == "intra", !is.na(imp_bloc)) %>%
  group_by(imp) %>%
  summarise(intra_imports = sum(value, na.rm = TRUE))

# GDP per country
gdp_latest <- trade_data %>%
  group_by(exp) %>%
  summarise(exp_gdp = max(exp_gdp, na.rm = TRUE)) %>%
  mutate(exp_gdp = ifelse(is.infinite(exp_gdp), NA, exp_gdp))

# Merge all
trade_balance_df <- intra_exports %>%
  full_join(intra_imports, by = c("exp" = "imp")) %>%
  left_join(gdp_latest, by = "exp") %>%
  mutate(
    intra_exports = ifelse(is.na(intra_exports), 0, intra_exports),
    intra_imports = ifelse(is.na(intra_imports), 0, intra_imports),
    intra_trade_balance = intra_exports - intra_imports
  )
View(trade_balance_df)

# Spearman
spearman_correlation <- cor.test(trade_balance_df$exp_gdp, trade_balance_df$intra_trade_balance, method = "spearman", use = "complete.obs")
print(spearman_correlation$p.value)

ggplot(trade_balance_df, aes(x = exp_gdp, y = intra_trade_balance)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_x_log10() +
  labs(
    title = paste("GDP vs intra-bloc Trade Balance (r =", round(spearman_correlation, 3), ")"),
    x = "Exporter GDP (USD)",
    y = "intra-bloc Trade Balance (USD)")
  