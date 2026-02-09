library(sqldf)
library(data.table)
library(tidyr)

trade_data <- fread('trade_final.csv')
bloc_membership <- fread('bloc_membership.csv')

country_trade <- sqldf('
  SELECT 
    t.exp AS exp,
    b.bloc,
    t.trade_type,
    SUM(t.value) AS trade_value
  FROM 
    trade_data t
  JOIN 
    bloc_membership b ON t.exp = b.iso3
  WHERE t.trade_type IN ("intra", "extra")
  GROUP BY t.exp, b.bloc, t.trade_type
')

country_totals <- sqldf('
  SELECT 
    exp,
    SUM(trade_value) AS total_trade
  FROM 
    country_trade
  GROUP BY exp
')

bloc_trade <- sqldf('
  SELECT 
    ct.exp,
    ct.bloc,
    ct.trade_type,
    ct.trade_value,
    (ct.trade_value / tt.total_trade) * 100 AS trade_perc
  FROM 
    country_trade ct
  JOIN 
    country_totals tt ON ct.exp = tt.exp
  ORDER BY 
    ct.exp, ct.trade_type
')

View(bloc_trade)

trade_data <- pivot_wider(
  data = bloc_trade,
  id_cols = c(exp, bloc),
  names_from = trade_type,
  values_from = c(trade_value, trade_perc)
)

trade_data$perc_diff <- trade_data$trade_perc_extra - trade_data$trade_perc_intra
View(trade_data)

t.test(trade_data$trade_perc_extra, trade_data$trade_perc_intra, paired = TRUE)
fwrite(trade_data, "trading.csv")

# Bar Chart Q1 ------------------------------------------------------------

library(ggplot2)
library(dplyr)
library(tidyr)

# Read and prepare data
trade_data <- read.csv("trading.csv")

# Select countries of interest
selected_countries <- c("USA", "CHN", "DEU", "FRA", "JPN", "IND", "GBR", "BRA", "CAN", "MEX")

# Prepare plot data - keeping in wide format
plot_data <- trade_data %>%
  filter(exp %in% selected_countries) %>%
  select(exp, bloc, trade_perc_extra, trade_perc_intra) %>%
  # Order countries by extra trade percentage
  arrange(desc(trade_perc_extra)) %>%
  mutate(exp = factor(exp, levels = exp))

# Convert to long format for plotting
plot_data_long <- plot_data %>%
  pivot_longer(
    cols = c(trade_perc_extra, trade_perc_intra),
    names_to = "trade_type",
    values_to = "percentage"
  ) %>%
  mutate(
    trade_type = case_when(
      trade_type == "trade_perc_extra" ~ "Extra-Bloc",
      trade_type == "trade_perc_intra" ~ "Intra-Bloc",
      TRUE ~ trade_type
    )
  )

# Create double bar chart
ggplot(plot_data_long, aes(x = exp, y = percentage, fill = trade_type)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_manual(values = c("Extra-Bloc" = "#1f77b4", "Intra-Bloc" = "#FF474C")) +
  labs(
    title = "Intra- vs Extra-Bloc Trade by Country",
    x = "Country",
    y = "Percentage of Total Trade (%)",
    fill = "Trade Type"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    legend.position = "top",
    plot.title = element_text(face = "bold", size = 14),
    panel.grid.major.x = element_blank()
  ) +
  scale_y_continuous(limits = c(0, 105))  # Add space for labels

# BarChart Blocs ----------------------------------------------------------

library(ggplot2)
library(dplyr)
library(tidyr)

# Read and prepare data
trade_data <- read.csv("trading.csv")

# Calculate TOTAL trade percentages by bloc (summing all member countries)
bloc_totals <- trade_data %>%
  group_by(bloc) %>%
  summarize(
    total_extra = sum(trade_value_extra, na.rm = TRUE),
    total_intra = sum(trade_value_intra, na.rm = TRUE)
  ) %>%
  mutate(
    total_trade = total_extra + total_intra,
    perc_extra = (total_extra / total_trade) * 100,
    perc_intra = (total_intra / total_trade) * 100
  ) %>%
  arrange(desc(perc_extra))  # Order blocs by extra-bloc percentage

# Convert to long format for plotting
bloc_totals_long <- bloc_totals %>%
  pivot_longer(
    cols = c(perc_extra, perc_intra),
    names_to = "trade_type",
    values_to = "percentage"
  ) %>%
  mutate(
    trade_type = case_when(
      trade_type == "perc_extra" ~ "Extra-Bloc",
      trade_type == "perc_intra" ~ "Intra-Bloc",
      TRUE ~ trade_type
    ),
    bloc = factor(bloc, levels = bloc_totals$bloc)  # Maintain order
  )

# Create double bar chart for bloc totals
ggplot(bloc_totals_long, aes(x = bloc, y = percentage, fill = trade_type)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_manual(values = c("Extra-Bloc" = "#1f77b4", "Intra-Bloc" = "#FF474C")) +
  labs(
    title = "Intra- vs Extra-Bloc Trade by Bloc",
    x = "Trade Bloc",
    y = "Percentage of Total Trade (%)",
    fill = "Trade Type",
    caption = "Calculated using sum of all member country trade values"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y = element_text(size = 11),
    legend.position = "top",
    plot.title = element_text(face = "bold", size = 14),
    panel.grid.major.x = element_blank(),
    plot.caption = element_text(face = "italic")
  ) +
  scale_y_continuous(limits = c(0, 105)) +
  coord_cartesian(clip = "off")

# Box plot

library(tidyr)
# Reshape data to long format for plotting
trade_data_long <- trade_data %>%
  pivot_longer(
    cols = c(trade_perc_extra, trade_perc_intra),
    names_to = "trade_type",
    values_to = "percentage"
  ) %>%
  mutate(trade_type = ifelse(trade_type == "trade_perc_extra", "Extra", "Intra"))

# Plot with bloc-specific comparisons
ggplot(trade_data_long, aes(x = bloc, y = percentage, fill = trade_type)) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = c("Extra" = "lightblue", "Intra" = "salmon")) +
  labs(
    x = "Trade Bloc", 
    y = "Percentage of Trade",
    title = "Extra- vs. Intra-Bloc Trade by Bloc",
    fill = "Trade Type"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
