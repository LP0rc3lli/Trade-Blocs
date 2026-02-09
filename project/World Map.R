library(tidyverse)
library(sf)
library(rnaturalearth)
library(WDI)

# 1. Get and prepare data
world <- ne_countries(scale = "medium", returnclass = "sf") %>% 
  mutate(iso_a3 = case_when(
    name == "France" ~ "FRA",
    name == "Norway" ~ "NOR",
    name == "Kosovo" ~ "XKX",
    TRUE ~ iso_a3
  )) %>% 
  left_join(
    read_csv("bloc_membership.csv") %>% 
      mutate(bloc = case_when(
        iso3 == "FRA" ~ "EU",
        iso3 == "DEU" ~ "EU",
        TRUE ~ bloc
      )),
    by = c("iso_a3" = "iso3")
  ) %>% 
  left_join(
    WDI(country = "all", indicator = "NY.GDP.MKTP.CD", start = 2022, end = 2022) %>% 
      as_tibble() %>% 
      select(iso3c, gdp = NY.GDP.MKTP.CD) %>% 
      filter(!is.na(gdp)),
    by = c("iso_a3" = "iso3c")
  )

# 2. Create map with compact legend
ggplot(world) +
  geom_sf(aes(fill = bloc, alpha = gdp), color = "white", size = 0.1) +
  scale_fill_manual(
    name = NULL,  # Remove legend title
    values = c(
      "AfCFTA" = "#1f77b4", "EU" = "#ff7f0e", "USMCA" = "#2ca02c",
      "RCEP" = "#d62728", "Mercosur" = "#9467bd", "Andean" = "#8c564b",
      "CARICOM" = "#e377c2", "CEFTA" = "#7f7f7f", "GCC" = "#bcbd22",
      "EFTA" = "#17becf"
    ),
    na.value = "gray90",
    guide = guide_legend(
      keywidth = unit(0.4, "cm"),  # Smaller color boxes
      keyheight = unit(0.4, "cm"),
      ncol = 1  # Single column layout
    )
  ) +
  scale_alpha_continuous(
    name = NULL,  # Remove legend title
    trans = "log10",
    labels = scales::dollar_format(scale = 1e-12, suffix = "T"),
    range = c(0.3, 1),
    guide = guide_legend(
      direction = "horizontal",  # Horizontal layout for GDP
      title.position = "top",
      label.position = "bottom",
      keywidth = unit(1.5, "cm"),  # Wider for gradient
      keyheight = unit(0.3, "cm")   # Thinner
    )
  ) +
  labs(title = "Global Trade Bloc Membership (2022)") +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.box = "vertical",  # Stack legends vertically
    legend.spacing.y = unit(0.2, "cm"),  # Reduce spacing
    legend.text = element_text(size = 7),  # Smaller text
    panel.grid = element_blank(),
    plot.title = element_text(size = 11, face = "bold", hjust = 0.5)  # Compact title
  ) +
  guides(
    fill = guide_legend(order = 1),  # Trade bloc first
    alpha = guide_legend(order = 2)   # GDP second
  )
