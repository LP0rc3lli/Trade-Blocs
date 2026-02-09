# Preprocessing -----------------------------------------------------------

library(sqldf)
library(data.table)

# Cleaning ----------------------------------------------------------------

trade <- fread("BACI_HS22_Y2022_V202501.csv")
countries <- fread("country_codes_V202501.csv")

trade_clean <- sqldf("
  SELECT 
    trade.t AS year,
    exp.country_iso3 AS exp,
    imp.country_iso3 AS imp,
    SUBSTR(trade.k, 1, 2) AS product,
    SUM(trade.v)*1000 AS value,
    SUM(trade.q) AS quantity
  FROM
    trade 
  LEFT JOIN
    countries exp ON trade.i = exp.country_code
  LEFT JOIN
    countries imp ON trade.j = imp.country_code
  GROUP BY
    trade.t, 
    exp.country_iso3, 
    imp.country_iso3,
    SUBSTR(trade.k, 1, 2)
")
View(trade_clean)


# WDI (GDP) ---------------------------------------------------------------

library(WDI)

gdp_data <- WDI(
  indicator = "NY.GDP.MKTP.CD", #GDP in $USD
  start = 2022, 
  end = 2022,
  extra = TRUE
)

trade_gdp <- sqldf('
  SELECT 
    trade_clean.*,
    imp_gdp."NY.GDP.MKTP.CD" as imp_gdp,
    exp_gdp."NY.GDP.MKTP.CD" as exp_gdp
  FROM 
    trade_clean
  LEFT JOIN 
    gdp_data imp_gdp ON trade_clean.imp = imp_gdp.iso3c
  LEFT JOIN 
    gdp_data exp_gdp ON trade_clean.exp = exp_gdp.iso3c
')

View(trade_gdp)

# Bloc_map ----------------------------------------------------------------

blocs <- fread("bloc_membership.csv")
View(blocs)
trade_gdp_bloc <- sqldf('
  SELECT
    trade_gdp.*,
    exp_bloc.bloc AS exp_bloc,
    imp_bloc.bloc AS imp_bloc,
    CASE
      WHEN exp_bloc.bloc IS NOT NULL AND imp_bloc.bloc IS NOT NULL AND exp_bloc.bloc = imp_bloc.bloc THEN "intra"
      WHEN exp_bloc.bloc IS NULL AND imp_bloc.bloc IS NULL THEN "neither"
      ELSE "extra"
    END AS trade_type
  FROM
    trade_gdp
  LEFT JOIN
    blocs exp_bloc ON trade_gdp.exp = exp_bloc.iso3
  LEFT JOIN
    blocs imp_bloc ON trade_gdp.imp = imp_bloc.iso3
')

View(trade_gdp_bloc)
fwrite(trade_gdp_bloc,"trade_final.csv")
