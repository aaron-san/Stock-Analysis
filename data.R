
library(tidyverse)
library(arrow)
library(data.table)
library(xts)


read_tibble <- function(x, date_format = "%Y-%m-%d") {
  x %>%
    fread(fill = TRUE, integer64 = "double", data.table = FALSE) %>% 
          # na.strings = c("", "NA", NA)) %>% 
    as_tibble() %>% 
    mutate(across(which(sapply(., class) == "integer64" ), as.numeric)) %>%
    mutate(across(contains("date"), ~as.Date(.x, date_format)))
}


# Load data
monthly_prices_file <- 
  list.files("data", pattern = "monthly_prices", full.names = TRUE) %>% max()
monthly_prices <- read_tibble(monthly_prices_file)
monthly_rets <- monthly_prices %>%  mutate(across(-date, ~.x / lag(.x) - 1))


prices_from_FRED <- list.files("data", pattern = "Prices from FRED", full.names = TRUE) %>% max()
prices_econ <- read_tibble(prices_from_FRED)


prices_combined <- 
  monthly_prices %>% 
  pivot_longer(-date, names_to = "symbol", values_to = "price") %>% 
  full_join(prices_econ)


symbol_choices <- prices_combined %>% distinct(symbol) %>% pull()


ticker_choices_bt <- colnames(monthly_rets)[-1]
cash_rets <- read_rds("data/cash_returns.rds")# %>% tibble(date = index(.), .) %>% transmute(date, SHV = as.numeric(.))

cash_rets_tmp <- 
  monthly_rets %>% 
  select(date, SHV)

cash_rets <-
  cash_rets_tmp %>% 
  select(SHV) %>% 
  xts(order.by = as.Date(cash_rets_tmp$date))


# asset_rets <- read_rds("data/asset_returns.rds")

asset_rets <- 
  monthly_rets %>% 
  select(-date) %>% 
  xts(order.by = monthly_rets$date)


# Load ratios
ratios_file <- 
  list.files("data/cleaned data", 
             pattern = "ratios_final \\(\\d{4} \\d{2} \\d{2}\\)$", 
             full.names = TRUE) %>% max()
ratios <- read_feather(ratios_file)


profiles_files <- list.files("data/cleaned data",
                          pattern = "profiles_final", full.names = TRUE)
profile_data <- read_tibble(profiles_files)

# profile_data %>% filter(sector_yhoo == "unknown") %>% 
  # pull(ticker)



ticker_choices_sec <- ratios %>% distinct(ticker) %>% pull()


# set.seed(123)
# ratios_wide <- ratios_wide %>%
#   group_by(industry_yhoo) %>%
#   filter(ticker %in% sample(ticker, 2)) %>%
#   ungroup()


ratio_choices <- ratios %>% select(where(is.numeric)) %>% colnames()
industry_choices <- ratios %>% distinct(industry_damodaran) %>% pull()

               
                 
current_econ_data_file <- list.files("data", pattern = "Prices from FRED", full.names = TRUE) %>% max()
col_types <- list(symbol = "c", date = "D", price = "d")
econ_data <- read_csv(current_econ_data_file, col_types = col_types) %>% 
  dplyr::group_by(symbol) %>%
  dplyr::arrange(symbol, date) %>% 
  rename(level = "price")

# current_yields_file <- list.files("data", pattern = "yields", full.names = TRUE) %>% max()
yields_monthly <- econ_data %>% 
    filter(symbol %in% c("DGS1MO", "DGS3MO", "DGS6MO", "DGS1", "DGS2", "DGS5", "DGS7", "DGS10", "DGS20", "DGS30")) %>% 
    dplyr::rename(yield = "level") %>%
    tibbletime::as_tbl_time(index = date) %>% 
    dplyr::group_by(symbol) %>%
    drop_na() %>%
    slice(endpoints(date, on = "months")) %>% 
    dplyr::mutate(symbol = factor(symbol, levels = c("DGS1MO", "DGS3MO", "DGS6MO", "DGS1", "DGS2", "DGS5", "DGS7", "DGS10", "DGS20", "DGS30"))) %>% 
    dplyr::mutate(symbol = dplyr::recode(symbol,
                                         "DGS1MO" = "1 month", 
                                         "DGS3MO" = "3 month",
                                         "DGS6MO" = "6 month", 
                                         "DGS1" = "1 year",
                                         "DGS2" = "2 year", 
                                         "DGS5" = "5 year", 
                                         "DGS7" = "7 year", 
                                         "DGS10" = "10 year", 
                                         "DGS20" = "20 year", 
                                         "DGS30" = "30 year"))

dates_yields <- 
  yields_monthly %>% 
    dplyr::distinct(date) %>% 
    dplyr::pull(date) %>% 
    unique()



gdp <- 
  econ_data %>%
    filter(symbol == "GDP") %>%
    mutate(change = TTR::ROC(level))

dates_gdp <- gdp %>%
    pull(date) %>%
    unique()


inflation <- 
  econ_data %>%
  filter(symbol == "CPIAUCNS") %>% # Consumer price index
  transmute(symbol,
            date,
            level = (level / dplyr::lag(level, n = 12) - 1) * 100) %>%
  drop_na()

dates_inflation <- 
  inflation %>%
    pull(date) %>%
    unique()

bond_yields <- econ_data %>%
    dplyr::filter(symbol %in% c("AAA", "BAA")) %>%
    dplyr::group_by(symbol)

dates_bond_yields <- bond_yields %>%
    dplyr::pull(date) %>%
    unique()




ratio_functions <- list(
market_cap = "close * shares_basic",
current_ratio = "total_current_assets / total_current_liabilities",
gross_margin = "ifelse(gross_profit == revenue, NA, gross_profit / revenue)",
operating_profit_margin = "operating_income_loss / revenue",
roe = "ifelse(total_stockholder_equity > 0, net_income_common / lag(total_stockholder_equity, 4), NA)",
roa = "income_loss_from_continuing_operations / total_assets",
pct_chg_accruals = "(net_income - total_cash_from_operating_activities)/ lag((net_income - total_cash_from_operating_activities), 4)",
roe_sma_4q = "slide_dbl(roe, mean, .before = 3)",
gross_margin_avg_3y = "slide_dbl(gross_margin, mean, .before = 4*3-1)",
gross_margin_sd_3y = "slide_dbl(gross_margin, sd, .before = 4*3-1)", 
gross_margin_stability_3y = "gross_margin_avg_3Y / gross_margin_sd_3Y",
operating_profit_margin_sd3y = "slide_dbl(operating_profit_margin, sd, .before = 4*3-1)",
gross_margin_pct_chg_4y = "gross_margin / lag(gross_margin, 4) - 1",
gross_margin_growth_7y_qrtly_effective_yield = 
  "slide_dbl(1 + gross_margin_pct_chg_4Q, prod, .before = 7*4-1) ^ (1/(7*4-1)) - 1",
scaled_total_accruals = "((total_current_assets - cash_and_short_term_investments) -
                           (total_current_liabilities - short_long_term_debt) - 
                           depreciation) / total_assets",
scaled_net_operating_assets = "((total_assets - cash_and_short_term_investments) - 
                                 (total_assets - short_long_term_debt - (total_liab - total_current_liabilities) - 
                                    (total_assets - total_liab))) / total_assets",
days_sales_outstanding = "365 / (revenue / net_receivables)",
gross_margin_index = "lag(gross_margin, 4) / gross_margin",
asset_quality_index = "(total_assets - total_current_assets - property_plant_equipment) / total_assets",
sales_growth_index = "revenue / lag(revenue, 4)",
depreciation_index = "lag(depreciation, 4) / depreciation",
sga_index = "selling_general_administrative / lag(selling_general_administrative, 4)",
debt_ratio = "total_liab / total_assets",
leverage_index = "debt_ratio / lag(debt_ratio, 4)",
working_cap_ex_cash = "total_current_assets - cash_and_short_term_investments - total_current_liabilities",
total_accruals_to_total_assets = "(working_cap_ex_cash - 
                                    lag(working_cap_ex_cash, 4) - depreciation) / total_assets,
capital = property_plant_equipment + total_current_assets -
  total_current_liabilities - cash_and_short_term_investments",
roc = "operating_income_loss / capital",
working_capital = "total_current_assets - cash_and_short_term_investments - total_current_liabilities",
free_cash_flow = "net_income + depreciation - c(NA, diff(working_capital)) + total_cashflows_from_investing_activities",
free_cash_flow_to_assets = "free_cash_flow / total_assets",
gross_margin_avg_8y = "slide_dbl(gross_margin, mean, .before = 8*4-1)",
gross_margin_sd_8y = "slide_dbl(gross_margin, sd, .before = 8*4-1)",
gross_margin_stability_8y = "gross_margin_avg_8Y/gross_margin_sd_8Y",
lt_debt_ratio = "(total_liab - total_current_liabilities) / total_assets",
working_capital_pct_chg_4q = "working_capital / lag(working_capital, n = 4) - 1",
asset_turnover = "revenue / total_assets",
excess_cash = 'cash_and_short_term_investments + total_current_assets - total_current_liabilities',
total_debt = "total_liab - total_current_liabilities + short_long_term_debt",
enterprise_value = "market_cap + total_debt - excess_cash",
ebit_to_ev = "ebit / enterprise value = ebit / (market_cap + total_debt - excess_cash)",
accruals_pct_chg_4q = "(net_income - total_cash_from_operating_activities)/ lag((net_income - total_cash_from_operating_activities), 4)")

ratio_guide <- list(
  asset_turnover = "(higher is better)",
  market_cap = "(higher is better)",
  current_ratio = "(higher is better)",
  operating_profit_margin = "(higher is better)",
  roa = "(higher is better)",
  roe = "(higher is better)",
  roe_sma_4q = "(higher is better)",
  pct_chg_accruals = "(lower is better)",
  gross_margin = "(higher is better)",
  gross_margin_avg_3y = "(higher is better)",
  gross_margin_sd_3y = "(lower is better)", 
  gross_margin_stability_3y = "(higher is better)",
  operating_profit_margin_sd3y = "(lower is better)",
  gross_margin_pct_chg_4y = "(higher is better)",
  gross_margin_growth_7y_qrtly_effective_yield = "(higher is better)",
  scaled_total_accruals = "(lower is better - measures the degree to which earnings have outpaced cash flows and 'bloated' the balance sheet)",
  scaled_net_operating_assets = "(lower is better - SNOA indicates the degree of both opportunistic earnings manipulation and over investment
            # Accruals create 'bloated' balance sheets and SNOA captures management's attempt
            # at historical earnings manipulations and firms find it increasingly difficult
            # to sustain earnings growth.
            # SNOA is a strong predictor of poor long-term stock returns)",
  days_sales_outstanding = "(lower is better)",
  gross_margin_index = "(lower is better)",
  asset_quality_index = "(lower is better)",
  sales_growth_index = "(higher is better)",
  depreciation_index = "(lower is better)",
  sga_index = "(lower is better)",
  debt_ratio = "(lower is better)",
  leverage_index = "(lower is better)",
  working_cap_ex_cash = "(higher is better)",
  total_accruals_to_total_assets = "(lower is better - total accruals to total assets (change in working capital accounts other than cash less depreciation)/total assets) Higher accruals could indicate earnings manipulation",
  capital = "(higher is better)",
  roc = "(higher is better)",
  working_capital = "(higher is better)",
  free_cash_flow = "(higher is better)",
  free_cash_flow_to_assets = "(higher is better)",
  gross_margin_avg_8y = "(higher is better)",
  gross_margin_sd_8y = "(lower is better)",
  gross_margin_stability_8y = "(higher is better)",
  lt_debt_ratio = "(lower is better)",
  working_capital_pct_chg_4q = "(higher is better)",
  excess_cash = "(higher is better)",
  total_debt = "(lower is better)",
  enterprise_value = "(higher is better)",
  ebit_to_ev = "(higher is better)",
  accruals_pct_chg_4q = "(lower is better)")




