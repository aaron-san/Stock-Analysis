

# mod_backtest_portfolio



mod_backtest_portfolio_ui <- function(id, ticker_choices) {
    ns <- NS(id)
    
    tagList(
        
        
    )
}
        


mod_backtest_portfolio_server <- function(id, monthly_rets) {
    moduleServer(id,
                 function(input, output, session) {
                 }
                 
                 # switch()
                 # # Plot returns
                 # plot(etf_prices)
                 # 
                 # DrawdownDeviation(returns_xts)
                 # chart.Correlation(returns_xts)
                 # SortinoRatio(returns_xts)
                 # DownsideFrequency(returns_xts)
                 # Return.portfolio(returns_xts)
                 # apply.fromstart(returns_xts, "mean")
                 # # table.SpecificRisk(returns_xts, Rb = returns_xts$SPY)
                 # chart.RollingCorrelation(subset(returns_xts, select = -SPY), Rb = returns_xts$SPY, legend.loc = "bottomleft")
                 # Return.cumulative(returns_xts)
                 # SharpeRatio.annualized(returns_xts)
                 # chart.TimeSeries(returns_xts)
                 # clean.boudt(returns_xts) # this does not remove data from the series, but only decreases the magnitude of the extreme events.
                 # chart.TimeSeries(returns_xts)
                 # chart.RiskReturnScatter(returns_xts)
                 # chart.Histogram(returns_xts[, "VOO"])
                 # chart.CumReturns(returns_xts, main = "Cumulative returns", legend.loc = "topleft")
                 # AverageLength(returns_xts)
                 # table.DownsideRiskRatio(returns_xts, digits = 4) ##### Includes a lot of stats!!!! ######
                 # table.DownsideRisk(returns_xts) ##### Includes a lot of stats!!!! ######
                 # 
                 # table.Drawdowns(returns_xts) 
                 # Drawdowns(returns_xts)
                 # PainIndex(returns_xts)
                 # DownsideDeviation(returns_xts)
                 # InformationRatio(subset(returns_xts, select = -SPY), returns_xts$SPY)
                 # chart.BarVaR(returns_xts[, 6], method = "ModifiedVaR")
                 # charts.PerformanceSummary(returns_xts)
                 # AverageRecovery(returns_xts)
                 # Return.calculate(100*prices_ts) %>% na.omit() %>% round(2)
                 # table.AnnualizedReturns(returns_xts)
                 # table.CalendarReturns(returns_xts)
                 # 
                 # chart.Boxplot(returns_xts)
                 # chart.StackedBar(returns_xts)
                 # chart.Drawdown(returns_xts)
                 # 
                 # table.Correlation(returns_xts)
                 # chart.Bar(returns_xts)
                 # chart.RollingPerformance(returns_xts)
                 # 
                 # chart.RollingMean(returns_xts)
                 # chart.Scatter(returns_xts[, 1], returns_xts[, 7])
                 # 
                 # table.Distributions(returns_xts)
                 # table.TrailingPeriods(returns_xts)
                 # VaR(returns_xts)
                 # charts.PerformanceSummary(returns_xts)
                 # 
                 # 
                 # # Get a correlation matrix
                 # assetsCorImagePlot(etf_returns)
    )
}