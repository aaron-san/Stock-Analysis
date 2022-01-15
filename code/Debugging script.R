# 
# fundamentals_data <- 
#     ratios %>% 
#     filter(industry == "biotechnology") %>% 
#     pivot_longer(-c(ticker, rounded_date, sector, industry, short_name),
#                             names_to = "ratio", values_to = "value")
# 
# fundamentals <- 
#     fundamentals_data %>%
#     filter(ratio == "accruals") %>%
#     # filter(ratio == janitor::make_clean_names(input$ratio)) %>%
#     mutate(ticker = factor(ticker),
#            ticker = fct_reorder(ticker, desc(value), .fun='median'))
#     
#     
# remove_outliers <- TRUE
# 
# # Expand plot limits
# if(remove_outliers) {
#         fundamentals <- 
#             fundamentals %>% 
#             filter(value < quantile(value, 0.6, na.rm = TRUE) & 
#                        value > quantile(value, 0.4, na.rm = TRUE))
#         max_value <- filtered_values %>% pull(value) %>% max(na.rm = TRUE) 
#         min_value <- filtered_values %>% pull(value) %>% min(na.rm = TRUE) 
#         min_value <- min_value - (max_value*1.2 - max_value) 
#     } else {
#         max_value <- fundamentals_data %>% pull(value) %>% max(na.rm = TRUE)
#         min_value <- fundamentals_data %>% pull(value) %>% min(na.rm = TRUE)
#         min_value <- min_value - (max_value*1.2 - max_value)
#     }
# 
# 
# 
# 
# 
# fundamentals %>%
#     ggplot(aes(x = rounded_date, y = value, fill = ticker)) + #size = value
#     geom_point(aes(color = ticker)) +
#     # scale_y_discrete(label = scales::dollar_format(scale = 1e-6, unit = "M")) +
#     expand_limits(y = c(min_value, max_value*1.2)) +
#     # labs(title = snakecase::to_title_case(as.character(input$ratio)),
#          # subtitle = "",
#          # x = "", y = "%") +
#     theme_minimal() +
#     theme(axis.title.y = element_text(color = "grey"))
#     
# ggplotly(plot) %>% config(displayModeBar = FALSE)