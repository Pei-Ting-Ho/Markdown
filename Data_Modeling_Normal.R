# Load packages
library(readr)
library(dplyr)
library(lubridate)
library(forcats)
library(ggplot2)
library(janitor)

# Preprocess data
  # Consistent column names
  sales_data <- sales_data %>% clean_names(case = "snake")
  # Date/Time variables
  sales_data <- sales_data %>% mutate(date = mdy(date), time = hms(time),
                                      weekday = wday(date),
                                      weekend = if_else(between(weekday, 2, 6), FALSE, TRUE),
                                      morning = am(time))
  # Continuous variables
  sales_data <- sales_data %>% mutate(price = unit_price * 1.05,
                                      cost = cogs / quantity)
  # Factor variables
  sales_data <- sales_data %>% mutate(customer_type = as.factor(customer_type),
                                      gender = as.factor(gender),
                                      product_line = as.factor(product_line),
                                      payment = as.factor(payment),
                                      weekday = as.factor(weekday),
                                      weekend = as.factor(weekend),
                                      morning = as.factor(morning))
  
# Encode values
  # Data preparation: Data selected for each product category
  data_selected <- function(x) {
    sales_data %>%
      filter(product_line == x) 
  }
  # Results
    # Health and beauty
    hb <- data_selected("Health and beauty")
    hb_m <- hb %>% filter(customer_type == 'Member')
    hb_n <- hb %>% filter(customer_type == 'Normal')
    # Electronic accessories
    ea <- data_selected("Electronic accessories")
    ea_m <- ea %>% filter(customer_type == 'Member')
    ea_n <- ea %>% filter(customer_type == 'Normal')
    # Home and lifestyle
    hl <- data_selected("Home and lifestyle")
    hl_m <- hl %>% filter(customer_type == 'Member')
    hl_n <- hl %>% filter(customer_type == 'Normal')
    # Sports and travel
    st <- data_selected("Sports and travel")
    st_m <- st %>% filter(customer_type == 'Member')
    st_n <- st %>% filter(customer_type == 'Normal')
    # Food and beverages
    fb <- data_selected("Food and beverages")
    fb_m <- fb %>% filter(customer_type == 'Member')
    fb_n <- fb %>% filter(customer_type == 'Normal')
    # Fashion accessories
    fa <- data_selected("Fashion accessories")
    fa_m <- fa %>% filter(customer_type == 'Member')
    fa_n <- fa %>% filter(customer_type == 'Normal')
  
# Prepare data
  # Create a dataframe: Price, Quantity, Marginal cost
  p_q_mc_obs <- function(prod_clus_data, prod_data, customer_type) {
    # Price-response
    p_q <- prod_clus_data %>%
      # Round prices
      mutate(price = round(price, 1)) %>% 
      # Aggregate quantity for a given price
      count(price, wt = quantity) 
    # Marginal cost
    mc <- prod_data %>%
      summarise(marginal_cost = round(mean(cost), 1)) %>% 
      as.numeric()
    # Combine price, quantity, marginal cost
    return(cbind(p_q, 
                 marginal_cost = rep(mc, nrow(p_q)),
                 cluster = rep(customer_type, nrow(p_q))))
  }
  # Results
    # Health and beauty
    hb_m_p_q_mc <- p_q_mc_obs(hb_m, hb, "Member") 
    hb_n_p_q_mc <- p_q_mc_obs(hb_n, hb, "Normal")
    # Electronic accessories
    ea_m_p_q_mc <- p_q_mc_obs(ea_m, ea, "Member")
    ea_n_p_q_mc <- p_q_mc_obs(ea_n, ea, "Normal")
    # Home and lifestyle
    hl_m_p_q_mc <- p_q_mc_obs(hl_m, hl, "Member")
    hl_n_p_q_mc <- p_q_mc_obs(hl_n, hl, "Normal")
    # Sports and travel
    st_m_p_q_mc <- p_q_mc_obs(st_m, st, "Member")
    st_n_p_q_mc <- p_q_mc_obs(st_n, st, "Normal")
    # Food and beverages
    fb_m_p_q_mc <- p_q_mc_obs(fb_m, fb, "Member")
    fb_n_p_q_mc <- p_q_mc_obs(fb_n, fb, "Normal")
    # Fashion accessories
    fa_m_p_q_mc <- p_q_mc_obs(fa_m, fa, "Member")
    fa_n_p_q_mc <- p_q_mc_obs(fa_n, fa, "Normal")

# Split data
  # Shuffle the order of rows
  data_shuffled <- function(data) {
    data %>% sample_frac(size = 1, replace = FALSE)
  }
  # Split the reshuffled dataset into train set and test set
  data_train <- function(data_shuffled) {
    data_shuffled %>% slice(1:round(nrow(data_shuffled) * 0.7))
  }
  data_test <- function(data_shuffled) {
    data_shuffled %>% slice((round(nrow(data_shuffled) * 0.7) + 1):nrow(data_shuffled))
  } 
  # Results
    # Health and beauty
    set.seed(100)
    hb_m_train <- data_train(data_shuffled(hb_m_p_q_mc)) 
    hb_m_test <- data_test(data_shuffled(hb_m_p_q_mc))
    
    set.seed(101)
    hb_n_train <- data_train(data_shuffled(hb_n_p_q_mc)) 
    hb_n_test <- data_test(data_shuffled(hb_n_p_q_mc))
    
    # Electronic accessories
    set.seed(200)
    ea_m_train <- data_train(data_shuffled(ea_m_p_q_mc)) 
    ea_m_test <- data_test(data_shuffled(ea_m_p_q_mc))
    
    set.seed(201)
    ea_n_train <- data_train(data_shuffled(ea_n_p_q_mc)) 
    ea_n_test <- data_test(data_shuffled(ea_n_p_q_mc))
    
    # Home and lifestyle
    set.seed(300)
    hl_m_train <- data_train(data_shuffled(hl_m_p_q_mc)) 
    hl_m_test <- data_test(data_shuffled(hl_m_p_q_mc))
    
    set.seed(301)
    hl_n_train <- data_train(data_shuffled(hl_n_p_q_mc)) 
    hl_n_test <- data_test(data_shuffled(hl_n_p_q_mc))
    
    # Sports and travel
    set.seed(400)
    st_m_train <- data_train(data_shuffled(st_m_p_q_mc)) 
    st_m_test <- data_test(data_shuffled(st_m_p_q_mc))
    
    set.seed(401)
    st_n_train <- data_train(data_shuffled(st_n_p_q_mc)) 
    st_n_test <- data_test(data_shuffled(st_n_p_q_mc))
    
    # Food and beverages
    set.seed(500)
    fb_m_train <- data_train(data_shuffled(fb_m_p_q_mc)) 
    fb_m_test <- data_test(data_shuffled(fb_m_p_q_mc))
    
    set.seed(501)
    fb_n_train <- data_train(data_shuffled(fb_n_p_q_mc)) 
    fb_n_test <- data_test(data_shuffled(fb_n_p_q_mc))
    
    # Fashion accessories
    set.seed(600)
    fa_m_train <- data_train(data_shuffled(fa_m_p_q_mc)) 
    fa_m_test <- data_test(data_shuffled(fa_m_p_q_mc))
    
    set.seed(601)
    fa_n_train <- data_train(data_shuffled(fa_n_p_q_mc)) 
    fa_n_test <- data_test(data_shuffled(fa_n_p_q_mc))

# Fit model
  # Model the price-response relationships
    # Model-level object
      # Cross validation: Choose the optimal number of degrees
      library(caret)
      fit_control = trainControl(method = "repeatedcv",
                                 number = 5,
                                 repeats = 5)
      degree <- 1:5
      r_sq <- rep(0, 5)
      rmse <- rep(0, 5)
      poly_models_cv <- function(train_data) {
        for(d in degree) {
        # Creates an expression -> Takes the variable from the enclosing environment -> Substitutes into the expression
        poly_reg <- bquote(n ~ poly(price, .(d)))
        poly_models <- train(as.formula(poly_reg), 
                             data = train_data, 
                             method = 'lm', 
                             trControl = fit_control)
        r_sq[d] <- poly_models$results$Rsquared
        rmse[d] <- poly_models$results$RMSE
        } 
        return(data.frame(degree, r_sq, rmse))
      }
      # Model functions
      lr_poly1_model_mod <- function(train_data) {
        lm(n ~ price, data = train_data)
      }
      lr_poly2_model_mod <- function(train_data) {
        lm(n ~ poly(price, 2), data = train_data) 
      }
      lr_poly3_model_mod <- function(train_data) {
        lm(n ~ poly(price, 3), data = train_data) 
      }
      lr_poly4_model_mod <- function(train_data) {
        lm(n ~ poly(price, 4), data = train_data) 
      }
      lr_poly5_model_mod <- function(train_data) {
        lm(n ~ poly(price, 5), data = train_data) 
      }
    # Observation-level object 
    library(moderndive)
    lr_model_obs <- function(model_object, test_data) {
      df <- model_object %>%
        get_regression_points(newdata = test_data) %>%
        mutate(marginal_cost = test_data$marginal_cost,
               profit = n * (price - marginal_cost), 
               profit_fitted = n_hat * (price - marginal_cost)) %>%
        mutate(rmse_demand = sqrt(mean(residual ^ 2)),
               rmse_profit = sqrt(mean((profit - profit_fitted) ^ 2))) 
      p_max_profit <- df %>%
        subset(profit_fitted == max(profit_fitted), price) %>% 
        as.numeric() 
      df %>%  
        mutate(optimal_price = p_max_profit)
    }
    # Price-response plot
    plot_price_response <- function(lr_model_obs, formula = formula) {
      ggplot(lr_model_obs, aes(x = price, y = n)) +
        geom_point() + 
        geom_smooth(method = 'lm', formula = formula, se = FALSE) +
        labs(x = "Price", y = "Demand (Response)") +
        theme_bw()
    }
    # Price-profit plot
    plot_price_profit <- function(lr_model_obs) {
      ggplot(lr_model_obs, aes(x = price, y = profit)) + 
        geom_point() +
        geom_line(aes(x = price, y = profit_fitted), color = 'blue') +
        geom_vline(xintercept = as.numeric(subset(lr_model_obs, profit_fitted == max(profit_fitted), price)),
                   color = 'red', 
                   lty = 'dashed') + 
        geom_hline(yintercept = 0, lty = 'dotted') +
        labs(x = "Price", y = "Profit") +
        theme_bw()
    }
    # Combine the plots
    library(gridExtra)
    plots_combined <- function(plot1, plot2) {
      grid.arrange(plot1, plot2, nrow = 2, ncol = 1)
    }
    # Total profit
    total_profit <- function(cluster1, cluster2 = 0, cluster3 = 0, cluster4 = 0, cluster5 = 0) {
      seq <- c( max(cluster1), max(cluster2), max(cluster3), max(cluster4), max(cluster5) )
      return(sum(seq))
    }
    # Results  
      # Health and beauty
        # Member
        # Optimal degree = 1
        set.seed(700)
        poly_models_cv(hb_m_train) %>% round(2)                  
        lr_model_obs(lr_poly1_model_mod(hb_m_train), hb_m_test)  
        # Plot
        hb_m_model <- lr_model_obs(lr_poly1_model_mod(hb_m_train), hb_m_test)
        plots_combined(plot_price_response(hb_m_model, formula = y ~ x), 
                       plot_price_profit(hb_m_model))
        # Normal
        # Optimal degree = 1
        set.seed(701)
        poly_models_cv(hb_n_train) %>% round(2)                 
        lr_model_obs(lr_poly1_model_mod(hb_n_train), hb_n_test)  
        # Plot
        hb_n_model <- lr_model_obs(lr_poly1_model_mod(hb_n_train), hb_n_test)
        plots_combined(plot_price_response(hb_n_model, formula = y ~ x), 
                       plot_price_profit(hb_n_model))
        # Total profit
        total_profit(hb_m_model$profit_fitted, 
                     hb_n_model$profit_fitted)
      # Electronic accessories
        # Member
        # Optimal degree = 1
        set.seed(800)
        poly_models_cv(ea_m_train) %>% round(2)                  
        lr_model_obs(lr_poly1_model_mod(ea_m_train), ea_m_test)  
        # Plot
        ea_m_model <- lr_model_obs(lr_poly1_model_mod(ea_m_train), ea_m_test)
        plots_combined(plot_price_response(ea_m_model, formula = y ~ x), 
                       plot_price_profit(ea_m_model))
        # Normal
        # Optimal degree = 1
        set.seed(801)
        poly_models_cv(ea_n_train) %>% round(2)                  
        lr_model_obs(lr_poly1_model_mod(ea_n_train), ea_n_test)  
        # Plot
        ea_n_model <- lr_model_obs(lr_poly1_model_mod(ea_n_train), ea_n_test)
        plots_combined(plot_price_response(ea_n_model, formula = y ~ x), 
                       plot_price_profit(ea_n_model))
        # Total profit
        total_profit(ea_m_model$profit_fitted, 
                     ea_n_model$profit_fitted)
      # Home and lifestyle
        # Member
        # Optimal degree = 1
        set.seed(900)
        poly_models_cv(hl_m_train) %>% round(2)                 
        lr_model_obs(lr_poly1_model_mod(hl_m_train), hl_m_test)  
        # Plot
        hl_m_model <- lr_model_obs(lr_poly1_model_mod(hl_m_train), hl_m_test)
        plots_combined(plot_price_response(hl_m_model, formula = y ~ x), 
                       plot_price_profit(hl_m_model))
        # Normal
        # Optimal degree = 2
        set.seed(901)
        poly_models_cv(hl_n_train) %>% round(2)               
        lr_model_obs(lr_poly2_model_mod(hl_n_train), hl_n_test)  
        # Plot
        hl_n_model <- lr_model_obs(lr_poly2_model_mod(hl_n_train), hl_n_test)
        plots_combined(plot_price_response(hl_n_model, formula = y ~ poly(x, 2)), 
                       plot_price_profit(hl_n_model))
        # Total profit
        total_profit(hl_m_model$profit_fitted, 
                     hl_n_model$profit_fitted)
      # Sports and travel
        # Member
        # Optimal degree = 4
        set.seed(1000)
        poly_models_cv(st_m_train) %>% round(2)                  
        lr_model_obs(lr_poly4_model_mod(st_m_train), st_m_test)  
        # Plot
        st_m_model <- lr_model_obs(lr_poly4_model_mod(st_m_train), st_m_test)
        plots_combined(plot_price_response(st_m_model, formula = y ~ poly(x, 4)), 
                       plot_price_profit(st_m_model))
        # Normal
        # Optimal degree = 1
        set.seed(1001)
        poly_models_cv(st_n_train) %>% round(2)                  
        lr_model_obs(lr_poly1_model_mod(st_n_train), st_n_test)  
        # Plot
        st_n_model <- lr_model_obs(lr_poly1_model_mod(st_n_train), st_n_test)
        plots_combined(plot_price_response(st_n_model, formula = y ~ x), 
                       plot_price_profit(st_n_model))
        # Total profit
        total_profit(st_m_model$profit_fitted, 
                     st_n_model$profit_fitted)
      # Food and beverages
        # Member
        # Optimal degree = 1
        set.seed(1100)
        poly_models_cv(fb_m_train) %>% round(2)                 
        lr_model_obs(lr_poly1_model_mod(fb_m_train), fb_m_test)  
        # Plot
        fb_m_model <- lr_model_obs(lr_poly1_model_mod(fb_m_train), fb_m_test) 
        plots_combined(plot_price_response(fb_m_model, formula = y ~ x), 
                       plot_price_profit(fb_m_model))
        # Normal
        # Optimal degree = 1
        set.seed(1101)
        poly_models_cv(fb_n_train) %>% round(2)                  
        lr_model_obs(lr_poly1_model_mod(fb_n_train), fb_n_test)  
        # Plot
        fb_n_model <- lr_model_obs(lr_poly1_model_mod(fb_n_train), fb_n_test) 
        plots_combined(plot_price_response(fb_n_model, formula = y ~ x), 
                       plot_price_profit(fb_n_model))
        # Total profit
        total_profit(fb_m_model$profit_fitted, 
                     fb_n_model$profit_fitted)
      # Fashion accessories
        # Member
        # Optimal degree = 1
        set.seed(1200)
        poly_models_cv(fa_m_train) %>% round(2)                  
        lr_model_obs(lr_poly1_model_mod(fa_m_train), fa_m_test)  
        # Plot
        fa_m_model <- lr_model_obs(lr_poly1_model_mod(fa_m_train), fa_m_test)
        plots_combined(plot_price_response(fa_m_model, formula = y ~ x), 
                       plot_price_profit(fa_m_model))
        # Normal 
        # Optimal degree = 2
        set.seed(1201)
        poly_models_cv(fa_n_train) %>% round(2)                 
        lr_model_obs(lr_poly2_model_mod(fa_n_train), fa_n_test) 
        # Plot
        fa_n_model <- lr_model_obs(lr_poly2_model_mod(fa_n_train), fa_n_test)
        plots_combined(plot_price_response(fa_n_model, formula = y ~ poly(x, 2)), 
                       plot_price_profit(fa_n_model))
        # Total profit
        total_profit(fa_m_model$profit_fitted, 
                     fa_n_model$profit_fitted)