# Load packages
library(readr)
library(dplyr)
library(lubridate)
library(forcats)
library(ggplot2)
library(janitor)

source("Data_Clustering.R")

# Prepare data
  # Create a dataframe: Price, Quantity, Marginal cost
  p_q_mc_obs <- function(prod_clus_data, prod_data, i) {
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
                 cluster = rep(i, nrow(p_q))))
  }
  # Results
    # Health and beauty
    hb_cluster_1 <- hb_kmeans_clustered %>% filter(cluster == 1) 
    hb_cluster_1_p_q_mc <- p_q_mc_obs(hb_cluster_1, hb, 1)          # 31
    
    hb_cluster_2 <- hb_kmeans_clustered %>% filter(cluster == 2)
    hb_cluster_2_p_q_mc <- p_q_mc_obs(hb_cluster_2, hb, 2)          # 89
    
    hb_cluster_3 <- hb_kmeans_clustered %>% filter(cluster == 3)
    hb_cluster_3_p_q_mc <- p_q_mc_obs(hb_cluster_3, hb, 3)          # 27
    
    # Electronic accessories
    ea_cluster_1 <- ea_kmeans_clustered %>% filter(cluster == 1) 
    ea_cluster_1_p_q_mc <- p_q_mc_obs(ea_cluster_1, ea, 1)          # 35
    
    ea_cluster_2 <- ea_kmeans_clustered %>% filter(cluster == 2)
    ea_cluster_2_p_q_mc <- p_q_mc_obs(ea_cluster_2, ea, 2)          # 124
    
    # Home and lifestyle
    hl_cluster_1 <- hl_kmeans_clustered %>% filter(cluster == 1) 
    hl_cluster_1_p_q_mc <- p_q_mc_obs(hl_cluster_1, hl, 1)          # 47
    
    hl_cluster_2 <- hl_kmeans_clustered %>% filter(cluster == 2)
    hl_cluster_2_p_q_mc <- p_q_mc_obs(hl_cluster_2, hl, 2)          # 34
    
    hl_cluster_3 <- hl_kmeans_clustered %>% filter(cluster == 3)
    hl_cluster_3_p_q_mc <- p_q_mc_obs(hl_cluster_3, hl, 3)          # 38
    
    hl_cluster_4 <- hl_kmeans_clustered %>% filter(cluster == 4)
    hl_cluster_4_p_q_mc <- p_q_mc_obs(hl_cluster_4, hl, 4)          # 40
    
    # Sports and travel
    st_cluster_1 <- st_kmeans_clustered %>% filter(cluster == 1) 
    st_cluster_1_p_q_mc <- p_q_mc_obs(st_cluster_1, st, 1)          # 39
    
    st_cluster_2 <- st_kmeans_clustered %>% filter(cluster == 2)
    st_cluster_2_p_q_mc <- p_q_mc_obs(st_cluster_2, st, 2)          # 30
    
    st_cluster_3 <- st_kmeans_clustered %>% filter(cluster == 3)
    st_cluster_3_p_q_mc <- p_q_mc_obs(st_cluster_3, st, 3)          # 89
    
    # Food and beverages
    fb_cluster_1 <- fb_kmeans_clustered %>% filter(cluster == 1) 
    fb_cluster_1_p_q_mc <- p_q_mc_obs(fb_cluster_1, fb, 1)          # 96
    
    fb_cluster_2 <- fb_kmeans_clustered %>% filter(cluster == 2)
    fb_cluster_2_p_q_mc <- p_q_mc_obs(fb_cluster_2, fb, 2)          # 43
    
    fb_cluster_3 <- fb_kmeans_clustered %>% filter(cluster == 3)
    fb_cluster_3_p_q_mc <- p_q_mc_obs(fb_cluster_3, fb, 3)          # 31
    
    # Fashion accessories
    fa_cluster_1 <- fa_kmeans_clustered %>% filter(cluster == 1) 
    fa_cluster_1_p_q_mc <- p_q_mc_obs(fa_cluster_1, fa, 1)          # 27
    
    fa_cluster_2 <- fa_kmeans_clustered %>% filter(cluster == 2)
    fa_cluster_2_p_q_mc <- p_q_mc_obs(fa_cluster_2, fa, 2)          # 95
    
    fa_cluster_3 <- fa_kmeans_clustered %>% filter(cluster == 3)
    fa_cluster_3_p_q_mc <- p_q_mc_obs(fa_cluster_3, fa, 3)          # 49
    
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
    set.seed(40)
    hb_cluster_1_train <- data_train(data_shuffled(hb_cluster_1_p_q_mc)) 
    hb_cluster_1_test <- data_test(data_shuffled(hb_cluster_1_p_q_mc))
    
    set.seed(41)
    hb_cluster_2_train <- data_train(data_shuffled(hb_cluster_2_p_q_mc)) 
    hb_cluster_2_test <- data_test(data_shuffled(hb_cluster_2_p_q_mc))
    
    set.seed(42)
    hb_cluster_3_train <- data_train(data_shuffled(hb_cluster_3_p_q_mc)) 
    hb_cluster_3_test <- data_test(data_shuffled(hb_cluster_3_p_q_mc))
    
    # Electronic accessories
    set.seed(50)
    ea_cluster_1_train <- data_train(data_shuffled(ea_cluster_1_p_q_mc)) 
    ea_cluster_1_test <- data_test(data_shuffled(ea_cluster_1_p_q_mc))
    
    set.seed(51)
    ea_cluster_2_train <- data_train(data_shuffled(ea_cluster_2_p_q_mc)) 
    ea_cluster_2_test <- data_test(data_shuffled(ea_cluster_2_p_q_mc))

    # Home and lifestyle
    set.seed(60)
    hl_cluster_1_train <- data_train(data_shuffled(hl_cluster_1_p_q_mc)) 
    hl_cluster_1_test <- data_test(data_shuffled(hl_cluster_1_p_q_mc))
    
    set.seed(61)
    hl_cluster_2_train <- data_train(data_shuffled(hl_cluster_2_p_q_mc)) 
    hl_cluster_2_test <- data_test(data_shuffled(hl_cluster_2_p_q_mc))
    
    set.seed(62)
    hl_cluster_3_train <- data_train(data_shuffled(hl_cluster_3_p_q_mc)) 
    hl_cluster_3_test <- data_test(data_shuffled(hl_cluster_3_p_q_mc))
    
    set.seed(63)
    hl_cluster_4_train <- data_train(data_shuffled(hl_cluster_4_p_q_mc)) 
    hl_cluster_4_test <- data_test(data_shuffled(hl_cluster_4_p_q_mc))
    
    # Sports and travel
    set.seed(70)
    st_cluster_1_train <- data_train(data_shuffled(st_cluster_1_p_q_mc)) 
    st_cluster_1_test <- data_test(data_shuffled(st_cluster_1_p_q_mc))
    
    set.seed(71)
    st_cluster_2_train <- data_train(data_shuffled(st_cluster_2_p_q_mc)) 
    st_cluster_2_test <- data_test(data_shuffled(st_cluster_2_p_q_mc))
    
    set.seed(72)
    st_cluster_3_train <- data_train(data_shuffled(st_cluster_3_p_q_mc)) 
    st_cluster_3_test <- data_test(data_shuffled(st_cluster_3_p_q_mc))
  
    # Food and beverages 
    set.seed(80)
    fb_cluster_1_train <- data_train(data_shuffled(fb_cluster_1_p_q_mc)) 
    fb_cluster_1_test <- data_test(data_shuffled(fb_cluster_1_p_q_mc))
    
    set.seed(81)
    fb_cluster_2_train <- data_train(data_shuffled(fb_cluster_2_p_q_mc)) 
    fb_cluster_2_test <- data_test(data_shuffled(fb_cluster_2_p_q_mc))
    
    set.seed(82)
    fb_cluster_3_train <- data_train(data_shuffled(fb_cluster_3_p_q_mc)) 
    fb_cluster_3_test <- data_test(data_shuffled(fb_cluster_3_p_q_mc))
    
    # Fashion accessories
    set.seed(90)
    fa_cluster_1_train <- data_train(data_shuffled(fa_cluster_1_p_q_mc)) 
    fa_cluster_1_test <- data_test(data_shuffled(fa_cluster_1_p_q_mc))
    
    set.seed(91)
    fa_cluster_2_train <- data_train(data_shuffled(fa_cluster_2_p_q_mc)) 
    fa_cluster_2_test <- data_test(data_shuffled(fa_cluster_2_p_q_mc))
    
    set.seed(92)
    fa_cluster_3_train <- data_train(data_shuffled(fa_cluster_3_p_q_mc)) 
    fa_cluster_3_test <- data_test(data_shuffled(fa_cluster_3_p_q_mc))

# Fit model
  # Visualize the price-response relationships
    # Plot function
    prod_clusters_plot <- function(...) {
      rbind(...) %>%
        ggplot(aes(x = price, y = n, color = factor(cluster))) +
          geom_point() +
          labs(x = "Price", y = "Demand (Response)") +
          geom_smooth(method = 'lm', se = FALSE) + 
          geom_vline(aes(xintercept = marginal_cost), lty = 'dashed')
    }
    # Results
      # Health and beauty
      prod_clusters_plot(hb_cluster_1_p_q_mc, 
                         hb_cluster_2_p_q_mc, 
                         hb_cluster_3_p_q_mc)
      # Electronic accessories
      prod_clusters_plot(ea_cluster_1_p_q_mc, 
                         ea_cluster_2_p_q_mc)
      # Home and lifestyle
      prod_clusters_plot(hl_cluster_1_p_q_mc, 
                         hl_cluster_2_p_q_mc, 
                         hl_cluster_3_p_q_mc,
                         hl_cluster_4_p_q_mc)
      # Sports and travel
      prod_clusters_plot(st_cluster_1_p_q_mc, 
                         st_cluster_2_p_q_mc, 
                         st_cluster_3_p_q_mc)
      # Food and beverages
      prod_clusters_plot(fb_cluster_1_p_q_mc, 
                         fb_cluster_2_p_q_mc, 
                         fb_cluster_3_p_q_mc)
      # Fashion accessories
      prod_clusters_plot(fa_cluster_1_p_q_mc, 
                         fa_cluster_2_p_q_mc, 
                         fa_cluster_3_p_q_mc)
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
        # Cluster 1
        # Optimal degree = 1
        set.seed(2000)
        poly_models_cv(hb_cluster_1_train) %>% round(2)                          
        lr_model_obs(lr_poly1_model_mod(hb_cluster_1_train), hb_cluster_1_test)  
        # Plot
        hb_cluster_1_model <- lr_model_obs(lr_poly1_model_mod(hb_cluster_1_train), hb_cluster_1_test)
        plots_combined(plot_price_response(hb_cluster_1_model, formula = y ~ x), 
                       plot_price_profit(hb_cluster_1_model))
        # Cluster 2
        # Optimal degree = 1
        set.seed(2001)
        poly_models_cv(hb_cluster_2_train) %>% round(2)                          
        lr_model_obs(lr_poly1_model_mod(hb_cluster_2_train), hb_cluster_2_test)  
        # Plot
        hb_cluster_2_model <- lr_model_obs(lr_poly1_model_mod(hb_cluster_2_train), hb_cluster_2_test)
        plots_combined(plot_price_response(hb_cluster_2_model, formula = y ~ x), 
                       plot_price_profit(hb_cluster_2_model))
        # Cluster 3
        # Optimal degree = 1
        set.seed(2002)
        poly_models_cv(hb_cluster_3_train) %>% round(2)                          
        lr_model_obs(lr_poly1_model_mod(hb_cluster_3_train), hb_cluster_3_test)  
        # Plot
        hb_cluster_3_model <- lr_model_obs(lr_poly1_model_mod(hb_cluster_3_train), hb_cluster_3_test)
        plots_combined(plot_price_response(hb_cluster_3_model, formula = y ~ x), 
                       plot_price_profit(hb_cluster_3_model))
        # Total profit
        total_profit(hb_cluster_1_model$profit_fitted, 
                     hb_cluster_2_model$profit_fitted, 
                     hb_cluster_3_model$profit_fitted)
      # Electronic accessories
        # Cluster 1
        # Optimal degree = 4
        set.seed(3000)
        poly_models_cv(ea_cluster_1_train) %>% round(2)                          
        lr_model_obs(lr_poly4_model_mod(ea_cluster_1_train), ea_cluster_1_test)  
        # Plot
        ea_cluster_1_model <- lr_model_obs(lr_poly4_model_mod(ea_cluster_1_train), ea_cluster_1_test)
        plots_combined(plot_price_response(ea_cluster_1_model, formula = y ~ poly(x, 4)), 
                       plot_price_profit(ea_cluster_1_model))
        # Cluster 2
        # Optimal degree = 1
        set.seed(3001)
        poly_models_cv(ea_cluster_2_train)  %>% round(2)                         
        lr_model_obs(lr_poly1_model_mod(ea_cluster_2_train), ea_cluster_2_test)  
        # Plot
        ea_cluster_2_model <- lr_model_obs(lr_poly1_model_mod(ea_cluster_2_train), ea_cluster_2_test)
        plots_combined(plot_price_response(ea_cluster_2_model, formula = y ~ x), 
                       plot_price_profit(ea_cluster_2_model))
        # Total profit
        total_profit(ea_cluster_1_model$profit_fitted, 
                     ea_cluster_2_model$profit_fitted)
      # Home and lifestyle
        # Cluster 1
        # Optimal degree = 2
        set.seed(4000)
        poly_models_cv(hl_cluster_1_train) %>% round(2)                          
        lr_model_obs(lr_poly2_model_mod(hl_cluster_1_train), hl_cluster_1_test)  
        # Plot
        hl_cluster_1_model <- lr_model_obs(lr_poly2_model_mod(hl_cluster_1_train), hl_cluster_1_test)
        plots_combined(plot_price_response(hl_cluster_1_model, formula = y ~ poly(x, 2)), 
                       plot_price_profit(hl_cluster_1_model))
        # Cluster 2
        # Optimal degree = 1
        set.seed(4001)
        poly_models_cv(hl_cluster_2_train) %>% round(2)                          
        lr_model_obs(lr_poly1_model_mod(hl_cluster_2_train), hl_cluster_2_test)  
        # Plot
        hl_cluster_2_model <- lr_model_obs(lr_poly1_model_mod(hl_cluster_2_train), hl_cluster_2_test)
        plots_combined(plot_price_response(hl_cluster_2_model, formula = y ~ x), 
                       plot_price_profit(hl_cluster_2_model))
        # Cluster 3
        # Optimal degree = 1
        set.seed(4002)
        poly_models_cv(hl_cluster_3_train) %>% round(2)                          
        lr_model_obs(lr_poly3_model_mod(hl_cluster_3_train), hl_cluster_3_test)  
        # Plot
        hl_cluster_3_model <- lr_model_obs(lr_poly3_model_mod(hl_cluster_3_train), hl_cluster_3_test) 
        plots_combined(plot_price_response(hl_cluster_3_model, formula = y ~ poly(x, 3)), 
                       plot_price_profit(hl_cluster_3_model))
        # Cluster 4
        # Optimal degree = 3
        set.seed(4003)
        poly_models_cv(hl_cluster_4_train) %>% round(2)                          
        lr_model_obs(lr_poly3_model_mod(hl_cluster_4_train), hl_cluster_4_test)  
        # Plot
        hl_cluster_4_model <- lr_model_obs(lr_poly3_model_mod(hl_cluster_4_train), hl_cluster_4_test) 
        plots_combined(plot_price_response(hl_cluster_4_model, formula = y ~ poly(x, 3)), 
                       plot_price_profit(hl_cluster_4_model))
        # Total profit
        total_profit(hl_cluster_1_model$profit_fitted, 
                     hl_cluster_2_model$profit_fitted, 
                     hl_cluster_3_model$profit_fitted,
                     hl_cluster_4_model$profit_fitted)
      # Sports and travel
        # Cluster 1
        # Optimal degree = 4
        set.seed(5000)
        poly_models_cv(st_cluster_1_train) %>% round(2)                          
        lr_model_obs(lr_poly4_model_mod(st_cluster_1_train), st_cluster_1_test)  
        # Plot
        st_cluster_1_model <- lr_model_obs(lr_poly4_model_mod(st_cluster_1_train), st_cluster_1_test)
        plots_combined(plot_price_response(st_cluster_1_model, formula = y ~ poly(x, 4)), 
                       plot_price_profit(st_cluster_1_model))
        # Cluster 2
        # Optimal degree = 1
        set.seed(5001)
        poly_models_cv(st_cluster_2_train) %>% round(2)                          
        lr_model_obs(lr_poly1_model_mod(st_cluster_2_train), st_cluster_2_test)  
        # Plot
        st_cluster_2_model <- lr_model_obs(lr_poly1_model_mod(st_cluster_2_train), st_cluster_2_test)
        plots_combined(plot_price_response(st_cluster_2_model, formula = y ~ x), 
                       plot_price_profit(st_cluster_2_model))
        # Cluster 3
        # Optimal degree = 1
        set.seed(5002)
        poly_models_cv(st_cluster_3_train) %>% round(2)                          
        lr_model_obs(lr_poly1_model_mod(st_cluster_3_train), st_cluster_3_test)  
        # Plot
        st_cluster_3_model <- lr_model_obs(lr_poly1_model_mod(st_cluster_3_train), st_cluster_3_test)
        plots_combined(plot_price_response(st_cluster_3_model, formula = y ~ x), 
                       plot_price_profit(st_cluster_3_model))
        # Total profit
        total_profit(st_cluster_1_model$profit_fitted, 
                     st_cluster_2_model$profit_fitted, 
                     st_cluster_3_model$profit_fitted)
      # Food and beverages
        # Cluster 1
        # Optimal degree = 2
        set.seed(6000)
        poly_models_cv(fb_cluster_1_train) %>% round(2)                          
        lr_model_obs(lr_poly2_model_mod(fb_cluster_1_train), fb_cluster_1_test)  
        # Plot
        fb_cluster_1_model <- lr_model_obs(lr_poly2_model_mod(fb_cluster_1_train), fb_cluster_1_test)
        plots_combined(plot_price_response(fb_cluster_1_model, formula = y ~ poly(x, 2)), 
                       plot_price_profit(fb_cluster_1_model))
        # Cluster 2
        # Optimal degree = 1
        set.seed(6001)
        poly_models_cv(fb_cluster_2_train) %>% round(2)                          
        lr_model_obs(lr_poly1_model_mod(fb_cluster_2_train), fb_cluster_2_test)  
        # Plot
        fb_cluster_2_model <- lr_model_obs(lr_poly1_model_mod(fb_cluster_2_train), fb_cluster_2_test)
        plots_combined(plot_price_response(fb_cluster_2_model, formula = y ~ x), 
                       plot_price_profit(fb_cluster_2_model))
        # Cluster 3
        # Optimal degree = 1
        set.seed(6002)
        poly_models_cv(fb_cluster_3_train) %>% round(2)                          
        lr_model_obs(lr_poly1_model_mod(fb_cluster_3_train), fb_cluster_3_test)  
        # Plot
        fb_cluster_3_model <- lr_model_obs(lr_poly1_model_mod(fb_cluster_3_train), fb_cluster_3_test)
        plots_combined(plot_price_response(fb_cluster_3_model, formula = y ~ x), 
                       plot_price_profit(fb_cluster_3_model))
        # Total profit
        total_profit(fb_cluster_1_model$profit_fitted, 
                     fb_cluster_2_model$profit_fitted, 
                     fb_cluster_3_model$profit_fitted)
      # Fashion accessories
        # Cluster 1
        # Optimal degree = 1
        set.seed(7000)
        poly_models_cv(fa_cluster_1_train) %>% round(2)                          
        lr_model_obs(lr_poly1_model_mod(fa_cluster_1_train), fa_cluster_1_test)  
        # Plot
        fa_cluster_1_model <- lr_model_obs(lr_poly1_model_mod(fa_cluster_1_train), fa_cluster_1_test)
        plots_combined(plot_price_response(fa_cluster_1_model, formula = y ~ x), 
                       plot_price_profit(fa_cluster_1_model))
        # Cluster 2
        # Optimal degree = 1
        set.seed(7001)
        poly_models_cv(fa_cluster_2_train) %>% round(2)                          
        lr_model_obs(lr_poly1_model_mod(fa_cluster_2_train), fa_cluster_2_test)  
        # Plot
        fa_cluster_2_model <- lr_model_obs(lr_poly1_model_mod(fa_cluster_2_train), fa_cluster_2_test)
        plots_combined(plot_price_response(fa_cluster_2_model, formula = y ~ x), 
                       plot_price_profit(fa_cluster_2_model))
        # Cluster 3
        # Optimal degree = 3
        set.seed(7002) 
        poly_models_cv(fa_cluster_3_train) %>% round(2)                          
        lr_model_obs(lr_poly3_model_mod(fa_cluster_3_train), fa_cluster_3_test) 
        # Plot
        fa_cluster_3_model <- lr_model_obs(lr_poly3_model_mod(fa_cluster_3_train), fa_cluster_3_test)
        plots_combined(plot_price_response(fa_cluster_3_model, formula = y ~ poly(x, 3)), 
                       plot_price_profit(fa_cluster_3_model))
        # Total profit
        total_profit(fa_cluster_1_model$profit_fitted, 
                     fa_cluster_2_model$profit_fitted, 
                     fa_cluster_3_model$profit_fitted)

