# ==============================================================================
# CityRide Data Analysis & Visualization Project (Functional R Implementation)
# ==============================================================================

# 1. Setup & Libraries
# ------------------------------------------------------------------------------
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("lubridate")) install.packages("lubridate")
if (!require("gridExtra")) install.packages("gridExtra")
if (!require("skimr")) install.packages("skimr")
if (!require("scales")) install.packages("scales")
if (!require("reshape2")) install.packages("reshape2") 

library(tidyverse)
library(lubridate)
library(gridExtra)
library(skimr)
library(scales)
library(reshape2)

# Set display options
options(width = 120)
theme_set(theme_minimal())

# ==============================================================================
# 2. Data Profiling Functions
# ==============================================================================

generate_profile_summary <- function(df, dataset_name) {
  cat(paste0("\n", strrep("=", 80), "\n"))
  cat(paste0("DATA PROFILE: ", dataset_name, "\n"))
  cat(paste0(strrep("=", 80), "\n\n"))
  
  # Basic Information
  cat(sprintf("Dataset Shape: %s rows x %s columns\n", 
              format(nrow(df), big.mark=","), ncol(df)))
  cat(sprintf("Memory Usage: %.2f MB\n", object.size(df) / 1024^2))
  cat(paste0("\n", strrep("-", 80), "\n\n"))
  
  # Summary
  print(skim(df))
}

identify_data_quality_issues <- function(df, dataset_name) {
  cat(paste0("\n", strrep("=", 80), "\n"))
  cat(paste0("DATA QUALITY ASSESSMENT: ", dataset_name, "\n"))
  cat(paste0(strrep("=", 80), "\n\n"))
  
  issues <- list()
  
  # Missing values
  missing_counts <- colSums(is.na(df))
  missing_cols <- names(missing_counts[missing_counts > 0])
  
  if (length(missing_cols) > 0) {
    cat("⚠ MISSING VALUES DETECTED:\n")
    for (col in missing_cols) {
      pct <- (missing_counts[col] / nrow(df)) * 100
      cat(sprintf("  • %s: %s (%.2f%%)\n", 
                  col, format(missing_counts[col], big.mark=","), pct))
    }
    cat("\n")
  } else {
    cat("✓ No missing values detected\n\n")
  }
  
  # Duplicates
  dupe_count <- sum(duplicated(df))
  if (dupe_count > 0) {
    cat(sprintf("⚠ DUPLICATE ROWS: %s (%.2f%%)\n\n", 
                format(dupe_count, big.mark=","), (dupe_count/nrow(df)*100)))
  } else {
    cat("✓ No duplicate rows detected\n\n")
  }
  
  # Outliers (IQR Method)
  numeric_cols <- df %>% select(where(is.numeric)) %>% names()
  if (length(numeric_cols) > 0) {
    cat("OUTLIER DETECTION (IQR Method):\n")
    for (col in numeric_cols) {
      stats <- boxplot.stats(df[[col]])$stats
      Q1 <- stats[2]; Q3 <- stats[4]
      IQR <- Q3 - Q1
      lower_bound <- Q1 - 1.5 * IQR
      upper_bound <- Q3 + 1.5 * IQR
      outliers <- df[[col]][df[[col]] < lower_bound | df[[col]] > upper_bound]
      
      if (length(outliers) > 0) {
        pct <- (length(outliers) / nrow(df)) * 100
        cat(sprintf("  • %s: %s outliers (%.2f%%)\n", 
                    col, format(length(outliers), big.mark=","), pct))
      }
    }
    cat("\n")
  }
}

analyze_relationships <- function(rides_df, drivers_df) {
  cat(paste0("\n", strrep("=", 80), "\n"))
  cat("DATASET RELATIONSHIP ANALYSIS\n")
  cat(paste0(strrep("=", 80), "\n\n"))
  
  rides_drivers <- unique(rides_df$Driver_ID)
  all_drivers <- unique(drivers_df$Driver_ID)
  orphaned <- setdiff(rides_drivers, all_drivers)
  
  cat(sprintf("Total Drivers in Drivers table: %d\n", length(all_drivers)))
  cat(sprintf("Unique Drivers in Rides table:  %d\n", length(rides_drivers)))
  cat(sprintf("Driver_ID overlap:              %.2f%%\n", 
              (length(intersect(rides_drivers, all_drivers)) / length(all_drivers)) * 100))
  
  if (length(orphaned) > 0) {
    cat(sprintf("\n⚠ WARNING: %d Driver IDs in Rides table not found in Drivers table\n", length(orphaned)))
  } else {
    cat("\n✓ All rides have corresponding driver records\n")
  }
}

# ==============================================================================
# 3. Data Transformation Functions
# ==============================================================================

transform_rides_data <- function(df) {
  cat("\n🚧 Transforming Rides Data...\n")
  
  # 1. Promo Features
  # FIX: Handle NA and empty strings as NO_PROMO to ensure comparison works
  df$Promo_Code[is.na(df$Promo_Code) | df$Promo_Code == ""] <- 'NO_PROMO'
  
  df$Has_Promo <- as.integer(df$Promo_Code != 'NO_PROMO')
  df$Promo_Discount <- as.numeric(str_extract(df$Promo_Code, "\\d+")) / 100
  df$Promo_Discount[is.na(df$Promo_Discount)] <- 0
  
  # 2. Derived Metrics
  df$Fare_per_km <- df$Fare / df$Distance_km
  df$Speed_kmh <- (df$Distance_km / df$Duration_min) * 60
  
  # 3. Temporal Features
  df$Date <- mdy(df$Date)
  df$Day_of_Week <- wday(df$Date, label = TRUE, abbr = FALSE)
  df$Week_Number <- isoweek(df$Date)
  df$Day_of_Month <- day(df$Date)
  df$Is_Weekend <- as.integer(wday(df$Date) %in% c(1, 7))
  
  cat("✓ Features added: Promo, Derived Metrics, Temporal\n")
  return(df)
}

merge_datasets <- function(rides_df, drivers_df) {
  cat("🚧 Merging Datasets...\n")
  merged_df <- left_join(rides_df, drivers_df, by = "Driver_ID", suffix = c("_ride", "_driver"))
  cat(sprintf("✓ Merged datasets -> %s records\n", format(nrow(merged_df), big.mark=",")))
  return(merged_df)
}

# ==============================================================================
# 4. EDA Visualization Functions
# ==============================================================================

plot_temporal_analysis <- function(rides_df) {
  cat("\nGenerating Temporal Analysis...\n")
  
  # 1. Daily Ride Volume
  daily_rides <- rides_df %>% count(Date)
  p1 <- ggplot(daily_rides, aes(x = Date, y = n)) +
    geom_line(color = "steelblue", linewidth = 1) +
    geom_point(color = "steelblue") +
    geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +
    labs(title = "Daily Ride Volume", x = "Date", y = "Count") +
    theme_minimal()
  
  # 2. Day of Week
  dow_data <- rides_df %>% count(Day_of_Week)
  p2 <- ggplot(dow_data, aes(x = Day_of_Week, y = n, fill = Day_of_Week)) +
    geom_col(color = "black") +
    geom_text(aes(label = n), vjust = -0.5) +
    labs(title = "Rides by Day of Week", y = "Count") +
    theme_minimal() + theme(legend.position = "none", axis.text.x = element_text(angle=45, hjust=1))
  
  # 3. Weekend Comparison (Normalized)
  weekend_data <- rides_df %>% 
    group_by(Is_Weekend) %>% 
    summarise(
      Count = n(),
      Avg_Fare = mean(Fare),
      Avg_Dist = mean(Distance_km),
      Avg_Rating = mean(Rating)
    ) %>%
    mutate(Type = ifelse(Is_Weekend == 1, "Weekend", "Weekday")) %>%
    pivot_longer(cols = c(Count, Avg_Fare, Avg_Dist, Avg_Rating), names_to = "Metric", values_to = "Value") %>%
    group_by(Metric) %>%
    mutate(Norm_Value = Value / max(Value) * 100)
  
  p3 <- ggplot(weekend_data, aes(x = Metric, y = Norm_Value, fill = Type)) +
    geom_col(position = "dodge", color = "black") +
    labs(title = "Weekday vs Weekend (Normalized)", y = "Score (%)") +
    theme_minimal()
  
  # 4. Daily Revenue
  daily_rev <- rides_df %>% group_by(Date) %>% summarise(Revenue = sum(Fare))
  avg_rev <- mean(daily_rev$Revenue)
  p4 <- ggplot(daily_rev, aes(x = Date, y = Revenue)) +
    geom_area(fill = "green", alpha = 0.3) +
    geom_line(color = "darkgreen") +
    geom_point() +
    geom_hline(yintercept = avg_rev, color = "red", linetype = "dashed") +
    labs(title = "Daily Revenue Trend", y = "Revenue ($)") +
    theme_minimal()
  
  grid.arrange(p1, p2, p3, p4, ncol = 2)
}

plot_geographic_analysis <- function(rides_df) {
  cat("\nGenerating Geographic Analysis...\n")
  
  # 1. Rides by City
  city_rides <- rides_df %>% count(City) %>% arrange(n)
  p1 <- ggplot(city_rides, aes(x = reorder(City, n), y = n, fill = City)) +
    geom_col() + coord_flip() +
    geom_text(aes(label = n), hjust = -0.2) +
    labs(title = "Ride Volume by City", x = "City", y = "Count") +
    theme_minimal() + theme(legend.position = "none")
  
  # 2. Avg Fare by City
  city_fare <- rides_df %>% group_by(City) %>% summarise(Avg_Fare = mean(Fare))
  p2 <- ggplot(city_fare, aes(x = reorder(City, Avg_Fare), y = Avg_Fare, fill = City)) +
    geom_col() + coord_flip() +
    geom_text(aes(label = sprintf("$%.2f", Avg_Fare)), hjust = -0.2) +
    labs(title = "Avg Fare by City", x = "City", y = "Fare ($)") +
    theme_minimal() + theme(legend.position = "none")
  
  # 3. Revenue Share
  city_rev <- rides_df %>% group_by(City) %>% summarise(Revenue = sum(Fare))
  p3 <- ggplot(city_rev, aes(x = "", y = Revenue, fill = City)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y", start = 0) +
    labs(title = "Revenue Contribution by City") +
    theme_void()
  
  # 4. Heatmap
  city_metrics <- rides_df %>% 
    group_by(City) %>% 
    summarise(
      Fare = mean(Fare),
      Dist = mean(Distance_km),
      Dur = mean(Duration_min),
      Rating = mean(Rating),
      Fare_km = mean(Fare_per_km)
    ) %>%
    pivot_longer(-City, names_to = "Metric", values_to = "Value") %>%
    group_by(Metric) %>%
    mutate(Norm_Value = (Value - min(Value)) / (max(Value) - min(Value)))
  
  p4 <- ggplot(city_metrics, aes(x = City, y = Metric, fill = Norm_Value)) +
    geom_tile() +
    scale_fill_gradient(low = "white", high = "red") +
    geom_text(aes(label = round(Value, 2))) +
    labs(title = "City Performance Heatmap") +
    theme_minimal()
  
  grid.arrange(p1, p2, p3, p4, ncol = 2)
}

plot_driver_performance <- function(rides_df, drivers_df, merged_df) {
  cat("\nGenerating Driver Performance Analysis...\n")
  
  # 1. Experience vs Rating
  p1 <- ggplot(drivers_df, aes(x = Experience_Years, y = Average_Rating)) +
    geom_point(alpha = 0.6, color = "steelblue", size = 2) +
    geom_smooth(method = "lm", color = "red", se = FALSE) +
    labs(title = "Experience vs Rating", x = "Years Exp", y = "Avg Rating") +
    theme_minimal()
  
  # 2. Age Distribution
  p2 <- ggplot(drivers_df, aes(x = Age)) +
    geom_histogram(fill = "coral", color = "black", bins = 15) +
    geom_vline(aes(xintercept = mean(Age)), color = "red", linetype = "dashed") +
    labs(title = "Driver Age Distribution", x = "Age", y = "Count") +
    theme_minimal()
  
  # 3. Active vs Inactive Status
  status_counts <- drivers_df %>% count(Active_Status)
  p3 <- ggplot(status_counts, aes(x = Active_Status, y = n, fill = Active_Status)) +
    geom_col(color = "black", width = 0.6) +
    geom_text(aes(label = n), vjust = -0.5, fontface = "bold") +
    scale_fill_manual(values = c("Active" = "#2ecc71", "Inactive" = "#e74c3c")) +
    labs(title = "Driver Active Status", x = "Status", y = "Count") +
    theme_minimal() + theme(legend.position = "none")
  
  # 4. Rides per Driver Distribution
  rides_per_driver <- rides_df %>% count(Driver_ID)
  p4 <- ggplot(rides_per_driver, aes(x = n)) +
    geom_histogram(fill = "mediumpurple", color = "black", bins = 20) +
    geom_vline(aes(xintercept = mean(n)), color = "red", linetype = "dashed") +
    labs(title = "Rides per Driver Dist", x = "Rides", y = "Frequency") +
    theme_minimal()
  
  # 5. Driver Rating Distribution
  p5 <- ggplot(drivers_df, aes(x = Average_Rating)) +
    geom_histogram(fill = "gold", color = "black", bins = 10) +
    labs(title = "Driver Rating Dist", x = "Rating", y = "Count") +
    theme_minimal()
  
  # 6. Top 10 Drivers by Revenue
  top_drivers <- merged_df %>% 
    group_by(Driver_ID) %>% 
    summarise(Revenue = sum(Fare)) %>% 
    top_n(10, Revenue)
  
  p6 <- ggplot(top_drivers, aes(x = reorder(factor(Driver_ID), Revenue), y = Revenue)) +
    geom_col(fill = "orange", width = 0.7) + coord_flip() +
    labs(title = "Top 10 Drivers (Revenue)", x = "Driver ID", y = "Revenue ($)") +
    theme_minimal()
  
  # SPLIT: 4 charts max per page
  grid.arrange(p1, p2, p3, p4, ncol = 2) # Page 1
  grid.arrange(p5, p6, ncol = 2)         # Page 2
}

plot_financial_analysis <- function(rides_df) {
  cat("\nGenerating Financial Analysis...\n")
  
  # 1. Fare Distribution
  p1 <- ggplot(rides_df, aes(x = Fare)) +
    geom_histogram(fill = "lightgreen", color = "black", bins = 30) +
    geom_vline(aes(xintercept = mean(Fare)), color = "red", linetype = "dashed") +
    labs(title = "Fare Distribution", x = "Fare ($)", y = "Count") +
    theme_minimal()
  
  # 2. Distance vs Fare
  corr <- cor(rides_df$Distance_km, rides_df$Fare)
  p2 <- ggplot(rides_df, aes(x = Distance_km, y = Fare)) +
    geom_point(alpha = 0.4, color = "navy") +
    geom_smooth(method = "lm", color = "red") +
    annotate("text", x = min(rides_df$Distance_km), y = max(rides_df$Fare), 
             label = sprintf("Corr: %.2f", corr), hjust = 0, vjust = 1) +
    labs(title = "Distance vs Fare", x = "Dist (km)", y = "Fare ($)") +
    theme_minimal()
  
  # 3. Fare per KM Distribution
  p3 <- ggplot(rides_df, aes(x = Fare_per_km)) +
    geom_histogram(fill = "orange", color = "black", bins = 30) +
    labs(title = "Fare per KM Dist", x = "$/km", y = "Count") +
    theme_minimal()
  
  # 4. Promo Code Impact (Normalized)
  promo_impact <- rides_df %>%
    group_by(Has_Promo) %>%
    summarise(
      Avg_Fare = mean(Fare),
      Avg_Dist = mean(Distance_km),
      Avg_Rating = mean(Rating),
      Count = n()
    ) %>%
    mutate(Type = ifelse(Has_Promo == 1, "Promo", "No Promo")) %>%
    pivot_longer(cols = c(Avg_Fare, Avg_Dist, Avg_Rating, Count), names_to = "Metric", values_to = "Value") %>%
    group_by(Metric) %>%
    mutate(Norm_Value = Value / max(Value) * 100)
  
  p4 <- ggplot(promo_impact, aes(x = Metric, y = Norm_Value, fill = Type)) +
    geom_col(position = "dodge", color = "black") +
    labs(title = "Promo Impact (Normalized)", y = "Score (%)") +
    theme_minimal() + theme(legend.position = "bottom")
  
  # 5. Top Promo Codes
  top_promos <- rides_df %>%
    filter(Promo_Code != "NO_PROMO") %>%
    group_by(Promo_Code) %>%
    summarise(Revenue = sum(Fare)) %>%
    top_n(10, Revenue)
  
  p5 <- ggplot(top_promos, aes(x = reorder(Promo_Code, Revenue), y = Revenue)) +
    geom_col(fill = "purple", color = "black") + coord_flip() +
    labs(title = "Top Promos by Revenue", x = "Code", y = "Revenue ($)") +
    theme_minimal()
  
  # 6. Revenue Trends (Dual Axis)
  rev_data <- rides_df %>% 
    group_by(Date) %>% 
    summarise(Daily = sum(Fare)) %>%
    mutate(Cumulative = cumsum(Daily))
  
  scale_factor <- max(rev_data$Cumulative) / max(rev_data$Daily)
  
  p6 <- ggplot(rev_data, aes(x = Date)) +
    geom_col(aes(y = Daily, fill = "Daily"), alpha = 0.5) +
    geom_line(aes(y = Cumulative / scale_factor, color = "Cumulative"), linewidth = 1) +
    scale_y_continuous(
      name = "Daily ($)",
      sec.axis = sec_axis(~ . * scale_factor, name = "Cumulative ($)")
    ) +
    scale_fill_manual(values = "green") +
    scale_color_manual(values = "darkblue") +
    labs(title = "Revenue Trends", x = "Date") +
    theme_minimal() + theme(legend.position = "none")
  
  # SPLIT: 4 charts max per page
  grid.arrange(p1, p2, p3, p4, ncol = 2) # Page 1
  grid.arrange(p5, p6, ncol = 2)         # Page 2
}

plot_operational_analysis <- function(rides_df) {
  cat("\nGenerating Operational Analysis...\n")
  
  # 1. Distance Distribution
  p1 <- ggplot(rides_df, aes(x = Distance_km)) +
    geom_histogram(fill = "skyblue", color = "black", bins = 30) +
    labs(title = "Distance Dist (km)", y = "Freq") + theme_minimal()
  
  # 2. Duration Distribution
  p2 <- ggplot(rides_df, aes(x = Duration_min)) +
    geom_histogram(fill = "lightcoral", color = "black", bins = 30) +
    labs(title = "Duration Dist (min)", y = "Freq") + theme_minimal()
  
  # 3. Speed Distribution
  p3 <- ggplot(rides_df, aes(x = Speed_kmh)) +
    geom_histogram(fill = "mediumpurple", color = "black", bins = 30) +
    labs(title = "Speed Dist (km/h)", y = "Freq") + theme_minimal()
  
  # 4. Distance vs Duration
  corr <- cor(rides_df$Distance_km, rides_df$Duration_min)
  p4 <- ggplot(rides_df, aes(x = Distance_km, y = Duration_min)) +
    geom_point(alpha = 0.4, color = "#008080", size = 1) + 
    geom_smooth(method = "lm", color = "red", linewidth = 0.8) +
    annotate("text", x=min(rides_df$Distance_km), y=max(rides_df$Duration_min), 
             label=sprintf("R: %.2f", corr), hjust=0) +
    labs(title = "Dist vs Duration", x = "km", y = "min") + theme_minimal()
  
  # 5. Customer Rating Distribution (Categorical)
  rating_data <- rides_df %>% count(Rating)
  p5 <- ggplot(rating_data, aes(x = factor(Rating), y = n, fill = factor(Rating))) +
    geom_col(color = "black") +
    geom_text(aes(label = n), vjust = -0.5, size = 3) +
    labs(title = "Ride Rating Counts", x = "Stars", y = "Count") +
    theme_minimal() + theme(legend.position = "none")
  
  # 6. City Efficiency Heatmap
  city_eff <- rides_df %>%
    group_by(City) %>%
    summarise(
      Speed = mean(Speed_kmh),
      Fare_km = mean(Fare_per_km),
      Rating = mean(Rating),
      Dur = mean(Duration_min)
    ) %>%
    pivot_longer(-City, names_to = "Metric", values_to = "Value") %>%
    group_by(Metric) %>%
    mutate(Norm = (Value - min(Value))/(max(Value)-min(Value)))
  
  p6 <- ggplot(city_eff, aes(x = City, y = Metric, fill = Norm)) +
    geom_tile() + scale_fill_gradient(low = "white", high = "green") +
    geom_text(aes(label = round(Value, 1)), size = 3) +
    labs(title = "City Efficiency Metrics", x = "") + theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # SPLIT: 4 charts max per page
  grid.arrange(p1, p2, p3, p4, ncol = 2) # Page 1
  grid.arrange(p5, p6, ncol = 2)         # Page 2
}

plot_correlation_analysis <- function(rides_df, drivers_df) {
  cat("\nGenerating Correlation Analysis...\n")
  
  # Rides Correlation
  rides_nums <- rides_df %>% select(Distance_km, Duration_min, Fare, Rating, Speed_kmh, Promo_Discount)
  rides_corr <- round(cor(rides_nums, use = "complete.obs"), 2)
  rides_melt <- melt(rides_corr)
  
  p1 <- ggplot(rides_melt, aes(Var1, Var2, fill = value)) +
    geom_tile() +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", limit = c(-1,1)) +
    geom_text(aes(label = value)) +
    labs(title = "Rides Data Correlation") +
    theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Driver Correlation
  driver_nums <- drivers_df %>% select(Age, Experience_Years, Average_Rating)
  driver_corr <- round(cor(driver_nums, use = "complete.obs"), 2)
  driver_melt <- melt(driver_corr)
  
  p2 <- ggplot(driver_melt, aes(Var1, Var2, fill = value)) +
    geom_tile() +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", limit = c(-1,1)) +
    geom_text(aes(label = value)) +
    labs(title = "Driver Characteristics Correlation") +
    theme_minimal()
  
  grid.arrange(p1, p2, ncol = 2)
}

print_advanced_insights <- function(rides_df, merged_df) {
  cat(paste0("\n", strrep("=", 80), "\n"))
  cat("ADVANCED INSIGHTS & SEGMENTATION\n")
  cat(paste0(strrep("=", 80), "\n"))
  
  # 1. Driver Segmentation
  driver_stats <- merged_df %>% 
    group_by(Driver_ID) %>% 
    summarise(Revenue = sum(Fare), Rides = n(), Rating = mean(Rating)) %>%
    mutate(Tier = cut(Revenue, breaks = 3, labels = c("Low", "Medium", "High")))
  
  cat("\n1. DRIVER SEGMENTATION (By Revenue):\n")
  print(driver_stats %>% group_by(Tier) %>% summarise(Count = n(), Avg_Rev = mean(Revenue), Avg_Rides = mean(Rides)))
  
  # 2. Ride Type Analysis
  rides_df$Ride_Type <- cut(rides_df$Distance_km, 
                            breaks = c(0, 5, 15, Inf), 
                            labels = c("Short (<5km)", "Medium (5-15km)", "Long (>15km)"))
  
  cat("\n2. RIDE TYPE ANALYSIS:\n")
  print(rides_df %>% group_by(Ride_Type) %>% summarise(Count = n(), Avg_Fare = mean(Fare), Avg_Rating = mean(Rating)))
  
  # 3. Promo Effectiveness by City
  cat("\n3. PROMO EFFECTIVENESS BY CITY:\n")
  print(rides_df %>% group_by(City, Has_Promo) %>% summarise(Count = n(), Avg_Fare = mean(Fare)) %>% spread(Has_Promo, Count, fill = 0))
  
  # 4. 80/20 Rule
  top_drivers_count <- round(nrow(driver_stats) * 0.2)
  top_revenue <- sum(head(sort(driver_stats$Revenue, decreasing = TRUE), top_drivers_count))
  total_revenue <- sum(driver_stats$Revenue)
  
  cat(sprintf("\n4. REVENUE CONCENTRATION:\n   Top 20%% of drivers generate %.1f%% of total revenue\n", 
              (top_revenue / total_revenue) * 100))
}

print_executive_summary <- function(rides_df, drivers_df) {
  cat("\n\n")
  cat(strrep("█", 80), "\n")
  cat(paste0(strrep(" ", 25), "EXECUTIVE SUMMARY\n"))
  cat(strrep("█", 80), "\n")
  
  total_rides <- nrow(rides_df)
  total_rev <- sum(rides_df$Fare)
  avg_fare <- mean(rides_df$Fare)
  active_drivers <- sum(drivers_df$Active_Status == "Active")
  
  cat("\nKEY PERFORMANCE INDICATORS (November 2024):\n")
  cat(strrep("=", 80), "\n")
  cat(sprintf("  Total Rides:       %s\n", format(total_rides, big.mark=",")))
  cat(sprintf("  Total Revenue:     $%s\n", format(round(total_rev, 2), big.mark=",")))
  cat(sprintf("  Average Fare:      $%.2f\n", avg_fare))
  cat(sprintf("  Active Drivers:    %d\n", active_drivers))
  
  top_city <- names(sort(table(rides_df$City), decreasing=TRUE))[1]
  cat(sprintf("  Top City:          %s\n", top_city))
  cat(paste0(strrep("=", 80), "\n"))
}

# ==============================================================================
# 5. MAIN EXECUTION BLOCK
# ==============================================================================

# 1. LOAD DATA
cat("🚀 Starting Analysis Pipeline...\n")
rides_df <- read.csv('Rides_Data.csv', stringsAsFactors = FALSE)
drivers_df <- read.csv('Drivers_Data.csv', stringsAsFactors = FALSE)

# 2. PROFILE DATA
generate_profile_summary(rides_df, "RIDES DATA")
generate_profile_summary(drivers_df, "DRIVERS DATA")
identify_data_quality_issues(rides_df, "RIDES DATA")
identify_data_quality_issues(drivers_df, "DRIVERS DATA")
analyze_relationships(rides_df, drivers_df)

# 3. TRANSFORM DATA
rides_df <- transform_rides_data(rides_df)
merged_df <- merge_datasets(rides_df, drivers_df)

# 4. RUN VISUALIZATIONS (DISPLAY TO SCREEN)
# Note: In a script run non-interactively, these might not appear unless saved.
# The PDF export step below ensures they are captured.
plot_temporal_analysis(rides_df)
plot_geographic_analysis(rides_df)
plot_driver_performance(rides_df, drivers_df, merged_df)
plot_financial_analysis(rides_df)
plot_operational_analysis(rides_df)
plot_correlation_analysis(rides_df, drivers_df)

# 5. PRINT INSIGHTS
print_advanced_insights(rides_df, merged_df)
print_executive_summary(rides_df, drivers_df)

# 6. EXPORT TO PDF
pdf_filename <- "CityRide_Analysis_Report.pdf"
cat(sprintf("\nSaving all plots to %s...\n", pdf_filename))

pdf(pdf_filename, width = 11, height = 8.5)
plot_temporal_analysis(rides_df)
plot_geographic_analysis(rides_df)
plot_driver_performance(rides_df, drivers_df, merged_df)
plot_financial_analysis(rides_df)
plot_operational_analysis(rides_df)
plot_correlation_analysis(rides_df, drivers_df)
dev.off()

cat("✓ Analysis Complete!\n")