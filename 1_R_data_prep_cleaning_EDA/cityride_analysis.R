# ==============================================================================
# CityRide Data Analysis & Visualization Project (Functional R Implementation)
# ==============================================================================

setwd("c:/Users/radus/Desktop/DMV PROJECT/R")

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
  cat(sprintf(
    "Dataset Shape: %s rows x %s columns\n",
    format(nrow(df), big.mark = ","), ncol(df)
  ))
  cat(sprintf("Memory Usage: %.2f MB\n", object.size(df) / 1024^2))
  cat(paste0("\n", strrep("-", 80), "\n\n"))

  # Column-by-column analysis
  profile_data <- data.frame()

  for (col in names(df)) {
    col_data <- df[[col]]

    # Basic stats
    col_info <- data.frame(
      Variable = col,
      Type = class(col_data)[1],
      Non_Null = format(sum(!is.na(col_data)), big.mark = ","),
      Null = format(sum(is.na(col_data)), big.mark = ","),
      Null_Pct = sprintf("%.2f%%", (sum(is.na(col_data)) / nrow(df)) * 100),
      Unique = format(length(unique(col_data)), big.mark = ","),
      Unique_Pct = sprintf("%.2f%%", (length(unique(col_data)) / nrow(df)) * 100)
    )

    # Add type-specific statistics
    if (is.numeric(col_data)) {
      col_info$Min <- sprintf("%.2f", min(col_data, na.rm = TRUE))
      col_info$Max <- sprintf("%.2f", max(col_data, na.rm = TRUE))
      col_info$Mean <- sprintf("%.2f", mean(col_data, na.rm = TRUE))
      col_info$Median <- sprintf("%.2f", median(col_data, na.rm = TRUE))
      col_info$Std <- sprintf("%.2f", sd(col_data, na.rm = TRUE))
    } else {
      # For non-numeric, show top value
      if (sum(!is.na(col_data)) > 0) {
        top_val <- names(sort(table(col_data), decreasing = TRUE))[1]
        top_count <- max(table(col_data))
        col_info$Top_Value <- substr(as.character(top_val), 1, 30)
        col_info$Top_Freq <- format(top_count, big.mark = ",")
      } else {
        col_info$Top_Value <- "N/A"
        col_info$Top_Freq <- "0"
      }
    }

    profile_data <- bind_rows(profile_data, col_info)
  }

  print(profile_data)
  cat("\n")

  return(profile_data)
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
      cat(sprintf(
        "  • %s: %s (%.2f%%)\n",
        col, format(missing_counts[col], big.mark = ","), pct
      ))
    }
    cat("\n")
  } else {
    cat("✓ No missing values detected\n\n")
  }

  # Duplicates
  dupe_count <- sum(duplicated(df))
  if (dupe_count > 0) {
    cat(sprintf(
      "⚠ DUPLICATE ROWS: %s (%.2f%%)\n\n",
      format(dupe_count, big.mark = ","), (dupe_count / nrow(df) * 100)
    ))
  } else {
    cat("✓ No duplicate rows detected\n\n")
  }

  # Outliers (IQR Method)
  numeric_cols <- df %>%
    select(where(is.numeric)) %>%
    names()
  if (length(numeric_cols) > 0) {
    cat("OUTLIER DETECTION (IQR Method):\n")
    for (col in numeric_cols) {
      stats <- boxplot.stats(df[[col]])$stats
      Q1 <- stats[2]
      Q3 <- stats[4]
      IQR <- Q3 - Q1
      lower_bound <- Q1 - 1.5 * IQR
      upper_bound <- Q3 + 1.5 * IQR
      outliers <- df[[col]][df[[col]] < lower_bound | df[[col]] > upper_bound]

      if (length(outliers) > 0) {
        pct <- (length(outliers) / nrow(df)) * 100
        cat(sprintf(
          "  • %s: %s outliers (%.2f%%)\n",
          col, format(length(outliers), big.mark = ","), pct
        ))
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
  drivers_with_rides <- length(rides_drivers)
  drivers_without_rides <- length(setdiff(all_drivers, rides_drivers))

  cat(sprintf("Total Drivers in Drivers table: %d\n", length(all_drivers)))
  cat(sprintf("Unique Drivers with rides: %d\n", drivers_with_rides))
  cat(sprintf("Drivers without rides: %d\n", drivers_without_rides))
  cat(sprintf(
    "Driver_ID overlap: %.2f%%\n",
    (length(intersect(rides_drivers, all_drivers)) / length(all_drivers)) * 100
  ))

  if (length(orphaned) > 0) {
    cat(sprintf("\n⚠ WARNING: %d Driver IDs in Rides table not found in Drivers table\n", length(orphaned)))
  } else {
    cat("\n✓ All rides have corresponding driver records\n")
  }

  # Rides per driver statistics
  rides_per_driver <- table(rides_df$Driver_ID)
  cat(sprintf("\nRides per Driver Statistics:\n"))
  cat(sprintf("  Mean: %.2f\n", mean(rides_per_driver)))
  cat(sprintf("  Median: %.0f\n", median(rides_per_driver)))
  cat(sprintf("  Min: %d\n", min(rides_per_driver)))
  cat(sprintf("  Max: %d\n", max(rides_per_driver)))
  cat(sprintf("  Std: %.2f\n", sd(rides_per_driver)))
}

generate_summary_statistics <- function(rides_df, drivers_df) {
  cat(paste0("\n", strrep("=", 80), "\n"))
  cat("RIDES DATA - SUMMARY STATISTICS\n")
  cat(paste0(strrep("=", 80), "\n\n"))

  # Rides numeric summary
  rides_numeric <- rides_df %>% select(where(is.numeric))
  print(as.data.frame(t(summary(rides_numeric))))

  cat(paste0("\n", strrep("=", 80), "\n"))
  cat("DRIVERS DATA - SUMMARY STATISTICS\n")
  cat(paste0(strrep("=", 80), "\n\n"))

  # Drivers numeric summary
  drivers_numeric <- drivers_df %>% select(where(is.numeric))
  print(as.data.frame(t(summary(drivers_numeric))))
}

generate_categorical_summary <- function(rides_df, drivers_df) {
  cat(paste0("\n", strrep("=", 80), "\n"))
  cat("CATEGORICAL VARIABLES SUMMARY\n")
  cat(paste0(strrep("=", 80), "\n\n"))

  # Rides categorical
  cat("RIDES DATA:\n")
  cat(strrep("-", 40), "\n")
  categorical_cols <- rides_df %>%
    select(where(is.character)) %>%
    names()
  for (col in categorical_cols) {
    cat(sprintf("\n%s:\n", col))
    value_counts <- table(rides_df[[col]])
    value_counts <- sort(value_counts, decreasing = TRUE)
    print(head(value_counts, 10))
    if (length(value_counts) > 10) {
      cat(sprintf("  ... and %d more unique values\n", length(value_counts) - 10))
    }
  }

  # Drivers categorical
  cat("\n\nDRIVERS DATA:\n")
  cat(strrep("-", 40), "\n")
  categorical_cols <- drivers_df %>%
    select(where(is.character)) %>%
    names()
  for (col in categorical_cols) {
    cat(sprintf("\n%s:\n", col))
    value_counts <- table(drivers_df[[col]])
    value_counts <- sort(value_counts, decreasing = TRUE)
    print(head(value_counts, 10))
    if (length(value_counts) > 10) {
      cat(sprintf("  ... and %d more unique values\n", length(value_counts) - 10))
    }
  }
}

# ==============================================================================
# 3. Data Transformation Functions
# ==============================================================================

transform_rides_data <- function(df) {
  cat("\n🚧 Transforming Rides Data...\n")
  cat(strrep("=", 80), "\n")

  # 1. Promo Features
  # FIX: Handle NA and empty strings as NO_PROMO to ensure comparison works
  df$Promo_Code[is.na(df$Promo_Code) | df$Promo_Code == ""] <- "NO_PROMO"

  df$Has_Promo <- as.integer(df$Promo_Code != "NO_PROMO")
  df$Promo_Discount <- as.numeric(str_extract(df$Promo_Code, "\\d+")) / 100
  df$Promo_Discount[is.na(df$Promo_Discount)] <- 0

  cat("✓ Added promo feature columns: Has_Promo, Promo_Discount\n")

  # 2. Derived Metrics
  df$Fare_per_km <- df$Fare / df$Distance_km
  df$Speed_kmh <- (df$Distance_km / df$Duration_min) * 60

  cat("✓ Added derived metrics: Fare_per_km, Speed_kmh\n")

  # 3. Temporal Features
  df$Date <- mdy(df$Date)
  df$Day_of_Week <- factor(wday(df$Date, label = TRUE, abbr = FALSE),
    levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
  )
  df$Week_Number <- isoweek(df$Date)
  df$Day_of_Month <- day(df$Date)
  df$Is_Weekend <- as.integer(wday(df$Date) %in% c(1, 7))

  cat("✓ Added temporal features: Day_of_Week, Week_Number, Day_of_Month, Is_Weekend\n")

  cat("\n✨ Transformation complete!\n")
  cat(strrep("=", 80), "\n")

  return(df)
}

merge_datasets <- function(rides_df, drivers_df) {
  cat("\n🚧 Merging Datasets...\n")
  merged_df <- left_join(rides_df, drivers_df, by = "Driver_ID", suffix = c("_ride", "_driver"))
  cat(sprintf("✓ Merged datasets -> %s records\n\n", format(nrow(merged_df), big.mark = ",")))
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
    labs(title = "Daily Ride Volume", x = "Date", y = "Number of Rides") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  # 2. Day of Week
  dow_order <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

  # Ensure Day_of_Week is a factor with correct ordering
  rides_df$Day_of_Week <- factor(as.character(rides_df$Day_of_Week), levels = dow_order)

  dow_data <- rides_df %>%
    count(Day_of_Week, .drop = FALSE) %>%
    arrange(Day_of_Week)
  dow_data$color <- ifelse(dow_data$Day_of_Week %in% c("Saturday", "Sunday"), "#ff6b6b", "#4ecdc4")

  p2 <- ggplot(dow_data, aes(x = Day_of_Week, y = n, fill = color)) +
    geom_col(color = "black") +
    geom_text(aes(label = n), vjust = -0.5, fontface = "bold") +
    scale_fill_identity() +
    labs(title = "Rides by Day of Week", x = "Day of Week", y = "Number of Rides") +
    theme_minimal() +
    theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))

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
    labs(title = "Weekend vs Weekday (Normalized)", x = "", y = "Normalized Score (%)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  # 4. Daily Revenue
  daily_rev <- rides_df %>%
    group_by(Date) %>%
    summarise(Revenue = sum(Fare))
  avg_rev <- mean(daily_rev$Revenue)
  p4 <- ggplot(daily_rev, aes(x = Date, y = Revenue)) +
    geom_area(fill = "green", alpha = 0.3) +
    geom_line(color = "darkgreen") +
    geom_point() +
    geom_hline(yintercept = avg_rev, color = "red", linetype = "dashed", linewidth = 1) +
    annotate("text",
      x = min(daily_rev$Date), y = avg_rev,
      label = sprintf("Avg: $%.0f", avg_rev), vjust = -0.5, hjust = 0
    ) +
    labs(title = "Daily Revenue Trend", x = "Date", y = "Total Revenue ($)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  grid.arrange(p1, p2, p3, p4, ncol = 2)

  # Print insights
  cat("\n", strrep("=", 80), "\n")
  cat("TEMPORAL INSIGHTS\n")
  cat(strrep("=", 80), "\n")
  cat(sprintf(
    "\n1. Busiest day: %s with %s rides\n",
    dow_data$Day_of_Week[which.max(dow_data$n)],
    format(max(dow_data$n), big.mark = ",")
  ))
  cat(sprintf(
    "2. Slowest day: %s with %s rides\n",
    dow_data$Day_of_Week[which.min(dow_data$n)],
    format(min(dow_data$n), big.mark = ",")
  ))
  weekend_rides <- sum(dow_data$n[dow_data$Day_of_Week %in% c("Saturday", "Sunday")])
  weekday_rides <- sum(dow_data$n[!dow_data$Day_of_Week %in% c("Saturday", "Sunday")])
  cat(sprintf(
    "3. Weekend vs Weekday rides: %s vs %s\n",
    format(weekend_rides, big.mark = ","),
    format(weekday_rides, big.mark = ",")
  ))
  cat(sprintf("4. Average daily rides: %.0f\n", mean(daily_rides$n)))
  cat(sprintf("5. Average daily revenue: $%s\n", format(round(avg_rev, 2), big.mark = ",")))
  cat("\n")
}

plot_geographic_analysis <- function(rides_df) {
  cat("\nGenerating Geographic Analysis...\n")

  # 1. Rides by City
  city_rides <- rides_df %>%
    count(City) %>%
    arrange(n)
  p1 <- ggplot(city_rides, aes(x = reorder(City, n), y = n, fill = City)) +
    geom_col() +
    coord_flip() +
    geom_text(aes(label = format(n, big.mark = ",")), hjust = -0.2) +
    labs(title = "Ride Volume by City", x = "City", y = "Number of Rides") +
    theme_minimal() +
    theme(legend.position = "none")

  # 2. Avg Fare by City
  city_fare <- rides_df %>%
    group_by(City) %>%
    summarise(Avg_Fare = mean(Fare)) %>%
    arrange(Avg_Fare)
  p2 <- ggplot(city_fare, aes(x = reorder(City, Avg_Fare), y = Avg_Fare, fill = City)) +
    geom_col() +
    coord_flip() +
    geom_text(aes(label = sprintf("$%.2f", Avg_Fare)), hjust = -0.2) +
    labs(title = "Average Fare by City", x = "City", y = "Average Fare ($)") +
    theme_minimal() +
    theme(legend.position = "none")

  # 3. Revenue Share
  city_rev <- rides_df %>%
    group_by(City) %>%
    summarise(Revenue = sum(Fare)) %>%
    mutate(
      Percentage = Revenue / sum(Revenue) * 100,
      Label = sprintf("%.1f%%", Percentage)
    )

  p3 <- ggplot(city_rev, aes(x = "", y = Revenue, fill = City)) +
    geom_bar(stat = "identity", width = 1) +
    geom_text(aes(label = Label),
      position = position_stack(vjust = 0.5),
      color = "white", fontface = "bold", size = 4
    ) +
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
    geom_text(aes(label = round(Value, 2)), size = 3) +
    labs(title = "City Performance Heatmap", x = "") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  grid.arrange(p1, p2, p3, p4, ncol = 2)

  # Print insights
  cat("\n", strrep("=", 80), "\n")
  cat("GEOGRAPHIC INSIGHTS\n")
  cat(strrep("=", 80), "\n")
  cat(sprintf(
    "\n1. Highest volume city: %s with %s rides\n",
    city_rides$City[which.max(city_rides$n)],
    format(max(city_rides$n), big.mark = ",")
  ))
  cat(sprintf(
    "2. Highest revenue city: %s with $%s\n",
    city_rev$City[which.max(city_rev$Revenue)],
    format(round(max(city_rev$Revenue), 2), big.mark = ",")
  ))
  cat(sprintf(
    "3. Highest avg fare: %s at $%.2f\n",
    city_fare$City[which.max(city_fare$Avg_Fare)],
    max(city_fare$Avg_Fare)
  ))
  cat("4. City market share (top 3):\n")
  top3_cities <- city_rev %>%
    arrange(desc(Revenue)) %>%
    head(3)
  for (i in 1:nrow(top3_cities)) {
    pct <- (top3_cities$Revenue[i] / sum(city_rev$Revenue)) * 100
    cat(sprintf(
      "   %d. %s: $%s (%.1f%%)\n", i, top3_cities$City[i],
      format(round(top3_cities$Revenue[i], 2), big.mark = ","), pct
    ))
  }
  cat("\n")
}

plot_driver_performance <- function(rides_df, drivers_df, merged_df) {
  cat("\nGenerating Driver Performance Analysis...\n")

  # 1. Experience vs Rating
  p1 <- ggplot(drivers_df, aes(x = Experience_Years, y = Average_Rating)) +
    geom_point(alpha = 0.6, color = "steelblue", size = 2) +
    geom_smooth(method = "lm", color = "red", se = FALSE) +
    labs(title = "Experience vs Driver Rating", x = "Years of Experience", y = "Average Rating") +
    theme_minimal()

  # 2. Age Distribution
  p2 <- ggplot(drivers_df, aes(x = Age)) +
    geom_histogram(fill = "coral", color = "black", bins = 15) +
    geom_vline(aes(xintercept = mean(Age)), color = "red", linetype = "dashed", linewidth = 1) +
    annotate("text",
      x = mean(drivers_df$Age), y = Inf,
      label = sprintf("Mean: %.1f", mean(drivers_df$Age)), vjust = 1.5, hjust = -0.1
    ) +
    labs(title = "Driver Age Distribution", x = "Age", y = "Frequency") +
    theme_minimal()

  # 3. Active vs Inactive Status
  status_counts <- drivers_df %>% count(Active_Status)
  p3 <- ggplot(status_counts, aes(x = Active_Status, y = n, fill = Active_Status)) +
    geom_col(color = "black", width = 0.6) +
    geom_text(
      aes(label = sprintf(
        "%s\n(%.1f%%)", format(n, big.mark = ","),
        n / sum(status_counts$n) * 100
      )),
      vjust = -0.5, fontface = "bold"
    ) +
    scale_fill_manual(values = c("Active" = "#2ecc71", "Inactive" = "#e74c3c")) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    labs(title = "Driver Active Status", x = "Status", y = "Number of Drivers") +
    theme_minimal() +
    theme(legend.position = "none")

  # 4. Rides per Driver Distribution
  rides_per_driver <- rides_df %>% count(Driver_ID)
  p4 <- ggplot(rides_per_driver, aes(x = n)) +
    geom_histogram(fill = "mediumpurple", color = "black", bins = 20) +
    geom_vline(aes(xintercept = mean(n)), color = "red", linetype = "dashed", linewidth = 1) +
    geom_vline(aes(xintercept = median(n)), color = "green", linetype = "dashed", linewidth = 1) +
    labs(title = "Rides per Driver Distribution", x = "Number of Rides", y = "Frequency") +
    theme_minimal()

  # 5. Driver Rating Distribution
  p5 <- ggplot(drivers_df, aes(x = Average_Rating)) +
    geom_histogram(fill = "gold", color = "black", bins = 10) +
    geom_vline(aes(xintercept = mean(Average_Rating)), color = "red", linetype = "dashed", linewidth = 1) +
    labs(title = "Driver Rating Distribution", x = "Average Rating", y = "Frequency") +
    theme_minimal()

  # 6. Top 10 Drivers by Revenue
  top_drivers <- merged_df %>%
    group_by(Driver_ID) %>%
    summarise(Revenue = sum(Fare)) %>%
    top_n(10, Revenue) %>%
    arrange(desc(Revenue))

  p6 <- ggplot(top_drivers, aes(x = reorder(factor(Driver_ID), Revenue), y = Revenue)) +
    geom_col(fill = "orange", width = 0.7) +
    coord_flip() +
    labs(title = "Top 10 Drivers by Revenue", x = "Driver ID", y = "Total Revenue ($)") +
    theme_minimal()

  # SPLIT: 4 charts max per page
  grid.arrange(p1, p2, p3, p4, ncol = 2) # Page 1
  grid.arrange(p5, p6, ncol = 2) # Page 2

  # Print insights
  cat("\n", strrep("=", 80), "\n")
  cat("DRIVER PERFORMANCE INSIGHTS\n")
  cat(strrep("=", 80), "\n")
  cat(sprintf(
    "\n1. Total active drivers: %s\n",
    format(sum(status_counts$n[status_counts$Active_Status == "Active"]), big.mark = ",")
  ))
  cat(sprintf(
    "2. Total inactive drivers: %s\n",
    format(sum(status_counts$n[status_counts$Active_Status == "Inactive"]), big.mark = ",")
  ))
  cat(sprintf("3. Average rides per driver: %.1f\n", mean(rides_per_driver$n)))
  cat(sprintf(
    "4. Most productive driver: %d with %d rides\n",
    rides_per_driver$Driver_ID[which.max(rides_per_driver$n)],
    max(rides_per_driver$n)
  ))
  cat(sprintf("5. Average driver rating: %.2f\n", mean(drivers_df$Average_Rating)))
  cat(sprintf("6. Drivers with rating < 4.0: %d\n", sum(drivers_df$Average_Rating < 4.0)))
  cat("\n")
}

plot_financial_analysis <- function(rides_df) {
  cat("\nGenerating Financial Analysis...\n")

  # 1. Fare Distribution
  p1 <- ggplot(rides_df, aes(x = Fare)) +
    geom_histogram(fill = "lightgreen", color = "black", bins = 30) +
    geom_vline(aes(xintercept = mean(Fare)), color = "red", linetype = "dashed", linewidth = 1) +
    geom_vline(aes(xintercept = median(Fare)), color = "blue", linetype = "dashed", linewidth = 1) +
    labs(title = "Fare Distribution", x = "Fare ($)", y = "Frequency") +
    theme_minimal()

  # 2. Distance vs Fare
  corr <- cor(rides_df$Distance_km, rides_df$Fare)
  p2 <- ggplot(rides_df, aes(x = Distance_km, y = Fare)) +
    geom_point(alpha = 0.4, color = "navy") +
    geom_smooth(method = "lm", color = "red") +
    annotate("text",
      x = min(rides_df$Distance_km), y = max(rides_df$Fare),
      label = sprintf("Correlation: %.3f", corr), hjust = 0, vjust = 1
    ) +
    labs(title = "Distance vs Fare Relationship", x = "Distance (km)", y = "Fare ($)") +
    theme_minimal()

  # 3. Fare per KM Distribution
  p3 <- ggplot(rides_df, aes(x = Fare_per_km)) +
    geom_histogram(fill = "orange", color = "black", bins = 30) +
    geom_vline(aes(xintercept = mean(Fare_per_km)), color = "red", linetype = "dashed", linewidth = 1) +
    labs(title = "Fare per Kilometer Distribution", x = "Fare per km ($)", y = "Frequency") +
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
    mutate(Type = ifelse(Has_Promo == 1, "With Promo", "No Promo")) %>%
    pivot_longer(cols = c(Avg_Fare, Avg_Dist, Avg_Rating, Count), names_to = "Metric", values_to = "Value") %>%
    group_by(Metric) %>%
    mutate(Norm_Value = Value / max(Value) * 100)

  p4 <- ggplot(promo_impact, aes(x = Metric, y = Norm_Value, fill = Type)) +
    geom_col(position = "dodge", color = "black") +
    labs(title = "Promo Code Impact (Normalized)", x = "", y = "Normalized Score (%)") +
    theme_minimal() +
    theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1))

  # 5. Top Promo Codes
  top_promos <- rides_df %>%
    filter(Promo_Code != "NO_PROMO") %>%
    group_by(Promo_Code) %>%
    summarise(Revenue = sum(Fare)) %>%
    top_n(10, Revenue) %>%
    arrange(desc(Revenue))

  p5 <- ggplot(top_promos, aes(x = reorder(Promo_Code, Revenue), y = Revenue)) +
    geom_col(fill = "purple", color = "black") +
    coord_flip() +
    labs(title = "Top 10 Promo Codes by Revenue", x = "Promo Code", y = "Total Revenue ($)") +
    theme_minimal()

  # 6. Revenue Trends (Dual Axis)
  rev_data <- rides_df %>%
    group_by(Date) %>%
    summarise(Daily = sum(Fare)) %>%
    mutate(Cumulative = cumsum(Daily))

  scale_factor <- max(rev_data$Cumulative) / max(rev_data$Daily)

  p6 <- ggplot(rev_data, aes(x = Date)) +
    geom_col(aes(y = Daily), fill = "green", alpha = 0.5) +
    geom_line(aes(y = Cumulative / scale_factor), color = "darkblue", linewidth = 1) +
    scale_y_continuous(
      name = "Daily Revenue ($)",
      sec.axis = sec_axis(~ . * scale_factor, name = "Cumulative Revenue ($)")
    ) +
    labs(title = "Revenue Trends", x = "Date") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  # SPLIT: 4 charts max per page
  grid.arrange(p1, p2, p3, p4, ncol = 2) # Page 1
  grid.arrange(p5, p6, ncol = 2) # Page 2

  # Print insights
  cat("\n", strrep("=", 80), "\n")
  cat("FINANCIAL INSIGHTS\n")
  cat(strrep("=", 80), "\n")
  total_revenue <- sum(rides_df$Fare)
  cat(sprintf("\n1. Total revenue: $%s\n", format(round(total_revenue, 2), big.mark = ",")))
  cat(sprintf("2. Average fare: $%.2f\n", mean(rides_df$Fare)))
  cat(sprintf("3. Average fare per km: $%.2f\n", mean(rides_df$Fare_per_km)))
  promo_rides <- sum(rides_df$Has_Promo)
  cat(sprintf(
    "4. Rides with promo codes: %s (%.1f%%)\n",
    format(promo_rides, big.mark = ","),
    (promo_rides / nrow(rides_df)) * 100
  ))
  promo_revenue <- sum(rides_df$Fare[rides_df$Has_Promo == 1])
  cat(sprintf("5. Revenue from promo rides: $%s\n", format(round(promo_revenue, 2), big.mark = ",")))
  cat(sprintf(
    "6. Average daily revenue: $%s\n",
    format(round(mean(rev_data$Daily), 2), big.mark = ",")
  ))
  cat("\n")
}

plot_operational_analysis <- function(rides_df) {
  cat("\nGenerating Operational Analysis...\n")

  # 1. Distance Distribution
  p1 <- ggplot(rides_df, aes(x = Distance_km)) +
    geom_histogram(fill = "skyblue", color = "black", bins = 30) +
    geom_vline(aes(xintercept = mean(Distance_km)), color = "red", linetype = "dashed", linewidth = 1) +
    geom_vline(aes(xintercept = median(Distance_km)), color = "green", linetype = "dashed", linewidth = 1) +
    labs(title = "Ride Distance Distribution", x = "Distance (km)", y = "Frequency") +
    theme_minimal()

  # 2. Duration Distribution
  p2 <- ggplot(rides_df, aes(x = Duration_min)) +
    geom_histogram(fill = "lightcoral", color = "black", bins = 30) +
    geom_vline(aes(xintercept = mean(Duration_min)), color = "red", linetype = "dashed", linewidth = 1) +
    geom_vline(aes(xintercept = median(Duration_min)), color = "green", linetype = "dashed", linewidth = 1) +
    labs(title = "Ride Duration Distribution", x = "Duration (minutes)", y = "Frequency") +
    theme_minimal()

  # 3. Speed Distribution
  p3 <- ggplot(rides_df, aes(x = Speed_kmh)) +
    geom_histogram(fill = "mediumpurple", color = "black", bins = 30) +
    geom_vline(aes(xintercept = mean(Speed_kmh)), color = "red", linetype = "dashed", linewidth = 1) +
    labs(title = "Average Speed Distribution", x = "Speed (km/h)", y = "Frequency") +
    theme_minimal()

  # 4. Distance vs Duration
  corr <- cor(rides_df$Distance_km, rides_df$Duration_min)
  p4 <- ggplot(rides_df, aes(x = Distance_km, y = Duration_min)) +
    geom_point(alpha = 0.4, color = "#008080", size = 1) +
    geom_smooth(method = "lm", color = "red", linewidth = 0.8) +
    annotate("text",
      x = min(rides_df$Distance_km), y = max(rides_df$Duration_min),
      label = sprintf("Correlation: %.3f", corr), hjust = 0, vjust = 1
    ) +
    labs(title = "Distance vs Duration", x = "Distance (km)", y = "Duration (min)") +
    theme_minimal()

  # 5. Customer Rating Distribution (Categorical)
  rating_data <- rides_df %>% count(Rating)
  rating_data$color <- ifelse(rating_data$Rating < 3, "red",
    ifelse(rating_data$Rating < 4, "orange", "lightgreen")
  )

  p5 <- ggplot(rating_data, aes(x = factor(Rating), y = n, fill = color)) +
    geom_col(color = "black") +
    geom_text(
      aes(label = sprintf(
        "%s\n(%.1f%%)", format(n, big.mark = ","),
        n / sum(rating_data$n) * 100
      )),
      vjust = -0.5, size = 3, fontface = "bold"
    ) +
    scale_fill_identity() +
    labs(title = "Customer Rating Distribution", x = "Rating", y = "Number of Rides") +
    theme_minimal() +
    theme(legend.position = "none")

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
    mutate(Norm = (Value - min(Value)) / (max(Value) - min(Value)))

  p6 <- ggplot(city_eff, aes(x = City, y = Metric, fill = Norm)) +
    geom_tile() +
    scale_fill_gradient(low = "white", high = "green") +
    geom_text(aes(label = round(Value, 1)), size = 3) +
    labs(title = "City Efficiency Metrics", x = "") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  # SPLIT: 4 charts max per page
  grid.arrange(p1, p2, p3, p4, ncol = 2) # Page 1
  grid.arrange(p5, p6, ncol = 2) # Page 2

  # Print insights
  cat("\n", strrep("=", 80), "\n")
  cat("OPERATIONAL INSIGHTS\n")
  cat(strrep("=", 80), "\n")
  cat(sprintf("\n1. Average ride distance: %.2f km\n", mean(rides_df$Distance_km)))
  cat(sprintf("2. Average ride duration: %.1f minutes\n", mean(rides_df$Duration_min)))
  cat(sprintf("3. Average speed: %.1f km/h\n", mean(rides_df$Speed_kmh)))
  cat(sprintf("4. Average customer rating: %.2f/5.0\n", mean(rides_df$Rating)))
  five_star <- sum(rides_df$Rating == 5)
  cat(sprintf(
    "5. Rides rated 5 stars: %s (%.1f%%)\n",
    format(five_star, big.mark = ","),
    (five_star / nrow(rides_df)) * 100
  ))
  low_rating <- sum(rides_df$Rating < 3)
  cat(sprintf(
    "6. Rides rated below 3 stars: %s (%.1f%%)\n",
    format(low_rating, big.mark = ","),
    (low_rating / nrow(rides_df)) * 100
  ))
  cat("\n")
}

plot_correlation_analysis <- function(rides_df, drivers_df) {
  cat("\nGenerating Correlation Analysis...\n")

  # Rides Correlation
  rides_nums <- rides_df %>% select(Distance_km, Duration_min, Fare, Rating, Speed_kmh, Promo_Discount, Fare_per_km)
  rides_corr <- round(cor(rides_nums, use = "complete.obs"), 2)
  rides_melt <- melt(rides_corr)

  p1 <- ggplot(rides_melt, aes(Var1, Var2, fill = value)) +
    geom_tile() +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", limit = c(-1, 1), midpoint = 0) +
    geom_text(aes(label = value)) +
    labs(title = "Rides Data Correlation Matrix") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  # Driver Correlation
  driver_nums <- drivers_df %>% select(Age, Experience_Years, Average_Rating)
  driver_corr <- round(cor(driver_nums, use = "complete.obs"), 2)
  driver_melt <- melt(driver_corr)

  p2 <- ggplot(driver_melt, aes(Var1, Var2, fill = value)) +
    geom_tile() +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", limit = c(-1, 1), midpoint = 0) +
    geom_text(aes(label = value)) +
    labs(title = "Driver Characteristics Correlation Matrix") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  grid.arrange(p1, p2, ncol = 2)

  # Print key correlations
  cat("\n", strrep("=", 80), "\n")
  cat("KEY CORRELATIONS\n")
  cat(strrep("=", 80), "\n")
  cat("\nRides Data:\n")
  cat(sprintf("1. Distance vs Fare: %.3f\n", rides_corr["Distance_km", "Fare"]))
  cat(sprintf("2. Duration vs Fare: %.3f\n", rides_corr["Duration_min", "Fare"]))
  cat(sprintf("3. Rating vs Fare: %.3f\n", rides_corr["Rating", "Fare"]))
  cat(sprintf("4. Speed vs Rating: %.3f\n", rides_corr["Speed_kmh", "Rating"]))
  cat(sprintf("5. Promo vs Rating: %.3f\n", rides_corr["Promo_Discount", "Rating"]))

  cat("\nDriver Data:\n")
  cat(sprintf("1. Experience vs Rating: %.3f\n", driver_corr["Experience_Years", "Average_Rating"]))
  cat(sprintf("2. Age vs Experience: %.3f\n", driver_corr["Age", "Experience_Years"]))
  cat(sprintf("3. Age vs Rating: %.3f\n", driver_corr["Age", "Average_Rating"]))
  cat("\n")
}

print_advanced_insights <- function(rides_df, merged_df) {
  cat(paste0("\n", strrep("=", 80), "\n"))
  cat("ADVANCED INSIGHTS & SEGMENTATION\n")
  cat(paste0(strrep("=", 80), "\n"))

  # 1. Driver Segmentation by Performance
  driver_stats <- merged_df %>%
    group_by(Driver_ID) %>%
    summarise(
      Total_Rides = n(),
      Total_Revenue = sum(Fare),
      Avg_Ride_Rating = mean(Rating),
      Driver_Rating = first(Average_Rating)
    )

  # Categorize drivers into performance tiers
  driver_stats$Performance_Category <- cut(driver_stats$Total_Revenue,
    breaks = 3,
    labels = c("Low Performer", "Medium Performer", "High Performer")
  )

  cat("\n1. DRIVER PERFORMANCE SEGMENTATION:\n")
  perf_summary <- driver_stats %>%
    group_by(Performance_Category) %>%
    summarise(
      Driver_Count = n(),
      Avg_Rides = round(mean(Total_Rides), 2),
      Avg_Revenue = round(mean(Total_Revenue), 2),
      Avg_Ride_Rating = round(mean(Avg_Ride_Rating), 2)
    )
  print(perf_summary)

  # 2. Ride Type Analysis
  rides_df$Ride_Type <- cut(rides_df$Distance_km,
    breaks = c(0, 5, 15, Inf),
    labels = c("Short (<5km)", "Medium (5-15km)", "Long (>15km)")
  )

  cat("\n2. RIDE TYPE ANALYSIS:\n")
  ride_type_summary <- rides_df %>%
    group_by(Ride_Type) %>%
    summarise(
      Count = n(),
      Avg_Fare = round(mean(Fare), 2),
      Avg_Rating = round(mean(Rating), 2),
      Avg_Fare_per_km = round(mean(Fare_per_km), 2)
    )
  print(ride_type_summary)

  # 3. Weekend vs Weekday Deep Dive
  cat("\n3. WEEKEND VS WEEKDAY DEEP DIVE:\n")
  weekend_analysis <- rides_df %>%
    group_by(Is_Weekend) %>%
    summarise(
      Ride_Count = n(),
      Total_Revenue = round(sum(Fare), 2),
      Avg_Fare = round(mean(Fare), 2),
      Avg_Distance = round(mean(Distance_km), 2),
      Avg_Rating = round(mean(Rating), 2),
      Promo_Rides = sum(Has_Promo)
    )
  weekend_analysis$Day_Type <- ifelse(weekend_analysis$Is_Weekend == 1, "Weekend", "Weekday")
  weekend_analysis <- weekend_analysis %>% select(-Is_Weekend)
  print(weekend_analysis)

  # 4. Promo Effectiveness by City
  cat("\n4. PROMO CODE EFFECTIVENESS BY CITY:\n")
  promo_city <- rides_df %>%
    group_by(City, Has_Promo) %>%
    summarise(
      Ride_Count = n(),
      Avg_Fare = round(mean(Fare), 2),
      Avg_Rating = round(mean(Rating), 2),
      .groups = "drop"
    ) %>%
    mutate(Promo_Status = ifelse(Has_Promo == 1, "With_Promo", "No_Promo")) %>%
    select(-Has_Promo)
  print(promo_city)

  # 5. Driver Utilization Rate
  if (exists("drivers_df", envir = .GlobalEnv)) {
    drivers_df <- get("drivers_df", envir = .GlobalEnv)
    active_drivers <- sum(drivers_df$Active_Status == "Active")
    drivers_with_rides <- length(unique(rides_df$Driver_ID))
    utilization_rate <- (drivers_with_rides / active_drivers) * 100

    cat(sprintf("\n5. DRIVER UTILIZATION:\n"))
    cat(sprintf("   Active drivers: %s\n", format(active_drivers, big.mark = ",")))
    cat(sprintf("   Drivers with rides in Nov: %s\n", format(drivers_with_rides, big.mark = ",")))
    cat(sprintf("   Utilization rate: %.1f%%\n", utilization_rate))
  }

  # 6. Revenue Concentration (80/20 Rule)
  revenue_by_driver <- merged_df %>%
    group_by(Driver_ID) %>%
    summarise(Revenue = sum(Fare)) %>%
    arrange(desc(Revenue))

  top_20_pct_count <- ceiling(nrow(revenue_by_driver) * 0.2)
  top_20_revenue <- sum(revenue_by_driver$Revenue[1:top_20_pct_count])
  total_revenue <- sum(revenue_by_driver$Revenue)
  bottom_80_revenue <- total_revenue - top_20_revenue

  cat(sprintf("\n6. REVENUE CONCENTRATION (80/20 RULE):\n"))
  cat(sprintf(
    "   Top 20%% of drivers (%d drivers) generate: $%s (%.1f%%)\n",
    top_20_pct_count,
    format(round(top_20_revenue, 2), big.mark = ","),
    (top_20_revenue / total_revenue) * 100
  ))
  cat(sprintf(
    "   Bottom 80%% of drivers (%d drivers) generate: $%s (%.1f%%)\n",
    nrow(revenue_by_driver) - top_20_pct_count,
    format(round(bottom_80_revenue, 2), big.mark = ","),
    (bottom_80_revenue / total_revenue) * 100
  ))
  cat("\n")
}

print_executive_summary <- function(rides_df, drivers_df) {
  cat("\n\n")
  cat(strrep("█", 80), "\n")
  cat(paste0(strrep(" ", 25), "EXECUTIVE SUMMARY\n"))
  cat(strrep("█", 80), "\n")

  total_rides <- nrow(rides_df)
  total_rev <- sum(rides_df$Fare)
  avg_fare <- mean(rides_df$Fare)
  avg_rating <- mean(rides_df$Rating)
  total_drivers <- nrow(drivers_df)
  active_drivers <- sum(drivers_df$Active_Status == "Active")
  unique_drivers_with_rides <- length(unique(rides_df$Driver_ID))

  cat(sprintf("\nKEY PERFORMANCE INDICATORS (November 2024):\n"))
  cat(strrep("=", 80), "\n")
  cat(sprintf("  Total Rides:              %10s\n", format(total_rides, big.mark = ",")))
  cat(sprintf("  Total Revenue:            $%9s\n", format(round(total_rev, 2), big.mark = ",")))
  cat(sprintf("  Average Fare:             $%9.2f\n", avg_fare))
  cat(sprintf("  Average Rating:           %10.2f/5.0\n", avg_rating))
  cat(sprintf("  Total Drivers:            %10s\n", format(total_drivers, big.mark = ",")))
  cat(sprintf("  Active Drivers:           %10s\n", format(active_drivers, big.mark = ",")))
  cat(sprintf("  Avg Rides/Driver:         %10.1f\n", total_rides / unique_drivers_with_rides))
  cat(sprintf("  Driver Utilization:       %10.1f%%\n", (unique_drivers_with_rides / active_drivers) * 100))

  # Geographic insights
  top_city_data <- rides_df %>%
    count(City) %>%
    arrange(desc(n)) %>%
    slice(1)

  cat(sprintf("\nGEOGRAPHIC PERFORMANCE:\n"))
  cat(strrep("=", 80), "\n")
  cat(sprintf(
    "  Top Performing City:      %s (%s rides)\n",
    top_city_data$City, format(top_city_data$n, big.mark = ",")
  ))
  cat(sprintf("  Cities Operating:         %d\n", length(unique(rides_df$City))))

  # Operational metrics
  avg_distance <- mean(rides_df$Distance_km)
  avg_duration <- mean(rides_df$Duration_min)
  avg_speed <- mean(rides_df$Speed_kmh)

  cat(sprintf("\nOPERATIONAL METRICS:\n"))
  cat(strrep("=", 80), "\n")
  cat(sprintf("  Avg Distance:             %10.2f km\n", avg_distance))
  cat(sprintf("  Avg Duration:             %10.1f min\n", avg_duration))
  cat(sprintf("  Avg Speed:                %10.1f km/h\n", avg_speed))

  # Customer satisfaction
  high_ratings <- sum(rides_df$Rating >= 4)
  low_ratings <- sum(rides_df$Rating < 3)

  cat(sprintf("\nCUSTOMER SATISFACTION:\n"))
  cat(strrep("=", 80), "\n")
  cat(sprintf(
    "  Ratings >=4.0:            %10s (%.1f%%)\n",
    format(high_ratings, big.mark = ","), (high_ratings / total_rides) * 100
  ))
  cat(sprintf(
    "  Ratings <3.0:             %10s (%.1f%%)\n",
    format(low_ratings, big.mark = ","), (low_ratings / total_rides) * 100
  ))

  # Promo impact
  promo_rides <- sum(rides_df$Has_Promo)
  promo_revenue <- sum(rides_df$Fare[rides_df$Has_Promo == 1])

  cat(sprintf("\nPROMOTIONAL IMPACT:\n"))
  cat(strrep("=", 80), "\n")
  cat(sprintf(
    "  Rides with Promos:        %10s (%.1f%%)\n",
    format(promo_rides, big.mark = ","), (promo_rides / total_rides) * 100
  ))
  cat(sprintf("  Revenue from Promos:      $%9s\n", format(round(promo_revenue, 2), big.mark = ",")))

  cat(sprintf("\n%s\n\n", strrep("=", 80)))
}

# ==============================================================================
# 5. MAIN EXECUTION BLOCK
# ==============================================================================

# 1. LOAD DATA
cat("🚀 Starting Analysis Pipeline...\n")
cat(strrep("█", 80), "\n")
rides_df <- read.csv("Rides_Data.csv", stringsAsFactors = FALSE)
drivers_df <- read.csv("Drivers_Data.csv", stringsAsFactors = FALSE)
cat("✓ Data loaded successfully\n\n")

# 2. PROFILE DATA
cat("\n", strrep("█", 80), "\n")
cat(" DATA PROFILING PHASE\n")
cat(strrep("█", 80), "\n")

rides_profile <- generate_profile_summary(rides_df, "RIDES DATA")
drivers_profile <- generate_profile_summary(drivers_df, "DRIVERS DATA")

identify_data_quality_issues(rides_df, "RIDES DATA")
identify_data_quality_issues(drivers_df, "DRIVERS DATA")

generate_summary_statistics(rides_df, drivers_df)
generate_categorical_summary(rides_df, drivers_df)
analyze_relationships(rides_df, drivers_df)

# 3. TRANSFORM DATA
cat("\n", strrep("█", 80), "\n")
cat(" DATA TRANSFORMATION PHASE\n")
cat(strrep("█", 80), "\n")

rides_df <- transform_rides_data(rides_df)
merged_df <- merge_datasets(rides_df, drivers_df)

# 4. RUN VISUALIZATIONS & ANALYSES
cat("\n", strrep("█", 80), "\n")
cat(" EXPLORATORY DATA ANALYSIS PHASE\n")
cat(strrep("█", 80), "\n\n")

cat("🚀 Starting Comprehensive EDA for CityRide...\n")
cat(strrep("=", 80), "\n")

plot_temporal_analysis(rides_df)
plot_geographic_analysis(rides_df)
plot_driver_performance(rides_df, drivers_df, merged_df)
plot_financial_analysis(rides_df)
plot_operational_analysis(rides_df)
plot_correlation_analysis(rides_df, drivers_df)

# 5. ADVANCED INSIGHTS
print_advanced_insights(rides_df, merged_df)

# 6. EXECUTIVE SUMMARY
print_executive_summary(rides_df, drivers_df)

# 7. EXPORT TO PDF
cat("\n", strrep("█", 80), "\n")
cat(" EXPORTING VISUALIZATIONS TO PDF\n")
cat(strrep("█", 80), "\n\n")

pdf_filename <- "CityRide_Analysis_Report.pdf"
cat(sprintf("Saving all plots to %s...\n", pdf_filename))

pdf(pdf_filename, width = 14, height = 10)
plot_temporal_analysis(rides_df)
plot_geographic_analysis(rides_df)
plot_driver_performance(rides_df, drivers_df, merged_df)
plot_financial_analysis(rides_df)
plot_operational_analysis(rides_df)
plot_correlation_analysis(rides_df, drivers_df)
dev.off()

cat("\n✅ EDA Complete! All visualizations saved.\n")
cat("📁 Generated files:\n")
cat("   - CityRide_Analysis_Report.pdf\n")
cat("\n", strrep("=", 80), "\n")
cat("✓ Analysis Complete!\n")
cat(strrep("█", 80), "\n")
