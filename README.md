# CityRide Analytics Dashboard

A comprehensive data analytics solution for CityRide, a ride-sharing platform, transforming operational data into actionable business insights through exploratory analysis, data warehousing, and interactive visualization.

[![Tableau](https://img.shields.io/badge/Tableau-Dashoard-27569a.svg?logo=data:image/svg+xml;base64,PHN2ZyByb2xlPSJpbWciIHZpZXdCb3g9IjAgMCAyNCAyNCIgeG1sbnM9Imh0dHA6Ly93d3cudzMub3JnLzIwMDAvc3ZnIiBpZD0iVGFibGVhdS0tU3RyZWFtbGluZS1TaW1wbGUtSWNvbnMiIGhlaWdodD0iMjQiIHdpZHRoPSIyNCI+CiAgPGRlc2M+CiAgICBUYWJsZWF1IFN0cmVhbWxpbmUgSWNvbjogaHR0cHM6Ly9zdHJlYW1saW5laHEuY29tCiAgPC9kZXNjPgogIDx0aXRsZT5UYWJsZWF1PC90aXRsZT4KICA8cGF0aCBkPSJNMTEuNjU0IDAuMTc0djIuMjAzSDkuNjgydjAuNThoMS45NzJWNS4xNmgwLjY5NlYyLjk1N2gxLjk3di0wLjU4aC0xLjk3VjAuMTc0aC0wLjM0OHptNi4wMyAyLjI2MiAtMC4wMDIgMS42MjN2MS42MjNoLTIuOTU3djAuOTI3aDIuOTU3djMuMTg4aDEuMDQzbDAuMDExIC0xLjU4MiAwLjAyIC0xLjU3NiAxLjQ2NSAtMC4wMiAxLjQ2IC0wLjAxdi0wLjkyN2gtMi45NTNWMi40MzZoLTAuNTIyem0tMTIuNDA3IDAuMDZ2My4xOUgyLjI5MXYwLjkyNWgyLjk4NnYzLjE5aDAuOTg1VjYuNjFoMy4wMTN2LTAuOTI1SDYuMjYyVjIuNDk2SDUuNzd6bTYuMDg2IDUuMjd2My41OTNIOC4wNnYxLjE4OGgzLjMwNHYzLjU5NmgxLjI4di0zLjU5NmgzLjMwOXYtMS4xODhoLTMuMzFWNy43NjZoLTAuNjM3em05LjcyMSAxLjU1djIuMjIxaC0yLjAxMnYwLjgxMWgyLjAxMnYyLjI2MWgwLjg4N3YtMi4yNjFIMjR2LTAuODExaC0yLjAyOXYtMi4yMmgtMC40MjJ6bS0xOS4xMTEgMC4xMzF2Mi4xNzRIMHYwLjYyMWgxLjk3M3YyLjE5NGgwLjY2N3YtMi4xOTRoMnYtMC42MkgyLjYwOVY5LjQ0NmgtMC4zMTh6bTE1LjcwOSA0LjUxNnYzLjI1NGgtMy4wMTZ2MC45MjdoMy4wMTZ2My4yMTdoMS4wNzJ2LTMuMjE2aDIuOTg2di0wLjkyOGgtMi45ODZ2LTMuMjU0aC0wLjUzM3ptLTEyLjQ2MyAwLjAwOHYzLjI0NkgyLjI2MnYwLjkyOGgyLjk1N3YzLjE4OUg2LjMydi0zLjE4OWgyLjk1NXYtMC45MjhINi4zMlYxMy45N2gtMC41NXptNi4zMTYgNC41NzggMC4wMDIgMS4xMDN2MS4xSDkuNTY2djAuODEyaDEuOTcxdjIuMjYyaDAuOTI4bDAuMDEyIC0xLjExOSAwLjAxNyAtMS4xNDNoMS45Njl2LTAuODEyaC0ydi0yLjIwM2gtMC40NjV6IiBmaWxsPSIjZmZmZmZmIiBzdHJva2Utd2lkdGg9IjEiPjwvcGF0aD4KPC9zdmc+)](https://public.tableau.com/app/profile/david.cod/viz/FinalProject_Tableau_Group30/RevenueRides)
[![R](https://img.shields.io/badge/R-Analysis-1b68bc?logo=r&logoColor=white)](https://github.com/zakyikow/CBS_CityRide-Analytics/tree/main/1_R_data_prep_cleaning_EDA)
[![Postgres](https://img.shields.io/badge/Postgres-Database-31648d?logo=postgresql&logoColor=white)](https://github.com/zakyikow/CBS_CityRide-Analytics/tree/main/2_SQL_data_transformation)

<br>

<table>
  <tr>
    <td width="30%" align="center"><img src="4_Tableau/Tableau_Dashboard1_RevenueRides.png" width="100%" /></td>
    <td width="30%" align="center"><img src="4_Tableau/Tableau_Dashboard2_GeographicAnalytics.png" width="100%" /></td>
    <td width="30%" align="center"><img src="4_Tableau/Tableau_Dashboard3_DriversPerformance.png" width="100%" /></td>
  </tr>
  <tr>
    <td align="center"><b>Revenue & Rides</b></td>
    <td align="center"><b>Geographic Analytics</b></td>
    <td align="center"><b>Drivers Performance</b></td>
  </tr>
</table>

<br>

## 📊 Project Overview

This project analyzes ride-sharing operations across 5 US cities, examining 1,000 rides and 100 drivers to identify performance patterns, optimize promotional strategies, and improve driver engagement. The solution provides a scalable data infrastructure and interactive dashboards for ongoing operational monitoring.

**Key Deliverables:**
- Exploratory data analysis revealing temporal and geographic patterns
- PostgreSQL star schema data warehouse with ETL pipeline
- Three-dashboard Tableau workbook for performance monitoring
- Data-driven recommendations for weekday growth and promotion optimization

<br>

## 🎯 Key Findings

- **Temporal Patterns**: Weekend demand peaks (Saturday: 161 rides) but weekday rides are 8% longer and generate higher average fares ($45.07 vs $43.48)
- **Promotion Overuse**: 73% of rides use promo codes, indicating discounts have become expected rather than strategic
- **Geographic Balance**: Revenue distributed evenly across markets (LA 21.8%, Chicago 21.2%, NY 19.4%)
- **Driver Engagement Gap**: 55% of registered drivers inactive; top 20% generate only 30% of revenue (flatter than typical 80/20 distribution)

<br>

## 🗂️ Repository Structure

```
CBS_CityRide-Analytics/
│
├── 0_raw_data/                                      # Original source data
│   ├── Drivers_Data.csv
│   └── Rides_Data.csv
│
├── 1_R_data_prep_cleaning_EDA/                      # R analysis workflow
│   ├── cityride_analysis.R                          # Complete R analysis script
│   └── CityRide_Analysis_Report.pdf                 # Detailed methodology report
│
├── 2_SQL_data_transformation/                       # Database implementation
│   ├── schema.sql                                   # Star schema DDL & ETL
│   └── star_schema.png                              # Database architecture diagram
│
├── 3_final_data/                                    # Analysis-ready dataset
│   └── vw_rides_analysis.csv                        # Unified analytical view
│
├── 4_Tableau/                                       # Interactive visualizations
│   ├── FinalProject_Tableau_Group30.twbx            # Packaged Tableau workbook
│   ├── Tableau_Dashboard1_RevenueRides.png          # Dashboard 1 preview
│   ├── Tableau_Dashboard2_GeographicAnalytics.png   # Dashboard 2 preview
│   └── Tableau_Dashboard3_DriversPerformance.png    # Dashboard 3 preview
│
├── .gitattributes
└── README.md
```

<br>

## 🛠️ Technologies Used

**Analysis & Visualization**
- **R**: Data cleaning, exploratory analysis, feature engineering
- **PostgreSQL**: Data warehousing with star schema architecture
- **Tableau**: Interactive dashboards with drill-down capabilities

**Key R Packages**: `dplyr`, `ggplot2`, `lubridate`

<br>

## 🔍 Database Design

Implemented star schema optimized for analytical queries:

**Fact Table**: `fact_rides` (ride transactions with pre-calculated metrics)

**Dimensions**: `dim_date`, `dim_city`, `dim_driver`, `dim_promo`

**ETL Process**: Three-stage pipeline (staging → transformation → fact/dimension loading) with data quality controls and derived metric calculations

<br>

## 📈 Dashboard Architecture

The Tableau workbook follows Shneiderman's "overview first, zoom and filter, details on demand" principle:

1. **Revenue & Rides**: Primary monitoring dashboard with KPIs, daily trends, weekday patterns, and promotional analysis
2. **Geographic Analytics**: City-level performance comparison with revenue share, distance/duration metrics, and map visualization
3. **Drivers Performance**: Driver segmentation, rating analysis, and individual ride details

<br>

## 💡 Business Recommendations

1. **Weekday Growth Strategy**: Implement weekday-specific driver incentives (5-10% higher payouts) and distance-based promotions targeting commuters (e.g., 10% off rides >23km, Monday-Friday only)

2. **Promotion Restructuring**: Move from blanket discounts to tiered, distance-based codes (10% off >20km, 15% off >30km) to protect margins on short trips while incentivizing high-value rides

3. **Driver Engagement Program**: Introduce "Premium Driver Status" for top performers with priority trip matching and quarterly bonuses; reactivation campaigns for inactive drivers with targeted incentives

4. **City-Specific Strategies**: Customize promotional approaches based on market behavior (Chicago shows promo users take longer trips; LA shows opposite pattern)

<br>

## ⚠️ Known Limitations

- **Timeframe**: Single-month snapshot (November 2024) limits seasonality analysis
- **Missing Cost Data**: Revenue tracking without driver payout or operational costs prevents profitability analysis
- **No Customer IDs**: Cannot track retention, lifetime value, or repeat usage patterns
- **External Context**: Lacks weather, traffic, and event data that may explain rating fluctuations

<br>

## 📊 View the Interactive Dashboard

[Live Tableau Dashboard](https://public.tableau.com/app/profile/david.cod/viz/FinalProject_Tableau_Group30/RevenueRides)

<br>
