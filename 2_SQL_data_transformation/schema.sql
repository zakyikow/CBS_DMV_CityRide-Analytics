-- ============================================================
-- CITYRIDE DATA WAREHOUSE - STAR SCHEMA IMPLEMENTATION
-- PostgreSQL DDL and ETL Scripts
-- ============================================================

CREATE SCHEMA IF NOT EXISTS cityride_dw;
SET search_path TO cityride_dw;

-- ============================================================
-- STAGING TABLES (Landing zone for raw CSV data)
-- ============================================================

CREATE TABLE stg_rides (
    ride_id INTEGER,
    driver_id INTEGER,
    city VARCHAR(50),
    date DATE,
    distance_km DECIMAL(10,2),
    duration_min INTEGER,
    fare DECIMAL(10,2),
    rating DECIMAL(3,2),
    promo_code VARCHAR(30)
);

CREATE TABLE stg_drivers (
    driver_id INTEGER,
    name VARCHAR(100),
    age INTEGER,
    city VARCHAR(50),
    experience_years INTEGER,
    average_rating DECIMAL(3,2),
    active_status VARCHAR(20)
);

COPY stg_rides FROM 'C:/Program Files/PostgreSQL/17/data/Rides_Data.csv' CSV HEADER;
COPY stg_drivers FROM 'C:/Program Files/PostgreSQL/17/data/Drivers_Data.csv' CSV HEADER;

-- ============================================================
-- DIMENSION TABLES
-- ============================================================

CREATE TABLE dim_date (
    date_key INTEGER PRIMARY KEY,
    full_date DATE NOT NULL UNIQUE,
    day_of_month INTEGER NOT NULL,
    day_name VARCHAR(10) NOT NULL,
    day_of_week INTEGER NOT NULL,
    week_of_year INTEGER NOT NULL,
    month INTEGER NOT NULL,
    month_name VARCHAR(10) NOT NULL,
    quarter INTEGER NOT NULL,
    year INTEGER NOT NULL,
    is_weekend BOOLEAN NOT NULL
);

CREATE TABLE dim_city (
    city_key SERIAL PRIMARY KEY,
    city_name VARCHAR(50) NOT NULL UNIQUE,
    region VARCHAR(50),
    country VARCHAR(50)
);

CREATE TABLE dim_promo (
    promo_key SERIAL PRIMARY KEY,
    promo_code VARCHAR(30) NOT NULL UNIQUE,
    is_promotional BOOLEAN NOT NULL,
	discount_percent DECIMAL(5,4) NOT NULL
);

CREATE TABLE dim_driver (
    driver_id INTEGER PRIMARY KEY,
    driver_name VARCHAR(100) NOT NULL,
    age INTEGER,
    city VARCHAR(50),
    experience_years INTEGER,
    average_rating DECIMAL(3,2),
    active_status VARCHAR(20)
);

-- ============================================================
-- FACT TABLE
-- ============================================================

CREATE TABLE fact_rides (
    ride_id INTEGER PRIMARY KEY,
    driver_id INTEGER NOT NULL REFERENCES dim_driver(driver_id),
    date_key INTEGER NOT NULL REFERENCES dim_date(date_key),
    city_key INTEGER NOT NULL REFERENCES dim_city(city_key),
    promo_key INTEGER NOT NULL REFERENCES dim_promo(promo_key),
    distance_km DECIMAL(10,2),
    duration_min INTEGER,
    fare_usd DECIMAL(10,2),
    rating DECIMAL(3,2),
    fare_per_km DECIMAL(10,4),
    fare_per_min DECIMAL(10,4)
);

CREATE INDEX idx_fact_rides_driver ON fact_rides(driver_id);
CREATE INDEX idx_fact_rides_date ON fact_rides(date_key);
CREATE INDEX idx_fact_rides_city ON fact_rides(city_key);
CREATE INDEX idx_fact_rides_promo ON fact_rides(promo_key);

-- ============================================================
-- ETL: POPULATE DIMENSION TABLES
-- ============================================================

-- 1. Populate DIM_DATE (November 2024)
INSERT INTO dim_date (date_key, full_date, day_of_month, day_name, 
                      day_of_week, week_of_year, month, month_name, 
                      quarter, year, is_weekend)
SELECT 
    TO_CHAR(d, 'YYYYMMDD')::INTEGER AS date_key,
    d AS full_date,
    EXTRACT(DAY FROM d)::INTEGER,
    TRIM(TO_CHAR(d, 'Day')),
    EXTRACT(ISODOW FROM d)::INTEGER,
    EXTRACT(WEEK FROM d)::INTEGER,
    EXTRACT(MONTH FROM d)::INTEGER,
    TRIM(TO_CHAR(d, 'Month')),
    EXTRACT(QUARTER FROM d)::INTEGER,
    EXTRACT(YEAR FROM d)::INTEGER,
    EXTRACT(ISODOW FROM d) IN (6, 7)
FROM generate_series('2024-11-01'::DATE, '2024-11-30'::DATE, '1 day'::INTERVAL) AS d;

-- 2. Populate DIM_CITY with Region + Country
INSERT INTO dim_city (city_name, region, country)
SELECT DISTINCT 
    city AS city_name,

    CASE 
        -- Northeast
        WHEN city IN ('New York', 'Boston', 'Philadelphia', 'Baltimore', 'Washington') 
            THEN 'Northeast'

        -- Midwest
        WHEN city IN ('Chicago', 'Detroit', 'Cleveland', 'Columbus', 'Minneapolis') 
            THEN 'Midwest'

        -- South
        WHEN city IN ('Atlanta', 'Miami', 'Dallas', 'Houston', 'Charlotte', 'Orlando') 
            THEN 'South'

        -- West
        WHEN city IN ('Los Angeles', 'San Francisco', 'Seattle', 'Denver', 'Phoenix', 'Las Vegas') 
            THEN 'West'

        -- Default region
        ELSE 'Unknown'
    END AS region,

    'US' AS country   -- fixed for all cities

FROM stg_rides
WHERE city IS NOT NULL
ON CONFLICT (city_name) DO UPDATE SET
    region = EXCLUDED.region,
    country = EXCLUDED.country;


-- 3. Populate DIM_PROMO
INSERT INTO dim_promo (promo_code, is_promotional, discount_percent)
SELECT DISTINCT
    CASE 
        WHEN promo_code IS NULL OR TRIM(promo_code) = '' THEN 'NO_PROMO'
        ELSE promo_code
    END AS promo_code_clean,

    CASE 
        WHEN promo_code IS NULL OR TRIM(promo_code) = '' OR promo_code = 'NO_PROMO' 
            THEN FALSE
        ELSE TRUE
    END AS is_promotional,

    CASE 
        WHEN promo_code ~ '\d+' 
            THEN regexp_replace(promo_code, '\D', '', 'g')::DECIMAL / 100.0
        ELSE 0
    END AS discount_percent

FROM stg_rides
ON CONFLICT (promo_code) DO UPDATE SET
    is_promotional = EXCLUDED.is_promotional,
    discount_percent = EXCLUDED.discount_percent;



-- 4. Populate DIM_DRIVER
INSERT INTO dim_driver (driver_id, driver_name, age, city, experience_years, 
                        average_rating, active_status)
SELECT 
    driver_id,
    name,
    age,
    city,
    experience_years,
    average_rating,
    active_status
FROM stg_drivers
ON CONFLICT (driver_id) DO UPDATE SET
    driver_name = EXCLUDED.driver_name,
    age = EXCLUDED.age,
    city = EXCLUDED.city,
    experience_years = EXCLUDED.experience_years,
    average_rating = EXCLUDED.average_rating,
    active_status = EXCLUDED.active_status;

-- ============================================================
-- ETL: POPULATE FACT TABLE
-- ============================================================

INSERT INTO fact_rides (ride_id, driver_id, date_key, city_key, promo_key,
                        distance_km, duration_min, fare_usd, rating,
                        fare_per_km, fare_per_min)
SELECT 
    r.ride_id,
    r.driver_id,
    TO_CHAR(r.date, 'YYYYMMDD')::INTEGER,
    c.city_key,
    p.promo_key,
    r.distance_km,
    r.duration_min,
    r.fare,
    r.rating,
    CASE WHEN r.distance_km > 0 
         THEN ROUND(r.fare / r.distance_km, 4) 
         ELSE NULL END,
    CASE WHEN r.duration_min > 0 
         THEN ROUND(r.fare / r.duration_min, 4) 
         ELSE NULL END
FROM stg_rides r
JOIN dim_driver d ON r.driver_id = d.driver_id
JOIN dim_city c ON r.city = c.city_name
JOIN dim_promo p 
    ON COALESCE(NULLIF(TRIM(r.promo_code), ''), 'NO_PROMO') = p.promo_code
ON CONFLICT (ride_id) DO NOTHING;

-- ============================================================
-- ANALYTICAL VIEW (For Tableau)
-- ============================================================

CREATE OR REPLACE VIEW vw_rides_analysis AS
SELECT 
    f.ride_id,
    dt.full_date,
    dt.day_name,
    dt.day_of_week,
    dt.week_of_year,
    dt.is_weekend,
    c.city_name AS ride_city,
    c.region,
    c.country,
    d.driver_id,
    d.driver_name,
    d.age AS driver_age,
    d.city AS driver_city,
    d.experience_years,
    d.average_rating AS driver_avg_rating,
    d.active_status,
    p.promo_code,
    p.is_promotional,
	p.discount_percent,
    f.distance_km,
    f.duration_min,
    f.fare_usd,
    f.rating AS ride_rating,
    f.fare_per_km,
    f.fare_per_min
FROM fact_rides f
JOIN dim_date dt ON f.date_key = dt.date_key
JOIN dim_city c ON f.city_key = c.city_key
JOIN dim_driver d ON f.driver_id = d.driver_id
JOIN dim_promo p ON f.promo_key = p.promo_key;