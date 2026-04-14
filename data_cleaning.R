### Set up
library(readxl)
library(readr)
library(dplyr)
library(lubridate)
library(purrr)
library(cellranger)
library(ggplot2)
library(scales)

### Import & join
## CAISO Load
# folder
hourly_load_dir <- "~/R/Huang_Liang_Shen_Chu_ENV797_TSA_FinalProject/hourly_load"

# all xlsx files
load_files <- list.files(hourly_load_dir, pattern = "\\.xlsx$", full.names = TRUE)

# simple reader
read_one_load <- function(f) {
  
  df <- read_excel(
    f,
    col_names = TRUE,
    col_types = "text"  
  )
  
  df <- df[, 1:7]
  names(df) <- c("date", "hr", "pge", "sce", "sdge", "vea", "caiso")
  
  df %>%
    mutate(
      date_raw = trimws(date),
      #parsing
      date = case_when(
        grepl("^[0-9]+$", date_raw) ~ 
          as.Date(as.numeric(date_raw), origin = "1899-12-30"),
        
        grepl("/", date_raw) ~ 
          lubridate::mdy(date_raw),
        
        TRUE ~ as.Date(NA)
      ),
      
      hr = as.integer(hr),
      pge = as.numeric(pge),
      sce = as.numeric(sce),
      sdge = as.numeric(sdge),
      vea = as.numeric(vea),
      caiso = as.numeric(caiso),
      source_file = basename(f)
    ) %>%
    select(date, hr, caiso, pge, sce, sdge, vea, source_file)
}

# import all files
load_list <- lapply(load_files, read_one_load)
load_hourly <- bind_rows(load_list)

# check
head(load_hourly)
summary(load_hourly$date)

# There are 38 NAs, need to figure out where thoses are.
load_hourly %>%
  filter(is.na(date)) %>%
  select(date_raw = source_file, hr, caiso) %>%
  head(20)

# It seems that these are just empty rows, since the hr and caiso columns are also empty, so as a next step, I'll just delete them.
load_daily <- load_hourly %>%
  group_by(date) %>%
  summarise(
    daily_avg_load  = mean(caiso),
    daily_peak_load = max(caiso),
    n_hours = n(),
    .groups = "drop"
  )

table(load_daily$n_hours)
# It is wierd and I inspected one day with 38 hours, so I'll filter out the bad day and check.
load_daily %>%
  filter(n_hours == 38)
bad_date <- load_daily %>%
  filter(n_hours == 38) %>%
  pull(date)
load_hourly %>%
  filter(date == bad_date) %>%
  arrange(hr)
# re-run everything filtering out NAs
load_hourly <- load_hourly %>%
  filter(
    !is.na(date),
    !is.na(hr),
    !is.na(caiso)
  )
load_daily <- load_hourly %>%
  group_by(date) %>%
  summarise(
    daily_avg_load  = mean(caiso),
    daily_peak_load = max(caiso),
    n_hours = n(),
    .groups = "drop"
  )
write.csv(load_daily, "caiso_daily_load_clean.csv", row.names = FALSE)

## historical temp
temp_dir <- "~/R/Huang_Liang_Shen_Chu_ENV797_TSA_FinalProject/temp"
temp_files <- list.files(temp_dir, pattern = "\\.csv$", full.names = TRUE)

read_one_temp <- function(f) {
  
  df <- read_csv(f, show_col_types = FALSE)
  
  df %>%
    rename_with(tolower) %>%
    mutate(
      date = ymd(date),
      tavg = as.numeric(tavg),
      tmax = as.numeric(tmax),
      tmin = as.numeric(tmin),
      tobs = as.numeric(tobs),
      source_file = basename(f)
    ) %>%
    select(station, name, date, tavg, tmax, tmin, tobs, source_file)
}

temp_list <- lapply(temp_files, read_one_temp)
temp_raw <- bind_rows(temp_list)

head(temp_raw)
summary(temp_raw$date)

# group by date
temp_daily <- temp_raw %>%
  group_by(date) %>%
  summarise(
    tavg_mean = mean(tavg, na.rm = TRUE),
    tmax_mean = mean(tmax, na.rm = TRUE),
    tmin_mean = mean(tmin, na.rm = TRUE),
    tobs_mean = mean(tobs, na.rm = TRUE),
    n_stations = n_distinct(station),
    .groups = "drop"
  ) %>%
  arrange(date)

temp_daily <- temp_daily %>%
  mutate(
    tavg_mean = ifelse(
      is.na(tavg_mean),
      (tmax_mean + tmin_mean) / 2,
      tavg_mean
    )
  )
write_csv(temp_daily, "temp_daily.csv")

### join datasets
final_data <- load_daily %>%
  left_join(temp_daily, by = "date")

write_csv(final_data, "caiso_load_temp_final.csv")

# load time series
ggplot(final_data, aes(x = date, y = daily_avg_load)) +
  geom_line() +
  scale_x_date(
    date_breaks = "6 months", 
    date_labels = "%Y/%m"
  ) +
  labs(title = "Daily Average CAISO Load", y = "MW", x = "Date")

## check to only look at after 2024
ggplot(final_data %>% filter(date >= "2024-01-01"),
       aes(x = date, y = daily_avg_load)) +
  geom_line() +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  labs(title = "Daily Load (2024)", y = "MW", x = "Month")

# temp time series
ggplot(final_data, aes(x = date, y = tavg_mean)) +
  geom_line(color = "orange") +
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y"
  ) +
  labs(
    title = "Daily Average Temperature (California)",
    y = "°F",
    x = "Date"
  ) +
  theme_minimal()
## check to only look at after 2024
ggplot(final_data %>% filter(date >= "2024-01-01"),
       aes(x = date, y = tavg_mean)) +
  geom_line(color = "orange") +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b"
  ) +
  labs(
    title = "Daily Temperature (2024)",
    y = "°F",
    x = "Month"
  ) +
  theme_minimal()

# scatter plot
ggplot(final_data, aes(x = tavg_mean, y = daily_avg_load)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess", color = "red") +
  labs(
    title = "Electricity Load vs Temperature",
    x = "Temperature (°F)",
    y = "Load (MW)"
  )
