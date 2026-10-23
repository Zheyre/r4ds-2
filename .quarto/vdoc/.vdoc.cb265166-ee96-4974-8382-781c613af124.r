#
#
#
#
#
#
#
#
#
#| message: false
library(tidyverse)
library(readxl)
```
#
read_excel("data/us_births_1994_2014.xlsx") |>
  glimpse()
#
#
#
#| cache: true
births_tibble <- read_excel("data/us_births_1994_2014.xlsx") |>
  mutate(
    day_of_week = factor(
      day_of_week,
      levels = c("Sun", "Mon", "Tues", "Wed", "Thurs", "Fri", "Sat"),
      ordered = TRUE
    )
  )
#
#
#
births_tibble |>
  summarize(
    avg_births = mean(births),
    .by = c(month, date_of_month)
  ) |>
  mutate(month = factor(month.abb[month], levels = rev(month.abb))) |>
  ggplot(aes(x = date_of_month, y = month, fill = avg_births)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c() +
  scale_x_continuous(breaks = c(1, 5, 10, 15, 20, 25, 31)) +
  labs(
    title = "Average US Births by Calendar Day, 1994 to 2014",
    x = "Day of Month",
    y = NULL,
    fill = "Mean births"
  ) +
  theme_minimal()
#
#
#
births_tibble |>
  filter(month == 12, date_of_month %in% c(20:24, 25, 27:30)) |>
  summarize(
    christmas = births[date_of_month == 25],
    baseline = mean(births[date_of_month != 25]),
    weekday = day_of_week[date_of_month == 25],
    .by = year
  ) |>
  mutate(pct_of_baseline = 100 * christmas / baseline) |>
  ggplot(aes(x = year, y = pct_of_baseline)) +
  geom_line(color = "gray70") +
  geom_point(aes(color = weekday), size = 3) +
  labs(
    title = "Christmas Day Births Compared to Nearby Days",
    subtitle = "December 25 as a percentage of the average for Dec 20 to 24 and 27 to 30",
    x = "Year",
    y = "Percent of Baseline",
    color = "Christmas fell on"
  ) +
  theme_minimal()
#
#
#
#
births_model <- lm(
  births ~ factor(year) + factor(month) + day_of_week,
  data = births_tibble
)

summary(births_model)$r.squared
#
#
#
#
#
