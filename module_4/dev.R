
params <- c()

params$period = "day"

params$carrier_1 = "AA"
params$carrier_2 = "YV"

if (params$period == "day") {
  flights_ts <- flights %>%
    group_by(date) %>%
    reframe(mean_dep_delay = mean(dep_delay, na.rm = TRUE))
  
  by_carrier_ts <- flights %>%
    group_by(date, code, carrier_name) %>%
    reframe(mean_dep_delay = mean(dep_delay, na.rm = TRUE)) %>%
    filter(code %in% c(params$carrier_1, params$carrier_2))
}



ggplot(data = flights_ts) +
  geom_point(aes(x = date, y = mean_dep_delay)) +
  geom_line(data = by_carrier_ts, aes(x = date, y = mean_dep_delay, color = carrier_name))
