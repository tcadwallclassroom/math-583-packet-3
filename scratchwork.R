# https://www.ncdc.noaa.gov/cdo-web/datasets

library(tidyverse)
X1976_2020_house <- read_csv("1976-2020-house.csv")

house_2012 <- 
  X1976_2020_house %>% 
  filter(year==2012) %>% 
  mutate(dist_id = paste(year, state_po, district), .after = state_po)

house_2012_dem <- 
  house_2012 %>%
  filter(party == "DEMOCRAT") %>% 
  mutate(dem_votes = candidatevotes)

house_2012_dem_reduced <- house_2012_dem[, c(4,22)]

house_2012_dem_reduced <- 
  house_2012_dem_reduced %>%
  group_by(dist_id) %>%
  summarise(dem_votes = sum(dem_votes, na.rm = T))


house_2012_rep <- 
  house_2012 %>%
  filter(party == "REPUBLICAN") %>% 
  mutate(rep_votes = candidatevotes)

house_2012_rep_reduced <- house_2012_rep[, c(4,22)]

house_2012_rep_reduced <- 
  house_2012_rep_reduced %>%
  group_by(dist_id) %>%
  summarise_each(funs(sum))

house_2012_reduced <- 
  full_join(house_2012_dem_reduced,house_2012_rep_reduced,by = "dist_id") %>% 
  arrange(dist_id)

house_2012_reduced <- 
  house_2012_reduced %>% 
  replace(is.na(.),0)

house_2012_reduced <- 
  house_2012_reduced %>% 
  mutate(total_votes = (dem_votes + rep_votes)) %>% 
  mutate(pct_dem_votes = dem_votes / total_votes * 100) %>% 
  mutate(pct_rep_votes = rep_votes / total_votes * 100)

house_2012_PA <- house_2012_reduced %>% 
  filter(grepl("PA", dist_id))

median_dem_pct_PA <- median(house_2012_PA$pct_dem_votes)
mean_dem_pct_PA <- mean(house_2012_PA$pct_dem_votes)

statsholder <- data.frame(stat = c("Median",
                                     "Mean"),
                          value = c(median_dem_pct_PA,
                                     mean_dem_pct_PA))

house_2012_PA %>% 
  ggplot(aes(x = pct_dem_votes)) +
  scale_color_brewer(palette = "Dark2") +
  geom_histogram(binwidth=1, color = "black", fill = "blue") + 
  geom_vline(data=statsholder ,aes(xintercept = value,
                               linetype = stat,
                               color = stat),size=1) +
  labs(title = "Percentage of Democratic votes by district",
       subtitle = "2012 Pennsylvania House race", 
       y = NULL,
       x = "Percentage of Democratic votes")

# ---- keep the state more general

state = "CA"

house_2012_state <- house_2012_reduced %>% 
  filter(grepl(state, dist_id))

median_dem_pct_state <- median(house_2012_state$pct_dem_votes)
mean_dem_pct_state <- mean(house_2012_state$pct_dem_votes)

statsholder <- data.frame(stat = c("Median",
                                   "Mean"),
                          value = c(median_dem_pct_state,
                                    mean_dem_pct_state))

house_2012_state %>% 
  ggplot(aes(x = pct_dem_votes)) +
  scale_color_brewer(palette = "Dark2") +
  geom_histogram(binwidth=1, color = "black", fill = "blue") + 
  geom_vline(data=statsholder ,aes(xintercept = value,
                                   linetype = stat,
                                   color = stat),size=1) +
  labs(title = "Percentage of Democratic votes by district",
       subtitle = paste("2012", state, "House race"), 
       y = NULL,
       x = "Percentage of Democratic votes")

