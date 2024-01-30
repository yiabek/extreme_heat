library(broom)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(dplyr)
library(corrplot)
library(cowplot)
library(ggh4x)


### Try filtering heatwaves by the number of days? By duration?
### Try a metric that is simply the extent multiplied by intensity instead of cumulative intensity which 
### is basically just extent all over again?
### What about cold season heatwaves? What is the literature on those?

## File name for characteristics CSV
filename <- "/Users/yiannabekris/Documents/record_smashers/csv/characteristics_3sig_250k_19400101_20221231.csv"
writename <- "/Users/yiannabekris/Documents/record_smashers/csv/processed_char_3sig_250k_19400301_20221130.csv"
writename_raw <- "/Users/yiannabekris/Documents/record_smashers/csv/processed_char_3sig_250k_19400301_20221130_wide.csv"

## Load CSVs with data
event_char <- read.csv(filename)
nino34 <- read.csv("/Users/yiannabekris/Documents/energy_data/clim_indices/nino34_0_5_thresh.csv")
# nino34_fn <- "/Users/yiannabekris/Documents/record_smashers/csv/nino34_0_5_thresh_annual.csv"

## Convert December to the following year, so it is clustered with
## the following January and February
nino34 <- nino34 %>% mutate(
  Year = ifelse(
    Month == 12, Year + 1, Year
  )
)

## Remove January and February 1980 because there is no December 1949
nino34 <- nino34 %>% filter(
  !Month %in% c(1, 2) | !Year %in% 1950
)

## Remove December 2022
nino34 <- nino34 %>% filter(
  Year != 2023
)

# Seasonal Phases
nino34 <- nino34 %>%
  select(Year:Mode) %>%
  group_by(Year, Season) %>%
  summarise(Index = mean(Index))

# Make it so that the phase for DJF
# is the phase for the entire year
# # Comment out for monthly values
# nino34 <- nino34 %>%
#   select(Year:Mode) %>%
#   filter(Season=="DJF") %>%
#   group_by(Year) %>%
#   summarise(Index = mean(Index))

nino34 <- nino34 %>% mutate(Phase = case_when(
  Index > 0.5 ~ 'Positive',
  Index < -0.5 ~ 'Negative',
  Index <= 0.5 & Index >= -0.5 ~ 'Neutral'
))

# write.csv(nino34, nino34_fn)

# nino34_sub <- nino34[,-c(5,6)]


# Make it so that the phase for DJF
# is the phase for the entire year
# Comment out for monthly values
nino34_annual <- nino34 %>%
  select(Year:Mode) %>%
  filter(Season=="DJF") %>%
  group_by(Year) %>%
  summarise(Index = mean(Index))

nino34_annual <- nino34_annual %>% mutate(Phase = case_when(
  Index > 0.5 ~ 'Positive',
  Index < -0.5 ~ 'Negative',
  Index <= 0.5 & Index >= -0.5 ~ 'Neutral'
))

nino34_annual_sub <- nino34_annual[,-2]

nino34_sub <- aggregate(Index ~ Season + Year, data=nino34, FUN=mean)

## El Nino first
elnino <- nino34_sub %>% 
  left_join(
    nino34_annual_sub,
    by = 
      c('Year'))

elnino$Cycle <- NA

i <- 4
while (i <= nrow(elnino)) {
  if (elnino$Season[i] == 'DJF' & elnino$Phase[i] == 'Positive') {
    elnino$Cycle[i] <- 'Positive'
    elnino$Cycle[i-1] <- 'Positive'
    elnino$Cycle[i-3] <- 'Positive'
    elnino$Cycle[i+1] <- 'Positive'
    elnino$Cycle[i+2] <- 'Positive'
  }
  i <- i + 1
}

elnino <- elnino %>%
  filter(
    Cycle == 'Positive'
  )

## Now La Nina
lanina <- nino34_sub %>% 
  left_join(
    nino34_annual_sub,
    by = 
      c('Year'))

lanina$Cycle <- NA

i <- 4
while (i <= nrow(lanina)) {
  if (lanina$Season[i] == 'DJF' & lanina$Phase[i] == 'Negative') {
    lanina$Cycle[i] <- 'Negative'
    lanina$Cycle[i-1] <- 'Negative'
    lanina$Cycle[i-3] <- 'Negative'
    lanina$Cycle[i+1] <- 'Negative'
    lanina$Cycle[i+2] <- 'Negative'
  }
  i <- i + 1
}

lanina <- lanina %>%
  filter(
    Cycle == 'Negative'
  )

## Neutral
neutral <- nino34_sub %>% 
  left_join(
    nino34_annual_sub,
    by = 
      c('Year'))

neutral$Cycle <- NA

i <- 4
while (i <= nrow(neutral)) {
  if (neutral$Season[i] == 'DJF' & neutral$Phase[i] == 'Neutral') {
    neutral$Cycle[i] <- 'Neutral'
    neutral$Cycle[i-1] <- 'Neutral'
    neutral$Cycle[i-3] <- 'Neutral'
    neutral$Cycle[i+1] <- 'Neutral'
    neutral$Cycle[i+2] <- 'Neutral'
  }
  i <- i + 1
}

neutral <- neutral %>%
  filter(
    Cycle == 'Neutral'
  )

seasonal_nino34 <- rbind(elnino, lanina, neutral)


## Column names for the imported CSVs
column_names <- c('Date','Label','Extent','Land Extent',
                  'Event Land Fraction','CI',
                  'Intensity',
                  'Centroid Latitude','Centroid Longitude','Duration')

## Add the column names
colnames(event_char) <- column_names


### Create month, year, and day columns
event_char <- event_char %>%
  #na.omit() %>%
  dplyr:: mutate(Year = lubridate::year(Date),
                 Month = lubridate::month(Date))

## Convert December to the following year, so it is clustered with
## the following January and February
event_char <- event_char %>% mutate(
  Year = ifelse(
    Month == 12, Year + 1, Year
  )
)

## Remove January and February 1980 because there is no December 1979
event_char <- event_char %>% filter(
  !Month %in% c(1, 2) | !Year %in% 1940
)

## Remove December 2022
event_char <- event_char %>% filter(
  Year != 2023
)


# Create a sequence of dates covering the desired date range
start_date <- as.Date("1940-03-01")
end_date <- as.Date("2022-11-30")
all_dates <- seq(start_date, end_date, by="day")
all_dates <- all_dates[!(month(all_dates) == 2 & day(all_dates) == 29)]


## Convert to date
event_char$Date <- as.Date(event_char$Date)

# Use the 'complete()' function to fill in the missing dates
event_char_filled <- event_char %>%
  complete(Date = all_dates)


### Add seasons column
event_char_filled <- event_char_filled %>% 
  mutate(Season = case_when(
    Month %in% c(6, 7, 8)  ~ "JJA" ,
    Month %in% c(9, 10, 11)  ~ "SON"  ,
    Month %in% c(12, 1, 2)  ~ "DJF"  ,
    Month %in% c(3, 4, 5) ~ "MAM"
  )
  )


## Categorize each event as land, marine, or both
event_char_filled <- event_char_filled %>%
  mutate(`Event Type` = case_when(
    `Event Land Fraction` >= 0.5 ~ 'Land',
    `Event Land Fraction` < 0.5 ~ 'Marine',
    # `Event Land Fraction` > 0.45 & `Event Land Fraction` < 0.55 ~ 'Land-Marine'
  )
  )

## Categorize each event type by latitudinal zone
event_char_filled <- event_char_filled %>%
  mutate(`Latitudinal Zone` = case_when(
    `Centroid Latitude` >= -23.5 & `Centroid Latitude` <= 23.5 ~ 'Tropics',
    `Centroid Latitude` < -23.5 & `Centroid Latitude` >= -35 ~ 'Subtropics',
    `Centroid Latitude` > 23.5 & `Centroid Latitude` <= 35 ~ 'Subtropics',
    `Centroid Latitude` < -35 & `Centroid Latitude` >= -66 ~ 'Temperate',
    `Centroid Latitude` > 35 & `Centroid Latitude` <= 66 ~ 'Temperate',
    `Centroid Latitude` < -66 ~ 'Polar',
    `Centroid Latitude` >  66 ~ 'Polar'
  )
  )

## Convert columns that are character to numeric
event_char_filled <- event_char_filled %>%
  mutate(`Extent` = as.numeric(Extent))

event_char_filled <- event_char_filled %>%
  mutate(CI = as.numeric(CI))

event_char_filled <- event_char_filled %>%
  mutate(Intensity = as.numeric(Intensity))

event_char_filled <- event_char_filled %>%
  mutate(Duration = as.numeric(Duration))

## Make it so duration is only stored in one row for multi-day events
duration_df <- event_char_filled %>%
  na.omit() %>%
  group_by(Label, Year) %>%
  mutate(distinct_duration = ifelse(row_number() == which.min(Duration), Duration, NA)) %>%
  ungroup()

duration_df <- duration_df %>%
  select(-Duration) %>%
  rename("Duration" = "distinct_duration")

concurrence_prep <- duration_df %>%
  group_by(Date) %>%
  mutate(Concurrence = n()) %>%
  ungroup()

concurrence_df <- concurrence_prep %>%
  group_by(Date) %>%
  mutate(distinct_concurrence = ifelse(row_number() == which.min(Concurrence), Concurrence, NA)) %>%
  ungroup()

event_df <- concurrence_df %>%
  select(-Concurrence) %>%
  rename("Concurrence" = "distinct_concurrence")

### ========== Counts ========== ###
## Count all events
event_daily_count <- event_char_filled %>%
  group_by(
    Season, Year
  ) %>%
  summarise(`Daily Count` = sum(!is.na(Label))) %>%
  ungroup() %>%
  complete(Season, Year, fill = list(`Daily Count` = 0))

## Remove NAs
event_daily_count <- na.omit(event_daily_count)

## Count by the type of event (land or marine)
counts_by_type <- event_char_filled %>%
  group_by(
    Season, Year, `Event Type`
  ) %>%
  summarise(`Type Count` = sum(!is.na(`Event Type`))) %>%
  ungroup() %>%
  complete(Season, Year, `Event Type`, fill = list(`Type Count` = 0))

counts_by_type <- na.omit(counts_by_type)

counts_by_type <- counts_by_type %>%
  filter(!Season %in% c("DJF") | !Year %in% 1940)

## Count by latitudinal zone
counts_by_zone <- event_char_filled %>%
  group_by(
    Season, Year, `Latitudinal Zone`
  ) %>%
  summarise(`Zone Count` = sum(!is.na(`Latitudinal Zone`))) %>%
  ungroup() %>%
  complete(Season, Year, `Latitudinal Zone`, fill = list(`Zone Count` = 0))

counts_by_zone <- na.omit(counts_by_zone)

counts_by_zone <- counts_by_zone %>%
  filter(!Season %in% c("DJF") | !Year %in% 1940)


event_char_filled <- event_df %>% left_join(event_daily_count, by = c("Year", "Season"))

event_char_filled <- event_char_filled %>% left_join(counts_by_type, by = c("Year", "Season", "Event Type"))

event_char_filled <- event_char_filled %>% left_join(counts_by_zone, by = c("Year", "Season", "Latitudinal Zone"))


## Perform inner join on all data frames based on a common key column called 'common_key'
event_modes <- event_char_filled %>%
  left_join(nino34, by = c("Year","Season"))

## Levels so season is in the correct order
season_levels <- c('DJF', 'MAM', 'JJA', 'SON')
event_modes$Season <- factor(event_modes$Season, levels=season_levels)

### ====== WRITE UNPIVOTED CSV HERE!!!! ====== ###
# write_csv(event_modes, writename_raw)

p1 = ggplot(na.omit(event_modes), aes(y = Season, fill = Phase)) + 
  geom_bar(position = "fill", color = "black", size = 1) +
  scale_fill_manual(values = c("#FFCC00","#999999","#663399")) +
  theme_bw(base_size = 15) + theme(legend.position = "none") +
  ylab(NULL)
  #ggtitle('Probability of 2σ Heatwave by ENSO Phase 1950-2022')

p2 =ggplot(na.omit(event_modes), aes(y = `Event Type`, fill = Phase)) + 
  geom_bar(position = "fill", color = "black", size = 1) +
  scale_fill_manual(values = c("#FFCC00","#999999","#663399")) +
  theme_bw(base_size = 15) + theme(legend.position = "none")+
  ylab(NULL) #+
  #ggtitle('Probability of 2σ Heatwave by ENSO Phase 1950-2022')

p3 = ggplot(na.omit(event_modes), aes(y = `Latitudinal Zone`, fill = Phase)) + 
  geom_bar(position = "fill", color = "black", size = 1) +
  scale_fill_manual(values = c("#FFCC00","#999999","#663399")) +
  theme_bw(base_size = 15) +
  ylab(NULL) #+
  #ggtitle('Probability of 2σ Heatwave by ENSO Phase 1950-2022')


plot_row <- plot_grid(p1,p2,p3, align = "h",
                      ncol = 3, labels ="AUTO",
                      label_size = 22,
                      label_colour = "blue")
title <- ggdraw() +
  draw_label(
    "3σ Heatwaves by ENSO Phase (Seasonal Definition)",
    fontface = 'bold',
    x = 0,
    hjust = 0
  )+
  theme(plot.margin = margin(0, 7, 7, 0))# add margin on the left of the drawing canvas, so title is aligned with left edge of first plot
stackedPlot<-plot_grid(title, plot_row,ncol = 1,align = "h",
                       # rel_heights values control vertical title margins
                       rel_heights = c(0.1, 1), rel_widths = rep(1, 3))
stackedPlot

# nohi_extent <- event_modes %>%
#   filter(
#     Extent < 2500000
#   )

cumu_extent <- aggregate(Extent ~ Date + Season + Year, data=event_modes, FUN=sum)

# nohi_extent <- aggregate(Extent ~ Year + Season, data=nohi_extent, FUN=mean)

nohi_extent <- aggregate(Extent ~ Season + Year, data=cumu_extent, FUN=max)

nohi_extent_mods <- nohi_extent %>%
  group_by(Season) %>%
  na.omit() %>%
  group_modify(
    ## options: tidy, glance, augment
    ~ tidy(lm(Extent ~ Year, data = .))
  ) %>% ungroup()

nohi_extent_mods <- nohi_extent_mods %>%
  filter(term == "Year")

nohi_extent_mods <- nohi_extent_mods %>% mutate_if(is.numeric, ~ round(., 2))

season_levels <- c('DJF', 'MAM', 'JJA', 'SON')

nohi_extent_mods$Season <- factor(nohi_extent_mods$Season, levels=season_levels)

nino <- nino34 %>% filter(Phase=="Positive")
nina <- nino34 %>% filter(Phase=="Negative")

ggplot(data = na.omit(nohi_extent), aes(x = Year, y = Extent)) +
  geom_line(size = 0.65) +
  geom_smooth(method=lm) +
  scale_color_manual(values = c("blue", "purple", "red", "orange")) +
  theme_bw() + 
  facet_wrap(~Season) +
  theme_bw(base_size = 12) + 
  theme(
    strip.background = element_blank(),
    strip.text = element_text(color = "black", face = "bold"),
    title = element_text(size = 15, face = "bold"),
    strip.text.y = element_text(size = 12),
    strip.text.x = element_text(size = 12),
    axis.text = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
  ) +
  geom_text(size=5, data = nohi_extent_mods, check_overlap = TRUE,
            mapping = aes(x=Inf, y=Inf,
                          hjust=1.1, vjust=1.2,
                          label = paste('  Trend = ', estimate, '\n',
                                        'p-value = ', p.value))) +
  annotate("rect", fill = "darkred", alpha = 0.4,
           xmin = nino$Year, xmax = nino$Year + 1,
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "darkblue", alpha = 0.2,
           xmin = nina$Year, xmax = nina$Year + 1,
           ymin = -Inf, ymax = Inf) +
  ylab('') +
  ggtitle("Peak Cumulative Extent for 3σ Heatwaves and ENSO 1940-2022")



## Multiple variables stored in column names
events_longer <- event_modes %>% pivot_longer(
  cols = c('Daily Count', 'Type Count', 'Zone Count', 'Extent',
           'CI',
           'Intensity',
           'Duration',
           'Concurrence'),
  names_to = "Metric",
  values_to = "Quantity"
)

events_longer <- events_longer %>% mutate(
  Quantity = if_else(
    Metric %in% c('CI','Duration','Concurrence','Extent'), log(Quantity), Quantity
  )
)


### ====== WRITE THE CSV HERE!!!! ====== ###
# write_csv(events_longer, writename)

## Multiple variables stored in column names
# events_longer_type <- event_type_modes %>% pivot_longer(
#   cols = c('Daily Count', 'Extent',
#            'CI',
#            'Intensity',
#            'Duration',
#            'Concurrence'),
#   names_to = "Metric",
#   values_to = "Quantity"
# )
# 
# ## Multiple variables stored in column names
# events_longer_zone <- event_type_zones %>% pivot_longer(
#   cols = c('Daily Count', 'Extent',
#            'CI',
#            'Intensity',
#            'Duration',
#            'Concurrence'),
#   names_to = "Metric",
#   values_to = "Quantity"
# )

season_levels <- c('DJF', 'MAM', 'JJA', 'SON')

# Convert all_dates to a data frame with day, month, and year columns
mode_start_date <- as.Date("1950-03-01")
mode_end_date <- as.Date("2022-11-30")
mode_dates <- seq(mode_start_date, mode_end_date, by="day")
mode_dates <- mode_dates[!(month(mode_dates) == 2 & day(mode_dates) == 29)]


date_df <- as.data.frame(all_dates)
date_df$Day <- day(all_dates)
date_df$Month <- month(all_dates)
date_df$Year <- year(all_dates)

date_df$Date <- date_df$all_dates

### Add seasons column
date_df <- date_df %>% 
  mutate(Season = case_when(
    Month %in% c(6, 7, 8)  ~ "JJA" ,
    Month %in% c(9, 10, 11)  ~ "SON"  ,
    Month %in% c(12, 1, 2)  ~ "DJF"  ,
    Month %in% c(3, 4, 5) ~ "MAM"
  )
  )

mode_date_df <- as.data.frame(mode_dates)

mode_date_df$Day <- day(mode_dates)
mode_date_df$Month <- month(mode_dates)
mode_date_df$Year <- year(mode_dates)

mode_date_df$Date <- mode_date_df$mode_dates

### Add seasons column
mode_date_df <- mode_date_df %>% 
  mutate(Season = case_when(
    Month %in% c(6, 7, 8)  ~ "JJA" ,
    Month %in% c(9, 10, 11)  ~ "SON"  ,
    Month %in% c(12, 1, 2)  ~ "DJF"  ,
    Month %in% c(3, 4, 5) ~ "MAM"
  )
  )



# Count the total number of days in each season
season_counts <- date_df %>%
  group_by(Season) %>%
  summarize(`Total Days` = n())

event_counts <- event_modes %>%
  group_by(Season) %>%
  summarize(`Heatwave Days` = n_distinct(Date))

total_events <- event_counts %>% left_join(season_counts, by='Season')
total_events$`All Probability` <- total_events$`Heatwave Days` / total_events$`Total Days` 

# write.csv(total_events, "/Users/yiannabekris/Documents/record_smashers/csv/total_probability.csv", row.names = FALSE)

all_counts <- event_modes %>%
  na.omit() %>%
  group_by(Season, `Latitudinal Zone`, `Event Type`) %>%
  summarize(`Zone and Type Heatwave Days` = n_distinct(Date))

all_events <- all_counts %>% left_join(season_counts, by='Season')
all_events$`Zone and Type Probability` <- all_events$`Zone and Type Heatwave Days` / total_events$`Total Days` 

# # all_mode_counts <- event_modes %>%
# #   na.omit() %>%
# #   group_by(Season, `Latitudinal Zone`, `Event Type`, Phase) %>%
# #   summarize(`Zone and Type Heatwave Days` = n_distinct(Date))
# #
# # all_events <- all_counts %>% left_join(season_counts, by='Season')
# # all_events$`Zone and Type Probability` <- all_events$`Zone and Type Heatwave Days` / total_events$`Total Days`
# 
# # type_counts <- event_types %>%
# #   group_by(Season, `Event Type`) %>%
# #   summarize(`Type Heatwave Days`= n_distinct(Date))
# #
# # type_events <- type_counts %>% left_join(season_counts, by='Season')
# # type_events$`Type Probability` <- type_events$`Type Heatwave Days` / type_events$`Total Days`

# zone_counts <- event_zones %>%
#   group_by(Season, `Latitudinal Zone`) %>%
#   summarize(`Zone Heatwave Days`= n_distinct(Date))
# 
# zone_events <- zone_counts %>% left_join(season_counts, by='Season')
# zone_events$`Zone Probability` <- zone_events$`Zone Heatwave Days` / zone_events$`Total Days` 

mode_season_counts <- mode_date_df %>%
  group_by(Season) %>%
  summarize(`Total Days` = n())

event_counts <- event_modes %>%
  filter(Year %in% c(1950:2022)) %>%
  group_by(Season) %>%
  summarize(`Heatwave Days` = n_distinct(Date))

mode_counts <- event_modes %>%
  group_by(Season, Phase) %>%
  summarize(`Phase Count` = n_distinct(Date))

# Pivot the mode_counts data to wide format
wide_mode_counts <- mode_counts %>%
  pivot_wider(names_from = Phase, values_from = `Phase Count`)

total_modes <- mode_counts %>% left_join(mode_season_counts, by='Season')
total_modes$Probability <- total_modes$`Phase Count` / total_modes$`Total Days` 

# write.csv(total_modes, "/Users/yiannabekris/Documents/record_smashers/csv/mode_probability.csv", row.names = FALSE)

fraction_data <- event_counts %>%
  left_join(wide_mode_counts, by = "Season") %>%
  group_by(Season) %>%
  mutate(Fraction_Positive = Positive / `Heatwave Days`,
         Fraction_Negative = Negative / `Heatwave Days`,
         Fraction_Neutral = Neutral / `Heatwave Days`,
         Ratio_Positive_Negative = Fraction_Positive / Fraction_Negative)

# write.csv(fraction_data, "/Users/yiannabekris/Documents/record_smashers/csv/mode_fractions.csv", row.names = FALSE)

total_modes_sub <- total_modes[,-c(3,4)]

total_modes_sub <- total_modes_sub %>%
  filter(
    Phase != "Neutral"
  )

# Calculate the ratio of the probability between positive and negative phases
ratio_modes <- total_modes_sub %>%
  na.omit() %>%
  pivot_wider(names_from = Phase, values_from = Probability) %>%
  mutate(Ratio_Positive_Negative = Positive / Negative)



# write.csv(ratio_modes, "/Users/yiannabekris/Documents/record_smashers/csv/mode_ratio.csv", row.names = FALSE)


### ============= By season first
# seasonal_means <- events_longer %>%
#   group_by(Year, Season, Metric, `Event Type`) %>%
#   summarise(Mean = mean(Quantity, na.rm=TRUE))

seasonal_means <- events_longer %>%
  group_by(Year, Season, Metric) %>%
  summarise(Mean = mean(Quantity, na.rm=TRUE))

seasonal_means <- seasonal_means %>% 
  mutate(Mean = ifelse(Mean == -Inf, 0, Mean))


#`Event Type`, `Latitudinal Zone`, 
linear_mean_mods <- seasonal_means %>%
  group_by(Season, Metric) %>%
  na.omit() %>%
  group_modify(
    ## options: tidy, glance, augment
    ~ tidy(lm(Mean ~ Year, data = .))
  ) %>% ungroup()

linear_mean_mods <- linear_mean_mods %>%
  filter(term == "Year")

linear_mean_mods <- linear_mean_mods %>% mutate_if(is.numeric, ~ round(., 2))

season_levels <- c('DJF', 'MAM', 'JJA', 'SON')

seasonal_means$Season <- factor(seasonal_means$Season, levels=season_levels)

ggpdata <- nino34_annual %>% filter(Phase == "Positive")

nino <- nino34_annual %>% filter(Phase=="Positive")
nina <- nino34_annual %>% filter(Phase=="Negative")

ggplot(data = na.omit(seasonal_means), aes(x=Year, y=Mean, color=Season)) +
  geom_line(size = 0.65) +
  scale_color_manual(values = c("blue", "purple", "red", "orange")) +
  theme_bw() + 
  facet_wrap(~Metric, scales = "free", ncol=2) +
  theme_bw(base_size = 12) + 
  theme(
    strip.background = element_blank(),
    strip.text = element_text(color = "black", face = "bold", size = 12),
    title = element_text(size = 15, face = "bold"),
    axis.text = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1),) +
  # ) +
  # annotate("rect", fill = "darkred", alpha = 0.4, 
  #          xmin = nino$Year, xmax = nino$Year + 1,
  #          ymin = -Inf, ymax = Inf) +
  # annotate("rect", fill = "darkblue", alpha = 0.2, 
  #          xmin = nina$Year, xmax = nina$Year + 1,
  #          ymin = -Inf, ymax = Inf) +
  ylab('') +
  ggtitle("Mean 3σ Heatwave Charactertistics and ENSO 1940-2022")

## Set hot event figure name
#fig_name <- '/Users/yiannabekris/Documents/record_smashers/figures/mean_char_2sig_metric_1940_2022.png'

## Save 
ggsave(fig_name,
       width = 12,
       height = 6, units = c("in"))


## Max characteristics
seasonal_peaks <- events_longer %>%
  group_by(Year, Season, Metric) %>%
  summarise(Peak = max(Quantity, na.rm=TRUE))

seasonal_peaks <- seasonal_peaks %>% 
  mutate(Peak = ifelse(Peak == -Inf, 0, Peak))

#`Event Type`, `Latitudinal Zone`, 
linear_peak_mods <- seasonal_peaks %>%
  group_by(Season, Metric) %>%
  na.omit() %>%
  group_modify(
    ## options: tidy, glance, augment
    ~ tidy(lm(Peak ~ Year, data = .))
  ) %>% ungroup()

linear_peak_mods <- linear_peak_mods %>%
  filter(term == "Year")

linear_peak_mods <- linear_peak_mods %>% mutate_if(is.numeric, ~ round(., 2))

season_levels <- c('DJF', 'MAM', 'JJA', 'SON')

seasonal_peaks$Season <- factor(seasonal_peaks$Season, levels=season_levels)

#ggpdata <- nino34_annual %>% filter(Phase == "Positive")

#nino <- nino34_annual %>% filter(Phase=="Positive")
#nina <- nino34_annual %>% filter(Phase=="Negative")


ggplot(data = na.omit(seasonal_peaks)
       %>% filter(
         Year %in% c(1980:2022)
       ), aes(x=Year, y=Peak, color=Season)) +
  geom_line(size = 0.65) +
  scale_color_manual(values = c("blue", "purple", "red", "orange")) +
  theme_bw() + 
  facet_wrap(~Metric, scales = "free", ncol=2) +
  theme_bw(base_size = 12) + 
  theme(
    strip.background = element_blank(),
    strip.text = element_text(color = "black", face = "bold", size = 12),
    title = element_text(size = 15, face = "bold"),
    axis.text = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1),
  ) +
  # annotate("rect", fill = "darkred", alpha = 0.4, 
  #          xmin = nino$Year, xmax = nino$Year + 1,
  #          ymin = -Inf, ymax = Inf) +
  # annotate("rect", fill = "darkblue", alpha = 0.2, 
  #          xmin = nina$Year, xmax = nina$Year + 1,
  #          ymin = -Inf, ymax = Inf) +
  # ylab('') +
  ggtitle("Peak Heatwave Characteristics and ENSO 1940-2022")

## Set hot event figure name
fig_name <- '/Users/yiannabekris/Documents/record_smashers/figures/peak_char_metric_1940_2022.pdf'

## Save 
# ggsave(fig_name,
#        width = 12,
#        height = 6, units = c("in"))


### ============= By season first
### This is with different Event Types
# seasonal_means <- events_longer %>%
#   group_by(Year, Season, Metric, `Event Type`) %>%
#   summarise(Mean = mean(Quantity, na.rm=TRUE))

seasonal_means <- events_longer_type %>%
  group_by(Year, Season, Metric, `Event Type`) %>%
  summarise(Mean = mean(Quantity, na.rm=TRUE))

seasonal_means <- seasonal_means %>% 
  mutate(Mean = ifelse(Mean == -Inf, 0, Mean))

#`Event Type`, `Latitudinal Zone`, 
linear_mean_mods <- seasonal_means %>%
  group_by(Season, Metric, `Event Type`) %>%
  na.omit() %>%
  group_modify(
    ## options: tidy, glance, augment
    ~ tidy(lm(Mean ~ Year, data = .))
  ) %>% ungroup()

linear_mean_mods <- linear_mean_mods %>%
  filter(term == "Year")

linear_mean_mods <- linear_mean_mods %>% mutate_if(is.numeric, ~ round(., 2))

season_levels <- c('DJF', 'MAM', 'JJA', 'SON')

seasonal_means$Season <- factor(seasonal_means$Season, levels=season_levels)

ggpdata <- nino34_annual %>% filter(Phase == "Positive")

nino <- nino34_annual %>% filter(Phase=="Positive")
nina <- nino34_annual %>% filter(Phase=="Negative")

ggplot(data = na.omit(seasonal_means), aes(x = Year, y = Mean, color = Season)) +
  geom_line(size = 0.65) +
  scale_color_manual(values = c("blue", "purple", "red", "orange")) +
  theme_bw() + 
  facet_grid(Metric ~ `Event Type`, scales = "free") +
  theme_bw(base_size = 12) + 
  theme(
    strip.background = element_blank(),
    strip.text = element_text(color = "black", face = "bold"),
    title = element_text(size = 15, face = "bold"),
    strip.text.y = element_text(size = 8),
    strip.text.x = element_text(size = 12),
    axis.text = element_text(size = 8),
    axis.text.x = element_text(angle = 45, hjust = 1),
  ) +
  annotate("rect", fill = "darkred", alpha = 0.4, 
           xmin = nino$Year, xmax = nino$Year + 1,
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "darkblue", alpha = 0.2, 
           xmin = nina$Year, xmax = nina$Year + 1,
           ymin = -Inf, ymax = Inf) +
  ylab('') +
  ggtitle("Mean Heatwave Charactertistics and ENSO 1940-2022")

## Set hot event figure name
fig_name <- '/Users/yiannabekris/Documents/record_smashers/figures/mean_char_enso_1940_2022.pdf'

## Save 
ggsave(fig_name,
       width = 12,
       height = 6, units = c("in"))


## Max characteristics
seasonal_peaks <- events_longer %>%
  group_by(Year, Season, Metric, `Event Type`) %>%
  summarise(Peak = max(Quantity, na.rm=TRUE))

seasonal_peaks <- seasonal_peaks %>% 
  mutate(Peak = ifelse(Peak == -Inf, 0, Peak))

#`Event Type`, `Latitudinal Zone`, 
linear_peak_mods <- seasonal_peaks %>%
  group_by(Season, Metric, `Event Type`) %>%
  na.omit() %>%
  group_modify(
    ## options: tidy, glance, augment
    ~ tidy(lm(Peak ~ Year, data = .))
  ) %>% ungroup()

linear_peak_mods <- linear_peak_mods %>%
  filter(term == "Year")

linear_peak_mods <- linear_peak_mods %>% mutate_if(is.numeric, ~ round(., 2))

season_levels <- c('DJF', 'MAM', 'JJA', 'SON')

seasonal_peaks$Season <- factor(seasonal_peaks$Season, levels=season_levels)

ggpdata <- nino34_annual %>% filter(Phase == "Positive")

nino <- nino34_annual %>% filter(Phase=="Positive")
nina <- nino34_annual %>% filter(Phase=="Negative")

ggplot(data = na.omit(seasonal_peaks), aes(x=Year, y=Peak, color=Season)) +
  geom_line(size = 0.65) +
  scale_color_manual(values = c("blue", "purple", "red", "orange")) +
  theme_bw() + 
  facet_grid(Metric ~ `Event Type`, scales = "free") +
  theme_bw(base_size = 9) + 
  theme(
    strip.background = element_blank(),
    strip.text = element_text(color = "black", face = "bold"),
    strip.text.y = element_text(size = 8),
    strip.text.x = element_text(size = 12),
    title = element_text(size = 15, face = "bold"),
    axis.text = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1),
  ) +
  annotate("rect", fill = "darkred", alpha = 0.4, 
           xmin = nino$Year, xmax = nino$Year + 1,
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "darkblue", alpha = 0.2, 
           xmin = nina$Year, xmax = nina$Year + 1,
           ymin = -Inf, ymax = Inf) +
  ylab('') +
  ggtitle("Peak Heatwave Characteristics and ENSO 1940-2022")

## Set hot event figure name
fig_name <- '/Users/yiannabekris/Documents/record_smashers/figures/peak_char_enso_1940_2022.pdf'

## Save 
ggsave(fig_name,
       width = 12,
       height = 6, units = c("in"))




### ===================== By Latitudinal Zone ===================== ###
zone_means <- events_longer_zone %>%
  group_by(Year, Season, `Latitudinal Zone`, Metric) %>%
  summarise(Mean = mean(Quantity, na.rm = TRUE))

#`Event Type`, `Latitudinal Zone`, 
zone_mods <- zone_means %>%
  group_by(Season, `Latitudinal Zone`, Metric) %>%
  na.omit() %>%
  group_modify(
    ## options: tidy, glance, augment
    ~ tidy(lm(Mean ~ Year, data = .))
  ) %>% ungroup()

zone_mods <- zone_mods %>%
  filter(term == "Year")

zone_mods <- zone_mods %>% mutate_if(is.numeric, ~ round(., 2))

zone_means$Season <- factor(zone_means$Season, levels=season_levels)

ggplot(data = na.omit(zone_means), aes(x=Year, y=Mean, color=Season)) +
  geom_line(size = 0.65) +
  scale_color_manual(values = c("blue", "purple", "red", "orange")) +
  facet_grid(Metric ~ `Latitudinal Zone`, scales="free") +
  theme_bw(base_size = 9) + 
  theme(
    strip.background=element_blank(),
    strip.text=element_text(color="black", face="bold"),
    strip.text.y = element_text(size = 8),
    strip.text.x = element_text(size = 12),
    title=element_text(size=15,face="bold"),
    axis.text = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1),
  ) +
  # geom_text(size=3, data = zone_mods, check_overlap = TRUE,
  #           mapping = aes(x=Inf, y=Inf, 
  #                         hjust=1.1, vjust=1.2,
  #                         label = paste('  Trend = ', estimate, '\n',
  #                                       'p-value = ', p.value))) +
  annotate("rect", fill = "darkred", alpha = 0.4, 
           xmin = nino$Year, xmax = nino$Year + 1,
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "darkblue", alpha = 0.2, 
           xmin = nina$Year, xmax = nina$Year + 1,
           ymin = -Inf, ymax = Inf) +
  ylab('') +
  ggtitle("Mean Heatwave Characteristics by Latitudinal Zone 1940-2022") 

## Set hot event figure name
fig_name <- '/Users/yiannabekris/Documents/record_smashers/figures/mean_char_enso_lat_zone_1940_2022.pdf'

## Save 
ggsave(fig_name,
       width = 12,
       height = 6, units = c("in"))


## Max and lat zone
#`Event Type`, `Latitudinal Zone`, 
zone_peaks <- events_longer_zone %>%
  group_by(Year, Season, `Latitudinal Zone`, Metric) %>%
  summarise(Peak = max(Quantity, na.rm = TRUE))

zone_peaks <- zone_peaks %>% 
  mutate(Peak = ifelse(Peak == -Inf, 0, Peak))

zone_peak_mods <- zone_peaks %>%
  group_by(Season, `Latitudinal Zone`, Metric) %>%
  na.omit() %>%
  group_modify(
    ## options: tidy, glance, augment
    ~ tidy(lm(Peak ~ Year, data = .))
  ) %>% ungroup()

zone_peak_mods <- zone_peak_mods %>%
  filter(term == "Year")

zone_peak_mods <- zone_peak_mods %>% mutate_if(is.numeric, ~ round(., 2))

zone_peaks$Season <- factor(zone_peaks$Season, levels=season_levels)

ggplot(data = na.omit(zone_peaks), aes(x=Year, y=Peak, color=Season)) +
  geom_line(size = 0.65) +
  scale_color_manual(values = c("blue", "purple", "red", "orange")) +
  facet_grid(Metric ~ `Latitudinal Zone`, scales="free") +
  theme_bw(base_size = 12) + 
  theme(
    strip.background=element_blank(),
    strip.text=element_text(color="black", face="bold"),
    strip.text.y = element_text(size = 8),
    strip.text.x = element_text(size = 12),
    title=element_text(size=15,face="bold"),
    axis.text = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1),
  ) +
  # geom_text(size=3, data = zone_mods, check_overlap = TRUE,
  #           mapping = aes(x=Inf, y=Inf, 
  #                         hjust=1.1, vjust=1.2,
  #                         label = paste('  Trend = ', estimate, '\n',
  #                                       'p-value = ', p.value))) +
  annotate("rect", fill = "darkred", alpha = 0.4, 
           xmin = nino$Year, xmax = nino$Year + 1,
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "darkblue", alpha = 0.2, 
           xmin = nina$Year, xmax = nina$Year + 1,
           ymin = -Inf, ymax = Inf) +
  ylab('') +
  ggtitle("Peak Heatwave Characteristics by Latitudinal Zone 1940-2022") 

## Set hot event figure name
fig_name <- '/Users/yiannabekris/Documents/record_smashers/figures/peak_char_enso_lat_zone_1940_2022.pdf'

## Save 
ggsave(fig_name,
       width = 12,
       height = 6, units = c("in"))


## For probability
unique_date_count <- event_char_filled %>%
  group_by(Season) %>%
  distinct(Date) %>%
  count()

# events_enso$Probability <- 

events_enso$Season <- factor(events_enso$Season, levels=seasonal_levels)


# Calculate y-axis limits for each facet
facet_limits <- events_longer %>%
  na.omit() %>%
  group_by(Metric) %>%
  summarise(ymin = quantile(Quantity, 0.05), ymax = quantile(Quantity, 0.95))




### Boxplots
### ============= General ENSO Boxplots ============= ### 
ggplot(na.omit(events_longer) %>%
         filter(!Metric %in% c("Type Count", "Zone Count"))
       , aes(x=Season, y=Quantity)) +
  geom_boxplot(aes(fill=Phase, color=Phase)) + 
  scale_fill_manual(values = c("#FFCC00","grey","#9966FF"),
                    name = "Phase") +
  scale_color_manual(values = c("darkgoldenrod","darkgrey","darkorchid"),
                    name = "Phase") +
  facet_wrap(~Metric,
             scales='free_y') +
  # facet_grid2(rows=vars(`Event Type`),
  #            cols=vars(Metric),
  #            scales='free',
  #            independent = "y") +
  # facet_grid(
  #   rows = vars(Season),
  #   cols = vars(NERC.Region_f), scales = "free_y"
  # ) + geom_text(
  #   data = events_longer, 
  #   aes(x=Mode, y=104, label = sig), 
  #   vjust = 1
  # ) + 
  labs(x="Metric") + ylab('') + 
  ggtitle('Distribution of 2σ Heatwave Characteristics and ENSO (annual) 1950-2022') +
  theme_bw() + theme(strip.background=element_blank()) +
  theme(strip.text=element_text(color="black", face="bold", size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1),
        title=element_text(size = 15, face = "bold")) 

## Set hot event figure name
fig_name <- '/Users/yiannabekris/Documents/record_smashers/figures/char_2sig_enso_ann_boxplots_1940_2022.png'

## Save 
ggsave(fig_name,
       width = 12,
       height = 6, units = c("in"))

### ============= Event Type ENSO Boxplots ============= ### 
ggplot(na.omit(events_longer_type), aes(x=Season, y=Quantity)) +
  geom_boxplot(aes(fill=Phase)) + 
  scale_fill_manual(values = c("#FFCC00","grey","#9966FF"),
                    name = "Phase") +
  # facet_wrap(~Metric,
  #            scales='free_y') +
  facet_grid2(rows=vars(`Event Type`),
              cols=vars(Metric),
              scales='free',
              independent = "y") +
  # facet_grid(
  #   rows = vars(Season),
  #   cols = vars(NERC.Region_f), scales = "free_y"
  # ) + geom_text(
  #   data = events_longer, 
  #   aes(x=Mode, y=104, label = sig), 
  #   vjust = 1
  # ) + 
  labs(x="Metric") + ylab('') + 
  ggtitle('Distribution of Characteristics by Event Type and ENSO 1940-2022') +
  theme_bw() + theme(strip.background=element_blank()) +
  theme(strip.text=element_text(color="black", face="bold", size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1),
        title=element_text(size = 15, face = "bold")) 

## Set hot event figure name
fig_name <- '/Users/yiannabekris/Documents/record_smashers/figures/char_type_enso_boxplots_1940_2022.pdf'

## Save 
ggsave(fig_name,
       width = 12,
       height = 6, units = c("in"))

### ============= Latitudinal Zone ENSO Boxplots ============= ### 
ggplot(na.omit(events_longer_zone), aes(x=Season, y=Quantity)) +
  geom_boxplot(aes(fill=Phase)) + 
  scale_fill_manual(values = c("#FFCC00","grey","#9966FF"),
                    name = "Phase") +
  # facet_wrap(~Metric,
  #            scales='free_y') +
  facet_grid2(rows=vars(`Latitudinal Zone`),
             cols=vars(Metric),
             scales='free',
             independent = "y") +
  # facet_grid(
  #   rows = vars(Season),
  #   cols = vars(NERC.Region_f), scales = "free_y"
  # ) + geom_text(
  #   data = events_longer, 
  #   aes(x=Mode, y=104, label = sig), 
  #   vjust = 1
# ) + 
labs(x="Metric") + ylab('') + 
  ggtitle('Distribution of Characteristics by Latitudinal Zone and ENSO 1940-2022') +
  theme_bw() + theme(strip.background=element_blank()) +
  theme(strip.text=element_text(color="black", face="bold", size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1),
        title=element_text(size = 15, face = "bold")) 

## Set hot event figure name
fig_name <- '/Users/yiannabekris/Documents/record_smashers/figures/char_zone_enso_boxplots_1940_2022.pdf'

## Save 
ggsave(fig_name,
       width = 12,
       height = 6, units = c("in"))

library(ggplot2)
library(ggh4x)

# Calculate y-axis limits for each facet
facet_limits <- events_longer %>%
  na.omit() %>%
  group_by(Metric) %>%
  summarise(ymin = quantile(Quantity, 0.05), ymax = quantile(Quantity, 0.95))

### Boxplots
### ============= General ENSO Boxplots ============= ### 
ggplot(na.omit(events_longer), aes(x=Season, y=Quantity)) +
  geom_boxplot(aes(fill=Phase), outlier.shape=NA) + 
  scale_fill_manual(values = c("#FFCC00","grey","#9966FF"),
                    name = "Phase") +
  facet_grid2(vars(Metric),
              scales='free_y',
              cols=3) +  # You can adjust the expansion
  theme_bw() +
  theme(strip.background=element_blank(),
        strip.text=element_text(color="black", face="bold", size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1),
        title=element_text(size = 15, face = "bold")) +
  geom_blank(data = facet_limits, aes(y = ymin, ymax = ymax), inherit.aes = True)



##### =========================================
### ===========================================
### Separating by zone and latitude, with a separate panel for each season
#`Event Type`, `Latitudinal Zone`, 
all_means <- events_longer %>%
  group_by(Year, Season, Metric, `Event Type`, `Latitudinal Zone`) %>%
  summarise(Mean = mean(Quantity, na.rm = TRUE))

all_mods <- all_means %>%
  group_by(Season, `Event Type`, `Latitudinal Zone`, Metric) %>%
  na.omit() %>%
  group_modify(
    ## options: tidy, glance, augment
    ~ tidy(lm(Mean ~ Year, data = .))
  ) %>% ungroup()

all_mods <- all_mods %>%
  filter(term == "Year")

all_mods <- all_mods %>% mutate_if(is.numeric, ~ round(., 2))

ggplot(data = na.omit(all_means) %>%
         filter(
           Season == "SON"
         ), aes(x = Year, y = Mean))+
  geom_line(color = "black", size = 0.65) +
  scale_fill_brewer() + theme_bw() + 
  geom_smooth(method=lm, se=FALSE, size=0.6) + 
  facet_grid(Metric ~ `Event Type` + `Latitudinal Zone`, scales="free") +
  scale_fill_brewer() + theme_bw() + 
  theme_bw(base_size = 12) + 
  theme(
    strip.background=element_blank(),
    strip.text=element_text(color="black", face="bold", size = 12),
    title=element_text(size=15,face="bold"),
    axis.text = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1),
  ) +
  geom_smooth(method=lm, se=FALSE, size=0.8, color="darkred") + 
  geom_text(size=3, data = all_mods %>%
              filter(
                Season == "SON"
              ), check_overlap = TRUE,
            mapping = aes(x=Inf, y=Inf, 
                          hjust=1.1, vjust=1.2,
                          label = paste('  Trend = ', estimate, '\n',
                                        'p-value = ', p.value))) +
  ylab('') +
  ggtitle("SON Heatwave Trends 1940-2022") 

##### ================= MAXIMUM
### ============= By season first
seasonal_max <- events_longer %>%
  group_by(Year, Season, Metric) %>%
  summarise(Max = max(Quantity, na.rm = TRUE))

#`Event Type`, `Latitudinal Zone`, 
linear_peak_mods <- seasonal_max %>%
  group_by(Season, Metric) %>%
  na.omit() %>%
  group_modify(
    ## options: tidy, glance, augment
    ~ tidy(lm(Max ~ Year, data = .))
  ) %>% ungroup()

linear_mods <- linear_mods %>%
  filter(term == "Year")

linear_mods <- linear_mods %>% mutate_if(is.numeric, ~ round(., 2))

ggplot(data = na.omit(seasonal_max), aes(x = Year, y = Max))+
  geom_line(color = "black", size = 0.65) +
  scale_fill_brewer() + theme_bw() + 
  geom_smooth(method=lm, se=FALSE, size=0.6) + 
  facet_grid(Metric ~ Season, scales="free") +
  scale_fill_brewer() + theme_bw() + 
  theme_bw(base_size = 12) + 
  theme(
    strip.background=element_blank(),
    strip.text=element_text(color="black", face="bold", size = 12),
    title=element_text(size=15,face="bold"),
    axis.text = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1),
  ) +
  geom_smooth(method=lm, se=FALSE, size=0.8, color="darkred") + 
  geom_text(size=3, data = linear_mods, check_overlap = TRUE,
            mapping = aes(x=Inf, y=Inf, 
                          hjust=1.1, vjust=1.2,
                          label = paste('  Trend = ', estimate, '\n',
                                        'p-value = ', p.value))) +
  ylab('') +
  ggtitle("Max Heatwave Trends 1940-2022") 

events_max <- events_longer %>%
  group_by(Year, Season, `Event Type`, Metric) %>%
  summarise(Max = max(Quantity, na.rm = TRUE))

#`Event Type`, `Latitudinal Zone`, 
events_mods <- events_max %>%
  group_by(Season, `Event Type`, Metric) %>%
  na.omit() %>%
  group_modify(
    ## options: tidy, glance, augment
    ~ tidy(lm(Max ~ Year, data = .))
  ) %>% ungroup()

events_mods <- events_mods %>%
  filter(term == "Year")

events_mods <- events_mods %>% mutate_if(is.numeric, ~ round(., 2))

ggplot(data = na.omit(seasonal_peaks), aes(x = Year, y = Max))+
  geom_line(color = "black", size = 0.65) +
  scale_fill_brewer() + theme_bw() + 
  geom_smooth(method=lm, se=FALSE, size=0.6) + 
  facet_grid(Metric~ Season  + `Event Type`, scales="free") +
  scale_fill_brewer() + theme_bw() + 
  theme_bw(base_size = 12) + 
  theme(
    strip.background=element_blank(),
    strip.text=element_text(color="black", face="bold", size = 12),
    title=element_text(size=15,face="bold"),
    axis.text = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1),
  ) +
  geom_smooth(method=lm, se=FALSE, size=0.8, color="darkred") + 
  geom_text(size=3, data = peak_mods, check_overlap = TRUE,
            mapping = aes(x=Inf, y=Inf, 
                          hjust=1.1, vjust=1.2,
                          label = paste('  Trend = ', estimate, '\n',
                                        'p-value = ', p.value))) +
  ylab('') +
  ggtitle("Maximum Heatwave Trends 1940-2022") 

### ===================== By Latitudinal Zone ===================== ###
zone_max <- events_longer %>%
  group_by(Year, Season, `Latitudinal Zone`, Metric) %>%
  summarise(Max = max(Quantity, na.rm = TRUE))

all_max <- all_max %>% 
  mutate(Max = ifelse(Peak == -Inf, 0, Peak))

#`Event Type`, `Latitudinal Zone`, 
zone_mods <- zone_max %>%
  group_by(Season, `Latitudinal Zone`, Metric) %>%
  na.omit() %>%
  group_modify(
    ## options: tidy, glance, augment
    ~ tidy(lm(Max ~ Year, data = .))
  ) %>% ungroup()

zone_mods <- zone_mods %>%
  filter(term == "Year")

zone_mods <- zone_mods %>% mutate_if(is.numeric, ~ round(., 2))

ggplot(data = na.omit(zone_max), aes(x = Year, y = Mean))+
  geom_line(color = "black", size = 0.65) +
  scale_fill_brewer() + theme_bw() + 
  geom_smooth(method=lm, se=FALSE, size=0.6) + 
  facet_grid(Metric ~ Season  + `Latitudinal Zone`, scales="free") +
  scale_fill_brewer() + theme_bw() + 
  theme_bw(base_size = 12) + 
  theme(
    strip.background=element_blank(),
    strip.text=element_text(color="black", face="bold", size = 12),
    title=element_text(size=15,face="bold"),
    axis.text = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1),
  ) +
  geom_smooth(method=lm, se=FALSE, size=0.8, color="darkred") + 
  geom_text(size=3, data = zone_mods, check_overlap = TRUE,
            mapping = aes(x=Inf, y=Inf, 
                          hjust=1.1, vjust=1.2,
                          label = paste('  Trend = ', estimate, '\n',
                                        'p-value = ', p.value))) +
  ylab('') +
  ggtitle("Max Heatwave Trends 1940-2022") 

##### =========================================
### ===========================================
### Separating by zone and latitude, with a separate panel for each season
#`Event Type`, `Latitudinal Zone`, 
all_max <- events_longer %>%
  group_by(Year, Season, Metric, `Event Type`, `Latitudinal Zone`) %>%
  summarise(Peak = max(Quantity, na.rm = FALSE))

all_max <- all_max %>% 
  mutate(Peak = ifelse(Peak == -Inf, 0, Peak))

all_mods <- all_max %>%
  group_by(Season, `Event Type`, `Latitudinal Zone`, Metric) %>%
  na.omit() %>%
  group_modify(
    ## options: tidy, glance, augment
    ~ tidy(lm(Peak ~ Year, data = .))
  ) %>% ungroup()

all_mods <- all_mods %>%
  filter(term == "Year")

all_mods <- all_mods %>% mutate_if(is.numeric, ~ round(., 2))

ggplot(data = na.omit(all_max) %>%
         filter(
           Season == "JJA"
         ), aes(x = Year, y = Peak))+
  geom_line(color = "black", size = 0.65) +
  scale_fill_brewer() + theme_bw() + 
  geom_smooth(method=lm, se=FALSE, size=0.6) + 
  facet_grid(Metric ~ `Event Type` + `Latitudinal Zone`, scales="free") +
  scale_fill_brewer() + theme_bw() + 
  theme_bw(base_size = 12) + 
  theme(
    strip.background=element_blank(),
    strip.text=element_text(color="black", face="bold", size = 12),
    title=element_text(size=15,face="bold"),
    axis.text = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1),
  ) +
  geom_smooth(method=lm, se=FALSE, size=0.8, color="darkred") + 
  geom_text(size=3, data = all_mods %>%
              filter(
                Season == "JJA"
              ), check_overlap = TRUE,
            mapping = aes(x=Inf, y=Inf, 
                          hjust=1.1, vjust=1.2,
                          label = paste('  Trend = ', estimate, '\n',
                                        'p-value = ', p.value))) +
  ylab('') +
  ggtitle("JJA Max Heatwave Trends 1940-2022") 




### ======================== Modes ======================== ###
modes_plot <- left_join(modes_stack, events_longer, 
                        by = c(
                          "Year", "Month", "Season"
  
))


### ============= Cumulative Intensity Boxplots ============= ### 
ggplot(na.omit(modes_plot), aes(x=Mode, y=Quantity)) +
  geom_boxplot(aes(fill=Phase)) + 
  scale_fill_manual(values = c("#FFCC00","grey", "#9966FF"),
                    name = "Phase") +
  facet_grid(
    rows = vars(Metric),
    cols = vars(Season),
    scales = "free_y"
  ) +
  theme_bw(base_size = 12) + 
  theme(
    strip.background=element_blank(),
    strip.text=element_text(color="black", face="bold", size = 12),
    title=element_text(size=15,face="bold"),
    axis.text = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1),
  )

# events_sub <- event_char[,-c(1,12,13)]
events_sub <- event_modes[, -1]

events_djf <- events_sub %>%
  filter(Season=="DJF")

# Using mutate_all
events_djf <- events_djf %>%
  na.omit() %>%
  mutate_all(as.numeric)

corr_events <- cor(events_djf)

corrplot(corr_events, method = 'ellipse', order = 'AOE', type = 'upper')

land_events <- events_sub %>%
  filter(
    `Event Land Fraction` >= 0.5
  )

land_corr <- cor(land_events)

corrplot(land_corr, method = 'ellipse', order = 'AOE', type = 'upper')

ocean_events <- events_sub %>%
  filter(
    `Event Land Fraction` < 0.5
  )

ocean_corr <- cor(ocean_events)

corrplot(ocean_corr, method = 'ellipse', order = 'AOE', type = 'upper')


tropical_events <- events_sub %>%
  filter(
    `Centroid Latitude` <= 23 & `Centroid Latitude` >= -23
  )

tropical_corr <- cor(tropical_events)  

corrplot(tropical_corr, method = 'ellipse', order = 'AOE', type = 'upper')

### Peaks
events_peaks <- events_longer %>%
  group_by(Year, Season, `Event Type`, Metric) %>%
  summarise(Peak = max(Quantity, na.rm = TRUE))

#`Event Type`, `Latitudinal Zone`, 
peak_mods <- events_peaks %>%
  group_by(Season, `Event Type`, Metric) %>%
  na.omit() %>%
  group_modify(
    ## options: tidy, glance, augment
    ~ tidy(lm(Peak ~ Year, data = .))
  ) %>% ungroup()

ggplot(data = na.omit(events_peaks), aes(x=Year, y=Mean, color=Season)) +
  geom_line(size = 0.65) +
  scale_color_manual(values = c("blue", "purple", "red", "orange")) +
  theme_bw() + 
  facet_grid(Metric ~ `Event Type`, scales = "free") +
  theme_bw(base_size = 12) + 
  theme(
    strip.background = element_blank(),
    strip.text = element_text(color = "black", face = "bold", size = 12),
    title = element_text(size = 15, face = "bold"),
    axis.text = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1),
  ) +
  annotate("rect", fill = "darkred", alpha = 0.2, 
           xmin = nino$Year, xmax = nino$Year + 1,
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "darkblue", alpha = 0.2, 
           xmin = nina$Year, xmax = nina$Year + 1,
           ymin = -Inf, ymax = Inf) +
  ylab('') +
  ggtitle("Peak Heatwave Charactertics and ENSO 1940-2022")

## Set hot event figure name
fig_name <- '/Users/yiannabekris/Documents/record_smashers/figures/peak_char_enso_1940_2022.pdf'

## Save 
ggsave(fig_name,
       width = 10,
       height = 8, units = c("in"))


peak_mods <- peak_mods %>%
  filter(term == "Year")

peak_mods <- peak_mods %>% mutate_if(is.numeric, ~ round(., 2))

ggplot(data = na.omit(events_peaks), aes(x = Year, y = Mean))+
  geom_line(color = "black", size = 0.65) +
  scale_fill_brewer() + theme_bw() + 
  geom_smooth(method=lm, se=FALSE, size=0.6) + 
  facet_grid(Metric~ Season  + `Event Type`, scales="free") +
  scale_fill_brewer() + theme_bw() + 
  theme_bw(base_size = 12) + 
  theme(
    strip.background=element_blank(),
    strip.text=element_text(color="black", face="bold", size = 12),
    title=element_text(size=15,face="bold"),
    axis.text = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1),
  ) +
  geom_smooth(method=lm, se=FALSE, size=0.8, color="darkred") + 
  geom_text(size=3, data = events_mods, check_overlap = TRUE,
            mapping = aes(x=Inf, y=Inf, 
                          hjust=1.1, vjust=1.2,
                          label = paste('  Trend = ', estimate, '\n',
                                        'p-value = ', p.value))) +
  
  ylab('') +
  ggtitle("Heatwave Trends 1940-2022") 
