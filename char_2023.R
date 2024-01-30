library(broom)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(dplyr)
library(corrplot)
library(ggh4x)
library(cowplot)

## File name for characteristics CSV
filename <- "/Users/yiannabekris/Documents/record_smashers/csv/characteristics_3sig_100peak_19400101_20221231.csv"
fn_2023 <- "/Users/yiannabekris/Documents/record_smashers/csv/characteristics_3sig_100k_2022-12-01_2023-11-30.csv"
writename <- "/Users/yiannabekris/Documents/record_smashers/csv/processed_char_3sig_100kpeak_19400301_20231130_seas_cyc.csv"
writename_raw <- "/Users/yiannabekris/Documents/record_smashers/csv/processed_char_3sig_100kpeak_19400301_20231130_wide.csv"

## Load CSVs with data
event_1940_2022 <- read_csv(filename)
event_2023 <- read_csv(fn_2023)


column_names <- c('Date','Label','Extent','Land Extent',
                  'Event Land Fraction','CI',
                  'Intensity', 'Peak Intensity',
                  'Centroid Latitude','Centroid Longitude','Duration')

colnames(event_1940_2022) <- column_names
colnames(event_2023) <- column_names

event_2023_valid <- event_2023 %>%
  filter(Label > 0)

event_char <- rbind(event_1940_2022, event_2023_valid)
nino34_fn <- "/Users/yiannabekris/Documents/record_smashers/csv/nino34_0_5_thresh_seas_cyc.csv"
nino34_csv <- "/Users/yiannabekris/Desktop/ersst5.nino.mth.91-20.csv"
nino34 <- read.csv(nino34_csv, head = TRUE, sep="")

nino34 <- nino34 %>%
  rename(
    "Month" = "MON",
    "Year" = "YR",
    "Index" = "ANOM.3"
  )


## Convert December to the following year, so it is clustered with
## the following January and February
nino34 <- nino34 %>% mutate(
  Year = ifelse(
    Month == 12, Year + 1, Year
  )
)

## Remove January and February 1980 because there is no December 1979
nino34 <- nino34 %>% filter(
  !Month %in% c(1, 2) | !Year %in% 1950
)

## Remove December 2022
nino34 <- nino34 %>% filter(
  Year != 2024
)

nino34_sub <- nino34[,c(1,2,10)]

nino34_sub <- nino34_sub %>%
  mutate(
    Season = case_when(
      Month %in% c(6, 7, 8)  ~ "JJA" ,
      Month %in% c(9, 10, 11)  ~ "SON"  ,
      Month %in% c(12, 1, 2)  ~ "DJF"  ,
      Month %in% c(3, 4, 5) ~ "MAM"
    )
  )

nino34_seasonal <- aggregate(Index ~ Season + Year, FUN=mean, data=nino34_sub)

nino34_annual <- nino34_seasonal %>%
  filter(Season=="DJF") %>%
  group_by(Year) %>%
  summarise(Index = mean(Index))

nino34_annual <- nino34_annual %>% mutate(Phase = case_when(
  Index > 0.5 ~ 'Positive',
  Index < -0.5 ~ 'Negative',
  Index <= 0.5 & Index >= -0.5 ~ 'Neutral'
))



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
  Year != 2024
)

## Convert to date
event_char$Date <- as.Date(event_char$Date)

# Create a sequence of dates covering the desired date range
start_date <- as.Date("1940-03-01")
end_date <- as.Date("2023-11-30")
all_dates <- seq(start_date, end_date, by="day")
all_dates <- all_dates[!(month(all_dates) == 2 & day(all_dates) == 29)]

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
  mutate(`Heatwave Type` = case_when(
    `Event Land Fraction` > 0.8 ~ 'Terrestrial',
    `Event Land Fraction` < 0.2 ~ 'Marine',
    `Event Land Fraction` >= 0.2 & `Event Land Fraction` <= 0.8 ~ 'Terrestrial-Marine'
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

event_char_filled <- event_char_filled %>%
  mutate(Hemisphere = case_when(
    `Centroid Latitude` >  0 ~ "Northern",
    `Centroid Latitude` <= 0 ~ "Southern",
    # `Centroid Latitude` == 0 ~ "Both"
  ))

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
# counts_by_type <- event_char_filled %>%
#   group_by(
#     Season, Year, `Heatwave Type`, Label
#   ) %>%
#   summarise(`Type Count` = sum(!is.na(`Heatwave Type`))) %>%
#   ungroup() %>%
#   complete(Season, Year, `Heatwave Type`, fill = list(`Type Count` = 0))

counts_by_type <- event_char_filled %>%
  group_by(Season, Year, `Heatwave Type`) %>%
  summarise(`Heatwave Count` = n()) %>%
  ungroup() %>%
  complete(Season, Year, `Heatwave Type`, fill = list(`Heatwave Count` = 0)) %>%
  na.omit()

# counts_by_type <- na.omit(counts_by_type)

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

## Count by the type of event (land or marine)
counts_by_hem <- event_char_filled %>%
  group_by(
    Season, Year, Hemisphere
  ) %>%
  summarise(`Hemisphere Count` = sum(!is.na(Hemisphere))) %>%
  ungroup() %>%
  complete(Season, Year, Hemisphere, fill = list(`Hemisphere Count` = 0))

counts_by_hem <- na.omit(counts_by_hem)

counts_by_hem <- counts_by_hem %>%
  filter(!Season %in% c("DJF") | !Year %in% 1940)

event_char_filled <- event_df %>% left_join(event_daily_count, by = c("Year", "Season"))

event_char_filled <- event_char_filled %>% left_join(counts_by_type, by = c("Year", "Season", "Heatwave Type"))

event_char_filled <- event_char_filled %>% left_join(counts_by_zone, by = c("Year", "Season", "Latitudinal Zone"))

event_char_filled <- event_char_filled %>% left_join(counts_by_hem, by = c("Year", "Season", "Hemisphere"))

## Perform inner join on all data frames
event_modes <- event_char_filled %>%
  left_join(nino34_annual, by = c("Year"))


elnino_years <- nino34_annual %>%
  filter(
    Phase == 'Positive'
  )

elnino_years <- aggregate(Index ~ Year, data=elnino_years, FUN=max)

elnino_maxyears <- elnino_years %>%
  arrange(desc(Index)) %>%  # Arrange in descending order by Index
  slice_head(n = 8)

lanina_years <- nino34_annual %>%
  filter(
    Phase == 'Negative'
  )


lanina_years <- aggregate(Index ~ Year, data=lanina_years, FUN=max)

lanina_maxyears <- lanina_years %>%
  arrange(Index) %>%  # Arrange in ascending order by Index
  slice_head(n = 8)

### Dateframe with all extent listed
cumu_extent <- event_modes %>%
  group_by(Year, `Heatwave Type`) %>%
  summarise(Extent = sum(Extent, na.rm = TRUE)) %>%
  ungroup() %>%
  complete(`Heatwave Type`, Year, fill = list(Extent = 0)) %>%
  mutate(Extent = replace_na(Extent, 0))

## Add stat column
cumu_extent$Stat <- "Cumulative"

### Mean extent
mean_extent <- event_modes %>%
  group_by(Year, `Heatwave Type`) %>%
  summarise(Extent = median(Extent, na.rm = TRUE)) %>%
  ungroup() %>%
  complete(`Heatwave Type`, Year, fill = list(Extent = 0)) %>%
  mutate(Extent = replace_na(Extent, 0))

## Add stat column
mean_extent$Stat <- "Mean"

### Maximum extent
max_extent <- event_modes %>%
  group_by(Year, `Heatwave Type`) %>%
  summarise(Extent = max(Extent, na.rm = TRUE)) %>%
  ungroup() %>%
  complete(`Heatwave Type`, Year, fill = list(Extent = 0)) %>%
  mutate(Extent = replace_na(Extent, 0))

## Add stat column
max_extent$Stat <- "Maximum"

### Bind all together
all_extent <- rbind(cumu_extent, max_extent)

### Linear models
all_extent_mods <- all_extent %>%
  group_by(`Heatwave Type`, Stat) %>%
  na.omit() %>%
  group_modify(
    ## options: tidy, glance, augment
    ~ tidy(lm(Extent ~ Year, data = .))
  ) %>% ungroup()

### Filter for text
all_extent_mods <- all_extent_mods %>%
  filter(term == "Year")

### Round
all_extent_mods <- all_extent_mods %>% mutate_if(is.numeric, ~ round(., 4))

type_levs <- c('Terrestrial', 'Terrestrial-Marine', 'Marine')

all_extent$`Heatwave Type` <- factor(all_extent$`Heatwave Type`, levels=type_levs)
all_extent_mods$`Heatwave Type` <- factor(all_extent_mods$`Heatwave Type`, levels=type_levs)

### ======= CUMULATIVE EXTENT BY PHASE ================================== ###
ggplot(data=all_extent, aes(x=Year, y=Extent)) +
  geom_line(size = 0.65) +
  geom_smooth(method=lm, color='maroon', se=FALSE) +
  facet_grid2(cols=vars(`Heatwave Type`), rows=vars(Stat), scales='free_y', independent='y') +
  theme_bw(base_size = 12) + 
  ggtitle(expression(bold("Extent for 3σ 100,000" ~ km^2 ~" Heatwaves 1940-2023"))) +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(color = "black", face = "bold"),
    title = element_text(size = 15, face = "bold", hjust=0.5),
    strip.text.y = element_text(size = 12),
    strip.text.x = element_text(size = 12),
    axis.text = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
  ) +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_text(size=4, data = all_extent_mods, check_overlap = TRUE,
            mapping = aes(x=1940, y=Inf,
                          hjust=0.1, vjust=1.2,
                          label = paste(' Trend = ', estimate, '\n',
                                        'p-value = ', p.value))) +
  ylab(expression(bold('Area (km'^2*')'))) +
  geom_vline(xintercept = elnino_maxyears$Year, linetype = 'dashed', color = 'firebrick1', alpha=0.6) +
  geom_vline(xintercept = lanina_maxyears$Year, linetype = 'dashed', color = 'blue', alpha=0.6)

### CUMULATIVE EXTENT BY HEMISPHERE
# cumu_extent <- event_modes %>%
#   group_by(Year, `Heatwave Type`, Hemisphere) %>%
#   summarise(Extent = mean(Extent, na.rm = TRUE)) %>%
#   ungroup() %>%
#   complete(`Heatwave Type`, Hemisphere, Year, fill = list(Extent = 0)) %>%
#   mutate(Extent = replace_na(Extent, 0))
# 
# cumu_extent_mods <- cumu_extent %>%
#   # group_by(`Heatwave Type`, Hemisphere) %>%
#   group_by(`Heatwave Type`, Hemisphere) %>%
#   na.omit() %>%
#   group_modify(
#     ## options: tidy, glance, augment
#     ~ tidy(lm(Extent ~ Year, data = .))
#   ) %>% ungroup()
# 
# cumu_extent_mods <- cumu_extent_mods %>%
#   filter(term == "Year")
# 
# cumu_extent_mods <- cumu_extent_mods %>% mutate_if(is.numeric, ~ round(., 2))
# 
# ### ======= CUMULATIVE EXTENT BY PHASE ================================== ###
# ggplot(data=cumu_extent, aes(x=Year, y=Extent, color=`Heatwave Type`)) +
#   geom_line(size = 0.65) +
#   # geom_smooth(method=lm, color='maroon', se=FALSE) +
#   # scale_color_manual(values = c("blue", "grey", "red")) +
#   theme_bw() + 
#   facet_grid(rows=vars(Hemisphere)) +
#   theme_bw(base_size = 12) + 
#   ggtitle(expression(bold("Mean Extent for 3σ 100,000" ~ km^2 ~" Heatwaves 1940-2023"))) +
#   theme(
#     strip.background = element_blank(),
#     strip.text = element_text(color = "black", face = "bold"),
#     title = element_text(size = 15, face = "bold"),
#     strip.text.y = element_text(size = 12),
#     strip.text.x = element_text(size = 12),
#     axis.text = element_text(size = 12),
#     axis.text.x = element_text(angle = 45, hjust = 1),
#   ) +
#   # geom_text(size=4, data = cumu_extent_mods, check_overlap = TRUE,
#   #           mapping = aes(x=Inf, y=Inf,
#   #                         hjust=1.1, vjust=1.2,
#   #                         label = paste('  Trend = ', estimate, '\n',
#   #                                       'p-value = ', p.value))) +
#   # annotate("rect", fill = "darkred", alpha = 0.01,
#   #          xmin = elnino$Year, xmax = elnino$Year + 1,
#   #          ymin = -Inf, ymax = Inf) +
#   # annotate("rect", fill = "darkblue", alpha = 0.01,
#   #          xmin = lanina$Year, xmax = lanina$Year + 1,
#   #          ymin = -Inf, ymax = Inf) +
#   ylab(expression(bold('Area (km'^2*')'))) +
#   geom_vline(xintercept = elnino_maxyears$Year, linetype = 'dashed', color = 'firebrick1') +
#   geom_vline(xintercept = lanina_maxyears$Year, linetype = 'dashed', color = 'blue')
# 

### CUMULATIVE EXTENT
cumu_extent <- event_modes %>%
  group_by(Year, `Heatwave Type`) %>%
  summarise(Extent = max(Extent, na.rm = FALSE)) %>%
  ungroup() %>%
  complete(`Heatwave Type`, Year, fill = list(Extent = 0)) %>%
  mutate(Extent = replace_na(Extent, 0))

cumu_extent_mods <- cumu_extent %>%
  # group_by(`Heatwave Type`, Hemisphere) %>%
  group_by(`Heatwave Type`) %>%
  na.omit() %>%
  group_modify(
    ## options: tidy, glance, augment
    ~ tidy(lm(Extent ~ Year, data = .))
  ) %>% ungroup()

cumu_extent_mods <- cumu_extent_mods %>%
  filter(term == "Year")

cumu_extent_mods <- cumu_extent_mods %>% mutate_if(is.numeric, ~ round(., 2))

type_levs <- c('Terrestrial', 'Terrestrial-Marine', 'Marine')

cumu_extent$`Heatwave Type` <- factor(cumu_extent$`Heatwave Type`, levels=type_levs)
cumu_extent_mods$`Heatwave Type` <- factor(cumu_extent_mods$`Heatwave Type`, levels=type_levs)
  
### ======= CUMULATIVE EXTENT BY PHASE ================================== ###
ggplot(data=cumu_extent, aes(x=Year, y=Extent)) +
  geom_line(size = 0.65) +
  geom_smooth(method=lm, color='maroon', se=FALSE) +
  # scale_color_manual(values = c("blue", "grey", "red")) +
  theme_bw() + 
  facet_grid(rows=vars(`Heatwave Type`), scales='free_y') +
  theme_bw(base_size = 12) + 
  ggtitle(expression(bold("Maximum Extent for 3σ 100,000" ~ km^2 ~" Heatwaves 1940-2023"))) +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(color = "black", face = "bold"),
    title = element_text(size = 15, face = "bold"),
    strip.text.y = element_text(size = 12),
    strip.text.x = element_text(size = 12),
    axis.text = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
  ) +
  geom_text(size=4, data = cumu_extent_mods, check_overlap = TRUE,
            mapping = aes(x=Inf, y=Inf,
                          hjust=1.1, vjust=1.2,
                          label = paste('  Trend = ', estimate, '\n',
                                        'p-value = ', p.value))) +
  # annotate("rect", fill = "darkred", alpha = 0.01,
  #          xmin = elnino$Year, xmax = elnino$Year + 1,
  #          ymin = -Inf, ymax = Inf) +
  # annotate("rect", fill = "darkblue", alpha = 0.01,
  #          xmin = lanina$Year, xmax = lanina$Year + 1,
  #          ymin = -Inf, ymax = Inf) +
ylab(expression(bold('Area (km'^2*')'))) +
  geom_vline(xintercept = elnino_maxyears$Year, linetype = 'dashed', color = 'firebrick1') +
  geom_vline(xintercept = lanina_maxyears$Year, linetype = 'dashed', color = 'blue')


### Intensity
cumu_intensity <- event_modes %>%
  group_by(`Heatwave Type`, Season, Year) %>%
  summarise(`Peak Intensity` = max(`Peak Intensity`, na.rm = TRUE)) %>%
  ungroup() %>%
  complete(Season, Year, fill = list(`Peak Intensity` = 0)) %>%
  mutate(`Peak Intensity` = replace_na(`Peak Intensity`, 0))


cumu_intensity_mods <- cumu_intensity %>%
  group_by(`Heatwave Type`, Season) %>%
  # group_by(Season) %>%
  na.omit() %>%
  group_modify(
    ## options: tidy, glance, augment
    ~ tidy(lm(`Peak Intensity` ~ Year, data = .))
  ) %>% ungroup()

cumu_intensity_mods <- cumu_intensity_mods %>%
  filter(term == "Year")

cumu_intensity_mods <- cumu_intensity_mods %>% mutate_if(is.numeric, ~ round(., 2))

season_levels <- c('DJF', 'MAM', 'JJA', 'SON')

cumu_intensity$Season <- factor(cumu_intensity$Season, levels=season_levels)
cumu_intensity_mods$Season <- factor(cumu_intensity_mods$Season, levels=season_levels)

type_levs <- c('Terrestrial','Terrestrial-Marine','Marine')

cumu_intensity$`Heatwave Type` <- factor(cumu_intensity$`Heatwave Type`, levels=type_levs)

### ======= INTENSITY BY PHASE ================================== ###
ggplot(data=cumu_intensity, aes(x=Year, y=`Peak Intensity`)) +
  geom_line(size = 0.65) +
  geom_smooth(method=lm, color='maroon', se=FALSE) +
  # scale_color_manual(values = c("blue", "grey", "red")) +
  theme_bw() + 
  facet_grid(rows=vars(Season), cols=vars(`Heatwave Type`)) +
  theme_bw(base_size = 12) + 
  ggtitle(expression(bold("Max Peak Intensity for 3σ 100,000" ~ km^2 ~" Heatwaves 1940-2023"))) +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(color = "black", face = "bold"),
    title = element_text(size = 15, face = "bold"),
    strip.text.y = element_text(size = 12),
    strip.text.x = element_text(size = 12),
    axis.text = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
  ) +
  geom_text(size=4, data = cumu_intensity_mods, check_overlap = TRUE,
            mapping = aes(x=Inf, y=Inf,
                          hjust=1.1, vjust=1.2,
                          label = paste('  Trend = ', estimate, '\n',
                                        'p-value = ', p.value))) +
  # annotate("rect", fill = "darkred", alpha = 0.01,
  #          xmin = elnino$Year, xmax = elnino$Year + 1,
  #          ymin = -Inf, ymax = Inf) +
  # annotate("rect", fill = "darkblue", alpha = 0.01,
  #          xmin = lanina$Year, xmax = lanina$Year + 1,
  #          ymin = -Inf, ymax = Inf) +
  ylab(expression(bold('Standard Deviation'))) +
  geom_vline(xintercept = elnino_maxyears$Year, linetype = 'dashed', color = 'firebrick1') +
  geom_vline(xintercept = lanina_maxyears$Year, linetype = 'dashed', color = 'blue')

### +++++++++++ Intensity vs Extent
# cumu_intensity <- event_modes %>%
#   group_by(`Heatwave Type`, Season, Year) %>%
#   summarise(`Peak Intensity` = max(`Peak Intensity`, na.rm = TRUE)) %>%
#   ungroup() %>%
#   complete(Season, Year, fill = list(`Peak Intensity` = 0)) %>%
#   mutate(`Peak Intensity` = replace_na(`Peak Intensity`, 0))


int_extent_mods <- event_modes %>%
  group_by(`Heatwave Type`, Season) %>%
  # group_by(Season) %>%
  na.omit() %>%
  group_modify(
    ## options: tidy, glance, augment
    ~ tidy(lm(`Peak Intensity` ~ Extent, data = .))
  ) %>% ungroup()

int_extent_mods <- int_extent_mods %>%
  filter(term == "Extent")

int_extent_mods <- int_extent_mods %>% mutate_if(is.numeric, ~ round(., 2))

season_levels = c('DJF', 'MAM', 'JJA', 'SON')

event_modes$Season = factor(event_modes$Season, levels=season_levels)
int_extent_mods$Season = factor(int_extent_mods$Season, levels=season_levels)

### ======= CUMULATIVE EXTENT BY PHASE ================================== ###
ggplot(data=event_modes, aes(x=Extent, y=`Peak Intensity`)) +
  geom_point() +
  geom_smooth(method=lm, color='maroon', se=FALSE) +
  # scale_color_manual(values = c("blue", "grey", "red")) +
  theme_bw() + 
  facet_grid(rows=vars(Season), cols=vars(`Heatwave Type`), scales='free_x') +
  theme_bw(base_size = 12) + 
  ggtitle(expression(bold("Max Peak Intensity vs Extent for 3σ 100,000" ~ km^2 ~" Heatwaves 1940-2023"))) +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(color = "black", face = "bold"),
    title = element_text(size = 15, face = "bold"),
    strip.text.y = element_text(size = 12),
    strip.text.x = element_text(size = 12),
    axis.text = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
  ) +
  geom_text(size=4, data = int_extent_mods, check_overlap = TRUE,
            mapping = aes(x=Inf, y=Inf,
                          hjust=1.1, vjust=1.2,
                          label = paste('  Trend = ', estimate, '\n',
                                        'p-value = ', p.value))) +
  # annotate("rect", fill = "darkred", alpha = 0.01,
  #          xmin = elnino$Year, xmax = elnino$Year + 1,
  #          ymin = -Inf, ymax = Inf) +
  # annotate("rect", fill = "darkblue", alpha = 0.01,
  #          xmin = lanina$Year, xmax = lanina$Year + 1,
  #          ymin = -Inf, ymax = Inf) +
  ylab(expression(bold('Standard Deviation')))
  # geom_vline(xintercept = elnino_maxyears$Year, linetype = 'dashed', color = 'firebrick1') +
  # geom_vline(xintercept = lanina_maxyears$Year, linetype = 'dashed', color = 'blue')

event_modes <- event_modes %>%
  rename(
    'Frequency' = 'Heatwave Count'
  )

event_modes$Count <- event_modes$Duration / event_modes$Frequency

## Multiple variables stored in column names
events_longer <- event_modes %>% pivot_longer(
  cols = c('Daily Count', 'Frequency', 'Zone Count', 'Extent',
           'CI',
           'Intensity',
           'Peak Intensity',
           'Duration',
           'Concurrence', 'Count'),
  names_to = "Metric",
  values_to = "Quantity"
)

events_max <- events_longer %>%
  group_by(`Heatwave Type`, Year, Metric, Season) %>%
  mutate(
    peak = max(Quantity, na.rm = FALSE)
  )

events_sub <- events_longer %>%
  filter(Metric %in% c('Extent', 'Intensity', 'Peak Intensity', 'Frequency', 'Duration','Count'))

season_levels <- c('DJF','MAM','JJA','SON')

## Reorder so boxplots will be in correct order
events_sub$Season <- factor(events_sub$Season, levels=season_levels)

kw_test <- events_sub %>%
  group_by(Metric, Season) %>%
  summarise(KW.pval = map_dbl(list(kruskal.test(Quantity, g=Phase)), ~.$p.value))

kw_test <- kw_test %>%
  mutate(
    sig = case_when(
      KW.pval <= 0.05 ~ '*'
    )
  )

kw_test$Phase <- 'Neutral'

### ============= Heatwave Type ENSO Boxplots ============= ### 
ggplot(na.omit(events_sub) %>%
         filter(
           !Metric %in% c('CI','Daily Count','Zone Count')
         ), aes(x=Season, y=Quantity, color=Phase)) +
  geom_boxplot(aes(fill=Phase)) + 
  scale_color_manual(values = c("blue3","grey17","firebrick4"),
                     name = "Phase") +
  scale_fill_manual(values = c("lightskyblue1","lightgrey","lightsalmon"),
                    name = "Phase") +
  facet_grid2(rows=vars(`Heatwave Type`),
              cols=vars(Metric),
              scales='free',
              independent = "y") +
  labs(x="Season") + ylab('') + 
  ggtitle('Distribution of Characteristics by Heatwave Type and ENSO 1950-2023') +
  theme_bw() + theme(strip.background=element_blank()) +
  theme(strip.text=element_text(color="black", face="bold", size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", size = 11),  # Bold x-axis tick labels
        axis.text.y = element_text(face = "bold", size = 11),  # Bold y-axis tick labels
        title=element_text(size = 15, face = "bold")) +
  geom_text(
    data = kw_test %>% filter(!Metric %in% c('CI','Daily Count','Zone Count')), 
    aes(x = Season, y = Inf, label = sig),
    hjust = 0.5, vjust = 1.2, size = 8, alpha=0.8, fontface='bold', color="darkorchid1"
  )

### ======== Time series
events_max <- events_longer %>%
  group_by(`Heatwave Type`, Year, Metric) %>%
  mutate(
    peak = max(Quantity, na.rm = FALSE)
  )

max_sub <- events_max %>%
  filter(Metric %in% c('Extent', 'Intensity', 'Peak Intensity', 'Frequency'))

max_mods <- max_sub %>%
  na.omit() %>%
  group_by(`Heatwave Type`, Metric) %>%
  # group_by(Season) %>%
  group_modify(
    ## options: tidy, glance, augment
    ~ tidy(lm(peak ~ Year, data = .))
  ) %>% ungroup()

max_mods <- max_mods %>%
  filter(term == "Year")

max_mods <- max_mods %>% mutate_if(is.numeric, ~ round(., 4))

# max_sub$Season = factor(event_modes$Season, levels=season_levels)
# int_extent_mods$Season = factor(int_extent_mods$Season, levels=season_levels)

### ======= CUMULATIVE EXTENT BY PHASE ================================== ###
ggplot(data=max_sub, aes(x=Year, y=peak)) +
  geom_line(size = 0.65) +
  geom_smooth(method=lm, color='maroon', se=FALSE) +
  theme_bw() + 
  facet_grid(rows=vars(Metric), cols=vars(`Heatwave Type`), scales='free_y') +
  theme_bw(base_size = 12) + 
  ggtitle(expression(bold("Maximum Characteristics for 3σ 100,000" ~ km^2 ~" Heatwaves 1940-2023"))) +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(color = "black", face = "bold"),
    title = element_text(size = 15, face = "bold"),
    strip.text.y = element_text(size = 12),
    strip.text.x = element_text(size = 12),
    axis.text = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
  ) +
  geom_text(size=4, data = max_mods, check_overlap = TRUE,
            mapping = aes(x=Inf, y=Inf,
                          hjust=1.1, vjust=1.2,
                          label = paste('  Trend = ', estimate, '\n',
                                        'p-value = ', p.value))) +
  # annotate("rect", fill = "darkred", alpha = 0.01,
  #          xmin = elnino$Year, xmax = elnino$Year + 1,
  #          ymin = -Inf, ymax = Inf) +
  # annotate("rect", fill = "darkblue", alpha = 0.01,
  #          xmin = lanina$Year, xmax = lanina$Year + 1,
  #          ymin = -Inf, ymax = Inf) +
  ylab(expression(bold(''))) +
geom_vline(xintercept = elnino_maxyears$Year, linetype = 'dashed', color = 'firebrick1') +
geom_vline(xintercept = lanina_maxyears$Year, linetype = 'dashed', color = 'blue')

### ======== Time series for mean
events_mean <- events_longer %>%
  group_by(`Heatwave Type`, Year, Metric) %>%
  mutate(
    Mean = mean(Quantity, na.rm = FALSE)
  )

mean_sub <- events_mean %>%
  filter(Metric %in% c('Extent', 'Intensity', 'Peak Intensity', 'Frequency'))

mean_mods <- mean_sub %>%
  na.omit() %>%
  group_by(`Heatwave Type`, Metric) %>%
  # group_by(Season) %>%
  group_modify(
    ## options: tidy, glance, augment
    ~ tidy(lm(Mean ~ Year, data = .))
  ) %>% ungroup()

mean_mods <- mean_mods %>%
  filter(term == "Year")

mean_mods <- mean_mods %>% mutate_if(is.numeric, ~ round(., 4))

# max_sub$Season = factor(event_modes$Season, levels=season_levels)
# int_extent_mods$Season = factor(int_extent_mods$Season, levels=season_levels)

### ======= CUMULATIVE EXTENT BY PHASE ================================== ###
ggplot(data=mean_sub, aes(x=Year, y=Mean)) +
  geom_line(size = 0.65) +
  geom_smooth(method=lm, color='maroon', se=FALSE) +
  theme_bw() + 
  facet_grid(rows=vars(Metric), cols=vars(`Heatwave Type`), scales='free_y') +
  theme_bw(base_size = 12) + 
  ggtitle(expression(bold("Mean Characteristics for 3σ 100,000" ~ km^2 ~" Heatwaves 1940-2023"))) +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(color = "black", face = "bold"),
    title = element_text(size = 15, face = "bold"),
    strip.text.y = element_text(size = 12),
    strip.text.x = element_text(size = 12),
    axis.text = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
  ) +
  geom_text(size=4, data = mean_mods, check_overlap = TRUE,
            mapping = aes(x=Inf, y=Inf,
                          hjust=1.1, vjust=1.2,
                          label = paste('  Trend = ', estimate, '\n',
                                        'p-value = ', p.value))) +
  # annotate("rect", fill = "darkred", alpha = 0.01,
  #          xmin = elnino$Year, xmax = elnino$Year + 1,
  #          ymin = -Inf, ymax = Inf) +
  # annotate("rect", fill = "darkblue", alpha = 0.01,
  #          xmin = lanina$Year, xmax = lanina$Year + 1,
  #          ymin = -Inf, ymax = Inf) +
  ylab(expression(bold(''))) #+
  # geom_vline(xintercept = elnino_maxyears$Year, linetype = 'dashed', color = 'firebrick1') +
  # geom_vline(xintercept = lanina_maxyears$Year, linetype = 'dashed', color = 'blue')


### ========== EVENT TYPE PLOTS
kw_test <- events_sub %>%
  group_by(Metric, Season) %>%
  summarise(KW.pval = map_dbl(list(kruskal.test(Quantity, g=`Heatwave Type`)), ~.$p.value))

kw_test <- kw_test %>%
  mutate(
    sig = case_when(
      KW.pval <= 0.05 ~ '*'
    )
  )

# kw_test$`Heatwave Type` <- 'Neutral'

events_sub$`Heatwave Type` <- factor(events_sub$`Heatwave Type`, levels=type_levs)

events_sub_log <- events_sub %>%
  mutate(Quantity = if_else(
    Metric %in% c('Extent'), log(Quantity), Quantity,
  ))

# 
# 
# events_sub_log <- events_sub_log %>%
#   mutate(Quantity = if_else(
#     Metric == 'Frequency', 
#   ))

### ============= Heatwave Type ENSO Boxplots ============= ### 
ggplot(na.omit(events_sub_log) %>%
         filter(
           !Metric %in% c('CI','Daily Count','Zone Count')
         ), aes(x=Season, y=Quantity, color=`Heatwave Type`)) +
  geom_boxplot(aes(fill=`Heatwave Type`)) + 
  scale_fill_manual(values = c("burlywood","turquoise","lightskyblue"),
                     name = "Heatwave Type") +
  scale_color_manual(values = c("darkorange4","darkgreen","darkblue"),
                    name = "Heatwave Type") +
  facet_grid2(
              cols=vars(Metric),
              scales='free',
              independent = "y") +
  labs(x="Season") + ylab('') + 
  ggtitle('Distribution of Individual Heatwave Characteristics 1940-2023') +
  theme_bw() + theme(strip.background=element_blank()) +
  theme(strip.text=element_text(color="black", face="bold", size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", size = 11),
        axis.text.y = element_text(face = "bold", size = 11),
        legend.text = element_text(size = 13),
        title=element_text(size = 15, face = "bold")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_text(
    data = kw_test %>% filter(!Metric %in% c('CI','Daily Count','Zone Count')), 
    aes(x = Season, y = Inf, label = sig),
    hjust = 0.5, vjust = 1.2, size = 8, alpha=0.8, fontface='bold', color="darkorchid1"
  )

