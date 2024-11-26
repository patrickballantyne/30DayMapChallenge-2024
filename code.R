library(ggplot2) ## plotting
library(sf) ## spatial data
library(tidyverse) ## data wrangling
library(classInt) ## categorisation of spatial data 
library(ggpubr) ## facet plotting

# 1. Data -----------------------------------------------------------------

## Overture points of interest data - Available at: https://data.cdrc.ac.uk/dataset/point-interest-data-united-kingdom. Too big for the repo (apologies!)
db <- st_read("data/poi_uk/poi_uk.gpkg")

## UK local authority districts - Available at: https://geoportal.statistics.gov.uk/search?q=BDY_LAD%3BDEC_2022&sort=Title%7Ctitle%7Casc
lad <- st_read("data/LAD_DEC_2022_UK_BFC.shp") 

## UK local authority districts deprivation
lad_dep <- read.csv("data/LAD-Deprivation.csv")

# 2. Processing -----------------------------------------------------------

## Get all UK deli's
deli <- db %>%
  filter(main_category == "delicatessen" | main_category == "sandwich_shop") %>%
  select(id, primary_name, main_category, geom)

## Calculate frequency by name (to identify where the big brands are)
deli_ls <- deli %>%
  as.data.frame() %>%
  select(primary_name) %>%
  group_by(primary_name) %>%
  add_count() %>%
  distinct() %>%
  arrange(desc(n))

## Drop big brands
to_drop <- c("Subway UK & Ireland", "Subway", "Pret A Manger")
deli <- deli %>%
  filter(!primary_name %in% to_drop)

## Tidy up the LAD data
lad <- lad %>%
  select(LAD22CD, LAD22NM, geometry)

## Tidy up the LAD deprivation data
lad_dep <- lad_dep %>%
  select(1:2, 5) %>%
  setNames(c("LAD22CD", "LAD22NM", "avgIMD"))

## Attach LAD codes
deli_lad <- st_join(deli, lad)

## Calculate total delis per lad
deli_count <- deli_lad %>%
  as.data.frame() %>%
  select(LAD22NM) %>%
  group_by(LAD22NM) %>%
  add_count() %>%
  distinct() %>%
  arrange(desc(n))

## Attach to boundaries
lad <- merge(lad, deli_count, by = "LAD22NM", all.x = TRUE)

## Calculate area (m2) and number per km2
lad <- lad %>%
  mutate(n = case_when(is.na(n) ~ 0, 
                       TRUE ~ n)) %>%
  mutate(area_meters = st_area(lad),
         area_kms = area_meters / 1000000) %>%
  mutate(n_area = n / area_kms,
         n_area = as.numeric(n_area)) %>%
  arrange(desc(n_area)) %>%
  select(LAD22NM, LAD22CD, n_area, geometry)

## Calculate jenks grouping
foo <- classIntervals(lad$n_area, n = 5, style = "quantile")
foo$brks

## Group
lad <- lad %>%
  mutate(n_grp = case_when(n_area < 0.005 ~ "0.000 - 0.005",
                           n_area >= 0.005 & n_area < 0.011 ~ "0.005 - 0.011",
                           n_area >= 0.011 & n_area < 0.032 ~ "0.011 - 0.032",
                           n_area >= 0.032 & n_area < 0.112 ~  "0.032 - 0.112",
                           n_area >= 0.112 ~ "0.112+")) %>%
  mutate(n_grp = factor(n_grp, levels = c("0.000 - 0.005", "0.005 - 0.011", "0.011 - 0.032", "0.032 - 0.112", "0.112+")))

## Attach deprivation data
lad_stat <- lad %>%
  inner_join(lad_dep, by = c("LAD22CD", "LAD22NM")) %>%
  as.data.frame() %>%
  select(LAD22NM, n_area, n_grp, avgIMD)

# 3. Map ------------------------------------------------------------------

## Map LAD counts of delis
map <- ggplot(lad) +
  geom_sf(aes(fill = n_grp), color = NA) +
  scale_fill_manual(values = c("#ebe47a", "#eacc6a", "#e9c062", "#e7843a", "#e56022")) +
  labs(title = "Where the Crust Is: Mapping the UK's Deli Divide", subtitle = "Day 29 - Overture, #30DayMapChallenge",
       fill = bquote("Delis per"~km^2)) +
  theme_void() +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(size = 14), legend.text = element_text(size = 10),
        legend.title = element_text(size = 10))

# 4. Graph ----------------------------------------------------------------

## Box plot of deli count versus deprivation level
graph <- ggplot(lad_stat, aes(x = n_grp, y = avgIMD, fill = n_grp)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#ebe47a", "#eacc6a", "#e9c062", "#e7843a", "#e56022")) +
  labs(title = "Where the Crust Is: Mapping the UK's Deli Divide", subtitle = "Delis appear more frequently in more deprived parts of the UK!",
       caption = "Visualisations by Patrick Ballantyne (@DrPatBallantyne)\nData from CDRC, Overture Maps Foundation and Office for National Statistics",
       x = bquote("Delis per"~km^2), y = "Average IMD score", fill = bquote("Delis per"~km^2)) +
  theme_minimal() +
  theme(legend.position = "none", 
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 14),
        axis.title = element_text(size = 12))

# 5. Figure ---------------------------------------------------------------

## Arrange
ggarrange(map,  graph)
ggsave("Day29-Overture.jpg", dpi = 600, height = 8, width = 14)
