library(ggplot2) ## plotting
library(sf) ## spatial data
library(tidyverse) ## data wrangling
library(classInt) ## categorisation of spatial data 
library(ggpubr) ## facet plotting

# 1. Data -----------------------------------------------------------------

## Overture points of interest data - Available at: https://data.cdrc.ac.uk/dataset/point-interest-data-united-kingdom
db <- st_read("data/poi_uk/poi_uk.gpkg")

## UK local authority districts - Available at: https://geoportal.statistics.gov.uk/search?q=BDY_LAD%3BDEC_2022&sort=Title%7Ctitle%7Casc
lad <- st_read("data/LAD_DEC_2022_UK_BFC.shp") 

## UK local authority districts deprivation
lad_dep <- read.csv("data/LAD-Deprivation.csv")

# 2. Processing -----------------------------------------------------------

## Get all UK deli's
deli <- db %>%
  filter(main_category == "delicatessen") %>%
  select(id, primary_name, main_category, geom)

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

## Calculate jenks grouping
foo <- classIntervals(lad$n, n = 5, style = "jenks")
foo$brks

## Group
lad <- lad %>%
  mutate(n = case_when(is.na(n) ~ 0,
                       TRUE ~ n),
         n_grp = case_when(n < 2 ~ "0 - 2",
                           n >= 2 & n < 5 ~ "2 - 5",
                           n >= 5 & n < 9 ~ "5 - 9",
                           n >= 9 & n < 17  ~  "9 - 17",
                           n >= 17 ~ "17+"), 
         n_grp = factor(n_grp, levels = c("0 - 2", "2 - 5", "5 - 9", "9 - 17", "17+")))

## Attach deprivation data
lad_stat <- lad %>%
  inner_join(lad_dep, by = c("LAD22CD", "LAD22NM")) %>%
  as.data.frame() %>%
  select(LAD22NM, n, n_grp, avgIMD)

# 3. Map ------------------------------------------------------------------

## Map LAD counts of delis
map <- ggplot(lad) +
  geom_sf(aes(fill = n_grp), color = NA) +
  scale_fill_manual(values = c("#ebe47a", "#eacc6a", "#e9c062", "#e7843a", "#e56022")) +
  labs(title = "Where the Crust Is: Mapping the UK's Deli Divide", subtitle = "Day 29 - Overture, #30DayMapChallenge",
       fill = "Number of Delis") +
  theme_void() +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(size = 14), legend.text = element_text(size = 12),
        legend.title = element_text(size = 12))

# 4. Graph ----------------------------------------------------------------

## Box plot of deli count versus deprivation level
graph <- ggplot(lad_stat, aes(x = n_grp, y = avgIMD, fill = n_grp)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#ebe47a", "#eacc6a", "#e9c062", "#e7843a", "#e56022")) +
  labs(title = " ", subtitle = "Delis appear more frequently in more deprived parts of the UK!",
       caption = "Visualisations by Patrick Ballantyne (@DrPatBallantyne)\nData from CDRC, Overture Maps Foundation and Office for National Statistics",
       x = "Number of Delis", y = "Average IMD score", fill = "Delicatessen (n)") +
  theme_minimal() +
  theme(legend.position = "none", plot.subtitle = element_text(size = 14),
        axis.title = element_text(size = 12))

# 5. Figure ---------------------------------------------------------------

## Arrange
ggarrange(map,  graph)
ggsave("Day29-Overture.jpg", dpi = 600)
