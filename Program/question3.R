
#############################
# Required packages
#############################
required_pkgs <- c(
  "tidyverse", "ggrepel", "ggplot2", "RColorBrewer",
  "lmtest", "sandwich", "patchwork", "gganimate",
  "gifski", "scales", "stringr", "WDI"
)

to_install <- required_pkgs[!(required_pkgs %in% installed.packages()[, "Package"])]
if (length(to_install)) install.packages(to_install, dependencies = TRUE)
lapply(required_pkgs, library, character.only = TRUE)

#############################
# Paths & outputs
#############################
gini_path <- "Data/gini-coefficient.csv"
democracy_path <- "Data/democracy.csv"


anim_duration <- 7
anim_fps <- 20

#############################
# Constants
#############################
X_MIN <- 0.2
X_MAX <- 0.65
Y_MIN <- 0
Y_MAX <- 10

region_levels <- c("Africa", "Asia", "Europe", "Latin America", "North America", "Oceania")

continent_base_colors <- c(
  "Africa"        = "#8c510a",
  "Asia"          = "#d8b365",
  "Europe"        = "#1b7837",
  "Latin America" = "#5ab4ac",
  "North America" = "#762a83",
  "Oceania"       = "#af8dc3"
)
set2_colors <- continent_base_colors

#############################
# 1) Load datasets
#############################
gini_data <- read.csv(gini_path, header = TRUE, stringsAsFactors = FALSE)
democracy_data <- read.csv(democracy_path, header = TRUE, stringsAsFactors = FALSE)

#############################
# 2) GDP via WDI
#############################
latest_gdp <- NULL
try({
  gdp_raw <- WDI::WDI(
    country = "all",
    indicator = "NY.GDP.MKTP.CD",
    start = 2015,
    end = as.integer(format(Sys.Date(), "%Y")),
    extra = TRUE
  )
  
  latest_gdp_all <- gdp_raw %>%
    rename(country = country, year = year, GDP = NY.GDP.MKTP.CD) %>%
    select(country, year, GDP)
}, silent = TRUE)

if (exists("latest_gdp_all") && nrow(latest_gdp_all) > 0) {
  latest_gdp <- latest_gdp_all %>%
    filter(!is.na(GDP)) %>%
    group_by(country) %>%
    arrange(desc(year)) %>%
    slice(1) %>%
    ungroup() %>%
    rename(Entity = country, Latest_GDP_Year = year) %>%
    mutate(logGDP = log(GDP + 1))
} else {
  latest_gdp_clean <- tibble(Entity = character(), GDP = numeric(), logGDP = numeric())
}

latest_gdp_clean <- latest_gdp %>% select(Entity, GDP, logGDP)

#############################
# 3) Population via WDI
#############################
population_raw <- WDI::WDI(
  country = "all",
  indicator = "SP.POP.TOTL",
  start = 2015,
  end = as.integer(format(Sys.Date(), "%Y")),
  extra = TRUE
)

population_latest <- population_raw %>%
  rename(Entity = country, Population = SP.POP.TOTL) %>%
  filter(!is.na(Population)) %>%
  group_by(Entity) %>%
  arrange(desc(year)) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(logPop = log(Population + 1)) %>%
  select(Entity, Population, logPop)

#############################
# 4) Region mapping
#############################
original_regions <- c("Africa", "Asia", "Europe", "North America", "South America", "Oceania")
target_continents <- c("Africa", "Asia", "Europe", "North America", "Oceania", "Latin America")

country_continent_map <- democracy_data %>%
  select(Entity, Year, World.regions.according.to.OWID) %>%
  rename(Region = World.regions.according.to.OWID) %>%
  filter(Region %in% original_regions) %>%
  filter(!Entity %in% original_regions) %>%
  mutate(
    Region = case_when(
      Region == "North America" & Entity %in% c("United States", "Canada") ~ "North America",
      Region == "North America" ~ "Latin America",
      Region == "South America" ~ "Latin America",
      TRUE ~ Region
    )
  ) %>%
  filter(Region %in% target_continents)

#############################
# 5) Clean datasets
#############################
gini_data_clean <- gini_data %>%
  rename(
    Gini_Coefficient = Gini.coefficient..World.Bank.PIP.,
    Entity = Country
  )

democracy_data_clean <- democracy_data %>%
  rename(Democracy_Index = Democracy.index) %>%
  select(-World.regions.according.to.OWID)

#############################
# 6) Merge (latest)
#############################
latest_gini <- gini_data_clean %>%
  group_by(Entity) %>%
  filter(Year == max(Year, na.rm = TRUE)) %>%
  distinct(Entity, .keep_all = TRUE)

latest_democracy <- democracy_data_clean %>%
  group_by(Entity) %>%
  filter(Year == max(Year, na.rm = TRUE)) %>%
  distinct(Entity, .keep_all = TRUE)

latest_continent_map <- country_continent_map %>%
  group_by(Entity) %>%
  filter(Year == max(Year)) %>%
  distinct(Entity, .keep_all = TRUE)

merged_democracy_cs <- latest_gini %>%
  full_join(latest_democracy, by = "Entity") %>%
  inner_join(latest_continent_map, by = "Entity") %>%
  left_join(latest_gdp_clean, by = "Entity") %>%
  left_join(population_latest, by = "Entity") %>%
  filter(!is.na(Gini_Coefficient), !is.na(Democracy_Index), !is.na(Region))

merged_democracy_cs$Region <- factor(merged_democracy_cs$Region, levels = region_levels)

merged_democracy_cs <- merged_democracy_cs %>%
  mutate(Weighted_Gini = Gini_Coefficient * (Population / max(Population, na.rm = TRUE)))

#############################
# 7) Violin plot
#############################
plot_democracy_boxplot <- ggplot(merged_democracy_cs,
                                 aes(x = Region, y = Democracy_Index, fill = Region)) +
  geom_violin(trim = FALSE, alpha = 0.7) +
  geom_jitter(width = 0.12, alpha = 0.5, size = 2) +
  scale_fill_manual(values = set2_colors) +
  scale_y_continuous(limits = c(0, 10))+
  labs(title = "Democracy Index within Continents",
       x = "Continent", y = "Democracy Index") +
  theme_minimal(base_size = 20) +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", hjust = 0.5,size = 28),
        axis.title = element_text(face = "bold", size = 24),            
        axis.text = element_text(size = 18))

print(plot_democracy_boxplot)


#############################

calculate_slope_and_plot <- function(data, plot_title, show_legend = TRUE) {
  
  data_clean <- data %>%
    filter(!is.na(Gini_Coefficient), !is.na(Democracy_Index), !is.na(Population))
  
  valid <- isTRUE(
    nrow(data_clean) > 2 &&
      sd(data_clean$Gini_Coefficient, na.rm = TRUE) > 0 &&
      sd(data_clean$Democracy_Index, na.rm = TRUE) > 0
  )
  
  if (valid) {
    model <- lm(Democracy_Index ~ Gini_Coefficient, data = data_clean, weights = Population)
    slope <- round(coef(model)[2], 2)
  } else {
    slope <- "N/A"
  }
  
  label_text <- paste0("Slope: ", slope, "  (N = ", nrow(data_clean), ")")
  
  ggplot(data_clean, aes(x = Gini_Coefficient, y = Democracy_Index)) +
    geom_smooth(method = "lm", se = FALSE, linetype = "dashed", aes(weight = Population)) + 
    geom_point(aes(color = Region, size = logPop), alpha = 0.8) +
    scale_size(range = c(1, 8), guide = "none") +
    scale_color_manual(values = set2_colors, name = "Continent") +
    annotate("text",
             x = X_MAX * 0.98,
             y = Y_MAX * 0.95,
             label = label_text,
             hjust = 1, vjust = 1,
             size = 9, fontface = "bold") +
    labs(
      title = plot_title,
      x = "Gini Coefficient",
      y = "Democracy Index"
    ) +
    theme_minimal(base_size = 20) +
    theme(plot.title.position = "plot",
      plot.title = element_text(face = "bold", hjust = 0.5,size = 28),
      legend.position = ifelse(show_legend, "right", "none"),
      axis.title = element_text(face = "bold", size = 25),
      axis.text = element_text(size = 21),
      legend.title = element_text(size = 24, face = "bold"), 
      legend.text = element_text(size = 20) 
    ) +
    coord_cartesian(xlim = c(X_MIN, X_MAX), ylim = c(Y_MIN, Y_MAX))
}

plot_world_annotated <- calculate_slope_and_plot(
  merged_democracy_cs,
  "Social Inequality vs. Democracy Worldwide",
  TRUE
)

print(plot_world_annotated)


calculate_slope_and_plot_region <- function(data, plot_title) {
  
  data_clean <- data %>%
    filter(!is.na(Gini_Coefficient), !is.na(Democracy_Index), !is.na(Population))
  
  valid <- isTRUE(
    nrow(data_clean) > 2 &&
      sd(data_clean$Gini_Coefficient, na.rm = TRUE) > 0 &&
      sd(data_clean$Democracy_Index, na.rm = TRUE) > 0
  )
  
  if (valid) {
    model <- lm(Democracy_Index ~ Gini_Coefficient, data = data_clean, weights = Population)
    slope <- round(coef(model)[2], 2)
  } else {
    slope <- "N/A"
  }
  
  label_text <- paste0("Slope: ", slope, "  (N = ", nrow(data_clean), ")")
  
  ggplot(data_clean, aes(x = Gini_Coefficient, y = Democracy_Index)) +
    geom_smooth(method = "lm", se = FALSE, linetype = "dashed", aes(weight = Population)) +
    geom_point(aes(color = Region, size = logPop), alpha = 0.45) +
    scale_size(range = c(1, 8), guide = "none") +
    scale_color_manual(values = set2_colors, guide = "none") +
    annotate("text",
             x = X_MAX * 0.98,
             y = Y_MAX * 0.98,
             label = label_text,
             hjust = 1, vjust = 1,
             size = 6, fontface = "bold") +
    labs(
      title = plot_title,
      x = "Gini Coefficient",   
      y = "Democracy Index"    
    ) +
    theme_minimal(base_size = 18) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5,size = 20),
      axis.title = element_text(face = "bold", size = 12),
      axis.text = element_text(size = 16),
    ) +
    coord_cartesian(xlim = c(X_MIN, X_MAX), ylim = c(Y_MIN, Y_MAX))
}

regional_filters_set <- c("Africa", "Asia", "Europe", "Latin America")

regional_plots <- map(regional_filters_set, ~{
  calculate_slope_and_plot_region(
    merged_democracy_cs %>% filter(Region == .x),
    plot_title = .x
  )
})

#############################
# Final regional plot
#############################
library(patchwork) 

combined <- wrap_plots(regional_plots, ncol = 2)

final_plot <- combined +
  plot_annotation(
    title = "Social Inequality vs. Democracy by Region",
    theme = theme(
      plot.title = element_text(size = 28, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 20, hjust = 0.5),
      plot.margin = margin(20,20,20,20)
    )
  ) &
  labs(
    x = "Gini Coefficient",
    y = "Democracy Index"
  )

print(final_plot)
























