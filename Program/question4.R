
# Required packages
#############################
library(tidyverse)
library(ggrepel)
library(ggplot2)
library(RColorBrewer)
library(lmtest)
library(sandwich)
library(patchwork)
library(gganimate)
library(scales)
library(stringr)
library(WDI)

#############################
# Paths & outputs
#############################
gini_path      <- "Data/gini-coefficient.csv"
homicide_path  <- "Data/homicide-rate-unodc.csv"
#############################
# Constants & Colors
#############################
X_MIN <- 0.2
X_MAX <- 0.65
Y_MIN <- 0
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
gini_data     <- read.csv(gini_path, header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
homicide_data <- read.csv(homicide_path, header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)

# Population via WDI
population_raw <- WDI::WDI(country = "all", indicator = "SP.POP.TOTL", start = 2015, end = 2023, extra = TRUE)
population_latest <- population_raw %>%
  rename(Entity = country, Population = SP.POP.TOTL) %>%
  filter(!is.na(Population)) %>%
  group_by(Entity) %>% arrange(desc(year)) %>% slice(1) %>% ungroup() %>%
  mutate(logPop = log(Population + 1)) %>%
  select(Entity, Population, logPop)

#############################
# 3) Data Cleaning & Region Mapping
#############################
# Gini Cleaning
gini_data_clean <- gini_data %>%
  rename(Entity = Country) %>%
  rename(Gini_Coefficient = matches("Gini|gini")) %>% 
  select(Entity, Year, Gini_Coefficient)

# Homicide Cleaning
homicide_col <- names(homicide_data)[grepl("Homicide", names(homicide_data), ignore.case = TRUE)][1]
homicide_data_clean <- homicide_data %>%
  rename(Homicide_Rate = all_of(homicide_col)) %>%
  filter(!Entity %in% c("World", "Europe (Eurasia)", "World (population-weighted)"))

# Region Mapping
original_regions <- c("Africa", "Asia", "Europe", "North America", "South America", "Oceania")
target_continents <- c("Africa", "Asia", "Europe", "North America", "Oceania", "Latin America")

region_col <- names(homicide_data)[grepl("Region|region", names(homicide_data))][1]
country_continent_map <- homicide_data %>%
  select(Entity, Year, Region = all_of(region_col)) %>%
  filter(Region %in% original_regions) %>%
  filter(!Entity %in% original_regions) %>%
  distinct(Entity, .keep_all = TRUE) %>%
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
# 4) Merge Data (Latest Available Year)
#############################
latest_gini <- gini_data_clean %>%
  group_by(Entity) %>% filter(Year == max(Year, na.rm = TRUE)) %>% ungroup()

latest_homicide <- homicide_data_clean %>%
  group_by(Entity) %>% filter(Year == max(Year, na.rm = TRUE)) %>% ungroup()

merged_homicide_cs <- latest_gini %>%
  inner_join(latest_homicide %>% select(Entity, Homicide_Rate), by = "Entity") %>%
  inner_join(country_continent_map %>% select(Entity, Region), by = "Entity") %>%
  left_join(population_latest, by = "Entity") %>%
  filter(!is.na(Gini_Coefficient), !is.na(Homicide_Rate))

merged_homicide_cs$Region <- factor(merged_homicide_cs$Region, levels = region_levels)


Y_MAX <- max(merged_homicide_cs$Homicide_Rate, na.rm = TRUE) * 1.05

#############################
# 5) Plot 1: Violin Plot & Boxplot
#############################
plot_homicide_violin <- ggplot(merged_homicide_cs, 
                               aes(x = Region, y = Homicide_Rate, fill = Region)) +
 
  geom_violin(trim = FALSE, alpha = 0.7) +
  
  geom_jitter(width = 0.12, alpha = 0.5, size = 2) +
  
  geom_boxplot(width = 0.1, color = "black", alpha = 0.8, outlier.shape = NA) +
  
  scale_fill_manual(values = set2_colors) +
  scale_y_continuous(limits = c(0, 50)) +
  labs(title = "Homicide Rate within Continents",
       x = "Continent", y = "Homicide Rate") +
  theme_minimal(base_size = 20) +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", hjust = 0.5, size = 32),
        axis.title = element_text(face = "bold", size = 26),
        axis.text = element_text(size = 18))

print(plot_homicide_violin)
#############################
# 6) Plot 2: World Scatter with Weighted Slope
#############################

calculate_slope_and_plot <- function(data, plot_title, show_legend = TRUE) {
  
  data_clean <- data %>% filter(!is.na(Gini_Coefficient), !is.na(Homicide_Rate), !is.na(Population))
  
  if (nrow(data_clean) > 2) {
    model <- lm(Homicide_Rate ~ Gini_Coefficient, data = data_clean, weights = Population)
    slope <- round(coef(model)[2], 2)
  } else {
    slope <- "N/A"
  }
  
  label_text <- paste0("Slope: ", slope, "  (N = ", nrow(data_clean), ")")
  
  ggplot(data_clean, aes(x = Gini_Coefficient, y = Homicide_Rate)) +
    geom_smooth(method = "lm", se = FALSE, linetype = "dashed", 
                mapping = aes(weight = Population)) +
    geom_point(aes(color = Region, size = logPop), alpha = 0.8) +
    scale_size(range = c(2, 9), guide = "none") +
    scale_color_manual(values = set2_colors, name = "Continent") +
    annotate("text",
             x = min(data_clean$Gini_Coefficient) * 1.1, 
             y = Y_MAX * 0.95,
             label = label_text,
             hjust = 0, vjust = 1,
             size = 7, fontface = "bold") +
    labs(
      title = plot_title,
      x = "Gini Coefficient",
      y = "Homicide Rate (per 100,000)"
    ) +
    theme_minimal(base_size = 20) +
    theme(plot.title.position = "plot",
      plot.title = element_text(face = "bold", hjust = 0.5, size = 28), 
          axis.title = element_text(face = "bold", size = 25),              
          axis.text = element_text(size = 21),                              
          legend.position = ifelse(show_legend, "right", "none"),
          legend.title = element_text(size = 24, face = "bold"), 
          legend.text = element_text(size = 20)
    )+
    coord_cartesian(xlim = c(X_MIN, X_MAX), ylim = c(Y_MIN, Y_MAX))
}

plot_world_slope <- calculate_slope_and_plot(
  merged_homicide_cs,
  "After Tax Social Inequality vs. Homicide Rate Worldwide",
  TRUE
)

print(plot_world_slope)


#############################
# 7) Plot 3: Regional Subplots (Specific Y Limits)
#############################

calculate_slope_and_plot_region <- function(data, plot_title, y_limit_custom) {
  
  data_clean <- data %>% filter(!is.na(Gini_Coefficient), !is.na(Homicide_Rate), !is.na(Population))
  
  if (nrow(data_clean) > 2) {
    model <- lm(Homicide_Rate ~ Gini_Coefficient, data = data_clean, weights = Population)
    slope <- round(coef(model)[2], 2)
  } else {
    slope <- "N/A"
  }
  
  label_text <- paste0("Slope: ", slope, "  (N = ", nrow(data_clean), ")")
  
  ggplot(data_clean, aes(x = Gini_Coefficient, y = Homicide_Rate)) +
    geom_smooth(method = "lm", se = FALSE, linetype = "dashed",
                mapping = aes(weight = Population)) +
    geom_point(aes(color = Region, size = logPop), alpha = 0.6) +
    scale_size(range = c(2, 8), guide = "none") +
    scale_color_manual(values = set2_colors, guide = "none") +
    annotate("text",
             x = -Inf, y = Inf,
             label = label_text,
             hjust = -0.1, vjust = 1.5,
             size = 5, fontface = "bold") + 
    labs(
      title = plot_title,
      x = "Gini Coefficient", 
      y = "Homicide Rate"
    ) +
    theme_minimal(base_size = 18) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 20), 
      axis.title = element_text(face = "bold", size = 15),              
      axis.text = element_text(size = 16)                               
    ) +
    coord_cartesian(xlim = c(0.2, 0.65), ylim = c(0, y_limit_custom))
}

region_configs <- list(
  list(name = "Europe",        ymax = 6),
  list(name = "Asia",          ymax = 6),
  list(name = "Latin America", ymax = 60)
)

regional_plots <- map(region_configs, function(cfg) {
  calculate_slope_and_plot_region(
    merged_homicide_cs %>% filter(Region == cfg$name),
    plot_title = cfg$name,
    y_limit_custom = cfg$ymax  
  )
})

final_regional_plot <- wrap_plots(regional_plots, ncol = 2) +
  plot_annotation(
    title = "After Tax Social Inequality vs. Homicide Rate by Region",
    theme = theme(
      plot.title = element_text(size = 28, face = "bold", hjust = 0.5), 
      plot.margin = margin(20, 20, 20, 20)
    )
  )

print(final_regional_plot)