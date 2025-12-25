# Wie hat sich die soziale Ungleichheit innerhalb verschiedener Länder und 
# insgesamweltweit über die Zeit entwickelt?
library(readxl)
library(tidyverse)
library(ggplot2)
library(colorspace)
library(ggrepel)
library(colorBlindness)
#The Gini coefficient measures inequality on a scale from 0 to 1. Higher values indicate higher inequality. 
#Depending on the country and year, the data relates to income (measured after taxes and benefits) or to consumption, per capita.

# Soziale Ungleichheit insgesamweltweit über die Zeit entwickelt
gini_coefficient <- read_csv("Data/gini-coefficient Before Tax.csv")
gini_coefficient1 <- read_csv("Data/gini-coefficient After Tax.csv")

as.data.frame(gini_coefficient)
gini1980_2025 <- gini_coefficient |> 
  filter(Year >= 1980 & Year < 2025)

gini_world <- gini1980_2025 |>
  filter(Country == "World")

p1<-ggplot(gini_world, aes(x = Year, y = `Gini coefficient (before tax) (World Inequality Database)`)) +
  geom_line(linewidth = 1.2) +
  labs(plot.title.position = "plot",
    title = "Before Tax Global Gini Coefficient Trend (1980-2024)",
    x = "Year",
    y = "Gini Coefficient"
  ) +
  theme_minimal() +
  theme(plot.title.position = "plot",
    plot.title = element_text(face = "bold", hjust = 0.5, size = 30),
    axis.title = element_text(face = "bold", size = 28),
    axis.text = element_text(size = 23)
  ) +
  coord_cartesian(ylim = c(0.55, 0.75))
p1

# Soziale Ungleichheit innerhalb verschiedener Kontinente


selected_continents <- c(
  "Africa (WID)",  
  "Asia (WID)",           
  "Europe (WID)",           
  "North America (WID)",
  "Oceania (WID)",
  "Latin America (WID)"
)

continent_base_colors <- c(
  "Africa"        = "#8c510a",
  "Asia"          = "#d8b365",
  "Europe"        = "#1b7837",
  "Latin America" = "#5ab4ac",
  "North America" = "#762a83",
  "Oceania"       = "#af8dc3"
)


gini_continents <- gini_coefficient |>
  filter(Country %in% selected_continents) |>
  mutate(Country = str_remove(Country, " \\(WID\\)")) |>
  mutate(`Gini coefficient (before tax) (World Inequality Database))` = as.numeric(`Gini coefficient (before tax) (World Inequality Database)`)) |>
  filter(!is.na(`Gini coefficient (before tax) (World Inequality Database)`)) |>
  filter(Year >= 1980)

p2 <- ggplot(gini_continents, aes(x = Year, y = `Gini coefficient (before tax) (World Inequality Database)`, color = Country, group = Country)) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(values = continent_base_colors) +
  labs(
    title = "Before Tax Social Inequality within Different Continents (1980-2024)",
    x = "Year",
    y = "Gini Coefficient",
    color = "Continent",
    caption = "Note: 'North America' includes only the United States and Canada.\n'Latin America' includes Mexico, Central America, the Caribbean, and all of South America."
  ) +
  theme_minimal() +
  theme(plot.title.position = "plot",
    plot.title = element_text(face = "bold", hjust = 0.5, size = 24),
    axis.title = element_text(face = "bold", size = 25),
    axis.text = element_text(size = 21),
    plot.caption = element_text(size = 14, hjust = 0.5 ),
    legend.title = element_text(face = "bold", size = 24),
    legend.text = element_text(size = 20)
  ) 
p2

# Soziale Ungleichheit innerhalb verschiedener Länder

selected_countries <- c(
  "China",
  "India",
  "Thailand",
  "Germany",
  "Spain",
  "Sweden",
  "South Africa",
  "Egypt",
  "Nigeria",
  "Brazil",
  "Mexico",
  "Chile",
  "United States",
  "Canada",
  "Australia"
)


continent_order <- c("Latin America", "Africa", "Asia", "Europe", "North America", "Oceania")
country_order <- c(
  "Brazil", "Chile", "Mexico",
  "South Africa", "Nigeria", "Egypt",
  "China", "India", "Thailand",
  "Germany", "Spain", "Sweden",
  "Canada", "United States",
  "Australia"
)
country_specific_colors <- c(
  "China"    = darken(continent_base_colors["Asia"], 0.4),
  "India"    = lighten(continent_base_colors["Asia"], 0.4),
  "Thailand" = darken(continent_base_colors["Asia"], 0.1),
  "Germany" = darken(continent_base_colors["Europe"], 0.4),
  "Sweden"   = lighten(continent_base_colors["Europe"], 0.4),
  "Spain"  = darken(continent_base_colors["Europe"], 0.1),
  "South Africa" = darken(continent_base_colors["Africa"], 0.4),
  "Egypt"        = lighten(continent_base_colors["Africa"], 0.4),
  "Nigeria"      = darken(continent_base_colors["Africa"], 0.1),
  "Mexico" = darken(continent_base_colors["Latin America"], 0.4),
  "Chile" = lighten(continent_base_colors["Latin America"], 0.4),
  "Brazil"  = darken(continent_base_colors["Latin America"], 0.1),
  "Australia" = darken(continent_base_colors["Oceania"], 0.1),
  "United States" = darken(continent_base_colors["North America"], 0.5),
  "Canada"        = lighten(continent_base_colors["North America"], 0.5)
)
names(country_specific_colors) <- gsub("\\..*", "", names(country_specific_colors))

gini_all_plot <- gini_coefficient1 |>
  filter(Country %in% selected_countries) |>
  mutate(
    Continent = case_when(
      Country %in% c("China", "India", "Thailand") ~ "Asia",
      Country %in% c("Germany", "Spain", "Sweden") ~ "Europe",
      Country %in% c("South Africa", "Egypt", "Nigeria") ~ "Africa",
      Country %in% c("Brazil", "Mexico", "Chile") ~ "Latin America",
      Country %in% c("United States","Canada") ~ "North America", 
      Country %in% c("Australia") ~ "Oceania",
      TRUE ~ "Other" 
    )
  ) |>
  mutate(
    Continent = factor(Continent, levels = continent_order),
    Country = factor(Country, levels = country_order)
  ) |>
  mutate(`Gini coefficient (World Bank PIP)` = as.numeric(`Gini coefficient (World Bank PIP)`)) |>
  filter(!is.na(`Gini coefficient (World Bank PIP)`)) |>
  filter(Year >= 1980)
label_data <- gini_all_plot |>
  group_by(Country, Continent) |> 
  filter(Year == max(Year)) |> 
  ungroup()
label_data_us <- label_data |> 
  filter(Country == "United States")
label_data_b <- label_data |>
  filter(Country == "Brazil")
label_data_c <- label_data |>
  filter(Country == "Chile")
label_data_m <- gini_all_plot |>
  filter(Country == "Mexico") |>
  filter(Year == 1992)
label_data_g <- gini_all_plot |>
  filter(Country == "Germany") |>
  filter(Year == 1993)
label_data_others <- label_data |> 
  filter(!Country %in% c("United States", "Brazil", "Mexico", "Chile", "Germany"))
p3 <- ggplot(gini_all_plot, aes(x = Year, y = `Gini coefficient (World Bank PIP)`)) +
  geom_line(aes(color = Country), size = 1) + 
  facet_wrap(~ Continent) + 
  scale_color_manual(values = country_specific_colors) +
  geom_text_repel(
    data = label_data_others, 
    aes(label = Country), 
    size = 4, 
    nudge_x = 1.0, 
    box.padding = 0.5,
    point.padding = 0.3,
    force = 1,segment.color = NA
  ) +
  geom_text_repel(
    data = label_data_us, 
    aes(label = Country), 
    size = 4, 
    nudge_y = 0.02, 
    point.padding = 1.0, 
    force = 4,segment.color = NA
  ) +
  geom_text_repel(
    data = label_data_g, 
    aes(label = Country), 
    size = 4, 
    nudge_y = 0.02, 
    point.padding = 1.0, 
    min.segment.length = 0,segment.color = NA
    
  )+
  geom_text_repel(
    data = label_data_b, 
    aes(label = Country), 
    size = 4, 
    nudge_y = 0.02, 
    point.padding = 1.0, 
    force = 4,segment.color = NA
  ) +
  geom_text_repel(
    data = label_data_m, 
    aes(label = Country), 
    size = 4, 
    nudge_y = -0.08, 
    nudge_x = -5,  
    point.padding = 1.0, 
    force = 4, 
    hjust = 1,segment.color = NA
  ) + 
  geom_text_repel(
    data = label_data_c, 
    aes(label = Country), 
    size = 4, 
    nudge_y = 0.04, 
    point.padding = 1.0, 
    force = 4,min.segment.length = 0,segment.color = NA
  ) +
  labs(
    title = "After Tax Social Inequality within Selected Countries by Continent (1980-2024)",
    x = "Year",
    y = "Gini Coefficient" 
  ) +
  theme_minimal() +
  theme(legend.position = "none") +
  theme(plot.title.position = "plot",
    plot.title = element_text(face = "bold", size = 20),
    axis.title = element_text(face = "bold", size = 20),
    axis.text = element_text(size = 14),
    plot.caption = element_text(size = 10),
    strip.text = element_text(size = 20, face = "bold")
  )+ 
  theme(
    plot.title = element_text(hjust = 0.5)
  )
p3  
