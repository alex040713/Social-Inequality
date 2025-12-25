# load package
library(dplyr)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)


# load data 
gini <- read.csv('Data/gini-coefficient.csv')
world_map <- ne_countries(scale = 'large', returnclass = 'sf')


# gini coefficient in 2018
gini |> 
  count(Year, sort = TRUE)

gini2018 <- gini |> 
  filter(Year == 2018)

p1<-ggplot(gini2018, aes(x = "",Gini.coefficient..World.Bank.PIP.)) +
  geom_violin(fill = "lightblue") +
  theme_minimal() +
  labs(
    title = 'After Tax Gini Coefficient Distribution(2018)',
    y = "Gini Coefficient",
    x = ""
  ) +
  theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 28), 
      axis.title = element_text(face = "bold", size = 25),              
      axis.text = element_text(size = 21),                              
      legend.position = "right",
      legend.title = element_text(size = 24, face = "bold"), 
      legend.text = element_text(size = 20)
    ) +
  theme(
    plot.title = element_text(hjust = 0.5)
  ) +
  ylim(0, 1)

world_map_with_gini <- world_map |> 
  left_join(gini2018, by = c('brk_name' = 'Country'))

p2 <- ggplot(world_map_with_gini) +
  geom_sf(aes(fill = Gini.coefficient..World.Bank.PIP.)) +
  scale_fill_distiller(direction = 1, palette = 'YlOrRd', name = 'Gini Coefficient',na.value = 'white') +
  labs(
    title = 'After Tax Gini Coefficient Distribution(2018)',
  ) +
  theme_minimal() +
  theme(plot.title.position = "plot",
    plot.title = element_text(face = "bold", hjust = 0.5, size = 28), 
    axis.title = element_text(face = "bold", size = 25),              
    axis.text = element_text(size = 21),                              
    legend.position = "right",
    legend.title = element_text(size = 24, face = "bold"), 
    legend.text = element_text(size = 20)
  )


gini_manipulate <- world_map_with_gini |> 
  st_drop_geometry() |> 
  filter(!is.na(Gini.coefficient..World.Bank.PIP.))

latam_in_na_countries <- c(
  "Mexico", "Panama", "Costa Rica", "Nicaragua", "Honduras", 
  "El Salvador", "Guatemala", "Belize", "Cuba", "Dominican Rep.", 
  "Haiti", "Jamaica", "Trinidad and Tobago", "Bahamas"
)


gini_manipulate <- gini_manipulate |> 
  mutate(
    continent = case_when(
      name %in% latam_in_na_countries & continent == "North America" ~ "Latin America",
      continent == "South America" ~ "Latin America",
      
      TRUE ~ continent
    )
  )

# dataset which can be manipulated
gini_manipulate <- world_map_with_gini |> 
  filter(!continent %in% c('Seven seas (open ocean)', 'Oceania', 'North America', "Antarctica"))




gini_manipulate <- gini_manipulate |> 
  mutate(continent = case_match(continent,
                                "South America" ~ "Latin America",  
                                .default = continent                
  ))

p3 <- ggplot(gini_manipulate, aes(
  x = Gini.coefficient..World.Bank.PIP.)) +
  geom_density(alpha = 0.7,   fill = 'lightblue') +
  facet_wrap(~continent) +
  labs(
    title = "After Tax Gini Coefficient Density by Continent (2018)",
    x = "Gini Coefficient",
    y = "Density"
  ) +
  theme_minimal() +
  theme(legend.position = "none") +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 28), 
    axis.title = element_text(face = "bold", size = 25),              
    axis.text = element_text(size = 21),                              
    legend.position = "right",
    legend.title = element_text(size = 24, face = "bold"), 
    legend.text = element_text(size = 20),
    strip.text = element_text(size = 24, face = "bold")
  ) +
  xlim(0.1,0.7)

