# Prepare packages and import table

library(readxl)
my_data<-read_xlsx("AI countries map.xlsx")


library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(plotly)
library(dplyr)
library(ggplot2)


# Get map of Europe
europe_map <- ne_countries(scale = "medium", continent = "Europe", returnclass = "sf")


# Reshape data
my_data <- my_data %>%
  rename(
    country = country,
    positive_2021 = `Positive effect 2021`,
    positive_2024 = `Positive effect 2024`,
    law_2024 = `Law number 2024`
  )

# Join map with data and rename countries

my_data <- my_data %>%
  mutate(country = recode(country,
                          "Danmark" = "Denmark",
                          "Porgugal" = "Portugal",
                          "UK" = "United Kingdom",
                          "Bosnia and Herzegovina" = "Bosnia and Herzegovina",
                          "France" = "France" 
  ))


map_data <- europe_map %>%
  left_join(my_data, by = c("name" = "country"))

# Prepare data
map_long <- map_data %>%
  pivot_longer(cols = c(positive_2021, positive_2024),
               names_to = "year", values_to = "positive_effect") %>%
  mutate(
    year = ifelse(year == "positive_2021", 2021, 2024),
    dot_size = ifelse(law_2024 == 0, 4, law_2024 * 20)  # Show dot for 0 laws too
  )

map_2021 <- map_long %>% filter(year == 2021)
map_2024 <- map_long %>% filter(year == 2024)
plot <- plot_ly()

# Add 2021 plot
plot <- plot %>%
  add_trace(
    data = map_2021,
    type = "scattergeo",
    locations = ~name,
    locationmode = "country names",
    text = ~paste0(
      "<b>Country:</b> ", name, "<br>",
      "Positive effect (2021): ", round(positive_effect, 2), "<br>",
      "Law number (2024): ", law_2024
    ),
    hoverinfo = "text",
    marker = list(
      size = map_2021$dot_size,
      color = map_2021$positive_effect,
      colorscale = "Blues",
      cmin = min(map_long$positive_effect, na.rm = TRUE),
      cmax = max(map_long$positive_effect, na.rm = TRUE),
      colorbar = list(title = "Positive Effect"),
      line = list(width = 0.5, color = "white")
    ),
    name = "Law number",
    visible = TRUE
  )

# Add 2024 plot
plot <- plot %>%
  add_trace(
    data = map_2024,
    type = "scattergeo",
    locations = ~name,
    locationmode = "country names",
    text = ~paste0(
      "<b>Country:</b> ", name, "<br>",
      "Positive effect (2024): ", round(positive_effect, 2), "<br>",
      "Law number (2024): ", law_2024
    ),
    hoverinfo = "text",
    marker = list(
      size = map_2024$dot_size,
      color = map_2024$positive_effect,
      colorscale = "Blues",
      cmin = min(map_long$positive_effect, na.rm = TRUE),
      cmax = max(map_long$positive_effect, na.rm = TRUE),
      colorbar = list(title = "Positive Effect"),
      line = list(width = 0.5, color = "white")
    ),
    name = "Law number",
    visible = FALSE
  )

# Change between between 2021 and 2024
plot <- plot %>%
  layout(
    title = "Positive Effect in Europe<br>Dot Size ∝ Law Number (2024)",
    geo = list(
      scope = 'europe',
      showframe = FALSE,
      showcoastlines = TRUE
    ),
    legend = list(
      x = 0.03,         # Left-right position (0 = far left, 1 = far right)
      y = 0.9,         # Top-bottom position (0 = bottom, 1 = top)
      xanchor = "left",
      yanchor = "middle")  
    ,
    updatemenus = list(
      list(
        type = "dropdown",
        direction = "down",
        buttons = list(
          list(method = "restyle",
               args = list("visible", list(TRUE, FALSE)),
               label = "2021"),
          list(method = "restyle",
               args = list("visible", list(FALSE, TRUE)),
               label = "2024")
        ),
        x = 0.1,
        y = 1.2
      )
    )
  )

plot


