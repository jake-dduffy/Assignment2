install.packages(c("ggplot2", "sf", "dplyr"))
install.packages("plotly")
install.packages("labels")
install.packages("patchwork")
install.packages("cowplot")
install.packages("magick")
install.packages("readr")


library(patchwork)
library(ggplot2)
library(labels)
library(plotly)
library(sf)
library(dplyr)
library(cowplot)
library(magick)
library(scales)
library(readr)

unicef_metadata_quarto_2019 <- read.csv("unicef_metadata_quarto_2019.csv")
unicef_indicator_2 <- read.csv("unicef_indicator_2.csv")
data_join <- full_join(unicef_metadata_quarto_2019, unicef_indicator_2, by = c("country", "alpha_2_code", "alpha_3_code", "numeric_code", "year" = "time_period"))

unicef_metadata_continent_r <- read.csv("unicef_metadata_continent_r.csv")

# Map 

map_world <- map_data("world")

map_data_join <- full_join(data_join, map_world, by = c("country" = "region"))


ggplot(map_data_join, aes(x = long, y = lat, group = group, fill = obs_value)) +
  geom_polygon(color = "white") + 
  scale_fill_viridis_c(name = "SIGI Value (%)") + 
  theme_minimal() + 
  labs(x = NULL, y = NULL, fill = "SIGI Value") + 
  coord_fixed(1.3) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank())

# Scatter Plot 1 GDP VS SIGI

ggplot(data_join, aes(x = gdpPercap, y = obs_value, color = Continent, size = Population)) +
  geom_point(alpha = 0.6) +
  scale_color_brewer(palette = "Set1") +
  labs(title = "GDP vs. SIGI Index", x = "GDP Per Capita (USD)", y = "SIGI Index (%)") +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_x_continuous(limits = c(0, 100000), 
                     labels = scales::comma)
  
scatter1 <- ggplot(data_join, aes(x = gdpPercap, y = obs_value, color = Continent, size = Population)) +
  geom_point(alpha = 0.6) +
  scale_color_brewer(palette = "Set1") +
  labs(title = "GDP vs. SIGI Index", x = "GDP Per Capita (USD)", y = "SIGI Index (%)") +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_x_continuous(limits = c(0, 100000), 
                     labels = scales::comma)

ttscatter1 <- ggplot(data_join, aes(x = gdpPercap, y = obs_value, color = Continent, size = Population)) +
  geom_point(alpha = 0.6) +
  scale_color_brewer(palette = "Set1") +
  labs(title = "GDP vs. SIGI Index", x = "GDP Per Capita (USD)", y = "SIGI Index (%)") +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_x_continuous(limits = c(0, 100000), 
                     labels = scales::comma)

ggplotly(ttscatter1, tooltip = c("x", "y", "color", "size", "text")) %>%
  layout(hoverlabel = list(bgcolor = "white"))

ttscatter10 <- ggplotly(ttscatter1, tooltip = c("x", "y", "color", "size", "text")) %>%
  layout(hoverlabel = list(bgcolor = "white"))

# Scatter Plot 2 GNI VS SIGI

ggplot(data_join, aes(x = GNI / 1e9, y = obs_value, color = Continent, size = Population)) +
  geom_point(alpha = 0.6) +
  scale_color_brewer(palette = "Set1") +
  labs(title = "GNI vs. SIGI Index", x = "GNI (in Billions USD)", y = "SIGI Index (%)") +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_x_continuous(limits = c(0, 6000))

scatter2 <- ggplot(data_join, aes(x = GNI / 1e9, y = obs_value, color = Continent, size = Population)) +
  geom_point(alpha = 0.6) +
  scale_color_brewer(palette = "Set1") +
  labs(title = "GNI vs. SIGI Index", x = "GNI (in Billions USD)", y = "SIGI Index (%)") +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_x_continuous(limits = c(0, 6000))

ttscatter2 <- ggplot(data_join, aes(x = GNI / 1e+9, y = obs_value, color = Continent, size = Population)) +
  geom_point(alpha = 0.6) +
  scale_color_brewer(palette = "Set1") +
  labs(x = "GNI (in Billions USD)", y = "SIGI Index (%)") +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_y_continuous(labels = scales::unit_format(unit = "%")) +
  scale_x_continuous(
    limits = c(0, 6000),
    labels = scales::unit_format(unit = "$"))

ggplotly(ttscatter2, tooltip = c("x", "y", "color", "size", "text")) %>%
  layout(hoverlabel = list(bgcolor = "white"))

ttscatter20 <- ggplotly(ttscatter2, tooltip = c("x", "y", "color", "size", "text")) %>%
  layout(hoverlabel = list(bgcolor = "white"))

# Scatter Plot 3 Life Exp VS SIGI

ggplot(data_join, aes(x = LifeExp, y = obs_value, color = Continent, size = Population)) +
  geom_point(alpha = 0.6) +
  scale_color_brewer(palette = "Set1") +
  labs(title = "Life Expectancy vs. SIGI Index", x = "Life Expectancy (Years)", y = "SIGI Index (%)") +
  theme_minimal() +
  scale_y_continuous(labels = scales::unit_format(unit = "%")) +
  scale_x_continuous(labels = scales::unit_format(unit = "Yrs")) +
  theme(legend.position = "none")

scatter3 <- ggplot(data_join, aes(x = LifeExp, y = obs_value, color = Continent, size = Population)) +
  geom_point(alpha = 0.6) +
  scale_color_brewer(palette = "Set1") +
  labs(x = "Life Expectancy (Years)", y = "SIGI Index (%)") +
  theme_minimal() +
  scale_y_continuous(labels = scales::unit_format(unit = "%")) +
  scale_x_continuous(labels = scales::unit_format(unit = "Yrs")) +
  theme(legend.position = "none")

ttscatter3 <- ggplot(data_join, aes(x = LifeExp, y = obs_value, color = Continent, size = Population)) +
  geom_point(alpha = 0.6) +
  scale_color_brewer(palette = "Set1") +
  labs(x = "Life Expectancy (Years)", y = "SIGI Index (%)") +
  theme_minimal() +
  scale_y_continuous(labels = scales::unit_format(unit = "%")) +
  scale_x_continuous(labels = scales::unit_format(unit = "Yrs")) +
  theme(legend.position = "none")

ggplotly(ttscatter3, tooltip = c("x", "y", "color", "size", "text")) %>%
  layout(hoverlabel = list(bgcolor = "white"))

ttscatter30 <- ggplotly(ttscatter3, tooltip = c("x", "y", "color", "size", "text")) %>%
  layout(hoverlabel = list(bgcolor = "white"))

# Scatter Plot 4 Inflation VS SIGI

ggplot(data_join, aes(x = Inflation, y = obs_value, color = Continent, size = Population)) +
  geom_point(alpha = 0.6) +
  scale_color_brewer(palette = "Set1") +
  labs(x = "Inflation (%)", y = "SIGI Index (%)") +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_y_continuous(labels = scales::unit_format(unit = "%")) +
  scale_x_continuous(limits = c(0, 50),
                     labels = scales::unit_format(unit = "%"))

scatter4 <- ggplot(data_join, aes(x = Inflation, y = obs_value, color = Continent, size = Population)) +
  geom_point(alpha = 0.6) +
  scale_color_brewer(palette = "Set1") +
  labs(x = "Inflation (%)", y = "SIGI Index (%)") +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_y_continuous(labels = scales::unit_format(unit = "%")) +
  scale_x_continuous(limits = c(0, 50),
                     labels = scales::unit_format(unit = "%"))

ttscatter4 <- ggplot(data_join, aes(x = Inflation, y = obs_value, color = Continent, size = Population)) +
  geom_point(alpha = 0.6) +
  scale_color_brewer(palette = "Set1") +
  labs(x = "Inflation (%)", y = "SIGI Index (%)") +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_y_continuous(labels = scales::unit_format(unit = "%")) +
  scale_x_continuous(limits = c(0, 50),
                     labels = scales::unit_format(unit = "%"))

ggplotly(ttscatter4, tooltip = c("x", "y", "color", "size", "text")) %>%
  layout(hoverlabel = list(bgcolor = "white"))

ttscatter40 <- ggplotly(ttscatter4, tooltip = c("x", "y", "color", "size", "text")) %>%
  layout(hoverlabel = list(bgcolor = "white"))

# Bar Chart 1 SIGI VS Continents

data_join <- filter(data_join, !is.na(Continent) & Continent != "Africa" & Continent != "Asia" & Continent != "Europe" & Continent != "North America" & Continent != "Oceania" & Continent != "South America")

averages <- data_join %>%
  group_by(Continent) %>%
  summarise(average_obs_value = mean(obs_value, na.rm = TRUE))

ggplot(averages, aes(x = Continent, y = average_obs_value, fill = Continent)) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal() +
  labs(
    x = "Continent",
    y = "SIGI Index (%)") +
  theme(axis.text.x = element_blank(),
        legend.position = c(0.93,0.88))

bc1 <- ggplot(averages, aes(x = Continent, y = average_obs_value, fill = Continent)) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal() +
  labs(
    x = "Continent",
    y = "SIGI Index (%)") +
  theme(axis.text.x = element_blank(),
        legend.position = c(0.8,0.88))

ggplot(averages, aes(x = Continent, y = average_obs_value, fill = Continent)) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal() +
  labs(
    x = "Continent",
    y = "SIGI Index (%)") +
      theme(axis.text.x = element_blank(),
            legend.position = c(0.9,0.9))


# Bar Chart 2 GDP VS Continents

averagesGDP <- data_join %>%
  group_by(Continent) %>%
  summarise(average_gdpPercap = mean(gdpPercap, na.rm = TRUE))

ggplot(averagesGDP, aes(x = Continent,y = average_gdpPercap, fill = Continent)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal() +
  labs(
    x = "Continent",
    y = "GDP Per Capita (USD)") +
  theme(axis.text.x = element_blank())

bc2 <- ggplot(averagesGDP, aes(x = Continent,y = average_gdpPercap, fill = Continent)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal() +
  labs(
    x = "Continent",
    y = "GDP Per Capita (USD)") +
  theme(axis.text.x = element_blank())

bc1 + bc2 + plot_layout(ncol = 2)

ggplot(data_join, aes(x = GNI / 1e+9, y = obs_value, color = Continent, size = Population)) +
  geom_point(alpha = 0.6) +
  scale_color_brewer(palette = "Set1") +
  labs(x = "GNI (in Billions USD)", y = "SIGI Index (%)") +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_y_continuous(labels = scales::unit_format(unit = "%")) +
  scale_x_continuous(
    limits = c(0, 6000),
    labels = scales::unit_format(unit = "$"))
    
ggplot(data_join, aes(x = gdpPercap, y = obs_value, color = Continent, size = Population)) +
  geom_point(alpha = 0.6) +
  scale_color_brewer(palette = "Set1") +
  labs(x = "GDP Per Capita (USD)", y = "SIGI Index (%)") +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_y_continuous(labels = scales::unit_format(unit = "%")) +
  scale_x_continuous(
    limits = c(0, 100000), 
    labels = scales::unit_format(unit = "$"),
    )

# Time Series Analysis

tsaverages <- unicef_metadata_continent_r %>%
  group_by(Continent) %>%
  summarise(GDP = mean(GDP, na.rm = TRUE))


 
 timeseries_avg_gdp_europe <- unicef_metadata_continent_r %>%
   filter(Continent == "Europe") %>%
   group_by(year) %>%
   filter(year >= 1980 & year <= 2022) %>%
   summarise(AvgGDP = mean(GDP, na.rm = TRUE)) %>%  
   ggplot(aes(x = year, y = AvgGDP)) +  
   geom_line(linewidth=1.5, color="Green") +  
   labs(
     title = "Average GDP Over Time for Europe",  
     x = "Year",  
     y = "GDP (in USD)"  
   ) +
   theme_minimal()

 timeseries_avg_gdp_europe 
 
 # Time series attempt 2
 
 average_gdp_by_year_continent <- unicef_metadata_continent_r %>%
   group_by(year, Continent) %>%
   filter(year >= 1980 & year <= 2022) %>%
   summarise(AverageGDP = mean(GDP, na.rm = TRUE)) %>%
   filter(Continent %in% c("Europe", "Africa"))

 timeseries2 <- ggplot(average_gdp_by_year_continent, aes(x = year, y = AverageGDP, color = Continent)) +
   geom_line(linewidth=1.5) +
   theme_minimal() + # Use a minimal theme
   labs(
        x = "Year",
        y = "GDP in USD",
        color = "Continent") +
   scale_x_continuous(breaks = seq(1980, 2022, by = 5)) +
   scale_color_manual(values = c("Europe" = "green", "Africa" = "red"))

 print(timeseries2) 
 
 
 