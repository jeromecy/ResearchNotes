# Load necessary libraries
library(dplyr)
library(tidyr)
library(purrr)
library(highcharter)
library(RColorBrewer)

# Create dummy data
set.seed(123)
data <- data.frame(
  Region = rep(c("Western", "Southern", "Northern"), each = 30),
  Crop = rep(c("Wheat", "Barley", "Canola"), times = 30),
  Disease = sample(c("Disease1", "Disease2", "Disease3"), 90, replace = TRUE),
  YLD = runif(90, 5, 10)  # Total $ instead of $/ha
)

# Aggregate data by region for the main series
region_data <- data %>%
  group_by(Region) %>%
  summarise(sumYLD = round(sum(YLD), 3)) %>%
  mutate(Region = paste(Region, "Region"),
         drilldown = Region)

national_data <- data %>%
  summarise(sumYLD = round(sum(YLD), 3)) %>%
  mutate(Region = 'National',
         drilldown = 'National')

combined_regionaldata <- bind_rows(region_data, national_data) %>% 
  arrange(desc(sumYLD))

crop_data <- data %>%
  group_by(Region, Crop) %>%
  summarise(sumYLD = round(sum(YLD), 3)) %>% 
  mutate(Region = paste(Region, "Region"))

crop_nationaldata <- data %>%
  group_by(Crop) %>%
  summarise(sumYLD = round(sum(YLD), 3)) %>% 
  mutate(Region = 'National')

combined_crop_data <- bind_rows(crop_data, crop_nationaldata) %>%
  arrange(desc(sumYLD)) %>% 
  nest(data = c(Crop, sumYLD))

combined_crop_data$data <- lapply(1:nrow(combined_crop_data), function(i) {
  combined_crop_data$data[[i]]$drilldown <- paste(combined_crop_data$Region[i], combined_crop_data$data[[i]]$Crop)
  combined_crop_data$data[[i]]
})

combined_crop_data <- combined_crop_data %>%
  mutate(data = map(data, list_parse2))

drilldown_data <- data %>%
  group_by(Region, Crop, Disease) %>%
  summarise(sumYLD = round(sum(YLD), 3)) %>% 
  mutate(Region = paste(Region, "Region"))

drilldown_nationaldata <- data %>%
  group_by(Crop, Disease) %>%
  summarise(sumYLD = round(sum(YLD), 3)) %>% 
  mutate(Region = 'National')

combined_drilldown_data <- bind_rows(drilldown_data, drilldown_nationaldata) %>%
  arrange(desc(sumYLD)) %>% 
  nest(data = c(Disease, sumYLD)) %>%
  mutate(data = map(data, list_parse2))



# Define colors
colors <- brewer.pal(12, "Paired")

# Create the highcharter plot
highchart() %>%
  hc_chart(type = "bar", colorByPoint = TRUE) %>%
  hc_xAxis(type = "category") %>%
  hc_add_series(
    name = "National and Regional Yield Loss Values",
    data = c(
      lapply(1:nrow(combined_regionaldata), function(i) {
        list(
          name = combined_regionaldata$Region[i],
          y = combined_regionaldata$sumYLD[i],
          drilldown = combined_regionaldata$drilldown[i],
          color = colors[i %% length(colors) + 1]
        )
      })
    ),
    colorByPoint = TRUE,
    colors = colors
  ) %>%
  hc_drilldown(
    series = c(
      lapply(1:nrow(combined_crop_data), function(i) {
        list(
          name = paste(combined_crop_data[[1]][i], "Region"),
          id = combined_crop_data[[1]][i],
          data = 
            lapply(1:length(combined_crop_data[[2]][i][[1]]), function(j) {
              list(
                name = combined_crop_data[[2]][i][[1]][[j]][[1]],
                y = combined_crop_data[[2]][i][[1]][[j]][[2]],
                drilldown = combined_crop_data[[2]][i][[1]][[j]][[3]],
                color = colors[j %% length(colors) + 1]
              )
            })
        )
      }),
      lapply(1:nrow(combined_drilldown_data), function(i) {
        list(
          name = paste(combined_drilldown_data[[1]][i], combined_drilldown_data[[2]][i]),
          id = paste(combined_drilldown_data[[1]][i], combined_drilldown_data[[2]][i]),
          data = 
            lapply(1:length(combined_drilldown_data[[3]][i][[1]]), function(j) {
              list(
                name = combined_drilldown_data[[3]][i][[1]][[j]][[1]],
                y = combined_drilldown_data[[3]][i][[1]][[j]][[2]],
                color = colors[j %% length(colors) + 1]
              )
            })
        )
      })
    )
  ) %>% 
  hc_title(text = "Yield Loss Value (Total $) by Crop and Disease") %>%
  hc_xAxis(type = "category") %>%
  hc_yAxis(title = list(text = "Yield Loss Value (Total $)"),
           labels = list(formatter = JS("function() { return (this.value) + ' $'; }")))  %>%
  hc_plotOptions(bar = list(stacking = 'normal', groupPadding = 0.1, maxPointWidth = 50,  # Set max width of bars
                            dataLabels = list(enabled = TRUE, format = '{point.y} $'))) %>%
  hc_legend(enabled = TRUE) %>%
  hc_tooltip(shared = TRUE, crosshairs = TRUE) %>%
  hc_exporting(enabled = TRUE,
               chartOptions = list(
                 chart = list(backgroundColor = "#FFFFFF")
               )) %>%
  hc_responsive(rules = list(list(
    condition = list(maxWidth = 500),
    chartOptions = list(legend = list(enabled = FALSE))
  )))


