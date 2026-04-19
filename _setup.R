# _setup.R
library(ggplot2)
library(brand.yml)
library(yaml)

# 1. Read the raw YAML list into R
brand_data <- yaml::read_yaml(here::here("_brand.yml"))

# 2. Flatten the background list to a single string (e.g., light mode)
brand_data$color$background <- brand_data$color$background$light
brand_data$color$foreground <- brand_data$color$foreground$light
brand_data$typography$headings$color <- brand_data$typography$headings$color$light

# Note: If you also defined light/dark for your foreground, flatten that too:
# brand_data$color$foreground <- brand_data$color$foreground$light

# 3. Convert it back to a formal brand object and generate the theme
my_theme <- theme_brand_ggplot2(as_brand_yml(brand_data)) +
    theme(
        text = element_text(size = 13,
        family = brand_data$typography$fonts[[1]]$family)
    )

theme_set(my_theme)
