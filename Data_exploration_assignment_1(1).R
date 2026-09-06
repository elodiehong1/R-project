#Setup package
library(ggplot2)
library(tidyr)
library(dplyr)
library(ggridges)
library(lubridate)

#Import data
fnhouseprice <- "houseprice.csv"
houseprice <- read.csv(fnhouseprice)

# Question 1: Univariate feature
##First figure
options(scipen = 999)
par(mfrow = c(1, 2))
hist(houseprice$price/ 1e6,
     breaks = 30,
     xlim = c(0, max(houseprice$price/ 1e6)),
     main = "Distribution of House Prices",
     cex.main = 0.95,
     xlab = "Price (Million USD)")
hist(
  log(houseprice$price),
  breaks = 30,
  main = "Distribution of Log House Prices",
  cex.main = 0.95,
  xlab = "Log Price"
)

##Question 1 second figure
par(mfrow = c(1, 1))
freq <- table(houseprice$Bedrooms)

bar_pos <- barplot(
  freq,
  #main = "Distribution of Number of Bedrooms",
  cex.main = 0.95,
  xlab = "Number of Bedrooms",
  ylab = "Frequency",
  ylim = c(0, max(freq) * 1.1),
  col = "grey80",
  border = "black"
)
box()
text(
  x = bar_pos,
  y = freq,
  labels = freq,
  pos = 3#,      
  #cex = 0.95     
)

# Question 2: Boxplot for detecting outliers in price
# First figure
city_missing <- houseprice %>%
  mutate(City_status = ifelse(is.na(City), "NA", "Non-NA")) %>%
  count(City_status) %>%
  mutate(prop = n / sum(n))

ggplot(city_missing, aes(x = City_status, y = n)) +
  geom_col(width = 0.2, fill = "grey70", color = "black") +
  geom_text(
    aes(label = scales::percent(prop, accuracy = 0.1)),
    vjust = -0.5,
    size = 4
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.25))  # above 25% space for pecentage
  ) +
  labs(
    #title = "Missingness in City Variable",
    x = "Value of City",
    y = "Number of observations"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 10),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    plot.margin = margin(80, 170, 80, 170)
  )
##Question 2 second figure
##count median per country
medians <- tapply(houseprice$price / 1e6,
                  houseprice$Country,
                  median)
bp <- boxplot(price/1e6 ~ Country, data = houseprice,
        #main = "House Price Distribution by Country",
        cex.main = 0.95,
        ylab = "Price (Million USD)",
        axes = FALSE)
countries <- bp$names
x_pos <- seq_along(countries) 
axis(2)
axis(1, at = x_pos, labels = FALSE)
axis(1, at = x_pos, labels = FALSE) 
text( x = x_pos, 
      y = par("usr")[3] - 0.05 * diff(par("usr")[3:4]), 
      labels = countries, srt = 45, adj = 1, cex = 0.95,
      xpd = TRUE) 
text(
  x = x_pos,
  y = medians[countries],
  labels = round(medians[countries], 2),
  pos = 3,
  cex = 0.75
)
box()


# Question 3 First figure
houseprice2 <- houseprice %>%
  mutate(
    Bedrooms_label = ifelse(
      Bedrooms == 1,
      "1 bedroom",
      paste0(Bedrooms, " bedrooms")
    )
  )

ggplot(houseprice2, aes(x = Surface, y = log(price))) +
  geom_point(alpha = 0.5, colour = "black") +
  facet_wrap(~ Bedrooms_label) +
  labs(
    x = "Surface (square meters)",
    y = "Log House Price",
    #title = "Relationship between Surface and Log House Price by Number of Bedrooms"
  ) +
  theme_minimal() +
  theme(
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.8),
    panel.spacing = unit(1, "lines"),
    strip.background = element_rect(
      fill = "grey70",
      colour = "black",
      linewidth = 0.6
    ),
    strip.text = element_text(
      face = "bold",
      size = 9
    )
  )

# Question 3 Second figure
vars <- houseprice[, c("Surface", "Density", "price")]
vars <- vars[vars$Density > 0, ]
pairs(
  log(vars),
  #main = "Scatterplot Matrix of Key Numerical Variables (Log-transformed)",
  pch = 16,
  col = rgb(0, 0, 0, 0.3)
)


# Question 4: First figure Surface vs Price
ggplot(houseprice, aes(x = Surface, y = log(price))) +
  geom_point(alpha = 0.3, colour = "black") +
  geom_smooth(
    method = "loess",
    se = TRUE,
    colour = "black",
    fill = "grey70"
  ) +
  labs(
    x = "Surface (square meters)",
    y = "Log House Price",
    #title = "Surface and Log House Price"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      size = 0.8
    )
  )

# Question 4: Second figure 
houseprice_density <- houseprice[houseprice$Density > 0, ]

ggplot(houseprice_density, aes(x = Density, y = log(price))) +
  geom_point(shape = 16, alpha = 0.45, size = 1.2, colour = "black") +
  facet_wrap(~ Country) +
  labs(
    x = "Population Density",
    y = "Log House Price",
    #title = "Relationship between Population Density and Log House Price by Country"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    panel.border = element_rect(colour = "black", fill = NA, size = 0.8),
    panel.spacing = unit(0.5, "lines"),
    strip.background = element_rect(fill = "grey70", colour = "black", linewidth = 0.6),
    strip.text = element_text(size = 9),
    panel.grid.major = element_blank(),   
    panel.grid.minor = element_blank(),  
    plot.margin = margin(t = 10, r = 30, b = 10, l = 20)
  )

##Question 5 First figure
houseprice <- houseprice %>%
  mutate(
    date1 = as.Date(date),
    Year = year(date1)
  )

hp_time <- houseprice %>%
  group_by(Year) %>%
  summarise(
    mean_log_price = mean(log(price), na.rm = TRUE),
    .groups = "drop"
  )

ggplot(hp_time, aes(x = Year, y = mean_log_price)) +
  geom_line(colour = "#4E79A7",, size = 0.9) +
  geom_smooth(
    method = "loess",
    se = TRUE,
    colour = "#E15759",
    fill = "grey70"
  ) +
  labs(
    x = "Year",
    y = "Mean Log House Price",
    #title = "Overall LOESS-Smoothed Trend of Mean Log House Prices"
  ) +
  scale_x_continuous(
    limits = c(2027, 2036),
    breaks = seq(2027, 2036, by = 1)
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      size = 11       
    ),
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      linewidth = 0.8
    )
  )

##Question 5 Second figure
houseprice <- houseprice %>%
  mutate(
    date1 = as.Date(date),
    Year = year(date1)
  )

houseprice_country_time <- houseprice %>%
  group_by(Country, Year) %>%
  summarise(
    mean_log_price = mean(log(price), na.rm = TRUE),
    .groups = "drop"
  )
ggplot(
  houseprice_country_time,
  aes(
    x = Year,
    y = mean_log_price,
    colour = Country,
    group = Country
  )
) +
  geom_line(alpha = 0.8, size = 0.9) +
  scale_colour_manual(values = c(
    "#1B9E77",  # green
    "#D95F02",  # orange
    "#7570B3",  # purple
    "#F781BF",  # pink
    "#66A61E",  # olive
    "#000000",  # black
    "#A6761D",  # brown
    "#666666",  # grey
    "#1F78B4",  # blue
    "#E41A1C"   # red
  )) +
  scale_x_continuous(
    limits = c(2027, 2036),
    breaks = seq(2027, 2036, by = 1)
  ) +
  scale_y_continuous(
    limits = c(13, NA)
  ) +
  labs(
    x = "Year",
    y = "Mean Log House Price",
    #title = "Mean Log House Prices over Time by Country",
    colour = "Country"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      linewidth = 0.8
    )
  )

