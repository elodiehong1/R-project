##Setup library
library(dplyr)
library(lubridate)
library(ggplot2)
library(sf)              # reads GIS shapefiles
#library(ineq)            # specialist package for inequality measures
library(stringr)
library(readxl)         #For read xlsx file
library(janitor)        #For parse number
library(DescTools)      #For Lorenz lc
library(readr)          #For clean name
#########################################################
##Processing the data 
#########################################################
# Import the infant mortality rate dataset
fninfant <- "cim2023deathcohortworkbook.xlsx"
infant <- read_xlsx(fninfant, sheet = "3", skip = 9) %>%
  clean_names()

# Filter LAD data and drop mortality not actual value
infant_ua <- infant %>%
  filter(area_of_usual_residence_geography %in% 
           c("Unitary Authority",
             "Metropolitan District",
             "London Borough",
             "Non-metropolitan District")) %>% 
  filter(infant_mortality_rate!="[x]") %>% 
  mutate(infant_mortality_rate = parse_number(infant_mortality_rate)) %>% 
  filter(!is.na(infant_mortality_rate),
         !is.na(live_births),
         live_births > 0)

### Get Deprivation data form link
fnDeprivation <- "https://assets.publishing.service.gov.uk/media/691ded56d140bbbaa59a2a7d/File_7_IoD2025_All_Ranks_Scores_Deciles_Population_Denominators.csv"
depriv <- read.csv(file=fnDeprivation, stringsAsFactors = FALSE)
#Aggregate to higher level geospatial
#Merge "E06000052", "E06000053" into one observation in deprivation data,
#Due to infant data using "E06000052", "E06000053",
#which area_of_usual_residence_code = "E06000052", "E06000053"
deprivLTLA <- depriv %>% 
  select(geoCode=Local.Authority.District.code..2024.,
         geoName=Local.Authority.District.name..2024.,
         popn=Total.population..mid.2022,
         IMD=Index.of.Multiple.Deprivation..IMD..Score) %>% 
  mutate(
    geoCode = ifelse(
      geoCode %in% c("E06000052", "E06000053"),
      "E06000052, E06000053",
      geoCode
    )
  ) %>% 
  group_by(geoCode) %>% 
  summarise(geoCode=first(geoCode),
            totPopn=sum(popn),
            IMD=weighted.mean(IMD,popn)) %>% 
  ungroup

infantDepriv <- merge(infant_ua, deprivLTLA,
                      by.x="area_of_usual_residence_code", by.y="geoCode")

#########################################################
##Generating the results.
#########################################################

##Figure 1: Weighted Lorenz Curve and gini index
##due to using infant_mortality_rate,
#So, need to weight live_births, then get shared of infant deaths
L_w <- Lc(
x = infant_ua$infant_mortality_rate,
weights = infant_ua$live_births
)
#Count gini index
gini_w <- L_w$Gini

plot(
  L_w,
  main = "",
  cex.main=0.95,
  xlab = "Cumulative proportion of live births",
  ylab = "Cumulative share of infant deaths",
  col = "black",
  lwd = 2
)
axis(1, at = seq(0, 1, by = 0.1)) 
box() 
#add equality line
abline(0, 1, lty = 2)
text(
  x = 0.25, 
  y = 0.75,
  labels = paste0("Weighted Gini = ", round(gini_w, 3)),
  cex = 1.1
)
#Add a line showing the x-axis value corresponding 
#to 50% of the y-axis value.
x_vals <- L_w$p
y_vals <- L_w$L
#value of x when y=0.5, this value will be written in the report
x_at_05 <- approx(y_vals, x_vals, xout = 0.5)$y
x_at_05

##Figure 2:
#Plot infant mortality rate by deprivation
ggplot(infantDepriv,
       aes(x = IMD,
           y = infant_mortality_rate)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm",
              aes(weight = live_births),
              se = TRUE,
              colour = "black") +
  labs(
    #title = "Infant Mortality Rate and Area Deprivation",
    x = "Deprivation score (higher indicates more deprived)",
    y = "Infant mortality rate (per 1000 live births)"
  ) +
  theme_minimal()+
  theme(
    plot.title = element_text(hjust = 0.5)
  )