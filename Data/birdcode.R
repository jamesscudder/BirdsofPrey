
#load packages ------

library(tidyverse)
library(here)
library(janitor) 
library(RColorBrewer)
library(skimr)
library(readr)


#load data -------

birds <- read_csv(here("Data", "raptors.csv"))

# Data exploration --------

install.packages("skimr")
library(skimr)
skim(birds)

# Data cleaning ---------

# Pipe %>% functions (cleaner code) --------

birds <- read_csv(here("Data", "raptors.csv"))
cleanbirds <- birds %>%
  clean_names() %>%
  rename(object_id = objectid) %>%
  select(incident_type, details, county, month, year) %>%
  arrange(desc(year))

# Save cleanbirds as csv for future access -------

write_csv(cleanbirds,"clean_birds.csv")

# Create a new data frame with the number of incidents by county -------

incidents_by_county <- cleanbirds %>%
  group_by(county) %>%
  count()

# Display the top 10 counties with the most incidents -------

incidents_by_county %>%
  arrange(desc(n)) %>%
  head(10)

# incidents by year -------

incidents_by_year <- cleanbirds %>%
  group_by(year) %>%
  count()

max_incidents <- incidents_by_year %>%
  arrange(desc(n)) %>%
  head(1)

# Most Common Incident-Type -------

most_common_incident_type <- cleanbirds %>%
  group_by(incident_type) %>%
  count() %>%
  arrange(desc(n)) %>%
  head(10)

view(most_common_incident_type)

# Most Incidents by Species --------

incident_species <- cleanbirds %>%
  group_by(details, year) %>%
  count() %>%
  arrange(desc(year))

view(incident_species)

# Visualization of the data with ggplot ------- 

# Number of Incidents Per Year --------

ggplot(incidents_by_year, aes(x = year, y = n)) +
  geom_bar(stat = "identity", fill  = "blue") +
  xlab("Year") +
  ylab("Num of Incidents")

# Most Common Incident type -------

ggplot(most_common_incident_type, aes(x = incident_type, y = n)) +
  geom_bar(stat = "identity", fill = "blue") +
  coord_flip() +
  xlab("Incident type") +
  ylab("Num of Incidents")

# Most Common Species -------

ggplot(incident_species %>%
         group_by(details) %>%
         summarise(n = sum(n)) %>%
         arrange(desc(n)) %>%
         head(10),
       aes(x = details, y = n)) +
  geom_bar(stat="identity",fill = "blue" ) +
  coord_flip() +
  xlab("Species") +
  ylab("Num of Incidents") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8))

# Incidents by County (Top 10) --------

ggplot(incidents_by_county %>%
         arrange(desc(n)) %>%
         head(10),
       aes(x = reorder(county, n), y = n)) +
  geom_bar(stat="identity", fill = "blue") +
  coord_flip() +
  xlab("County") +
  ylab("Num of Incidents") +
  theme(axis.text.x = element_text(angle = 90))


