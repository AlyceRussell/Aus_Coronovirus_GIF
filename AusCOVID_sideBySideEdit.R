devtools::install_github("RamiKrispin/coronavirus", force = TRUE) #load updates to data in github
library(coronavirus)
library(tidyverse)
library(gganimate)
library(transformr)
library(gifski)
library(magick)
library(lubridate)
library(grid)
library(scales)

data("coronavirus")
head(coronavirus)
tail(coronavirus)
names(coronavirus)


# Subset Aussie Data and Update labels
xAus <- coronavirus %>% 
  filter(Country.Region == "Australia")
colnames(xAus) <- c("Region", "Country", "Lat", "Long", "Date", "Cases", "Type")
xAus$Region <- factor(xAus$Region, labels = c("ACT", "NSW", "NT", "QLD", "SA", "Tas", "Vic", "WA"))

# Australia by Region (State/Territory)
cumDat <- xAus %>%
  filter(Region != "Diamond Princess") %>% #removed Diamond Princess in original iteration
  filter(Type == "confirmed") %>%
  filter(Cases != -1) %>% 
  group_by(Region) %>%
  mutate(cum_Cases = cumsum(Cases)) %>%
  mutate(log_cum_Cases = log10(cumsum(Cases)+1)) %>% # Add 0 for transformation after cumSum 
  ungroup()


# Create formatted labels
cumDatformatted <- cumDat %>%
  group_by(Date) %>%
  mutate(Value_lbl = paste0("  ",cum_Cases)) %>%
  group_by(Region) %>% 
  ungroup()


# Create plots (p1 = cumulative sum, p2 = natural log of cumulative sum)
cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

p1 <- ggplot(cumDatformatted, aes(Date, cum_Cases, group = Region, colour = Region)) + 
  geom_line(size=1.2) + 
  scale_colour_manual(values=cbPalette) +
  theme_minimal() +
  geom_text(aes(y=cum_Cases, label = Value_lbl, hjust=0)) + # Adds changing text
  transition_reveal(Date) +  # Changes plot by date
  coord_cartesian(clip = 'off') + 
  labs(title = 'Date: {frame_along}',
       y = "Cumulative Confirmed Cases", 
       caption = "COVID-19 | Dr Alyce Russell  |  Twitter: @nerdrusty  |") + 
  scale_x_date(limits = as.Date(c('2020-01-22','2020-04-30'))) +  #I add 2 weeks to x-axis so you can see the numbers
  theme(axis.title.x = element_text(face="bold", size=14),
        axis.title.y = element_text(face="bold", size=14),
        plot.caption = element_text(hjust=1, face="italic", color="#333333"),
        legend.position = c(.05, .95),
        legend.justification = c("left", "top")) 

a_gif <- animate(p1, 200, fps = 20, width = 310, height = 400)

p2 <- ggplot(cumDatformatted, aes(Date, log_cum_Cases, group = Region, colour = Region)) + 
  geom_line(size=1.2) + 
  scale_colour_manual(values=cbPalette) +
  theme_minimal() +
  geom_text(aes(y=log_cum_Cases, label = Value_lbl, hjust=0)) + # Adds changing text
  transition_reveal(Date) +  # Changes plot by date
  coord_cartesian(clip = 'off') + 
  labs(title = " ",
         y = "Log10 Cumulative Confirmed Cases", 
       caption = "Source: coronavirus package, last updated 14/04/2020") + 
  scale_x_date(limits = as.Date(c('2020-01-22','2020-04-30'))) + #I add 2 weeks to x-axis so you can see the numbers
  scale_y_continuous(breaks = c(0,1,2,3),
                     labels = c(1,10,100,1000)) +
  theme(axis.title.x = element_text(face="bold", size=14),
        axis.title.y = element_text(face="bold", size=14),
        plot.caption = element_text(hjust=4.8, face="italic", color="#333333"),
        legend.position = "none")
  
b_gif <- animate(p2, 200, fps = 20, width = 270, height = 400)
  
# Combine GIF (i must be indexed from 2 to the number of frames)
a_mgif <- image_read(a_gif)
b_mgif <- image_read(b_gif)

new_gif <- image_append(c(a_mgif[1], b_mgif[1]))
for(i in 2:200){ 
  combined <- image_append(c(a_mgif[i], b_mgif[i]))
  new_gif <- c(new_gif, combined)
}

new_gif

# Save GIF
savename <- file.path(paste0("COVID_Aus_", format(Sys.Date(), "%d%m%Y"), ".gif")) #current date, data for day before
new_gif %>%
  image_write(path=savename)

