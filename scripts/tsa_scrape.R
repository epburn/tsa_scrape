# Checks the US Transportation Security Administration Covid-19 site for 
# new passenger screening data. Compares this to 2020 and 2019 levels. 
# This script is not built to go through site's pagination and repopulate
# an entire new table, but it will fill in everything new it finds on the 
# default landing page. 
# ElliottB

# find and load packages
packages <- c("tidyverse", "rvest", "extrafont", "ggrepel")

for (package in packages) {
  #install packages that the script can't find
  if (!package %in% installed.packages()) {
    install.packages(
      package,
      dependencies = TRUE
    )
    
  }
  
  #load the packages we need
  if (!package %in% .packages()) {
    library(
      package,
      character.only = TRUE
    )
  }
}

# load fonts
loadfonts(device = "win", quiet = TRUE)

# fetch the latest screening numbers
stored <- "data/tsa_screen.csv"
url <- "https://www.tsa.gov/coronavirus/passenger-throughput"
TSA <- url %>%
  xml2::read_html() %>%
  html_table(fill = TRUE)

tsa_colors = c(`2019` = "green3", `2020` = "firebrick2", `2021` = "lightskyblue")

# process the HTML table into a tall dataframe
passengers <- na.omit(TSA[[1]])
passengers <- passengers %>%
  pivot_longer(-Date, names_to = "Year", 
               names_pattern = "^(\\d+)", values_to = "Passengers") %>%
  mutate(Passengers = as.numeric(gsub(",", "", Passengers)))
passengers$Date <- lubridate::mdy(passengers$Date)
passengers <- passengers %>%
  arrange(desc(Date))

# check this table for new material. update backup if new data exists.
old <- read.csv(stored, header = TRUE, sep=",")
old$Date <- lubridate::ymd(old$Date)
colnames(old) <- c("Date", "2021", "2020", "2019")

if (old[1,1] < passengers[1,1]){
  passengers <- passengers %>%
    pivot_wider(names_from = "Year", values_from = "Passengers")
  colnames(old) <- colnames(passengers)
  new <- rbind(old, passengers)
  new <- unique(new)
  write.csv(new,stored, row.names = FALSE)
  passengers <- new %>%
    mutate(pc = (`2021`/`2019`)*100) %>%
    pivot_longer(`2019`:`2021`, names_to = "Year", 
                 names_pattern = "^(\\d+)", values_to = "Passengers") %>%
    mutate(Passengers = as.numeric(gsub(",", "", Passengers))) %>%
    arrange(desc(Date), Year)
  rm(TSA, new, old)
} else {
  passengers <- passengers %>%
    pivot_longer(-Date, names_to = "Year", 
                 names_pattern = "^(\\d+)", values_to = "Passengers") %>%
    mutate(Passengers = as.numeric(gsub(",", "", Passengers))) %>%
    arrange(Date, Year)
  rm(TSA, old)
}

# set up some baseline comparisons used in the graphs
maxmonth <- passengers %>%
  arrange(desc(Date)) %>%
  filter(Date >= as.Date(format(Sys.Date()-months(1), "%Y-%m-01")) & Year != "2019" & !is.na(Passengers)) %>%
  summarize(max(Passengers))

ref2019 <- passengers %>%
  filter(Date == max(Date) & Year == 2019) %>%
  summarize(reference = Passengers*.8)

pcref <- passengers %>%
  filter(Date == max(Date))

# plot the current and year-ago screenings
f <- passengers %>%
  filter(Date >= as.Date(format(Sys.Date(), "%Y-01-01"))) %>%
  arrange(Date) %>%
  ggplot(., aes(x=Date, y=Passengers, col=Year)) +
  geom_line(size=0.8) +
  geom_hline(aes(yintercept=100000), color="#990000", linetype="dashed") +
  geom_text(aes(as.Date(format(Sys.Date(), "%Y-03-20")),100000,label = "100,000 passengers", vjust=1, family = "Segoe UI Semibold"), color="#990000", show.legend = FALSE) +
  geom_text_repel(aes(label = if_else(Date == max(Date) & Year == 2021, paste(round(pc,1), "pc\nof 2019", sep = ""), NULL), family = "Segoe UI Semibold"),
                   nudge_x = 1,
                   color = "orangered") +
  scale_y_continuous(labels = scales::comma) +
  scale_color_manual(values = tsa_colors) +
  labs(title = paste("TSA Passengers Screened as of", format(max(passengers$Date), "%B %d"), sep = " "),
       subtitle = paste("Current as of ", format(max(passengers$Date), "%d %B"), sep = ""),
       caption = "Source: Transportation Security Administration",
       x = element_blank(),
       y = element_blank()) +
  theme_light(base_family = "Segoe UI Semibold") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "bottom",
        legend.title = element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.border = element_blank(),
        panel.grid.major.y = element_line(size = 0.25, linetype = "solid", color = "gray"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        strip.background = element_rect(color = "#005DAA", fill = "#6CADDF"),
        strip.text.x = element_text(color = "White"))
  
## How do daily numbers compare to the previous week?
passengers %>%
  arrange(Date) %>%
  group_by(Year) %>%
  mutate(WoW = Passengers-lag(Passengers, 7),
         WoWpc = round((WoW/lag(Passengers, 7))*100, 1)) %>% # Add a column for prior week data and pc change
  arrange(desc(Date)) %>%
  filter(Date >= as.Date(Sys.Date()-1-lubridate::weeks(4))) %>%
  select(Date, Year, WoWpc) %>%
  ggplot(., aes(Date, WoWpc, fill = WoWpc < 0, color = Year)) +
  geom_col(position = "dodge") +
  facet_grid(. ~ Year) +
  labs(title = paste("Weekly changes in TSA Passenger Screenings since", format(lubridate::rollback(Sys.Date()), "%B")),
       caption = "Source: Transportation Security Administration",
       x = element_blank(),
       y = "Percent Change") +
  scale_fill_manual(values = c("cornflowerblue", "red3"), guide = FALSE) +
  theme_light(base_family = "Segoe UI Semibold") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        legend.title = element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.border = element_blank(),
        panel.grid.major.y = element_line(size = 0.25, linetype = "solid", color = "gray"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        strip.background = element_rect(color = "#005DAA", fill = "#6CADDF"),
        strip.text.x = element_text(color = "White")) +
  scale_y_continuous(labels = scales::comma)

# zoom in on current month
f + coord_cartesian(xlim = c(as.Date(format(Sys.Date()-1, "%Y-%m-01"))-months(1), as.Date(format(Sys.Date()-1, "%Y-%m-01"))+months(1)-1)) +
  labs(title = paste("TSA Passengers Screened since", format(Sys.Date()-months(1)-1, "%B"), sep=" "),
       subtitle = paste("Updated through ", format(max(passengers$Date), "%d %B"), sep = ""))

passengers %>% # What is the latest number of passengers screened?
  filter(Date == max(Date) & Year == lubridate::year(Sys.Date()))
passengers %>%
  filter(Date >= as.Date(format(Sys.Date()-1, "%Y-%m-01"))) %>%
  group_by(Year) %>%
  dplyr::summarize(average = mean(Passengers), days=n())

passengers %>%
  filter(Date>= as.Date("2020-03-16") & Year == as.character(lubridate::year(Date))) %>%
  slice_max(Passengers, n = 5)
