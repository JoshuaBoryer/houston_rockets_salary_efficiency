library(tidyr)
library(dplyr)
library(ggplot2)
library(ggimage)

houston_stats <- read.csv("houston_rockets_stats.csv")

#Rescale AAV to millions
houston_stats <- houston_stats %>%
  mutate(AAV_million = AAV / 1e6)

#Create new column of AAV per PER unit
houston_stats <- houston_stats %>%
  mutate(
    AAV_million = AAV / 1e6,
    AAV_per_PER = AAV_million / PER
  )

#Remove unwanted column
houston_stats <- houston_stats %>%
  select(-X)

#Image creation for plot
houston_stats <- houston_stats %>%
  mutate(Image = case_when(
    Player == "Kevin Durant" ~ "https://www.basketball-reference.com/req/202106291/images/headshots/duranke01.jpg",
    Player == "Alperen Sengun" ~ "https://www.basketball-reference.com/req/202106291/images/headshots/sengual01.jpg",
    Player == "Fred Vanvleet" ~ "https://www.basketball-reference.com/req/202106291/images/headshots/vanvlfr01.jpg",
    Player == "Steven Adams" ~ "https://www.basketball-reference.com/req/202106291/images/headshots/adamsst01.jpg",
    Player == "Dorian Finney Smith" ~ "https://www.basketball-reference.com/req/202106291/images/headshots/finnedo01.jpg",
    Player == "Jabari Smith Jr" ~ "https://www.basketball-reference.com/req/202106291/images/headshots/smithja05.jpg",
    Player == "Reed Sheppard" ~ "https://www.basketball-reference.com/req/202106291/images/headshots/sheppre01.jpg",
    Player == "Amen Thompson" ~ "https://www.basketball-reference.com/req/202106291/images/headshots/thompam01.jpg",
    Player == "Clint Capela" ~ "https://www.basketball-reference.com/req/202106291/images/headshots/capelca01.jpg",
    Player == "Jeff Green" ~ "https://www.basketball-reference.com/req/202106291/images/headshots/greenje02.jpg",
    Player == "Aaron Holiday" ~ "https://www.basketball-reference.com/req/202106291/images/headshots/holidaa01.jpg",
    Player == "Tari Eason" ~ "https://www.basketball-reference.com/req/202106291/images/headshots/easonta01.jpg",
    Player == "Jae-sean Tate" ~ "https://www.basketball-reference.com/req/202106291/images/headshots/tateja01.jpg",
    TRUE ~ NA_character_  
  ))

#Filter irrelevant players
houston_stats_filtered <- houston_stats %>%
  filter(!Player %in% c("Jeff Green", "Aaron Holiday", "Tari Eason", "Jae-Sean Tate"))

#Salary Efficiency plot
ggplot(houston_stats_filtered, aes(x = reorder(Player, AAV_per_PER), y = AAV_per_PER)) +
  geom_col(width = 0.45, fill = "red3") +
  geom_image(aes(image = Image), size = 0.08, by = "width") +
  geom_text(aes(label = sprintf("%.2f", AAV_per_PER)),
            hjust = -0.7, size = 3.5, color = "red4", fontface = "bold") +
  coord_flip(clip = "off") +
  labs(
    title = "Salary Efficiency",
    x = NULL,
    y = "Average Annual Value $M per PER unit"
  ) +
  theme_minimal(base_family = "Helvetica") +
  theme(
    plot.title = element_text(size = 17, face = "bold", hjust = 0.5, margin = margin(b = 10)),
    axis.title.y = element_blank(),
    axis.title.x = element_text(size = 13, margin = margin(t = 10)),
    axis.text = element_text(size = 13, color = "black"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 40, 10, 10)
  )


