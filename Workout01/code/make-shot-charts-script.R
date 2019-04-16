# Title: Making Shot Charts
# Description: Using the ggplot2 graphical package and a jpeg of NBA basketball courts to create scatterplots of
#              the shots taken by 5 select NBA players
# Input(s): The five imported data frames and the nba court image
# Output(s): Five scatterplots/shot charts of shots taken for the 5 NBA players
# Author: Demetrius Sarcos
# Date: 03-13-2019
#=============================================================================================================

# Loading the Required R Packages
library(ggplot2)
library(grid)
library(jpeg)

# court image (to be used as background of plot)
court_file <- "../images/nba-court.jpg"

# Create raste object
court_image <- rasterGrob(
  readJPEG(court_file),
  width = unit(1, "npc"),
  height = unit(1, "npc"))

# Creating Shot Charts for all 5 Players and Saving them as PDFs
andre_iguodala_shot_chart <- ggplot(data = iguodala) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Chart: Andre Iguodala (2016 season)') +
  theme_minimal()
ggsave("../images/andre-iguodala-shot-chart.pdf",
       width = 6.5,
       height = 5)

draymond_green_shot_chart <- ggplot(data = green) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Chart: Draymond Green (2016 season)') +
  theme_minimal()
ggsave("../images/draymond-green-shot-chart.pdf",
       width = 6.5,
       height = 5)

kevin_durant_shot_chart <- ggplot(data = durant) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Chart: Kevin Durant (2016 season)') +
  theme_minimal()
ggsave("../images/kevin-durant-shot-chart.pdf",
       width = 6.5,
       height = 5)

klay_thompson_shot_chart <- ggplot(data = klay) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Chart: Klay Thompson (2016 season)') +
  theme_minimal()
ggsave("../images/klay-thompson-shot-chart.pdf",
       width = 6.5,
       height = 5)

stephen_curry_shot_chart <- ggplot(data = curry) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Chart: Stephen Curry (2016 season)') +
  theme_minimal()
ggsave("../images/stephen-curry-shot-chart.pdf",
       width = 6.5,
       height = 5)

# Using Facetting to Show All Charts in One Image
gsw_shot_charts <- ggplot(data = gsw) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  facet_wrap(~ name) +
  ggtitle('Shot Chart: GSW (2016 season)') +
  theme_minimal()
ggsave("../images/gsw-shot-charts.pdf",
       width = 8,
       height = 7)
ggsave("../images/gsw-shot-charts.png",
       width = 8,
       height = 7)


