# title: "make-shot-charts-script-viren-gupta"
# description: ""
# input: stephen-curry.csv, kevin-durant.csv, klay-thompson.csv, andre-iguodala.csv, draymond-green.csv  
# output: 

# scatterplot
library(ggplot2)
klay_scatterplot <- ggplot(data = thompson) + geom_point(aes(x = x, y = y, color = shot_made_flag))
court_file <- "../images/nba-court.jpg"
library(grid)
library(jpeg)
court_image <- rasterGrob( readJPEG(court_file), width = unit(1, "npc"), height = unit(1, "npc"))
klay_shot_chart <- ggplot(data = thompson) + annotation_custom(court_image, -250, 250, -50, 420) + geom_point(aes(x = x, y = y, color = shot_made_flag)) + ylim(-50, 420) +
  ggtitle("Shot Chart: Klay Thompson (2016 season)") + theme_minimal()
iguodala_shot_chart <- ggplot(data = iguodala) + annotation_custom(court_image, -250, 250, -50, 420) + geom_point(aes(x = x, y = y, color = shot_made_flag)) + ylim(-50, 420) +
  ggtitle("Shot Chart: Draymond Green (2016 season)") + theme_minimal()
curry_shot_chart <- ggplot(data = curry) + annotation_custom(court_image, -250, 250, -50, 420) + geom_point(aes(x = x, y = y, color = shot_made_flag)) + ylim(-50, 420) +
  ggtitle("Shot Chart: Stephen Curry (2016 season)") + theme_minimal()
curry_shot_chart
durant_shot_chart <- ggplot(data = durant) + annotation_custom(court_image, -250, 250, -50, 420) + geom_point(aes(x = x, y = y, color = shot_made_flag)) + ylim(-50, 420) +
  ggtitle("Shot Chart: Kevin Durant (2016 season)") + theme_minimal()
durant_shot_chart
green_shot_chart <- ggplot(data = green) + annotation_custom(court_image, -250, 250, -50, 420) + geom_point(aes(x = x, y = y, color = shot_made_flag)) + ylim(-50, 420) +
  ggtitle("Shot Chart: Draymond Green (2016 season)") + theme_minimal()
green_shot_chart
library(gridExtra)
all_shot_chart <- ggplot(data = total) + annotation_custom(court_image, -250, 250, -50, 420) + geom_point(aes(x = x, y = y, color = shot_made_flag)) + ylim(-50, 420) +
  ggtitle("Shot Chart: All (2016 season)") + theme_minimal()
final <- all_shot_chart + facet_wrap( ~ name, ncol = 3)
