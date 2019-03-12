curry <- read.csv("../data/stephen-curry.csv", stringsAsFactors = FALSE)
iguodala <- read.csv("../data/andre-iguodala.csv", stringsAsFactors = FALSE)
durant <- read.csv("../data/kevin-durant.csv", stringsAsFactors = FALSE)
thompson <- read.csv("../data/klay-thompson.csv", stringsAsFactors = FALSE)
green <- read.csv("../data/draymond-green.csv", stringsAsFactors = FALSE)
curry <- cbind(name  = "Stephen Curry", curry)
iguodala <- cbind(name  = "Andre Iguodala", iguodala)
durant <- cbind(name  = "Kevin Durant", durant)
thompson <- cbind(name = "Klay Thompson", thompson)
green <- cbind(name = "Draymond Green", green)
green$shot_made_flag[green$shot_made_flag == 'y'] = 'shot_yes'
green$shot_made_flag[green$shot_made_flag == 'n'] = 'shot_no'
curry$shot_made_flag[curry$shot_made_flag == 'y'] = 'shot_yes'
curry$shot_made_flag[curry$shot_made_flag == 'n'] = 'shot_no'
durant$shot_made_flag[durant$shot_made_flag == 'y'] = 'shot_yes'
durant$shot_made_flag[durant$shot_made_flag == 'n'] = 'shot_no'
thompson$shot_made_flag[thompson$shot_made_flag == 'y'] = 'shot_yes'
thompson$shot_made_flag[thompson$shot_made_flag == 'n'] = 'shot_no'
iguodala$shot_made_flag[iguodala$shot_made_flag == 'y'] = 'shot_yes'
iguodala$shot_made_flag[iguodala$shot_made_flag == 'n'] = 'shot_no'
green$minute = 12 * green$period + 12 - green$minutes_remaining
curry$minute = 12 * curry$period + 12 - curry$minutes_remaining
durant$minute = 12 * durant$period + 12 - durant$minutes_remaining
thompson$minute = 12 * thompson$period + 12 - thompson$minutes_remaining
iguodala$minute = 12 * iguodala$period + 12 - iguodala$minutes_remaining
total <- rbind(iguodala, green, durant, thompson, curry)
write.csv(total, file = "../data/shots-data.csv")
sink(file = "../output/shots-data-summary.txt")
summary(total)
sink()


