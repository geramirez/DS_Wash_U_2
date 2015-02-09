setwd("/home/ramirezg/Documents/UW_DataScience/Semester2/week4/hwrk")

# 1
lrmc <- c(44, 42, 47, 43, 46)
m.lrmc <- mean(lrmc)
sd.lrmc <- sd(lrmc)
pnorm(q =  67*.64, mean = m.lrmc, sd = sd.lrmc, lower.tail = T)
# The lrmc model is not significantly more accurate than the experts
qnorm(p = .05, mean = m.lrmc, sd = sd.lrmc , lower.tail = T)
# in order for the lrmc model to be significantly more accurate the
# expert would have 61% a prediction rate or less.  


#2. Use the birth data to perform a t-test to determine whether the 
# mean due date of a second child is significantly greater than that 
# of a first in mothers with two or more children.
birth_data <- read.csv("birthDataDays.csv")
t.test(birth_data$Second, birth_data$First, alternative="greater", paired = T)
# Although the mean gestation period of the second child is longer,
# we cannot reject the null hypothesis that the due date of the second
# child is not significantly greater than the due date of the first child.
# In other words it is not significantly greater. 

# Load relevant libraries, and init functions
library(XML)
xmlCleanValue <-function(x) {
  cleaned_data <- gsub("[[:blank:]\n+]", "", xmlValue(x))
  return (as.numeric(gsub("\\(.*\\)", "",cleaned_data)))
}
cleanTable <- function(table) {
    scores <- getNodeSet(doc = table, path = './/tr/td/text()') 
    return(c(xmlCleanValue(scores[[11]]),
      xmlCleanValue(scores[[13]]),
      xmlCleanValue(scores[[16]]),
      xmlCleanValue(scores[[17]])))
}
# Colect data
data <- c()
for (page in 1:17){
  site = paste("http://www.oddsshark.com/stats/scoreboardbyweek/football/nfl/",page,"/2013", sep="")
  tree = htmlTreeParse(site,useInternal = T)
  new_data <- t(xpathSApply(doc = tree, 
                        path = '//*[@id="block-system-main"]/div/div/table[@class="scores"]',
                        cleanTable))
  data <- rbind(data, new_data)
}
# Put data into data frame and set names
data <- data.frame(data)
names(data) <- c("team_1_line", "team_1_score", "team_2_line", "team_2_score")

# Getting winners
data$winner <- data$team_1_score - data$team_2_score

spread_u3 <- data[abs(data$team_1_line) <= 3,] 

fav.team.wins <- nrow(spread_u3[spread_u3$team_1_line < 0 & spread_u3$winner > 0, ])
fav.team.losses <- nrow(spread_u3[spread_u3$team_1_line < 0 & spread_u3$winner < 0, ])
unfav.team.wins <-  nrow(spread_u3[spread_u3$team_1_line > 0 & spread_u3$winner > 0, ])
unfav.team.looses <-  nrow(spread_u3[spread_u3$team_1_line > 0 & spread_u3$winner < 0, ])

ind.score <- nrow(spread_u3)/2

prob.matrix <- matrix(data = c(fav.team.wins + unfav.team.looses, 
                fav.team.losses + unfav.team.wins,
                49.5, 49.5 ), nrow = 2, ncol = 2)

chisq.test(prob.matrix, correct = F)

