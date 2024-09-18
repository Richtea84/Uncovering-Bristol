# Regression modelling

# Usual suspects
if(!require(jquerylib)) install.packages("jquerylib") & require(jquerylib)
if(!require(proxy)) install.packages("proxy") & require(jquerylib)
if(!require(labelled)) install.packages("labelled") & require(labelled)

if(!require(gdalraster)) install.packages("gdalraster") & require(gdalraster)
if(!require(terra)) install.packages("terra") & require(terra)

# Packages
if(!require(tidyverse)) install.packages("tidyverse") &
  require(tidyverse)

if(!require(randomcoloR)) install.packages("randomcoloR") &
  require(randomcoloR)

# Data and processing
data_in <- read.csv(
  list.files(path = ".", pattern = "_Survey.csv"
             )
)

data_in |>
  dplyr::rename(
    "Stop 1" = "Stop.1",
    "Stop 2" = "Stop.2",
    "Stop 3" = "Stop.3",
    "Stop 4" = "Stop.4",
    "Stop 5" = "Stop.5",
    "Stop 6" = "Stop.6",
    "Stop 7" = "Stop.7",
    "Stop 8" = "Stop.8",
    "Stop 9" = "Stop.9",
    "Stop 10" = "Stop.10"
  ) -> data_out

# Unweighted scores
colMeans(x = data_out[,-c(1,2)]) -> Unweighted_average

# Weighted scores 
apply(
  X = data_out[,-c(1,2)], MARGIN = 2,
  FUN = function(x) weighted.mean(
    x = as.numeric(x[1:8]), w = data_out$Weights
    )
  ) -> Weighted_average

# IMD data

IMD_scores <- read.csv(
  list.files(path = ".", pattern = "_deprivation")
)

IMD_scores_agg <- aggregate(Soc_grad ~ Stop.no., data = IMD_scores, FUN = mean)

# Regression model & Pearson's correlation coefficient

my_pal = randomcoloR::randomColor(count = 10)


jpeg("Weighted Regression Plot.jpg", height = 720, width = 1080)
plot(IMD_scores_agg$Soc_grad ~ Weighted_average,
     xlab = expression(paste("weighted ", bar(x))),
     ylab = "Index of multiple\ndeprivation ranking")
abline(lm(IMD_scores_agg$Soc_grad ~ Weighted_average), col = my_pal)
dev.off()

jpeg("Unweighted Regression Plot.jpg", height = 720, width = 1080)
plot(IMD_scores_agg$Soc_grad ~ Unweighted_average,
     xlab = expression(paste("unweighted ", bar(x))),
     ylab = "Index of multiple\ndeprivation ranking")
abline(lm(IMD_scores_agg$Soc_grad ~ Weighted_average), col = my_pal)
dev.off()


cor.test(x = Weighted_average, y = IMD_scores_agg$Soc_grad)
cor.test(x = Unweighted_average, y = IMD_scores_agg$Soc_grad)

# Unique Identifier
UI <- ifelse("E00174307" %in% IMD_scores$name, "Route_1",
             ifelse("E00073150" %in% IMD_scores$name, "Route_2",
                    ifelse("E00073179" %in% IMD_scores$name, "Route_3",
                           ifelse("E00073653" %in% IMD_scores$name, "Route_4")
                           )
                    )
             )


