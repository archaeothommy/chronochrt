setwd("C:/Users/Dell/Desktop/chronochrt")
data <- read_excel("Example_data/ex_urnfield_periods.xlsx")
data <- rename(region = data$Region,
               name = data$Name,
               start = data$Start,
               end = data$End,
               level = data$Level
)
# ich habe keine Ahnung warum nicht mal das funktioniert.
