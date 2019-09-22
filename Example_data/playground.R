setwd("C:/Users/Dell/Desktop/chronochrt")
data <- read_excel("Example_data/ex_urnfield_periods.xlsx")
data <- rename(region = data$Region,
               name = data$Name,
               start = data$Start,
               end = data$End,
               level = data$Level
)
# ich habe keine Ahnung warum nicht mal das funktioniert.

London data
import dataset
generate labels: 2 general ones, one specific. if not possible, just the great plague one
  1559 - Coronation of Elizabeth I
  12.04.1665 - The "Great Plague of London" begins [in plague deaths]
  1666 - Great Fire of London
