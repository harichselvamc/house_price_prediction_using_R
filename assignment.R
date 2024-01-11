
library(readxl)


df <- read_excel("Cardata.xlsx" )

str(df)

model <- lm(mpg ~ cyl + disp + HP + wt + accel, data = df)

summary(model)
