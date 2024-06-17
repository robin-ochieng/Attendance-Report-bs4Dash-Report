library(readxl)




Data <- read_excel("data/Data.xlsx", col_types = c("text", 
                                                   "text", "date", "text", "text", "text", 
                                                   "text", "text"))

