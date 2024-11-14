
# Load the data
cli_data <- read.csv("imputed_cohort_data.csv")
View(cli_data)

# Filter Relevant Columns
lab_vitals <- cli_data[, c("wbc_first", "creatinine_first", "hr_1st", "temp_1st", "map_1st")]

# Boxplot (Optional)
boxcol <- c("steelblue", "magenta", "darkgreen", "#FFA500")
boxplot(lab_vitals, 
        col = boxcol, border = "black")
# Kolmogorov-Smirnov (K-S) test
ks.test(lab_vitals, "pnorm")

# Correlation Matrix
cor_matrix <- cor(lab_vitals, method = "spearman")

# Prepare the correlation plot
install.packages("corrplot")
library(corrplot)
corrplot(cor_matrix, method = "circle", type = "lower",
         tl.col = "black", tl.srt = 45,
         addCoef.col = "black", col = COL2('PiYG'))


# Hypothetical testing
ht <- cor.test(cli_data$wbc_first, cli_data$hr_1st, method = "spearman")





