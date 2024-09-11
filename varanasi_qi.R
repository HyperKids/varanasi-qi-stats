# Hey! Please download the CSV file from Qualtrics using
# "Numeric Values" and not text.

# load file vqi-presurvey.csv
vqi_presurvey <- read.csv("./data/vqi-presurvey.csv",
  header = TRUE, sep = ","
)

# discard the first two rows - they are not needed
vqi_presurvey <- vqi_presurvey[-c(1, 2), ]

# columns we care about are Q1, Q2_1, Q3_1, Q4, Q5
# delete the rest
vqi_presurvey <- vqi_presurvey[, c("Q1", "Q2_1", "Q3_1", "Q4", "Q5")]

# rename the columns
colnames(vqi_presurvey) <- c("Q1", "Q2", "Q3", "Q4", "Q5")

# print the table
print(vqi_presurvey)

# Q2, Q3 are 1-7 likert scale
# Q4, Q5 are a free form number

# convert Q2, Q3, Q4, Q5 to numeric
vqi_presurvey$Q2 <- as.numeric(vqi_presurvey$Q2)
vqi_presurvey$Q3 <- as.numeric(vqi_presurvey$Q3)
vqi_presurvey$Q4 <- as.numeric(vqi_presurvey$Q4)
vqi_presurvey$Q5 <- as.numeric(vqi_presurvey$Q5)

# load file vqi-postsurvey.csv
vqi_postsurvey <- read.csv("./data/vqi-postsurvey.csv",
  header = TRUE, sep = ","
)

# discard the first two rows - they are not needed
vqi_postsurvey <- vqi_postsurvey[-c(1, 2), ]

# columns we care about are Q1, Q2_1, Q3_1, Q4, Q5, Q6, Q7_1, Q8_1, Q9
# delete the rest
vqi_postsurvey <- vqi_postsurvey[, c(
  "Q1", "Q2_1", "Q3_1", "Q4", "Q5", "Q6", "Q7_1", "Q8_1"
)]

# rename the columns
colnames(vqi_postsurvey) <- c(
  "Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8"
)

# print the table
print(vqi_postsurvey)

# Q2, Q3, Q7, Q8 are 1-7 likert scale
# Q4, Q5 are a free form number
vqi_postsurvey$Q2 <- as.numeric(vqi_postsurvey$Q2)
vqi_postsurvey$Q3 <- as.numeric(vqi_postsurvey$Q3)
vqi_postsurvey$Q4 <- as.numeric(vqi_postsurvey$Q4)
vqi_postsurvey$Q5 <- as.numeric(vqi_postsurvey$Q5)
vqi_postsurvey$Q7 <- as.numeric(vqi_postsurvey$Q7)
vqi_postsurvey$Q8 <- as.numeric(vqi_postsurvey$Q8)

# Q6 is 1, 2, 3, or 4 corresponding to "Posters", "Pamphlet", "Both", "Neither"
# Separate out the data into 3 columns: 1 for either (both, pamphlet,
# or poster), 1 for pamphlet, and 1 for poster
vqi_postsurvey$Q6_either <- ifelse(
  vqi_postsurvey$Q6 == 3,
  1,
  ifelse(
    vqi_postsurvey$Q6 == 1 | vqi_postsurvey$Q6 == 2,
    1,
    0
  )
)

vqi_postsurvey$Q6_pamphlet <- ifelse(
  vqi_postsurvey$Q6 == 3 | vqi_postsurvey$Q6 == 2,
  1,
  0
)

vqi_postsurvey$Q6_poster <- ifelse(
  vqi_postsurvey$Q6 == 3 | vqi_postsurvey$Q6 == 1,
  1,
  0
)


# print
print(vqi_postsurvey)

### BASIC STATISTICS ###
# pre-survey: put mean, median, sd, min, max in a table
vqi_presurvey_stats <- data.frame(
  colname = c(
    "Q2 confidence", "Q3 ability", "Q4 no handwash", "Q5 no mask"
  ),
  mean = c(
    mean(vqi_presurvey$Q2), mean(vqi_presurvey$Q3),
    mean(vqi_presurvey$Q4), mean(vqi_presurvey$Q5)
  ),
  median = c(
    median(vqi_presurvey$Q2), median(vqi_presurvey$Q3),
    median(vqi_presurvey$Q4), median(vqi_presurvey$Q5)
  ),
  sd = c(
    round(sd(vqi_presurvey$Q2), 2), round(sd(vqi_presurvey$Q3), 2),
    round(sd(vqi_presurvey$Q4), 2), round(sd(vqi_presurvey$Q5), 2)
  ),
  min = c(
    min(vqi_presurvey$Q2), min(vqi_presurvey$Q3),
    min(vqi_presurvey$Q4), min(vqi_presurvey$Q5)
  ),
  max = c(
    max(vqi_presurvey$Q2), max(vqi_presurvey$Q3),
    max(vqi_presurvey$Q4), max(vqi_presurvey$Q5)
  )
)

# print the table
print("Pre-survey statistics")
print(vqi_presurvey_stats)

# put mean, median, sd, min, max in a table
vqi_postsurvey_stats <- data.frame(
  colname = c(
    "Q2 confidence", "Q3 ability", "Q4 no handwash", "Q5 no mask",
    "Q7 writ helpful", "Q8 writ satisfied"
  ),
  mean = c(
    mean(vqi_postsurvey$Q2), mean(vqi_postsurvey$Q3),
    mean(vqi_postsurvey$Q4), mean(vqi_postsurvey$Q5),
    mean(vqi_postsurvey$Q7, na.rm = TRUE), mean(vqi_postsurvey$Q8, na.rm = TRUE)
  ),
  median = c(
    median(vqi_postsurvey$Q2), median(vqi_postsurvey$Q3),
    median(vqi_postsurvey$Q4), median(vqi_postsurvey$Q5),
    mean(vqi_postsurvey$Q7, na.rm = TRUE), mean(vqi_postsurvey$Q8, na.rm = TRUE)
  ),
  sd = c(
    round(sd(vqi_postsurvey$Q2), 2), round(sd(vqi_postsurvey$Q3), 2),
    round(sd(vqi_postsurvey$Q4), 2), round(sd(vqi_postsurvey$Q5), 2),
    round(sd(vqi_postsurvey$Q7, na.rm = TRUE), 2),
    round(sd(vqi_postsurvey$Q8, na.rm = TRUE), 2)
  ),
  min = c(
    min(vqi_postsurvey$Q2), min(vqi_postsurvey$Q3),
    min(vqi_postsurvey$Q4), min(vqi_postsurvey$Q5),
    mean(vqi_postsurvey$Q7, na.rm = TRUE), mean(vqi_postsurvey$Q8, na.rm = TRUE)
  ),
  max = c(
    max(vqi_postsurvey$Q2), max(vqi_postsurvey$Q3),
    max(vqi_postsurvey$Q4), max(vqi_postsurvey$Q5),
    mean(vqi_postsurvey$Q7, na.rm = TRUE), mean(vqi_postsurvey$Q8, na.rm = TRUE)
  )
)

# print the table
print("Post-survey statistics")
print(vqi_postsurvey_stats)

### THEORY 1 ###
# This assumes everything goes perfectly - all post-test
# results are perfect (7 or 0)

# Assume Q2, Q3 are all 7, and Q4, Q5 are all 0
# Perform a t-test to see if the means are different

# Create dummy data
vqi_presurvey_theory1 <- vqi_presurvey
vqi_presurvey_theory1$Q2 <- rep(7, nrow(vqi_presurvey_theory1))
vqi_presurvey_theory1$Q3 <- rep(7, nrow(vqi_presurvey_theory1))
vqi_presurvey_theory1$Q4 <- rep(0, nrow(vqi_presurvey_theory1))
vqi_presurvey_theory1$Q5 <- rep(0, nrow(vqi_presurvey_theory1))

# Print the table
print(vqi_presurvey_theory1)

# Perform t-test
t_test <- t.test(vqi_presurvey$Q2, vqi_presurvey_theory1$Q2, paired = FALSE)
print(t_test)

t_test <- t.test(vqi_presurvey$Q3, vqi_presurvey_theory1$Q3, paired = FALSE)
print(t_test)

t_test <- t.test(vqi_presurvey$Q4, vqi_presurvey_theory1$Q4, paired = FALSE)
print(t_test)

t_test <- t.test(vqi_presurvey$Q5, vqi_presurvey_theory1$Q5, paired = FALSE)
print(t_test)

### THEORY 2 ###
# This assumes all post-test results improve by (delta) from
# the pre-test results

delta <- 2

# Add 1 to Q2 and Q3 unless it is 7
vqi_presurvey_theory2 <- vqi_presurvey
vqi_presurvey_theory2$Q2 <- ifelse(
  vqi_presurvey_theory2$Q2 > 7 - delta, 7, vqi_presurvey_theory2$Q2 + delta
)
vqi_presurvey_theory2$Q3 <- ifelse(
  vqi_presurvey_theory2$Q3 > 6, 7, vqi_presurvey_theory2$Q3 + delta
)

# Remove 1 from Q4 and Q5 unless it is 0
vqi_presurvey_theory2$Q4 <- ifelse(
  vqi_presurvey_theory2$Q4 < delta, 0, vqi_presurvey_theory2$Q4 - delta
)
vqi_presurvey_theory2$Q5 <- ifelse(
  vqi_presurvey_theory2$Q5 < delta, 0, vqi_presurvey_theory2$Q5 - delta
)

# Print the table
print(vqi_presurvey_theory2)

# Perform t-test
t_test <- t.test(vqi_presurvey$Q2, vqi_presurvey_theory2$Q2, paired = FALSE)
print(t_test)

t_test <- t.test(vqi_presurvey$Q3, vqi_presurvey_theory2$Q3, paired = FALSE)
print(t_test)

t_test <- t.test(vqi_presurvey$Q4, vqi_presurvey_theory2$Q4, paired = FALSE)
print(t_test)

t_test <- t.test(vqi_presurvey$Q5, vqi_presurvey_theory2$Q5, paired = FALSE)
print(t_test)

### CORRELATION ###

# Calculate correlation between Q2, Q3, Q4, Q5, make sure to remove Q1
vqi_presurvey_corr <- vqi_presurvey[, c("Q2", "Q3", "Q4", "Q5")]
correlation <- cor(vqi_presurvey_corr)
print(correlation)

### PLOTS ###
# Install ggplot2 (uncomment if you need to)
# install.packages("ggplot2")

# Load ggplot2
library(ggplot2)

# Create a histogram for Q2, Q3, Q4, Q5
histogram_q2 <- ggplot(vqi_presurvey, aes(x = Q2)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
  ggtitle("Q2")
histogram_q3 <- ggplot(vqi_presurvey, aes(x = Q3)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
  ggtitle("Q3")
histogram_q4 <- ggplot(vqi_presurvey, aes(x = Q4)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
  ggtitle("Q4")
histogram_q5 <- ggplot(vqi_presurvey, aes(x = Q5)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
  ggtitle("Q5")

# Print the histograms
print(histogram_q2)
print(histogram_q3)
print(histogram_q4)
print(histogram_q5)

# Create a boxplot for Q2, Q3, Q4, Q5
boxplot_q2 <- ggplot(vqi_presurvey, aes(y = Q2)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  ggtitle("Q2")
boxplot_q3 <- ggplot(vqi_presurvey, aes(y = Q3)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  ggtitle("Q3")
boxplot_q4 <- ggplot(vqi_presurvey, aes(y = Q4)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  ggtitle("Q4")
boxplot_q5 <- ggplot(vqi_presurvey, aes(y = Q5)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  ggtitle("Q5")

# Print the boxplots
print(boxplot_q2)
print(boxplot_q3)
print(boxplot_q4)
print(boxplot_q5)

# Create a scatterplot for Q2 vs Q3, Q4 vs Q5
scatterplot_q2_q3 <- ggplot(vqi_presurvey, aes(x = Q2, y = Q3)) +
  geom_point(color = "blue") +
  ggtitle("Q2 vs Q3")
scatterplot_q4_q5 <- ggplot(vqi_presurvey, aes(x = Q4, y = Q5)) +
  geom_point(color = "blue") +
  ggtitle("Q4 vs Q5")

# Print the scatterplots
print(scatterplot_q2_q3)
print(scatterplot_q4_q5)

# Correlation

# Choose the method you find most informative
ggplot(vqi_presurvey_corr, aes(x = Q2, y = Q3)) +
  geom_jitter(color = "blue", width = 0.05, height = 0.05) +
  ggtitle("Q2 vs Q3 (Jittered)") +
  xlab("Question 2") +
  ylab("Question 3")

# Calculate and print Spearman's correlation
correlation <- cor(vqi_presurvey$Q2, vqi_presurvey$Q3, method = "spearman")
print(paste("Spearman's correlation:", correlation))
