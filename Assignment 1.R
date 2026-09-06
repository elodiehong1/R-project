# Input data
dog1<-read.csv("D:\\durham_material\\Introduction to Statistics for Data Science\\assignment 1\\dog1.csv", stringsAsFactors = FALSE)
dog2<-read.csv("D:\\durham_material\\Introduction to Statistics for Data Science\\assignment 1\\dog2.csv", stringsAsFactors = FALSE)

############################################################################
# (b) 
############################################################################

# Count proportions
dog1$size <- factor(dog1$size,
                    levels = c("Small", "Medium", "Large", "Very large"),
                    ordered = TRUE)
size_proportions <- prop.table(table(dog1$size))

# Create a bar plot

bp <- barplot(
  size_proportions,
  main = "Proportion of Each Dog Size",
  ylab = "Proportion",
  xlab = "Dog Size",
  col = c("lightblue", "lightgreen", "salmon","orange"),
  names.arg = names(size_proportions),
  ylim = c(0, max(size_proportions) * 1.10)
)

# Add proportion labels on top of each bar
text(
  x = bp,
  y = size_proportions + 0.001,
  labels = paste0(round(size_proportions, 2)),
  pos = 3,           # position: above bars
  cex = 0.9,         # label size
  col = "black"
)

############################################################################
# (c)
############################################################################
str(dog1)
stem(dog1$time, scale = 2)
# Find modal grooming time
mode_time <- as.numeric(names(sort(table(dog1$time), decreasing = TRUE)[1]))
cat("modal grooming time is: ", mode_time, "\n")

############################################################################
# (d)
############################################################################
#Draw boxplots of grooming time by size
bp<-boxplot(time ~ size, 
        data = dog1,
        main = "Grooming Time by Dog Size",
        xlab = "Dog Size",
        ylab = "Grooming Time (minutes)",
        col = c("lightblue", "lightgreen", "salmon", "orange"))
grid()
# extra median
medians <- bp$stats[3, ]
medians
# Add label
text(x = 1:4, y = medians, 
     labels = paste0(round(medians, 1)),
     pos = 3, cex = 0.8, col = "black")

############################################################################
# (e)
############################################################################
# (i) Calculate percentage of dogs taking more than 20 minutes
percent_refund <- mean(dog1$time > 20) * 100
cat("Percentage of dog owners who get refund:", paste0(round(percent_refund, 1)), " %\n")

# (ii) Find the 99th percentile (only 1% above it)
threshold_1percent <- quantile(dog1$time, 0.99)
cat("Time threshold for only 1% refunds:", paste0(round(threshold_1percent, 3)), " minutes\n")