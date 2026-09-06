###########################################################################
# library packages
###########################################################################
library(dplyr)
library(ggplot2)
library(naniar)
library(corrplot)
library(reshape2)
library(mice)
library(factoextra)
library(ggpubr)
library(cluster)
library(ggrepel)
# Import breast cancer dataset
fncancer <- "breastcancer.csv"
cancer <- read.csv(fncancer, stringsAsFactors = FALSE) %>% 
  rename(
    radius = radius_mean,
    texture = texture_mean,
    perimeter = perimeter_mean,
    area = area_mean,
    smoothness = smoothness_mean,
    compactness = compactness_mean,
    concavity = concavity_mean,
    concave_points = concave_points_mean,
    symmetry = symmetry_mean,
    fractal_dimension = fractal_dimension_mean
  )
# Remove ID column
cancer <- cancer %>% select(-X)

###########################################################################
# 1.Exploratory and missing data analysis
###########################################################################
str(cancer)
#check the correlation index
cor(cancer, use = "pairwise.complete.obs")
# plot correlation figure
png("corrplot.png", width = 2000, height = 1200, res = 300)
corrplot(cor(cancer, use = "pairwise.complete.obs"),
         method = "color",
         type = "upper",
         tl.cex = 1,
         tl.col = "black")     
dev.off()
# Convert to long format
cancer_long <- cancer %>%
  select(area, concavity, smoothness) %>%
  na.omit() %>%
  melt(measure.vars = c("area", "concavity", "smoothness"))

# Plot histograms
png("hist_density.png", width = 2200, height = 800, res = 300)
ggplot(cancer_long, aes(x = value)) +
  geom_histogram(aes(y = after_stat(density)),   
                 bins = 30,
                 fill = "steelblue",
                 color = "black",
                 alpha = 0.6) +          
  geom_density(color = "black", linewidth = 0.8) +  
  facet_wrap(~ variable, scales = "free") +
  scale_x_continuous(
    n.breaks = 5,
    labels = function(x) {
      if (max(x, na.rm = TRUE) < 1) {
        sprintf("%.2f", x)   #  smoothness keep 2 decimal
      } else {
        x                   
      }
    }
  ) +  
  theme_minimal() +
  labs(x = "Value", y = "Density") +   
  theme(axis.title = element_text(size = 12, color = "black"),
        axis.text = element_text(size = 12, color = "black"),
        strip.text = element_text(size = 12, color = "black"))
dev.off()
#Display outlier using boxplot
png("boxplot.png", width = 2000, height = 1200, res = 300)
par(mar = c(6, 4, 4, 2)) #Increase bottom margin
boxplot(scale(cancer),
        xaxt = "n",
        col = "lightblue",
        border = "black",
        whisklty = 1,
        staplecol = "black",
        boxwex = 0.6,
        outline = TRUE)
text(
  x = 1:ncol(cancer),
  y = par("usr")[3] - 0.5,
  labels = colnames(cancer),
  srt = 45,      # Rotation x label 45°
  adj = 1,
  col = "black",
  xpd = TRUE
)
axis(2, col = "black", col.axis = "black")
dev.off()

par(mar = c(5, 4, 4, 2))

# Collect some stats summary value for report
summary(cancer)

# Check missing values
colSums(is.na(cancer))

# Calculate percentage of missing values for each variable
missing_pct <- colSums(is.na(cancer)) / nrow(cancer) * 100
# Append missing percentage to variable names (only if > 0)
colnames(cancer) <- ifelse(
  missing_pct > 0,
  paste0(colnames(cancer), " (", round(missing_pct, 1), "%)"),
  colnames(cancer)
)
# Visualise missing data pattern as a heatmap
png("vismissplot.png", width = 1500, height = 800, res = 300)
vis_miss(cancer) +
  scale_x_discrete(position = "bottom") +  # Move labels to bottom
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 45,    # Rotate labels for readability
      hjust = 1,
      size = 10,
      color = "black" 
    ),
    axis.text.y = element_text(
      color = "black"   
    ),
    axis.title = element_blank()  # Remove axis titles for cleaner look
  )
dev.off()

# As stated in the report, textures were removed 
# because they accounted for more than half of the
# missing variables and were not highly correlated with other variables.
cancer_drop_texture <- cancer %>% 
  select(-texture)
# Impute smoothness using multiple imputation with MICE (method = pmm)
pred <- make.predictorMatrix(cancer_drop_texture)

# Do not use perimeter and fractal_dimension to predict smoothness due to loggedEvents
pred["smoothness", "perimeter"] <- 0
pred["smoothness", "fractal_dimension"] <- 0

# Run MICE (method = pmm)
set.seed(123)
imp <- mice(cancer_drop_texture,
            m = 10,
            method = "pmm",
            predictorMatrix = pred,
            maxit = 5)

# check loggedEvents 
imp$loggedEvents
png("densityplot.png", width = 1000, height = 800, res = 300)
densityplot(imp,
            scales = list(
              tck = c(1, 0),
              x = list(alternating = 1),
              y = list(alternating = 1)
            ))
dev.off()
# Select one final dataset after imputation 
data_final <- complete(imp, 1)

############################################################################
# 2.Dimension reduction
############################################################################
# PCA analysis
pca_result <- prcomp(data_final, center = TRUE, scale. = TRUE)
summary(pca_result)
# Extracting the proportion of variance explained
var_explained <- pca_result$sdev^2 / sum(pca_result$sdev^2)
# 2.1 Draw Scree Plot
# Because the percentages calculated by fviz_screeplot were not rounded correctly,
# I calculated the percentages myself and set the labels for values
# less than 0.1% to <0.1%.
png("screeplot.png", width = 1500, height = 1200, res = 300)
labels <- ifelse(var_explained * 100 < 0.1,
                 "<0.1%",
                 paste0(sprintf("%.1f", var_explained * 100), "%"))
p_screeplot <- fviz_screeplot(pca_result, addlabels = FALSE)
print(
  p_screeplot + 
    labs(title = NULL) +
    geom_text(aes(x = 1:length(var_explained),
                  y = var_explained * 100,
                  label = labels),
              vjust = -0.5) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
)
dev.off()
# Calculate the interpretation of cumulative variance
cum_var <- cumsum(var_explained)
cum_var
# Select the top 2 principal components
pca_scores <- pca_result$x[, 1:2]   
# To examine the contribution of variables to the principal components (loading)
loadings <- pca_result$rotation
loadings

# 2.2 Draw loading plot
# Extract loadings for the first two principal components 
loadings_2d <- loadings[, 1:2]
# Convert to data frame
loadings_df <- as.data.frame(loadings_2d)
loadings_df$var <- rownames(loadings_df)
# Scaling factor (optional, keep or remove)
scale_factor <- 2
loadings_df$PC1 <- loadings_df$PC1 * scale_factor
loadings_df$PC2 <- loadings_df$PC2 * scale_factor

#Plot loading points figure
ggplot(loadings_df, aes(x = PC1, y = PC2, color = var)) +
  # point
  geom_point(size = 2, alpha = 1) +
  # label
  geom_text_repel(
    aes(label = var),
    size = 5,
    box.padding = 0.3,
    point.padding = 0.2,
    max.overlaps = Inf,
    show.legend = FALSE
  ) +
  # reference line
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  # xy-axis label
  labs(
    x = paste0("PC1 (", round(var_explained[1]*100,1), "%)"),
    y = paste0("PC2 (", round(var_explained[2]*100,1), "%)")
  ) +
  # drop legend
  theme_minimal() +
  theme(legend.position = "none")
# 2.3 Draw plot of contribution of variables to the principal components (loading)
png("contribplot.png", width = 2500, height = 1200, res = 300)
p1 <- fviz_contrib(pca_result, choice = "var", axes = 1) +
  labs(title = "Contribution to PC1", x = NULL) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 15, color = "black", angle = 46, hjust = 1),
    axis.text.y = element_text(size = 15, color = "black"),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 13, face = "bold"),
    plot.margin = margin(10, 10, 10, 30) 
  )

p2 <- fviz_contrib(pca_result, choice = "var", axes = 2) +
  labs(title = "Contribution to PC2", x = NULL) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 15, color = "black", angle = 46, hjust = 1),
    axis.text.y = element_text(size = 15, color = "black"),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 13, face = "bold")
  )
ggarrange(p1, p2, ncol = 2, nrow = 1)
dev.off()
###############################################################################
# 3.Cluster analysis
# 3.1 k-means
###############################################################################
# k-means
# silhouette initialize
sil.width <- rep(NA, 6)
sil.width[1] <- 0

# Compute silhouette width for k = 2 to 6
for(i in 2:6){
  kmeans_result <- kmeans(pca_scores, centers = i, nstart = 25)
  sil <- silhouette(kmeans_result$cluster, dist(pca_scores))
  sil.width[i] <- summary(sil)$avg.width
}
# Display max silhouette width
max(sil.width)

# Combine and compare Plot of Elbow Method select number of clusters and silhouette results
# Double check that the selected number of clusters is 2
# png("combplot.png", width = 2000, height = 1300, res = 300)
par(mfrow = c(1, 2))
# Plot of Elbow Method select number of clusters
#fviz_nbclust(pca_scores, kmeans, method = "wss")
wss <- numeric(10)
for (k in 1:10) {
  wss[k] <- kmeans(pca_scores, centers = k, nstart = 25)$tot.withinss
}
plot(1:10, wss, type = "b",
     xlab = "Number of clusters",
     ylab = "Within-cluster sum of squares",
     main = "Elbow Method",
     cex.main = 0.9)
# Plot silhouette results
plot(1:6, sil.width, type = "b",
     xlab = "Number of clusters",
     ylab = "Silhouette width",
     main = "Silhouette Method",
     cex.main = 0.9)
# Add vertical line at optimal k
abline(v = which.max(sil.width), col = "red", lty = 2)
#dev.off()
par(mfrow = c(1, 1))

# Show best k
which.max(sil.width)
# Choose optimal k=2
k_optimal <- which.max(sil.width)
# Final clustering
kmeans_final <- kmeans(pca_scores, centers = k_optimal, nstart = 25)
kmeans_final$cluster
# View cluster assignment
png("kmeanplot.png", width = 1600, height = 800, res = 300)
fviz_cluster(kmeans_final, data = pca_scores,
             ellipse.type = "convex",
             geom = "point",
             main = "K-means Clustering (PCA-reduced data)")
dev.off()
##############################################################################
# 3.Cluster analysis
# 3.2 K-Medoids Clustering
##############################################################################
# Draw kmedoids silhouette figure
sil.width <- rep(NA, 6)
sil.width[1] <- 0
for(i in 2:6){
  kmedoids_result <- pam(pca_scores, k = i)
  sil <- silhouette(kmedoids_result$clustering, dist(pca_scores))
  sil.width[i] <- summary(sil)$avg.width
}
# check best k
which.max(sil.width)
# Draw K-medoids (PAM) silhouette width plot
png("silhouetteplot.png", width = 1200, height = 1000, res = 300)
plot(1:6, sil.width[1:6], type = "b",
     xlab = "Number of clusters",
     ylab = "Silhouette width")
# Add vertical line at optimal k
abline(v = which.max(sil.width), col = "red", lty = 2)
k_optimal_kmedoids <- which.max(sil.width)
dev.off()
# K-medoids (PAM)
kmedoids_result <- pam(pca_scores, k = k_optimal_kmedoids)
#kmedoids_result$cluster <- factor(
#  ifelse(kmedoids_result$clustering == 1, 2, 1),
#  levels = c(1, 2))
# Check clustering result
kmedoids_result$cluster <- factor(
  ifelse(kmedoids_result$clustering == 1, 2, 1),
  levels = c(1, 2)
)
png("kmedoidsplot.png", width = 1600, height = 800, res = 300)
fviz_cluster(kmedoids_result, data = pca_scores,
             ellipse.type = "convex",
             geom = "point",
             main = "K-medoids Clustering (PCA-reduced data)")
dev.off()
table(kmeans_final$cluster, kmedoids_result$cluster)
