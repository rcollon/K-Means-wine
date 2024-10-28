#libraries
library (tidyclust)
library (tidyverse)
library (tidymodels)

tidymodels_prefer()

# Inspect dataset
data(wine, package="rattle")
glimpse (wine)

# Set seed for reproducibility
set.seed(28)

# Remove wine type
wineExType <- wine %>% select (-Type)

# Examine the metric variables
summary(wineExType)

# Specify model
modelKM <- k_means(num_clusters = 3)

# Re-scale variables because of the large differences in the distances
recipeKM <- recipe (~ ., data=wineExType) %>%
   step_normalize(all_numeric_predictors())

# Workflow for fit 
workflowKM <- workflow() %>%
  add_model(modelKM) %>%
  add_recipe(recipeKM)

# K-Means clustering to see if different types of wine can be seen 
fitKM <- workflowKM %>% fit(wineExType)

# Show cluster centers
fitKM %>% extract_centroids() %>% view ()

fitKM$size()

