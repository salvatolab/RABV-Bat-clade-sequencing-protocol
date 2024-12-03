# Load the libraries
library(ggplot2)
library(reshape2)

# Read the data from the CSV file
data <- read.csv("/Users/richardsalvato/Documents/rabv_paper/tree/divergence_matrix.csv", row.names = 1)

# Transform the data into long format
data_long <- melt(as.data.frame(data), variable.name = "Var2", value.name = "value")

# Add an identification column (Var1) based on the row names
data_long$Var1 <- rep(rownames(data), times = ncol(data))

# Define the desired order for the axes
order_x <- c("rabv_44_2022", "rabv_02_2022", "rabv_49_2022",
             "rabv_61_2022", "rabv_97_2022", "rabv_07_2022", "rabv_10_2022",
             "rabv_11_2022", "rabv_46_2022", "rabv_31_2022", "rabv_47_2022",
             "rabv_62_2022", "rabv_06_2022", "rabv_65_2022", "rabv_32_2022",
             "rabv_64_2022", "rabv_51_2022", "rabv_63_2022", "rabv_52_2022",
             "rabv_23_2022", "rabv_78_2022", "rabv_35_2022", "rabv_66_2022",
             "rabv_16_2022", "rabv_71_2022")

data_long$Var1 <- factor(data_long$Var1, levels = order_x)  # Order Var1
data_long$Var2 <- factor(data_long$Var2, levels = order_x)  # Order Var2

# Create the heatmap
ggplot(data_long, aes(x = Var2, y = Var1)) +  # Flip Var1 and Var2 for the plot
  geom_tile(aes(fill = value), color = "black") +  # Add black borders for the cells
  geom_text(aes(label = round(value, 1)), color = "white", size = 3) +  # Add numbers in the cells
  scale_fill_gradientn(colors = c("navy", "grey"),
                       limits = c(0, 15),
                       name = "Sequence divergence (%)") +
  theme_classic() +
  labs(x = "", y = "", title = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom") +
  coord_flip()  # Flip the axes for a better layout
