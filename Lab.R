library("ggplot2")
library("dplyr")
# Install and load the VIM package if not already installed
library("VIM")
# install.packages("VIM")
# Load the Superstore dataset and libraries needed.
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
df <- read.csv("dataset/Sample - Superstore.csv")

# A summary table or boxplot can be useful to visualize the distribution of key variables. 
# Generate a boxplot to visualize Sales and another to visualizr Profit
p_sales <- ggplot(df, aes(x = "", y = Sales)) +
    geom_boxplot(fill = "skyblue") +
    labs(title = "Sales Distribution", x = "", y = "Sales") +
    theme(
        plot.title = element_text(size = 36),
        axis.title = element_text(size = 28),
        axis.text = element_text(size = 24)
    )
# print(p_sales)

p_profit <- ggplot(df, aes(x = "", y = Profit)) +
    geom_boxplot(fill = "skyblue") +
    labs(title = "Profit Distribution", x = "", y = "Profit") +
    theme(
        plot.title = element_text(size = 36),
        axis.title = element_text(size = 28),
        axis.text = element_text(size = 24)
    )
# print(p_profit)

# Create a bar plot to show the top 10 orders with highest value of sales.
top_10 <- df %>%
    arrange(desc(Sales)) %>%
    slice_head(n = 10)
# print(top_10)

p_top_10_sales <- ggplot(top_10,  aes(x = reorder(Customer.Name, Sales), y = Sales)) +
    geom_col() +
    coord_flip() +
    labs(title = "Top 10 Sales",  x = "Customer Name", y = "Sales") + 
    theme(
        plot.title = element_text(size = 36),
        axis.title = element_text(size = 28),
        axis.text = element_text(size = 24)
    )
# print(p_top_10_sales)

# Use a heatmap to visualize the pattern of missing data.

# Visualize missing data pattern
missing_pattern <- aggr(df,
                     col=c('navyblue','red'), 
                     numbers=TRUE, 
                     sortVars=TRUE, 
                     labels=names(df), 
                     cex.axis=.7,
                     gap=3,
                     ylab=c("Missing data","Pattern")
                )


# Generate a histogram that can show the distribution of the Profit column.
p_histogram <- ggplot(df, aes(x = Profit)) +
    geom_histogram(alpha=0.5, binwidth=50) + # alpha is transparency
    # labs(title= "Survival Rate by gender", c = "Gender", y = "Survival Rate") + 
    scale_fill_brewer(palette = "Set2") + 
    theme(
        plot.title = element_text(size = 36),
        axis.title = element_text(size = 28),
        axis.text = element_text(size = 24)
    )
# print(p_histogram)

# Generate a bar plot that shows Total Sales by Category and a barplot that shows Profit by Category.
total_sales <- df %>%
    group_by(Category) %>%
    summarize(total_profit = sum(Profit))

p_total_sales <- ggplot(total_sales, aes(y = total_profit, x= factor(Category))) +
    geom_col() +
    labs(
        title = "Total Profit by Category", 
        y = "Total Profit", 
        x = "Category"
    ) + 
    theme(
        plot.title = element_text(size = 36),
        axis.title = element_text(size = 28),
        axis.text = element_text(size = 24)
    )
print(p_total_sales)