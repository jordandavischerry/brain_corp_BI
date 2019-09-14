### Data Visualization and Exploration 
### For Brain Corporation Interview 
### by Jordan Cherry 
### 09/09/2019

### Libraries 
library(Amelia)
library(ggplot2)
library(readr)
library(dplyr)

### Load Data 
data <- read_csv('fleet_visualization_data_analyst.csv')

### Visual Examination of Data
head(data)
summary(data)

### Check for Missingness 
missmap(data)


### DATA CLEANING ###

### Convert Unit Cost, Total Cost to Numeric
data$`Unit Cost` <- as.numeric(gsub("[[:punct:]]", " ", data$`Unit Cost`))
data$`Total Cost (Current)` <- as.numeric(gsub("[[:punct:]]", " ", data$`Total Cost (Current)`))

### Replace NA with 0 in Numeric Cols
data$Current[is.na(data$Current)] <- 0
data$Future[is.na(data$Future)] <- 0
data$Historic[is.na(data$Historic)] <- 0 
data$Orders[is.na(data$Orders)] <- 0 

### Sets Total_Current_Cost as Formula COL 
data$total_current_cost <- data$Current * data$`Unit Cost`

### Sets Future_Cost as Formula COL 
data$future_cost <- data$Future * data$`Unit Cost`

### Tabulates top 20 Current Customers 
top_20_current_cost <- data %>%
  group_by(Airline) %>%
  summarise(cost = sum(total_current_cost)) %>%
  top_n(20) %>%
  arrange(desc(cost))
  

### Projects top 20 future customers
top_20_future_cost <- data %>%
  group_by(Airline) %>%
  summarise(future_revenue = sum(future_cost)) %>%
  top_n(20, future_revenue) %>%
  arrange(desc(future_revenue))


### Tabulates top 20 Models (Current)
top_20_current_models <- data %>%
  group_by(`Aircraft Type`) %>%
  summarise(total_current = sum(Current)) %>%
  top_n(20, total_current) %>%
  arrange(desc(total_current))


### Tabulates top 20 Models (future)
top_20_future_models <- data %>%
  group_by(`Aircraft Type`) %>%
  summarise(total_future = sum(Future)) %>%
  top_n(20, total_future) %>%
  arrange(desc(total_future))

### Visualization for top 20 customers
top_20_current_cost$Airline <- factor(top_20_current_cost$Airline, 
                                      levels = top_20_current_cost$Airline[order(top_20_current_cost$cost)]) 

ggplot(top_20_current_cost, aes(cost, Airline)) + 
  geom_segment(aes(x = 0, y = Airline, xend = cost, 
                   yend = Airline), color = "orangered3") + 
  geom_point(color = 'orangered3') + 
  geom_text(aes(label = cost), hjust = -.2, size = 3) + 
  xlab("Current Revenue") +
  theme_minimal() + 
  labs(title = "Top 20 Current Customer Airlines")

### Visualization for top 20 future customers 
top_20_future_cost$Airline <- factor(top_20_future_cost$Airline, 
                                      levels = top_20_future_cost$Airline[order(top_20_future_cost$future_revenue)]) 

ggplot(top_20_future_cost, aes(future_revenue, Airline)) + 
  geom_segment(aes(x = 0, y = Airline, xend = future_revenue, 
                   yend = Airline), color = "orangered3") + 
  geom_point(color = 'orangered3') + 
  geom_text(aes(label = future_revenue), hjust = -.2, size = 3) + 
  xlab("Future Revenue") +
  theme_minimal() + 
  ggtitle("Top 20 Future Customer Airlines")



### Visualization for top 20 current models 

top_20_current_models$'Aircraft Type' <- factor(top_20_current_models$`Aircraft Type`, 
                                levels = top_20_current_models$'Aircraft Type'[order(desc(top_20_current_models$total_current))])
top_20_current_models$models <- top_20_current_models$'Aircraft Type'
top_20_current_models$totalcurrent <- top_20_current_models$total_current

ggplot(top_20_current_models, aes(models,totalcurrent)) + 
  geom_bar(stat = 'identity', aes(fill = models)) + 
  xlab("Aircraft Models") + 
  ylab("Current Number") +  
  theme(axis.text.x = element_text(angle = -90, hjust = 0)) + 
  ggtitle("Current Top 20 Most Popular Aircraft Models") 
  



