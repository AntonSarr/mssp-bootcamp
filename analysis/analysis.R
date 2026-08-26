library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)

# Data uploading and cleaning
data <- read_csv("data/Affordable_Housing_Production_by_Building_20260821.csv")
data_f <- data[!is.na(data$`All Counted Units`), ]

data_f$`Project Start Date` <- mdy(data_f$`Project Start Date`)

n_unique_projects <- length(unique(data_f$`Project ID`)) # save the number of unique projects 

data_f$`Project Start Month` <- floor_date(data_f$`Project Start Date`, unit="month")
data_f$`Project Start Year` <- floor_date(data_f$`Project Start Date`, unit="year")

# Aggregating by proejct (collapsing buildings)
projects <- data_f |> 
  summarise(
    .by = c(`Project ID`, `Project Start Month`, `Project Start Year`), 
    `Units` = sum(`All Counted Units`)
  )
projects$`Projects Cnt` <- 1

# Visualize units per project distribution
  
projects <- projects |>
  mutate(
    `# Units - Grouped` = cut(
      `Units`,
      breaks = c(1, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, Inf),
      right = FALSE
    )
  )

ggplot(projects, aes(x = `# Units - Grouped`)) +
  geom_bar() +
  labs(
    x = "Number of Units per Project",
    y = "Number of Projects",
    title = "Affordable Units per Project Distribution"
  )

# Aggregating by month
by_month <- projects |> 
  summarise(
    .by = c(`Project Start Month`), 
    `# Units` = sum(`Units`),
    `# Projects` = sum(`Projects Cnt`)
  )
by_month <- by_month |>
  mutate(`6th or 12th month` = if_else(format(`Project Start Month`, "%m") %in% c("06", "12"), 1, 0))

# Aggregating by year
by_year <- projects |> 
  summarise(
    .by = c(`Project Start Year`), 
    `# Units` = sum(`Units`),
    `# Projects` = sum(`Projects Cnt`)
  )
by_year <- by_year[by_year$`Project Start Year` < '2026-01-01', ]
by_year <- by_year |> arrange(`Project Start Year`)
by_year$`Project Start Year Numeric` <- 0:(nrow(by_year)-1)

# Visualize monthly data

ggplot(by_month, aes(`Project Start Month`, `# Units`)) +
  geom_col(fill="orange") +
  labs(
    title="Units Involved In Projects by Month"
  )

ggplot(by_month, aes(`Project Start Month`, `# Projects`)) +
  geom_col(fill="steelblue") +
  labs(
    title="Started Projects by Month"
  )

# Visualize yearly data

ggplot(by_year, aes(`Project Start Year`, `# Units`)) +
  geom_col(fill="orange") +
  labs(
    title="Units Involved In Projects by Year"
  )

ggplot(by_year, aes(`Project Start Year`, `# Projects`)) +
  geom_col(fill="steelblue") +
  labs(
    title="Started Projects by Year"
  )

ggplot(by_month, aes(`Project Start Month`, `# Projects`)) +
  geom_col(fill="steelblue") +
  labs(
    title="Started Projects by Month"
  )

# Calculate and visualize a # units trend
units_trend <- lm(`# Units` ~ `Project Start Year Numeric`, data = by_year)
by_year$`# Units Predicted` <- predict(units_trend)
by_year$`# Units Predicted Error` <- by_year$`# Units Predicted` - by_year$`# Units`

ggplot(by_year, aes(x = `Project Start Year`, y = `# Units`)) +
  geom_point(color = "orange") +
  geom_line(aes(y = `# Units Predicted`), color = "darkred") +
  labs(title="Trend of Units Involved In Projects by Year") +
  annotate(
    "text",
    x = max(by_year$`Project Start Year`, na.rm = TRUE),
    y = max(by_year$`# Units`, na.rm = TRUE),
    label = paste0(
      "Reg. Coefficient = ",
      round(coef(units_trend)["`Project Start Year Numeric`"], 3),
      " (YoY)"
    ),
    hjust = 1.1,
    vjust = 1.5
  )

# Calculate and visualize a # projects trend
projects_trend <- lm(`# Projects` ~ `Project Start Year Numeric`, data = by_year)
by_year$`# Projects Predicted` <- predict(projects_trend)
by_year$`# Projects Predicted Error` <- by_year$`# Projects Predicted` - by_year$`# Projects`

ggplot(by_year, aes(x = `Project Start Year`, y = `# Projects`)) +
  geom_point(color = "steelblue") +
  geom_line(aes(y = `# Projects Predicted`), color = "darkred") + 
  labs(title="Trend of Projects Started by Year") +
  annotate(
    "text",
    x = max(by_year$`Project Start Year`, na.rm = TRUE),
    y = max(by_year$`# Projects`, na.rm = TRUE),
    label = paste0(
      "Reg. Coefficient = ",
      round(coef(projects_trend)["`Project Start Year Numeric`"], 3),
      " (YoY)"
    ),
    hjust = 1.1,
    vjust = 1.5
  )

# Correlation matrix
cor(by_month[,c("# Units", "# Projects", "6th or 12th month")], method = "pearson")