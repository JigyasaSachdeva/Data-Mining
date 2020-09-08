library(tidyverse)

my_data <- as_tibble(iris)
my_data

#Extract column values as a vector
a <- my_data$Species
#OR
my_data %>% pull(Species)

#Extract columns as a data table
my_data %>% select(1:3)
my_data %>% select('Sepal.Length', 'Species')
my_data %>% select(-'Species')

#Extract columns with names starting and ending with
my_data %>% select(starts_with("Petal"))
my_data %>% select(ends_with("Width"))

#Extract columns names containing "etal"
my_data %>% select(contains("etal"))

#Extract column names that match a regular expression
my_data %>% select(matches(".t."))

#Atleast one of the variables will be shown irrespective of being present
my_data %>% select(one_of(c("Sepal.Length", "Petal.Length", "x")))

#Selecting based on a condition
my_data %>% select_if(is.numeric)
my_data %>% select_if(is.factor)
my_data %>% select_if(is.character)

#Removing Columns
my_data %>% select(-Sepal.Length)
my_data %>% select(-Sepal.Length, -Petal.Width)
#Removing Sequence of columns
my_data %>% select(-(Sepal.Length:Petal.Length))

#Extract rows by position
my_data %>% slice(1:6)

#Filter rows
my_data %>% filter(Petal.Length > 2)
my_data %>% filter(Petal.Length <= 2) 
my_data %>% filter(Petal.Length <= 2, Sepal.Length > 1) #, : And
my_data %>% filter(Species %in% c('setosa', 'versicolor'))
my_data %>% filter(Species != 'virginica')
my_data %>% filter(Sepal.Width > 1 | Petal.Length<=2)
my_data %>% filter(Petal.Length %in% x)

#Not In
x = seq(3,4,0.1)
`%notin%` <- Negate(`%in%`)
a <- my_data %>% filter(Petal.Length %notin% x) 
a$Petal.Length

#Dropping one row
data <- my_data %>% select(-Species)
#Dropping multiple rows
data <- my_data %>% select(-c(Species, Petal.Length))

#Selecting rows when all variables > 2.4
#filter_all
data %>% filter_all(all_vars(.> 2.4))

#Selecting rows when any variable has > 2.4
data %>% filter_all(any_vars(.>2.4))

#filter_at 
my_data %>% filter_at(vars(starts_with("Sepal")),
                      any_vars(.>2.4))
my_data %>% filter_at(vars(matches(".t.")),
                      all_vars(.>2.4))


friends_data <- data_frame(
  name = c("A", "B", "C", "D"),
  age = c(27, 25, 29, 26),
  height = c(180, NA, NA, 169),
  married = c("yes", "yes", "no", "no")
)
friends_data

#Select where null values are present
friends_data %>% filter(is.na(height))

#Drop rows where height is null
friends_data %>% filter(!is.na(height))

#Select random rows from a dataset
my_data %>% sample_n(5, replace = F) #Number of rows
my_data %>% sample_n(0.05, replace = F) #Percentage of rows

#Select top n rows ordered by a variable
my_data %>% top_n(5, Sepal.Length)

#GSelect top 5 petal lengths of each specie
my_data %>% group_by(Species) %>% top_n(5, Petal.Length)








