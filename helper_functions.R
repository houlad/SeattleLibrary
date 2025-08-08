filter_by_condition <- function(df, condition, ...){
  #filters the data based on supplied conditions and then collects, returning a tibble
  df |> 
    filter({{condition}}, ...) |> 
    collect()
  
}

select_by_variable <- function(df, variable, ...){
  df |> 
    select({{variable}}) |> 
    collect()
}

add_dates <- function(df){
  df |> 
    mutate(
      dow = wday(CheckoutDateTime, label = TRUE),
      month = month(CheckoutDateTime),
      hour = hour(CheckoutDateTime),
      week = week(CheckoutDateTime)
    )
}

add_age_group <- function(df){
  df |> 
    mutate(age = case_when(
      Collection %in% (type_data |> filter(age_group == 'Teen') |> pull(code)) ~ "Teen",
      Collection %in% (type_data |> filter(age_group == 'Juvenile') |> pull(code)) ~ "Juvenile",
      Collection %in% (type_data |> filter(age_group == 'Adult') |> pull(code)) ~ "Adult"),
    )
}


get_year_and_add_age_group <- function(df, condition){
  df |> 
    filter_by_condition({{condition}}) |> 
    add_age_group()
}

#TODO Plot theme function


#TODO Plotting function/s

#Summing the total count of books when a book is labeled with multiple ItemTypes