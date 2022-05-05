source("./street claners.R")

sanitize_better = function(df) {
  df |>
    filter(addr_name |> str_detect("[0-9]"), addr_name |> str_detect("[a-z]"), !is.na(occupation), !is.na(addr_name)) |>
    filter(occupation|>str_detect("[0-9]", negate = TRUE ))
}

eliminate_overfull_addresses = function(frame) {
  counts = frame |>
    count(entry_uuid, addr_h) |> arrange(-n)
  # Get a list of entry_uuids that appear more than 4 times.
  overcounted = counts |> filter(n > 4) |> select(-n)
  # the last item in a function is what's passed out of it.  
  frame |> anti_join(overcounted)
}

library(arrow)
read_directory = function(year) {
  # str_glue inserts the year into the filename.
  filename = str_glue("flat_parquet/{year}.parquet")
  raw = read_parquet(filename)
  
  raw |> 
    eliminate_overfull_addresses() |>
    filter(str_length(name) > 8) |>
    sanitize_better() |> 
    mutate(directory_year = str_replace(directory_year, "18..-", "18") |> as.numeric()) |>
    clean_streets()
  
}


eliminate_overfull_addresses = function(frame) {
  overcounted = frame |>
    count(entry_uuid, addr_h) |> 
    filter(n > 4) |> 
    select(-n) |>
    anti_join(frame)
  
  # the last item in a function is what's passed out of it.
  frame |> anti_join(overcounted)
}

