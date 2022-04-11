
read_directory = function(year) {
  # str_glue inserts the year into the filename.
  filename = str_glue("flat_parquet/{year}.parquet")
  raw = read_parquet(filename)
  raw |> 
    eliminate_overfull_addresses() |>
    filter(str_length(name) > 10)
}

eliminate_overfull_addresses = function(frame) {
  counts = frame |>
    count(entry_uuid, addr_h) |> arrange(-n)
  # Get a list of entry_uuids that appear more than 4 times.
  overcounted = counts |> filter(n > 4) |> select(-n)
  # the last item in a function is what's passed out of it.  
  frame |> anti_join(overcounted)
}

