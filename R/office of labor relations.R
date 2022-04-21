

supplement_occupations = function(frame) {
  frame
}

read_year = function(year) {
  arrow::read_parquet("flat_parquet/{year}.parquet" |> str_glue(), col_select = c(occupation))
}

write_all_occupation_counts = function() {
  total_counts = 1850:1889 |> map_dfr(read_year) |> count(occupation)
  total_counts |> arrange(-n) |>
    write_csv("all_occupation_counts.csv")
}