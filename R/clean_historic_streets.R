
street_shapes = NULL

get_street_shapes = function() {
  if (!is.null(street_shapes)) {return(street_shapes)}
  if (!file.exists("data/nyc_streets_historical_20220408.json")) {
    R.utils::gunzip("data/nyc_streets_historical_20220408.json.gz", remove=FALSE)
  }
  street_shapes <<- read_sf("data/nyc_streets_historical_20220408.json")
}
street_geom = NULL
street_geometry = function() {
  if (!is.null(street_geom)) return(street_geom)
  street_geom <<- get_street_shapes() |>
  select(OBJECTID, City, Boro)
}

matchable_set = NULL
get_matchable_set = function() {
  if (!is.null(matchable_set)) return(matchable_set)
  # Create a metadata-only version with the numbers ranges pivoted to include 
  # different years.
  
  # Drop the geometry
  just_metadata = get_street_shapes() |> st_set_geometry(NULL)
  
  cleaned = just_metadata |>
    mutate(across(starts_with("y"), as.character)) |> 
    select(OBJECTID, starts_with("y")) |>
    # Pull out the year columns
    pivot_longer(starts_with("y"), names_pattern = "y([[:digit:]]+)(.*)", names_to = c("year", "variable")) |> 
    # Fold back everything but the year variables
    pivot_wider(names_from = variable, values_from = value) |>
    # These are computational things not of core relevance
    select(-WardLeft, -WardRight, -ED_Left, -ED_Right) |>
    # If a street doesn't exist that year, drop it.
    filter(Valid == 1) |>
    
    select(OBJECTID, year, NameStreet, `_hnyc_street`, Left_Low, Left_High, Right_Low, Right_High) |>
    # Make 'high' and 'low' be numeric. (Dicey--probably there is an "A" or two in there.) 
    mutate(across(c(ends_with("Low"), ends_with("High")), function(x) x |> str_extract("[[:digit:]]+") |> as.integer())) |>
    filter(!is.na(Left_Low + Left_High) | !is.na(Right_Low + Right_High))
  
  cleaned = cleaned |>
    # is this a block that has numbers only on left, only on right, or both?
    # affects how we use odd and even numbers.
    mutate(group = case_when(
      is.na(Left_Low + Left_High) ~ "right only",
      is.na(Right_Low + Right_High) ~ "left only",
      TRUE ~ "both"
    )
    ) |> rename(streetname = `_hnyc_street`)
  
  source("R/street cleaners.R")
  cleaned_renamed = cleaned |> 
    street_type() |>
    fix_avenues()
  
  exploded = cleaned_renamed |>
    pivot_longer(`Left_Low`:`Right_High`, names_sep = "_", names_to = c("side", "highness")) |> 
    pivot_wider(names_from = "highness", values_from = "value") |>
    filter(!is.na(Low), !is.na(High)) |> 
    filter(Low <= High) |> # Weirdly, this happens.
    rowwise() |>
    mutate(number = list(seq(Low, High, by = ifelse(group == "both", 2, 1)))) |>
    unnest(number)
  
  matchable_set <<- exploded |> mutate(position_on_street = (number - Low) / (High - Low)) |>
    select(-Low, -High)
  
}
