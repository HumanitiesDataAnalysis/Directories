

#getting the street_type & boro (excluding those not in Manhattan and Brooklyn)

final_changes = function(df) {
  df |> 
    mutate(streetname = streetname |> str_replace("CENTER ", "CENTRE ")) |>
    mutate(streetname = case_when(
      streetname == "W ST" ~ "WEST ST",
      streetname == "E ST" ~ "EAST ST",
      TRUE ~ streetname
    ))
}

fix_direction = function(df) {
  df |>
    mutate(streetname = streetname |> str_replace("^WEST ", "W ")) |>
    mutate(streetname = streetname |> str_replace("^EAST ", "E "))
}

remove_number_helpers = function(df) {
  df |> 
    mutate(streetname = streetname |> str_replace("([:digit:]+)(TH|ND|ST|RD) ", "\\1 "))
}



fix_avenues = function(df) {
  df |>
  mutate(streetname = case_when(
    streetname == "1 AVE"  ~ "FIRST AVE",
    streetname == "2 AVE"  ~ "SECOND AVE",
    streetname == "3 AVE"  ~ "THIRD AVE",
    streetname == "4 AVE"  ~ "FOURTH AVE",
    streetname == "5 AVE"  ~ "FIFTH AVE",
    streetname == "6 AVE"  ~ "SIXTH AVE",
    streetname == "7 AVE"  ~ "SEVENTH AVE",
    streetname == "8 AVE"  ~ "EIGHTH AVE",
    streetname == "9 AVE"  ~ "NINTH AVE",
    streetname == "10 AVE" ~ "TENTH AVE",
    streetname == "11 AVE" ~ "ELEVENTH AVE",
    streetname == "12 AVE" ~ "TWELFTH AVE",
    streetname == "13 AVE" ~ "THIRTEENTH AVE",
    TRUE ~ streetname
  ))
}
street_type = function(df){
  # Change the street type.
  df|>
    mutate(streetname = streetname |> str_replace(" +$", "") |> 
                                      str_replace("^ +", "")) |>
    mutate(
      streetname = case_when(
        str_detect(streetname, "AVENUE$") ~ str_replace(streetname,"AVENUE$","AVE"),
        str_detect(streetname, " AV$") ~ str_replace(streetname,"AV$","AVE"),
        str_detect(streetname, " STREET$") ~ str_replace(streetname,"STREET$","ST"),
        str_detect(streetname, " PLACE$") ~ str_replace(streetname,"PLACE$","PL"),
        str_detect(streetname, " SLIP$") ~ str_replace(streetname,"SLIP$","SLIP"),
        str_detect(streetname, " ROAD$") ~ str_replace(streetname,"ROAD$","RD"),
        str_detect(streetname, " LANE$") ~ str_replace(streetname,"LANE$","LA"),
        str_detect(streetname, "^AVENUE") ~ str_replace(streetname,"^AVENUE ","AVE "), 
        str_detect(streetname, "^AV ") ~ str_replace(streetname,"^AV ","AVE "), 
        # These ones are fine.
        str_detect(streetname, " (AVE|ST|PL|SLIP|LA|MARKET|RD)$") ~ streetname,
        str_detect(streetname, "^AVE ") ~ streetname,
        # also applies to E BROADWAY, EAST BROADWAY, WEST BROADWAY, etc.
        str_detect(streetname, "BROADWAY$") ~ streetname,
        streetname == "BRD" ~ "BROADWAY",
        streetname == "BOWERY" ~ "BOWERY",
        streetname == "MADISON" ~ "MADISON AVE",
        streetname == "LEXINGTON" ~ "LEXINGTON AVE",
        streetname == "FIRST" ~ "FIRST AVE",
        streetname == "SECOND" ~ "SECOND AVE",
        streetname == "THIRD" ~ "THIRD AVE",
        streetname == "FOURTH" ~ "FOURTH AVE",
        streetname == "FIFTH" ~ "FIFTH AVE",
        streetname == "SIXTH" ~ "SIXTH AVE",
        streetname == "SEVENTH" ~ "SEVENTH AVE",
        streetname == "EIGHTH" ~ "EIGHTH AVE",
        streetname == "NINTH" ~ "NINTH AVE",
        streetname == "TENTH" ~ "TENTH AVE",
        streetname == "ELEVENTH" ~ "ELEVENTH AVE",
        streetname == "TWELFTH" ~ "TWELFTH AVE",
        streetname == "THIRTEENTH" ~ "THIRTEENTH AVE",
        #↑ don't want "BROADWAY ST" but "123 BROADWAY"
        TRUE ~ str_c(streetname, " ST")
        #without the TRUE... line, value would be changed into NA instead of "ST"
      ))
}

# the Avenue types are still looking weird, mostly with digits dangling at the end; but works okay with normal examples like "Greenwich Avenue" and "Lexington Avenue"

#only keeping addresses registered in the boros of Manhattan and Brooklyn
boro_city_state = function(df){
  df|>
    mutate(
      Boro = case_when(
        str_detect(addr_name, "BROOKLYN$") ~ "Brooklyn",
        str_detect(addr_name,"Queens Long Island|Bronx$|New Jersey|N J|Jersey City|Germany|Italy|France|New Britain") ~ "",
        TRUE ~ "Manhattan"
      ))|>
    mutate(
      streetname = case_when(
        str_detect(Boro,"Brooklyn") ~ str_replace(streetname,"BROOKLYN ",""),
        TRUE ~ as.character(streetname)
      ))|>
    filter(Boro != "")|>
    mutate(City = "New York", State = "NY")
}

# cleaned = boro_city_state(cleaned,cleaned$addr_name,cleaned$streetname)
# cleaned = ordering(cleaned)

#changing the order of columns to make "streetname" "number" etc at front and move "entry_uuid" etc to the end
#don't run this function twice though!

# Note from Ben--this would be clearer if it used `select` and the names of the columns, rather than the numbers
# here 

ordering = function(frame){
  cols = colnames(frame)
  new_cols = c(cols[13],cols[length(cols)],cols[length(cols)-1],cols[length(cols)-2],cols[3],cols[4],cols[10],cols[12],cols[14],cols[15],cols[2],cols[1],cols[6],cols[7],cols[8],cols[9])
  frame[,new_cols]
}  

clean_streets = function(frame) {
  frame |> 
    mutate(addr_name = toupper(addr_name))|>
    extract(
      addr_name, 
      c("number", "streetname"), 
      "([[:digit:]]+) (.*)", remove = FALSE) |>
    street_type() |>
    boro_city_state() |>
    remove_number_helpers() |>
    fix_direction() |>
    final_changes() |>
    mutate(number = as.numeric(number))
  #    ordering()
}

#To be revised: the following consider the case when a "number" observation contains two numbers and an "&", otherwise the "streetname" won't split properly; however, I don't know what to do with the split numbers, unnest() seems not working
#clean_streets = function(frame) {
#  frame |> 
#    mutate(addr_name = toupper(addr_name))|>
#     extract(addr_name, 
#             c("number", "streetname", "city"), 
#             "([\\d]+[\\s]*&*[\\s]*[\\d]*)(.*)(Manhattan|Brooklyn)?", remove = FALSE)|>
#     case_when(
#      str_detect(number,"&") ~ mutate(number = unnest(str_split(number," & "))),
#       TRUE ~ as.numeric()
#     }
