

clean_streets = function(frame) {
  frame |> 
    mutate(addr_name = toupper(addr_name))|>
    extract(
      addr_name, 
      c("number", "direction", "streetname", "street_type", "city"), 
      "([[:digit:]]+) (WEST |EAST |NORTH |SOUTH |E |W |N |S )?(.* ?)(AVE|LANE)?(Manhattan|Brooklyn)?", remove = FALSE)
}

