

clean_streets = function(frame) {
  frame |> 
    extract(
      addr_name, 
      c("number", "direction", "streetname", "street_type", "city"), 
      "([[:digit:]]+) (West |East |North |South |E |W |N |S )?(.* ?)(AVE|ST|LANE)?(Manhattan|Brooklyn)?", remove = FALSE)
}