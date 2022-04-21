clean_streets = function(frame) {
  frame |> 
    mutate(addr_name = toupper(addr_name))|>
    extract(
      addr_name, 
      c("number", "streetname","city"), 
      "([[:digit:]]+)(.*)(Manhattan|Brooklyn)?", remove = FALSE)
}

dir = read_directory(1850)
cleaned = dir |> clean_streets()

#getting the street_type & boro (excluding those not in Manhattan and Brooklyn)

#the following code is written by myself so any mistakes not attributable to other team members

street_type= function(df,x){
  df|>
    mutate(
      streetname = case_when(
        str_detect(x, "AVENUE") ~ str_replace(x,"AVENUE","AV"),
        str_detect(x, "BROAD$") ~ str_replace(x,"BROAD$","BROADWAY"), 
        #↑ don't want "BROADWAY ST" but "123 BROADWAY"
        TRUE ~ str_c(x, " ST")
        #without the TRUE... line, value would be changed into NA instead of "ST"
      ))
}

cleaned = street_type(cleaned,cleaned$streetname)
# the Avenue types are still looking weird, mostly with digits dangling at the end; but works okay with normal examples like "Greenwich Avenue" and "Lexington Avenue"

#only keeping addresses registered in the boros of Manhattan and Brooklyn
boro_city_state = function(df,x,y){
  df|>
    mutate(
      Boro = case_when(
        str_detect(x, "BROOKLYN$") ~ "Brooklyn",
        str_detect(x,"Queens Long Island|Bronx$|New Jersey|N J|Jersey City|Germany|Italy|France|New Britain") ~ "",
        TRUE ~ "Manhattan"
      ))|>
    mutate(
      streetname = case_when(
        str_detect(Boro,"Brooklyn") ~ str_replace(y,"BROOKLYN ",""),
        TRUE ~ as.character(y)
      ))|>
    filter(Boro != "")|>
    mutate(City = "New York", State = "NY")
}

cleaned = boro_city_state(cleaned,cleaned$addr_name,cleaned$streetname)

#changing the order of columns to make "streetname" "number" etc at front and move "entry_uuid" etc to the end
#don't run this function twice though!
ordering = function(frame){
  cols = colnames(cleaned)
  new_cols = c(cols[13],cols[length(cols)],cols[length(cols)-1],cols[length(cols)-2],cols[3],cols[4],cols[10],cols[12],cols[14],cols[15],cols[2],cols[1],cols[6],cols[7],cols[8],cols[9])
  frame[,new_cols]
}  

cleaned = ordering(cleaned)


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
