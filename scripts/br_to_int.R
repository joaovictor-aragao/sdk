br_to_int <- function(data, vars=FALSE) {
  #' Convert to numeric
  #' @description This function convert BR numeric into R understandable language
  #' 
  #' By using this you will notice there are two params.
  #' 
  #' @param data character or dataset. Paste a string, vector, matrix or dataframe on it.
  #' @param vars numeric or vector. Paste a bunch of columns need to be converted (only TRUE if it is a matrix or dataframe)
  #' 
  #' @usage br_to_int(dataframe, vector) or br_to_int(dataframe, number) or br_to_int(string) or br_to_int(c(string1, string2))
  #' 
  #' @note R doesn't modify in place, you have to assign results.
  #' 
  #' @examples 
  #' br_to_int(my_data, c(1,3,4))
  #' br_to_int("34.456.767,34")
  #' 
  if (vars == TRUE) {
    for (var in vars) {
      while (any(unlist(gregexpr("\\.", data[, var])) > 0)) {
        data[, var] <- sub("\\.", "", as.character(data[, var]))
      }
      data[, var] <- as.numeric(sub(",", ".", data[, var]))
    }  
  } else {
    while (any(unlist(gregexpr("\\.", data)) > 0)) {
      data <- sub("\\.", "", as.character(data))
    }
    data <- as.numeric(sub(",", ".", data))
  }
  
  return(data)
}
