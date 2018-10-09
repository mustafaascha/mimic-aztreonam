
assert_concordant_arguments <- function(only_first, duration, named, collect) {
  if (!collect) {
    if (only_first | duration | named) {
      stop ("If 'collect' is false, none of 'only_first', 'duration', 
            or 'named' can be true. This is to accomodate query planning")
    }
  }
}

