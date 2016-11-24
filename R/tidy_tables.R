tidy_listing_table <- function(tab) {
  for(row in 1:length(tab$Species)) {
    sub <- tab[row, ]
    if(sub$Species == sub$YearListed) {
      cur_head <- sub
    }
  }
}