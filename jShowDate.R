"jShowDate"<-
function(df) {
    airedIndex <- regexpr(" aired ", df$Title)
    tapedIndex <- regexpr(" taped ", df$Title)
    atIndex <- ifelse(tapedIndex > 0, tapedIndex, airedIndex)
    if(any(atIndex < 1)) {
        warning("Found ", sum(atIndex), " problems with Date")
    }
    Date <- substr(df$Title, atIndex + 7, nchar(df$Title))
    as.Date(Date)
}
