# Write a function that allows you to add up the payments at an office/month
# (Note: The function is designed to work with this specific dataset!)

revenues <- function (x, office = NULL) {
  if (is.character(office))
    df <- x[x$off == office, ]
  else df <- x
  ans <- NULL
  
  levels(df$revenue.cat) <- 1:14
  for (i in 1:14) {
    dm <- df[df$revenue.cat == i, ]
    ans[i] <- sum(dm$amount)
  }
  ans
}