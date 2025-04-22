print.to.file <- function(df, filename) {
  cnames <- colnames(df)
  n      <- as.matrix(nchar(cnames))
  
  d <- apply(df, 2, format)
  n <- apply(cbind(n, nchar(d[1,])), 1, max)
  
  fmts <- paste0("%",n,"s")
  for(i in 1:length(cnames)) {
    cnames[i] <- sprintf(fmts[i], cnames[i])
    d[,i] <- sprintf(fmts[i], trim(d[,i]))
  }
  d <- rbind(cnames, d)
  write.table(d, filename, quote=FALSE, row.names=FALSE, col.names=FALSE)
}