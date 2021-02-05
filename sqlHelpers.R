
insertSqlData <- function(table, conn, ...){
  arguments <- list(...)
  values <- paste(arguments, collapse = "','")
  sql <- paste0("INSERT INTO ", table, " VALUES ('", values,"\');")
  DBI::dbSendQuery(conn = conn, sql)
}
