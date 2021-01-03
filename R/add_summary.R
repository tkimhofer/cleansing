#' @title clean variables and create summary stats
#' @param ds1 data.frame with variables in columns
#' @details Variable type is inferred (str, num, bool, logic) and the following summary stats are created: n (number of entries), completeness (% not NA, null, na, nan, inf, empty strings), number of unique values, univariate descriptors (median and range for number variables, count and fraction for categorical variables)
#' @return data.frame of summary stats that can be row-appended to the original data frame
#' @examples
#'\dontrun{
#'query=dbSendQuery(ch,"SELECT * FROM app_data;")
#'ds1=dbFetch(query)
#'dbClearResult(query)
#'test=add_summary(ds1)
#'t1=as.data.frame(test)
#'WriteXLS("t1", "sums.xlsx", row.names = T, AdjWidth = F, BoldHeaderRow = T, FreezeRow = 1, FreezeCol = 1, AllText = T)
#'
#'}
#' @export
add_summary<-function(ds1){
  summary_list=lapply(1:ncol(ds1), function(i){
    s=.summary_var(ds1[,i])
    print(i)
    tbl_summary(s)
  })

  out=do.call(cbind, summary_list)
  rownames(out)=c('n', 'dtype', 'Complt', 'Unique', 'Avg Resp', 'Summary')
  colnames(out)=colnames(ds1)
  return(out)

}





