#' @export
#' @title Summaris variable dependent on stratification level
#' @param list of two: 1: p values semgentation levels, 2: contigency table (abs, %)
#' @return table ready to export

prep_var<-function(x, y, x.multi=F, y.multi=F, x.dtype=c('num', 'str', 'fac'),
                   x.levs=NULL, y.dtype=c('num', 'str', 'fac'), y.levs=NULL, n_perc=5){


  if(is.null(y)){
    y=rep('All', length(x))
    y.multi=F
    y.dtype='fac'
  }


  x.ori=x
  y.ori=y

  # x_na=.count_na(x)
  # y_na=.count_na(y)

  x=.rm_na(x)
  y=.rm_na(y)


  if(is.null(x.multi)) x.multi=ifelse(infer_level(x)=="Single level", F, T)
  if(is.null(y.multi)) y.multi=ifelse(infer_level(y)=="Single level", F, T)

  nas=.count_na(x, y)

  # determinet type of variable uni, multi; num, char, fac
  # if(is.null(x.dtype)){
  # iid=length(which(is.na(as.numeric(x))))
  # if(iid>length(x)*0.4){x.dtype='str'}else{
  #   x.dtype='num'
  # }
  # }


  if(x.multi){
    x=.split_multiresp(x, sep=', ', return='list')
  }
  if(y.multi){
    y=.split_multiresp(y, sep=', ', return='list')
  }

  if(is.null(x.dtype)) x.dtype=infer_dtype(x)
  if(is.null(y.dtype)) y.dtype=infer_dtype(y)


  x=.conv_dtype(x, x.dtype, x.levs)
  y=.conv_dtype(y, y.dtype, y.levs)

  if(y.dtype=='num'){
    y=cut(y, breaks = quantile(y, probs=c(0, 0.25, 0.5, 0.75, 1), na.rm = T), include.lowest = T)
  }

  df=.comb_xy(x, y)
  idx_keep=df[[2]]
  ds=df[[1]]

  #browser()
  ds$y=.combine_lowFreqLevels(ds$y, n=n_perc, repl='Other', includeLevN=T)

  if(x.dtype!='num'){
    ds$x=.combine_lowFreqLevels(ds$x, n=n_perc, repl='Other', includeLevN=T)
    res=.countsFreq_chiSq(ds, x.multi = x.multi, y.multi = y.multi)
    res_perc=.ct_to_perc(res[[1]])
    return(list(counts=res, perc=res_perc, nas=nas))
  }else{
    res=.num_groupComp(ds$x, ds$y)
    res_descr=.num_descrStats(ds$x, ds$y)
    res_pairwise<-.num_groupComp_pairwise(x = ds$x, y = ds$y)
    return(list( nas=nas, stats=res, summary=res_descr, p_values_cd=res_pairwise))
  }
}











