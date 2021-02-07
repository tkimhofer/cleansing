.prep_x=function(x, x.multi=NULL, un=NULL, n_perc, x_lev=NULL, x.dtype=NULL){
  x_nas=.count_na_x(x)
  df=data.frame(x=x[x_nas$idx_x], stringsAsFactors = F)
  if(is.null(x.dtype)){x.dtype=infer_dtype(df$x)$dtype_inferred}

  df$x=.conv_dtype(df$x, x.dtype, x_lev)
  if(x.dtype=='str' | x.dtype=='bool'){
    # check if multi
    x_un=unique(df$x)
    xn_un=length(x_un)
    if(is.null(x_lev)){x_lev=infer_level(df$x, n_un = xn_un)}
    if(is.null(x.multi)) x.multi=x_lev$multiresponse
    if(x.multi){
      # reformat
      ds=.split_multiresp_df(x = df$x, sep = x_lev$sep_inferred)
      df=ds
      colnames(df)[2]='x'
    }
    # reduce low freq levels to other
    df$x=.combine_lowFreqLevels(df$x, n=n_perc, repl='Other', includeLevN=T)

    if(is.null(un)){
      un=unique(df$x)
    }
    # assigne levels
    df$x=factor(df$x, levels = un)
  }else{
    un=NA
  }

  return(list(x.dtype, df, un, x_lev))

}
