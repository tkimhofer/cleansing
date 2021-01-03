#' @export
#' @title clean data and prepare for pca
#' @description data canversion to numeric, dummy variables for multi-level categorical vars, missing value imputation
#' @param t1 output uf add_summary fct
#' @param ds1 data.frame to be cleaned
#' @param imp_cutoff_perc percentage of missing values for a variable to be excluded
#' @param un_filter_perc percentage of unique values of a categorical variable to be excluded
#' @return data.frame of cleaned numberic variables
#' @examples  df_to_numeric(d)

df_to_numeric<-function(t1, df1, imp_cutoff_perc=25, un_filter_perc=20){

  t2=as.data.frame(t(t1), stringsAsFactors = F)
  un_val=as.numeric(gsub('%|<|>', '', t2$Unique))
  comp_val=as.numeric(gsub('%', '', t2$Complt))


  # rm vars that are str and have a hign nb of unique vals (e.g. id variable)
  iid1=which(un_val>un_filter_perc & t2$dtype=='str')
  if(length(iid)>0){
    message('The following variabes are excluded due to a high number of unique categorical values:')
    cat(colnames(ds1)[iid], sep='\n')
  }

  # rm vars that are below imp_cutoff
  iid2=which(comp_val>imp_cutoff_perc)
  if(length(iid)>0){
    message('The following variabes are excluded due to a high number of missing values:')
    cat(colnames(ds1)[iid], sep='\n')
  }

  iid=setdiff(seq(1:nrow(ds1)), unique(c(iid1, iid2)))
  summary_list=lapply(iid, function(i){

    x=.conv_na(ds1[,i])
    # missing value imputation
    idx_na=which(is.na(x))
    nas=.count_na_x(x)
    x_na_rm=x[nas$idx_x]

    ml_x=infer_level(x, split_cutoff = 20,  n_un = length(unique(x)))
    if(ml_x$multiresponse){
      x_conv=.split_to_dummy(x, sep=ml_x$sep_inferred)
      x_conv[idx_na,]=NA

    }else{
      x_dtype=infer_dtype(x_na_rm)
      if(x_dtype$dtype_inferred %in% c('num', 'logic')) {x_conv=.conv_dtype(x, dtype = 'num')
      x_conv[idx_na]=NA} else{
        # convert multi-level vars to dummy
        x_conv=.split_to_dummy(x, sep=NULL)
        x_conv[idx_na,]=NA
      }

    }


    # missing value imputation
    if(!is.null(ncol(x_conv))){
      me=colMeans(x_conv, na.rm=T)
      x_conv[idx_na,]=matrix(rep(me, length(idx_na)), ncol=length(me), nrow=length(idx_na), byrow=T)
    }else{
      x_conv[idx_na]=mean(x_conv, na.rm = T)
    }


    return(x_conv)

  })


}
