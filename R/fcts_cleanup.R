#' @title Set non-informative values to NA
#' @param x variable to be cleaned
#' @return variable with values such as "na" set to NA
#' @details Replaced with NA are strings: NA, nan, inf, "NA", "NAN", "null"
.clean_na<-function(x){
  # remove leading and traling whitespaces
  x=gsub('^ | $', '',  x)
  # clean NA
  gsub('^na$|^null$|^nan$|^inf$|^null$^NA$', NA, x, ignore.case = T)

}

#' @title Counting the number of NA
#' @param x variable for summary statistic
#' @param x variable for stratification
#' @return df counting NA, abs, %
.count_na<-function(x, y){
  # x is variable to be summaryised
  # y is stratification
  # x,y are clean, ie no other variables
  n_x=length(x)
  n_y=length(y)

  if(n_x != n_y){stop('count_na -> vectors have different lengths')}

  idx_x=(is.na(x)| is.infinite(x)| is.nan(x))
  idx_y=(is.na(y)| is.infinite(y)| is.nan(y))

  x_na=length(which(idx_x & !idx_y))
  y_na=length(which(idx_y & !idx_x))
  xy_na=length(which(idx_x & idx_y))

  x_na=c(x_na, x_na/n_x*100)
  y_na=c(y_na, y_na/n_x*100)
  xy_na=c(xy_na, xy_na/n_x*100)

  data.frame(x_na, y_na, xy_na)

}


#' @title Variable class conversion
#' @param x variable to be converted
#' @param dtype desired variable class (numeric, string, factor)
#' @param levs levels in case of class factor
#' @return converted x
.conv_dtype<-function(x, dtype=c('num', 'str', 'fac'), levs=NULL){


  .conv<-function(x, dtype){
    switch(dtype,
           'num'={as.numeric(x)},
           'fac'={
             if(!is.null(levs)) {factor(x, levels=levs)} else{
               as.factor(x)
             }
           },
           'str'=as.character(x))
  }


  if(is.list(x)){
    out=lapply(x, function(xx) {.conv(xx, dtype)})
  }else{
    out=.conv(x, dtype)
  }

  return(out)

}

#' @title Split multilevel variables
#' @param x Multilevel variable to be split, given as string
#' @param sep Symbol that separates levels within a string
#' @param return return list or vector (see Details)
#' @return converted x
#' @details return argument is list: each x element is list element, return argument is vector: list is vectorised to 1D array, length does not equal number of elements in x then.
.split_multiresp<-function(x, sep=', ', return=c('list', 'vector')){

  x_split=strsplit(x, sep) # this is a list
  x_split=lapply(x_split, gsub, pattern="^ | $", replacement="")

  if(return=='vector'){
    # create array of indeces to track responses from db rows/entries
    names(x_split)=paste0(as.character(1:length(x_split)), '.')
    return(unlist(x_split))
  }else{
    return(x_split)
  }
}


#' @title Merge low freq levels into a single level
#' @param x array, categorical variable to be processed
#' @param n num Percent frequency value of x level, for assigning new level
#' @param repl char, Name of new level
#' @param includeLevN logic, Level name indicates number of merges
#' @return x with low freq levels merged into new level
.combine_lowFreqLevels<-function(x, n=2, repl='Other', includeLevN=T){
  cs=table(x)
  idx=which(cs<(sum(cs)*(n/100)))

  if(length(idx)>0){
    lev_repl=names(idx)
    if(includeLevN) {
      n=length(idx)
      repl=paste0(repl, ' (', n, ' levels)')
    }

    lev_repl=gsub('(', '\\(', lev_repl, fixed=T)
    lev_repl=gsub(')', '\\)', lev_repl, fixed=T)
    x_repl=gsub(paste0('^', paste0(lev_repl, collapse='$|^'), '$'), repl, x, fixed = F)
    return(x_repl)
  }else{
    return(x)
  }

}

#' @title Combine uni and/or multilevel variables, rm NA's
#' @param x array, categorical or numeric variable to be merged with y
#' @param y array, categorical or numeric variable to be merged with x
#' @return list, 1: merged x and y, 2: indices of NA in x or y
.comb_xy<-function(x, y){
  if(length(x)!=length(y)) {stop('unequal x and y length')}

  if(is.list(x) & !is.list(y)){
    # unlist and combine with y (rep y)
    ids=unlist(sapply(seq(length(x)), function(i){
      rep(i,  length(x[[i]]))
    }))
    #browser()
    df=data.frame(x=unlist(x), y=y[ids], ids)}

  if(!is.list(x) & is.list(y)){
    # unlist and combine with y (rep y)
    ids=unlist(sapply(seq(length(y)), function(i){
      rep(i,  length(y[[i]]))
    }))
    #browser()
    df=data.frame(x=x[ids], y=unlist(y), ids)}

  if(is.list(x) & is.list(y)){
    stop('both vars, x and y are lists.')
    }

  if(!is.list(x) & !is.list(y)){
    df=data.frame(x, y, ids=seq(length(x)))
  }

  idx=which(!(is.na(df$x) | is.na(df$y)))
  return(list(df[idx,], idx))
}


#' @title Univariate stats for a categorical variable
#' @param ds data.frame, categorical variable and segmentation variable
#' @param x.multi logic, is x cleaned multi-level variable?
#' @param y.multi logic, is y cleaned multi-level variable?
#' @return list, 1: contingency table(x,y) and y, 2: Chi-squared p value
.countsFreq_chiSq<-function(ds, x.multi, y.multi){
  if(y.multi & !x.multi){
    #browser()
    y_lev=unique(ds$y)
    ds_uni=unique(ds[, colnames(ds) %in% c('x', 'ids')])
    ds_uni$y=F
    ds_uni$y=factor(ds_uni$y, levels=c(T, F))
    ds_uni$x=factor(ds_uni$x, levels=unique(ds$x))
    ct=sapply(y_lev, function(lev, ds_u=ds_uni){
      idc=ds$ids[which(ds$y==lev)]
      ds_u$y[ds_u$ids %in% idc]=T
      #ds_u
      table(x=ds_u$x, seg=ds_u$y)[,1]
    })
  }

  if(x.multi & !y.multi){
    x_lev=unique(ds$x)
    ds_uni=unique(ds[, colnames(ds) %in% c('y', 'ids')])
    ds_uni$x=F
    ds_uni$y=factor(ds_uni$y, levels=unique(ds$y))
    ds_uni$x=factor(ds_uni$x, levels=c(T, F))
    ct=sapply(x_lev, function(lev, ds_u=ds_uni){
      idc=ds$ids[which(ds$x==lev)]
      ds_u$x[ds_u$ids %in% idc]=T
      #ds_u
      table(x=ds_u$x, seg=ds_u$y)[1,]
    })
  }

  if(!x.multi & !y.multi){
    ct=table(ds$x, ds$y)
  }

  if(any(ct<5 & ct!=0)) warning('Low number of counts, p value instable')
   cs=chisq.test(ct)
   return(list(ct, cs))
}

.ct_to_perc<-function(ct){
  if(is.null(nrow(ct))){ct/sum(ct)}else{
    ct=apply(ct, 2, function(x){
      x/sum(x)
    })
  }
  ct*100
}


#' @title Statistical group comparison for a numeric variable
#' @param x num array, variable for group comparison
#' @param y cat array, segementation variable defining groups
#' @return list, 1: p value for comparing all groups and y, 2: contingency table y
#' @details Pariwise group comparison is performed with fct .num_groupComp_pariwise
.num_groupComp<-function(x, y){
  ct=table(y)
  ctp=(ct/sum(ct))*100
  if(any(ct<5)) stop('low number of counts')
  if(any(ctp<15)) warning('unequal group sizes, p value instable')
  if(length(ct)==1) stop('no segmantation level')
    p=kruskal.test(x, factor(y))$p.value
    # could do pairwise effect sizes here
    return(list(p, ctp))
}


#' @title Pairwise statistical group comparison for a numeric variable
#' @param x num array, variable for group comparison
#' @param y cat array, segementation variable defining groups
#' @return matrix, row 1: p value, row 2: Cliff's delta effect size
.num_groupComp_pairwise<-function(x, y){
  pw_combn=combn(unique(as.character(y)), 2)
  ps=apply(pw_combn, 2, function(lev){
    idx=which(y %in% lev)
    g=factor(y[idx])
    val=x[idx]
    p=kruskal.test(val, g)$p.value
    dc=es_cdelta(ref=x[which(g==lev[1])], comp=x[which(g==lev[2])])
    c(p, dc)
  })
  ps=as.data.frame(ps)
  colnames(ps)=apply(pw_combn, 2, paste, collapse=' vs.')

  return(ps)
}

#' @title Pairwise effect size - Cliff's delta
#' @param ref num array, variable values for group 1 (reference group)
#' @param comp ref num array, variable values for group 2 (comparison group)
#' @return Cliff's delta value
es_cdelta <- function(ref, comp) {

  if (!is.numeric(ref) | !is.numeric(comp))
    stop("Input not numeric")

  ind_ref <- is.na(ref) | is.infinite(ref)
  ind_comp <- is.na(comp) | is.infinite(comp)

  if (any(ind_ref)) {
    ref <- ref[!ind_ref]
    message("Removing NA or infinite values from reference group.")
  }
  if (any(ind_comp)) {
    comp <- comp[!ind_comp]
    message("Removing NA or infinite values from comparator group.")
  }

  if (length(ref) < 5 | length(comp) < 5)
    stop("Low number of values (< 5)")

  top_counts <- vapply(ref, function(x, y = comp) {
    names(x) <- NULL
    names(y) <- NULL
    c(length(which(x > y)), length(which(x < y)))
  }, FUN.VALUE = c(2, length(ref)))
  out <- ((sum(top_counts[1, ]) - sum(top_counts[2, ]))/(length(ref) * length(comp))) *
    (-1)
  return(out)
}






#' @title Descriptive stats for a numeric variable startified by second variable
#' @param x num array, numeric variable
#' @param y cat array, segementation variable
#' @return array of quantiles (range, IQR point estimates, median), length and %total for each segmentation level
.num_descrStats<-function(x, y){
  require(plyr)
  ct=table(y)
  ctp=(ct/sum(ct))*100

  if(any(ct<5)) stop('low number of counts')
  if(any(ctp<15)) warning('unequal group sizes, p value instable')
  if(length(ct)==1) stop('no segmantation level')

  cd=data.frame(x, y)
  quants=ddply(cd, .(y), function(ds, ntot=nrow(cd)){
    c(quantile(ds$x, probs=c(0, 0.25, 0.5, 0.75, 1)), n=nrow(ds), n_perc=(nrow(ds)/ntot) *100)
  })

 return(quants)
  # could do pairwise effect sizes here
}

#' @title Create table of abs and rel frequencies (%) for categorical variables
#' @param list of two: 1: p values semgentation levels, 2: contigency table (abs, %)
#' @return table ready to export
.to_tbl_cat<-function(pv){
  nr=nrow(pv[[1]][[1]])
  nc=ncol(pv[[1]][[1]])
  ct_out=t(sapply(seq(nr), function(i){
    paste0(pv[[1]][[1]][i,], ' (', round(pv[[2]][i,]), '%)')
  }))

  colnames(ct_out)=colnames(pv[[2]])

  ct_out=data.frame(id=rownames(pv[[2]]), ct_out)
  add=rbind(rep("", nc+1), rep("", nc+1), rep("", nc+1))
  add[2,1]='p value'
  add[3,1]=format.pval(pv[[1]][[2]]$p.value, digits = 2)
  colnames(add)=colnames(ct_out)

  rbind(ct_out, add)


}


#' @title Create table of summary stats for continuous variables
#' @param list of two: 1: p values semgentation levels, 2: quantiles for segm. levels
#' @return table ready to export
.to_tbl_num<-function(pv){
  nr=nrow(pv[[1]][[1]])
  nc=ncol(pv[[1]][[1]])
  ct_out=t(sapply(seq(nr), function(i){
    paste0(pv[[1]][[1]][i,], ' (', round(pv[[2]][i,]), '%)')
  }))

  colnames(ct_out)=colnames(pv[[2]])

  ct_out=data.frame(id=rownames(pv[[2]]), ct_out)
  add=rbind(rep("", nc+1), rep("", nc+1), rep("", nc+1))
  add[2,1]='p value'
  add[3,1]=format.pval(pv[[1]][[2]]$p.value, digits = 2)
  colnames(add)=colnames(ct_out)

  rbind(ct_out, add)


}



.comb_tbls<-function(abs, ct, format, html=T){

  if(format=='long'){
    abs=addmargins(abs)
    ct=addmargins(ct)

    out=sapply(seq(ncol(abs)), function(i){
      pp=as.character(round(ct[,i]))
      pp[pp=='0' & abs[,i]>0]='<1'
      paste0(formatC(abs[,i], big.mark = ','),' (', pp, '%)')
    })


    if(is.null(nrow(out))) out=t(out)
    colnames(out)=colnames(abs)
    rownames(out)=rownames(abs)
    return(out)
  }
  if(format=='1row'){

    out=sapply(seq(nrow(abs)), function(i){
      #if(i %% 2 == 0){oo=rep('', 1); return(oo)}else{
        #i=1+((i-1)/2)
        pp=as.character(round(ct[i,]))
        pp[pp=='0' & abs[i,]>0]='<1'
        oo=paste0(formatC(abs[i,], big.mark = ','),' (', pp, '%)')
        oo
      #}
    })
    out=c(out)
    #names(out)=rep(c(colnames(ct), ''), nrow(ct))[1:length(out)]
    #if(is.null(nrow(out))) out=t(out)
    # idx_c=seq(from=1, to=ncol(out), by=2)
    # colnames(out)[idx_c]=rownames(abs)
    # colnames(out)[-idx_c]=""
    # rownames(out)=colnames(abs)

    olong=matrix(out, nrow=1, ncol=length(out))
    olong=rbind(rep(colnames(ct), nrow(ct)), olong)
    # #olong=rbind(out)
    #
    # add=c(sapply(seq(nrow(ct)), function(i){
    #   add=rep('', ncol(ct)+1)
    #   add[1]=rownames(ct)[i]
    #   add
    # }))

    if(html==T){
      tbl=olong
      # html table
      tbl[2,] %>%
        addHtmlTableStyle(align="c",
                          # col.columns = c(rep("none", 2),
                          #                 rep("#F5FBFF", 4)),
                          css.cgroup='background-color: #F5FBFF;') %>%

        htmlTable(
          header=olong[1,],
          cgroup=rownames(ct),
          n.cgroup=c(rep(ncol(ct), nrow(ct)))
        ) -> ex

    }else{
      ex=rbind(add[1:ncol(olong)], olong)
      rownames(ex)=NULL


    }


    return(ex)

  }


}



#' @export
.tbl_output<-function(res, format, html=T){

  if(format=='long'){
    n_tot=formatC(sum(res$counts[[1]])+sum(res$nas[1,]), big.mark = ',')
    out<-.comb_tbls(res$counts[[1]], res$perc, format)
    if(ncol(out)<3) {
      add=matrix("", nrow=nrow(out), ncol=ncol(out)-3)
      out=cbind(out, add)
    }
    out[,ncol(out)]=gsub(' \\(.*', '', out[,ncol(out)])
    add<-add1<-rep("", ncol(out))
    out<-rbind(out, " "=add)
    add1[1]=n_tot
    out=rbind(out, "n_total"=add1)
    add[1]<-c(format.pval(res$counts[[2]]$p.value, 2))
    out<-rbind(out, p.value=add)

    nas<-.comb_tbls(t(t(res$nas[1,])), t(t(res$nas[2,])), format)
    nas=c(paste(c('Var:', 'Seg:', 'Both:'), nas)[c(2,1,3)], rep('', ncol(out)-3))
    out=rbind(out, "nas"=nas)
    rownames(out)[nrow(out)]='NAs'


  }
  if(format=='1row'){

    if(html){
      out=.comb_tbls(res$counts[[1]], res$perc, format, html)
    }else{
      out=.comb_tbls(res$counts[[1]], res$perc, format)
    }
  }


  return(out)

}


# infer num, str, logic, factor

infer_dtype=function(x){

  x=x[!is.na(x)]

  t_num=NA
  t_str=NA
  t_bool=NA

  n=length(x)
  n_un=length(unique(x))

  x_num=as.numeric(gsub('<|>', '', x))
  if(length(which(is.na(x_num)))<(n*0.20) & n_un>2){t_num=T; t_str=F; t_bool}else{t_num=F}

  if(n_un==2){
    t_bool=T; t_str=F}else{t_bool=F}

  if((!t_bool & !t_num) | (!t_bool & n_un<5)){
    t_bool=F
    t_str=T
  }

  return(c('num', 'str', 'bool')[which(c(t_num, t_str, t_bool))])
}

infer_level=function(x, oc=20){

  x=x[!is.na(x)]

  n=length(x)
  n_un=length(unique(x))

  seps=c(',', ';')
  mat=sapply(seps, function(sep){
    idx=grep(sep, x)
    if(length(idx)<(n*(oc/100)) & length(idx)!=0){
      if(length(unique(x[-idx]))<10){

        test=unique(gsub('^ | $', '', unique(unlist(strsplit(x, sep, fixed = T)))))
        out=all(test!='' & test %in% unique(x[-idx]))
        return(out)
      }else(T)
    }else{
      return(F)
    }
  })
  if(any(mat)){
    out=paste('Multilevel, separators:', paste(names(mat)[which(mat)], sep=' and '))
  }else{out='Single level'}

  return(out)

}


.tbl_output_x_num<-function(res, format, html=T){

  x_tot=formatC(sum(c(unlist(res$nas[1,]), res$summary$n)), big.mark = ',', format='d')
  if(format=='long'){
    out=res$summary[,-1]
    out$n_perc=round(as.numeric(out$n_perc), 1)
    out$n=formatC(as.numeric(out$n), big.mark = ',', format='d')
    add=add1=rep('', ncol(out))
    out=rbind(out, add)
    add[1]=x_tot
    out=rbind(out, add)
    add[1]=format.pval(res$stats[[1]], digits = 2)
    out=rbind(out, add)
    add[1]=round(res$stats[[1]], digits = 2)
    out=rbind(out, add)

    rownames(out)=c(as.character(res$summary[,1]),'','n', 'p value', 'Cd')

  }
  if(format=='1row'){
    x_sum=apply(res$summary[,-1], 1, function(x){
      # check if integer
      if(all(as.integer(x[1:5])==x[1:5])) {r=0 }else{ r=2}
      ra=round(c(x[1], x[5]), r)
      paste(round(x[3], r), ' (', ra[1], '-', ra[2], ')', sep='')

    })
    df=as.data.frame(t(x_sum))
    colnames(df)=res$summary[,1]

    out=df
    if(html){

      tbl=df
      rownames(tbl)=NULL
      # html table
      tbl %>%
        addHtmlTableStyle(align="c",
                          css.cell = "padding-left: .5em; padding-right: .4em;") %>%
        htmlTable(
          rnames=F,
          n.cgroup=c(rep(ncol(tbl)))
        ) -> out
    }else{
      out=df
    }
  }

  return(out)

}


