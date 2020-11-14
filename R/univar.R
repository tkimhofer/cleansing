# cont.test=function(df, cs=cs.na, n_tot, nas, type, n.min=1){
#   #print('conti')
#
#   # reduce segs to lev with more than x entries
#   # eg sex: male, female, not transgenders bc there is only a single one
#   #segs.omitted=rownames(cs)[cs[,2]<=n.min]
#   iid=cs[,2]>n.min
#
#   n_tot=n_tot-sum(cs[!iid,1]+cs[!iid,2])
#   segs.use=df$segs %in% rownames(cs)[iid]
#   cs=cs[iid,]
#   nas=nas[iid]
#   df=df[segs.use, ]
#
#
#   mat=ddply(df, .(segs), function(sub){
#
#     # summary stats
#     cent.x=median(sub$x, na.rm =T)
#     dis.x=quantile(sub$x, probs=c(0, 1), na.rm =T)
#
#     # test for normality, add star if normal distributed
#     tsw=try(shapiro.test(sub$x), silent=T)
#     if(class(tsw) == "try-error"){
#       paste(round(cent.x,2), " (",paste(round(dis.x,2), collapse="-")   ,")*", sep="") # if star - not normal dist
#     }else{
#       paste(round(cent.x,2), " (",paste(round(dis.x,2), collapse="-")   ,")", sep="")
#     }
#
#   })
#
#
#   # n also needs new definition bc seg level removed
#   if(class(cs)=='table'){
#     mat=t(cbind('n'=apply(cs, 1, sum), 'NA'=nas, 'Median (range)'=mat[,2]))
#   }else{
#     mat=t(cbind('n'=n_tot, 'NA'=nas, 'Median (range)'=mat[,2]))
#   }
#
#
#   # define sets for p val calculation
#   sets=ddply(df, .(segs), function(sub){
#     sub
#   })
#
#   # compute p values for every pairwise combination
#   if(class(cs)=='table'){
#     combs=combn(rownames(cs),2)
#     pvals=apply(combs, 2, function(i, ss=sets){
#       y=ss$x[ss$segs %in% i[2]]
#       x=ss$x[ss$segs %in% i[1]]
#       tsw.x=try(shapiro.test(x), silent=T)
#       tsw.y=try(shapiro.test(y), silent=T)
#       if(class(tsw.x) == "try-error" | class(tsw.y) == "try-error"){
#         "Not possible"
#       }else{
#         if(shapiro.test(x)$p.value >=0.05 & shapiro.test(y)$p.value >=0.05){
#           pval=t.test(x,y, alterntive="two.sided", paired=F)$p.value
#         }else{
#           pval=wilcox.test(x,y, alterntive="two.sided", paired=F)$p.value
#         }
#       }
#     })
#     pvals=format(pvals, scientific = T, digits=2)
#     names(pvals)=apply(combs,2, paste, collapse=" vs. ")
#     #pvals=sort(pvals)
#   }else{pvals=NULL}
#
#
#
#   return(list(mat, pvals))
# }
#
#
# cat.test=function(df, cs=cs.na, n_tot, nas, type, n.min=1){
#
#   #cs is table(segs, is.na(x))
#
#   # define category 'others', eg country of birth.... more than 100 levels with most of the only few entries,
#   # # so combine low represented levels in x together into cagegory: Other
#   df=combine.levs.x(df, lim=0.01)
#
#
#
#   if(is.factor(df$x)==F){
#     tt=table(df$x)
#     levs=names(tt)[order(tt, decreasing = T)]
#     df$x=factor(df$x, levels = levs)
#   }
#
#   # reduce segs to reasonable levels
#   iid=cs[,2]>n.min
#
#   n_tot=n_tot-sum(cs[!iid,1]+cs[!iid,2])
#   segs_name=rownames(cs)[iid]
#   segs.use=df$segs %in% segs_name
#
#   cs=cs[iid,]
#   nas=nas[iid]
#   df=df[segs.use, ]
#   df$segs=factor(df$segs, levels=segs_name)
#
#   # if more than one level in the stratification factor
#   if(length(which(iid))>1){
#     mat=table(df$x, df$segs)
#     #mat=mat[order(mat[,1], decreasing=T),]
#     nams=rownames(mat)
#     mat=(apply(rbind(1:ncol(mat)), 2, function(i, nn=cs){
#       perc=(mat[,i]/nn[i,2]*100)
#       percR=round(perc)
#       percR[perc>0 & perc<1]='<1'
#       paste0(mat[,i], " (", percR, "%)")
#     }))
#     if(is.null(dim(mat))){
#       mat1=t(matrix(mat))
#       colnames(mat1)=colnames(mat)
#       mat=mat1
#     }
#     rownames(mat)=nams
#     colnames(mat)=rownames(cs)
#     mat=rbind('Tot'=apply(cs,1, sum), 'NA'=nas, 'n'=cs[,2],'type'=type,  mat)
#
#     # compute p values for every pairwise combination
#     combs=combn(rownames(cs),2)
#     pvals=apply(combs, 2, function(i, tt=df$x, s=df$segs){
#       ctab=table(tt[s %in% i], s[s %in% i])
#       iid=which(apply(ctab,1,sum)!=0)
#       if(length(iid)<2){return('Not possible')}else{
#         ctab=ctab[apply(ctab,1,sum)!=0,]
#         out=try(fisher.test(ctab, simulate.p.value = T)$p.value)
#         if(class(out) == "try-error"){return("Not possible")}else{
#           format(out, scientific = T, digits=2)
#         }
#       }
#     })
#     names(pvals)=apply(combs,2, paste, collapse=" vs. ")
#     #pvals=sort(pvals, decreasing = F)
#
#   }else{
#     mat=table(df$x, df$segs)
#
#     perc=(mat/cs[2]*100)
#     percR=round(perc)
#     percR[perc>0 & perc<1]='<1'
#     mat[,]=paste0(mat, " (", percR, "%)")
#
#     mat=rbind('Tot'=n_tot, 'NA'=nas, n=cs[2],'type'=type, mat)
#     pvals=NULL
#   }
#
#   return(list(mat, pvals))
#
#
# }
#
# splits=function(x, segs){
#   # split at sign and edit segs
#   x=gsub('^ | $', '', x)
#   sps=lapply(x,  function(y) (gsub('^ | $', '', strsplit( y, ',')[[1]])) )
#
#   segs=unlist(lapply(1:length(sps), function(s){
#     rep(segs[s], length(sps[[s]]))
#   }))
#
#   x=unlist(sps)
#
#   if(length(x)!=length(segs)){stop('There\'s a mismatch in length between x and segs.'); return(0)}
#
#   return(data.frame(x, segs))
# }
#
# # so combine low represented levels in x together into cagegory: Other
# combine.levs.x=function(df, lim=0.01){
#   cs.x=table(df$x)
#   cs.x=cs.x/sum(cs.x) # percentage in x
#   idx=which(cs.x<0.01)
#
#   # get all levels of x
#   if(length(idx)>1){
#     if(is.factor(df$x)){
#       levs=levels(df$x)
#       df$x=as.character(df$x)
#     }else{
#       tt=table(df$x)
#       levs=names(tt)[order(tt, decreasing = T)]
#     }
#
#     df$x[df$x %in% names(cs.x)[idx]]=paste0('Other (',length(idx),' levels)') # redefine levels as Other and factor
#     levs=c(levs[!levs %in% names(cs.x)[idx]], paste0('Other (',length(idx),' levels)'))
#     df$x=factor(df$x, levels=levs)
#   }
#   return(df)
# }
#
#
# # load('/Volumes/Torben_1/Rproj/ch2017cleanup/Clean1718data.Rdata')
# # x=ds$age
# # #segs=ds$gender
# # segs=NA
# # #segs=NA
# # type='check'
# # split=','
# # n.min=1
#
# # addmargins(cs)
# #
# # out=splits_lev(x, segs, type, split)[[1]]
#
# # function to establish nas and split
# splits_lev=function(x, segs=NA, type=c('check', 'cont', 'cat'), split=','){
#
#   # define segmentation level if undefined: all in same segment
#   # remove all NA in segs
#   if(all(is.na(segs))){segs=rep('Entire Cohort', length(x))}else{
#     idx.seg.na=which(!is.na(segs))
#     cat('There are ', round(100-(length(idx.seg.na)/length(segs)*100),1), '% NA\'s in segs variable.\n', sep='')
#     segs=segs[idx.seg.na]
#     x=x[idx.seg.na]
#   }
#
#   ##########
#   # get n and na's in x and segs
#   n_tot=length(x)
#   na.x=factor(is.na(x), levels=c(T, F))
#   cs.na=table(segs, na.x, useNA = 'no') # NA in x by resp seg level
#   n_nonNA=cs.na[,2]
#   nas=cs.na[,1]/apply(cs.na, 1, sum)*100
#   nas1=nas
#   nas=round(nas)
#   nas[nas1>0 & nas1<1]='<1'
#   nas=paste0(cs.na[,1], ' (', nas, '%)') # nas
#
#
#   # idx.na is different now, because of split function before
#   df=data.frame(x, segs)
#   df=df[na.x==F,]
#   nn.df=nrow(df)
#
#   #########
#   # split at sign and edit segs
#   if(!is.na(split) & !is.numeric(df$x)){
#     df=splits(df$x, df$segs)
#   }
#   # table(df$x, useNA = 'always')
#   # table(df$segs, useNA = 'always')
#
#   if(type=='check'){
#     # decide if variable is numeric or factor
#     if(is.numeric(df$x)){type='cont'}else{
#       if(nrow(df)!=nn.df){type='cat-multiR'} #  multiresponse (more than one answer option/quenstionnaire)
#       if(nrow(df)==nn.df){type='cat-uniR'} #  uniresponse (a single answer option/quenstionnaire)
#     }
#   }
#
#
#
#
#
#   # perform t test or chi square
#
#   switch(type,
#          'cont'= {out=cont.test(df, cs=cs.na, n_tot, nas, type, n.min=1)},
#          'cat-multiR'={out=cat.test(df, cs=cs.na, n_tot, nas, type, n.min=10)},
#          'cat-uniR'={out=cat.test(df, cs=cs.na, n_tot, nas, type, n.min=10)}
#   )
#
#   return(list(out, df))
#
#
# }
#
#
#
#
#
