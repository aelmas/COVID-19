
drawImages = function(dat, dra.var, name1, name2){
  mat = setNames(melt(cbind(dat,Country=rownames(dat)), id.vars='Country'), c('Country','Date',dra.var))
  p1 = ggplot(mat, aes(x = Date, y = mat[,dra.var], group = Country, colour = Country)) + geom_line(na.rm = T)
  p1 = p1 + scale_colour_discrete(guide = 'none') + scale_x_discrete(expand=c(0, 1))
  p1 = p1 + geom_dl(aes(label = Country), method = list(dl.trans(x = x + 0), "last.points", cex = 0.8))
  p1 = p1 + theme(axis.text.x = element_text(angle = -90, vjust = .5)) + xlab('Days after first death recorded') + ylab(dra.var)
  p1 = p1 + scale_y_log10()
  p2 = pheatmap(log2(dat+1), cluster_cols = F, silent = T)
  pdf(name1, w = 3+.1*ncol(dat), h = 2+.1*nrow(dat)); print(p1); dev.off()
  pdf(name2, w = 3+.1*ncol(dat), h = 2+.1*nrow(dat)); print(p2); dev.off()
  return(list(p1,p2))
}

getResults = function(){
  library(readr)
  library(reshape)
  library(pheatmap)
  library(ggplot2)
  library(directlabels)
  
  con = read.csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"))
  dea = read.csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv"))
  rec = read.csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv"))
  dat = list(con,dea,rec)
  dat = lapply(dat, function(x){x = setNames(x, gsub('^X','Date.0',colnames(x)))})
  countries = sort(unique(dat[[1]]$Country.Region))
  que = countries
  dat = lapply(dat, function(x){x = x[x$Country.Region %in% que,]})
  
  # dra.var = 'Cases'; use.dat = dat[[1]]
  dra.var = 'Deaths'; use.dat = dat[[2]]

  use.dat[,'Country.Region'] = as.factor(gsub('Turkey','TURKEY ***',use.dat[,'Country.Region']))
     
  non.empty = which(rowSums(use.dat[,-c(1:4)],na.rm = T)>0)
  y = aggregate(use.dat[non.empty,][, -c(1:4)], by=use.dat[non.empty,]["Country.Region"], FUN=sum)
  y = data.frame(y[,-1], row.names = y[,1])
  z = setNames(data.frame(t(apply(y,1,function(x){c(x[which(x>0)], array(NA,length(x)-sum(x>0,na.rm=T)))}))), seq_len(ncol(y)))
  
  # Correlation tests
  ref = 'TURKEY ***'
  cor.ref = setNames(data.frame(matrix(NA,nrow(z),3), row.names = rownames(z)), c('R','p','FDR'))
  for (i in seq_len(nrow(z))){
    if (sum(!is.na(z[i,]))>2){ #sum(!is.na(z[ref,]))
      # z[c(i,ref),]      
      r = tryCatch({cor = cor.test(as.numeric(z[ref,]),as.numeric(z[i,]), method = 'pearson', use = 'pairwise.complete')}, error = function(e) e, finally = {})
      cor.ref$R[i] = cor$estimate
      cor.ref$p[i] = cor$p.value
    }
  }
  cor.ref$FDR = p.adjust(cor.ref$p, method = 'BH')
  cor.ref = cor.ref[order(cor.ref$FDR),]
  
  # Draw
  res.all = drawImages(z,dra.var,
                   paste('All.countries.from.case1.line.',gsub('-','\\.',Sys.Date()),'.pdf',sep=''),
                   paste('All.countries.from.case1.heat.',gsub('-','\\.',Sys.Date()),'.pdf',sep=''))
  res = drawImages(z[rownames(cor.ref)[which(cor.ref$FDR<0.05)],],dra.var,
                   paste('Turkey.cor.fdr005.countries.from.case1.line.',gsub('-','\\.',Sys.Date()),'.pdf',sep=''),
                   paste('Turkey.cor.fdr005.countries.from.case1.heat.',gsub('-','\\.',Sys.Date()),'.pdf',sep=''))
  return(res)
}