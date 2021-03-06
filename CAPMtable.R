# table.SFM(na.omit(RawData.r[, 1:2]), 
#           na.omit(RawData.r[,7]), 
#           scale = 54,
#           Rf = 0,
#           digits = 4)
library(PerformanceAnalytics)

RawData.r$Rf <- c(NA, rep(0,334))

Ra <- checkData(RawData.r[, 1:6])
Rf <- checkData(RawData.r[,8, drop = F])
Rb <- checkData(RawData.r[,7, drop = F])

if(!is.null(dim(Rf)))
  Rf = checkData(Rf)

# Get dimensions and labels
columns.a = ncol(Ra)
columns.b = ncol(Rb)
columnnames.a = colnames(Ra)
columnnames.b = colnames(Rb)

Ra.excess = Return.excess(Ra, Rf)
Rb.excess = Return.excess(Rb, Rf)

scale = NA
digits = 4

if(is.na(scale)) {
  freq = periodicity(Ra)
  switch(freq$scale,
         minute = {stop("Data periodicity too high")},
         hourly = {stop("Data periodicity too high")},
         daily = {scale = 252},
         weekly = {scale = 52},
         monthly = {scale = 12},
         quarterly = {scale = 4},
         yearly = {scale = 1}
  )
}

for(column.a in 1:columns.a) { # for each asset passed in as R
  for(column.b in 1:columns.b) { # against each asset passed in as Rb
    merged.assets = merge(Ra.excess[,column.a,drop=FALSE], Rb.excess[,column.b,drop=FALSE])
    merged.assets = as.data.frame(na.omit(merged.assets)) # leaves the overlapping period
    # these should probably call CAPM.alpha and CAPM.beta for consistency (not performance)
    model.lm = lm(merged.assets[,1] ~ merged.assets[,2])
    alpha = coef(model.lm)[[1]]
    beta = coef(model.lm)[[2]]
    CAPMbull = CAPM.beta.bull(Ra[,column.a], Rb[,column.b],Rf) #inefficient, recalcs excess returns and intercept
    CAPMbear = CAPM.beta.bear(Ra[,column.a], Rb[,column.b],Rf) #inefficient, recalcs excess returns and intercept
    htest = cor.test(merged.assets[,1], merged.assets[,2])
    #active.premium = (Return.annualized(merged.assets[,1,drop=FALSE], scale = scale) - Return.annualized(merged.assets[,2,drop=FALSE], scale = scale))
    active.premium = ActivePremium(Ra=Ra[,column.a],Rb=Rb[,column.b], scale = scale)
    #tracking.error = sqrt(sum(merged.assets[,1] - merged.assets[,2])^2/(length(merged.assets[,1])-1)) * sqrt(scale)
    tracking.error = TrackingError(Ra[,column.a], Rb[,column.b],scale=scale)
    #treynor.ratio = Return.annualized(merged.assets[,1,drop=FALSE], scale = scale)/beta
    #treynor.ratio = TreynorRatio(Ra=Ra[,column.a], Rb=Rb[,column.b], Rf = Rf, scale = scale)
    
    z = c(
      alpha,
      beta,
      CAPMbull,
      CAPMbear,
      summary(model.lm)$r.squared,
      ((1+alpha)^scale - 1),
      htest$estimate,
      htest$p.value,
      tracking.error,
      active.premium,
      active.premium/tracking.error#,
      #treynor.ratio
    )
    
    znames = c(
      "Alpha",
      "Beta",
      "Beta+",
      "Beta-",
      "R-squared",
      "Annualized Alpha",
      "Correlation",
      "Correlation p-value",
      "Tracking Error",
      "Active Premium",
      "Information Ratio"#,
      #"Treynor Ratio"
    )
    
    if(column.a == 1 & column.b == 1) {
      result.df = data.frame(Value = z, row.names = znames)
      colnames(result.df) = paste(columnnames.a[column.a], columnnames.b[column.b], sep = " to ")
    }
    else {
      nextcolumn = data.frame(Value = z, row.names = znames)
      colnames(nextcolumn) = paste(columnnames.a[column.a], columnnames.b[column.b], sep = " to ")
      result.df = cbind(result.df, nextcolumn)
    }
  }
}

result.df = base::round(result.df, digits)

textplot(result.df, rmar = 0.8, cmar = 1.5,  max.cex=.9,
         halign = "center", valign = "top", row.valign="center",
         wrap.rownames=15, wrap.colnames=10, mar = c(0,0,3,0)+0.1)
title(main="Single Factor Model Related Statistics")


#TreynorRatio(Ra, Rb, Rf = Rf, scale = 54, modified = FALSE)
