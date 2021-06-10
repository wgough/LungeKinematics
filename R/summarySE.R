summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else length(x)
  }
  datac <- plyr::ddply(data, groupvars, .drop=.drop,
                       .fun = function(xx, col) {
                         c(N = length2(xx[[col]], na.rm=na.rm),
                           mean = mean(xx[[col]], na.rm=na.rm),
                           median = median(xx[[col]], na.rm=na.rm),
                           sd = sd(xx[[col]], na.rm=na.rm),
                           iqr = IQR(xx[[col]], na.rm=na.rm),
                           var = var(xx[[col]], na.rm=na.rm),
                           max = max(xx[[col]], na.rm=na.rm),
                           min = min(xx[[col]], na.rm=na.rm)
                         )
                       },
                       measurevar
  )

  datac <- plyr::rename(datac, c("mean" = paste(measurevar, "_mean", sep = "")))
  datac <- plyr::rename(datac, c("median" = paste(measurevar, "_median", sep = "")))

  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean

  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult

  return(datac)
}
