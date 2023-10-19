library(openxlsx)
library(fitdistrplus)
library(evd)
# download data
r.file =
  "https://d32ogoqmya1dw8.cloudfront.net/files/hydromodules/steps/annual_peak_streamflow_data.xlsx"
# read a xlsx file as r.data
r.data = 
  read.xlsx(xlsxFile = r.file,sheet = "USGS 03335500")
# look into data: r.data, Time, Streamflow
head(r.data)
head(r.data$Time)
head(r.data$`Annual.Peak.Streamflow.(cfs)`)
# change unit from cfs to cms (conversion factor: 0.02832)
# r.data.cfs
r.data.cfs <- r.data$`Annual.Peak.Streamflow.(cfs)`
r.data.cms <-  r.data.cfs * 0.02832
str(r.data.cms)
# plot distribution using plotdist
plotdist(r.data.cms, histo = TRUE, demp = TRUE,breaks = 8) 
summary(r.data.cms)
# fitdist data
# weibull distribution
fw <- fitdist(r.data.cms, "weibull")
summary(fw)

plot(density(r.data.cms))
lines(dweibull(x=seq(0,3000), shape = 3.401504, 
               scale = 1653.576910, log = FALSE),col="red")
# gamma distribution
fg <- fitdist(r.data.cms, "gamma")
summary(fg)

# lnorm distribution
fln <- fitdist(r.data.cms, "lnorm")
summary(fln)

# multiple plots
# par(mfrow = c(2, 2),oma=c(0,1,1,1))
# plot.legend <- c("Weibull", "lognormal", "gamma")
# denscomp(list(fw, fln, fg), legendtext = plot.legend)
# qqcomp(list(fw, fln, fg), legendtext = plot.legend)
# cdfcomp(list(fw, fln, fg), legendtext = plot.legend)
# ppcomp(list(fw, fln, fg), legendtext = plot.legend)
# mtext("created by seongjin noh", 
#       cex=1, col="blue", outer=TRUE)

par(mfrow = c(2, 2),oma=c(0,1,1,1))
plot.legend <- c("Weibull", "lognormal", "gamma")

denscomp(list(fw, fln, fg), legendtext = plot.legend)
qqcomp(list(fw, fln, fg), legendtext = plot.legend)
cdfcomp(list(fw, fln, fg), legendtext = plot.legend)
ppcomp(list(fw, fln, fg), legendtext = plot.legend)
mtext("created by seongjin noh", 
      cex=1, col="blue", outer=TRUE)

# goodness of statistics: gofstat
# gofstat(list(fw,fln,fg),fitnames =
#           c("weibull", "lognormal","gamma"))
gofstat(list(fw,fln,fg),fitnames =
          c("weibull", "lognormal","gamma"))