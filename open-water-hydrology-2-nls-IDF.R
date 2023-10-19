# 50-yr frequency rainfall at the Gimcheon area
# define data: rainfall intensity and duration 
intensity <-
  c(213.3, 154.5, 129.8, 113.3, 91.5, 
    77.5,  67.8,  60.7,  47.3,  39.7, 
    31.2,  26.5,  21.3,  17.1,  14.6, 
    12.9,  11.5,  9.6,   5.8,   4.2)
duration <-
  c(5,10,15,20,30,
    40,50,60,90,120,
    180,240,360,540,720,
    900,1080,1440,2880,4320)

# draw a plot for duration-intensity relation
plot(duration,intensity,
     xlab = "Duration (min)",
     ylab = "Rain intensity (mm/hr)",
     main = "IDF curve")

# Define Talbot-type IDF curve
a = 6600
b = 37
intensity.talbot = a/(duration + b)

# add a Talbot curve with non-optimal parameters
lines(duration,intensity.talbot, col = "red")

# nonlinear regression to find optimal parameters
nls_talbot=nls(intensity ~ a/(duration+b),start=list(a=6600,b=37)) 

# draw a Talbot curve with optimal parameters
lines(duration,predict(nls_talbot),col="blue")

# check the regression result
nls_talbot

# nonlinear regression to find optimal parameters
nls_sherman=nls(intensity ~ a/(duration^n),start=list(a=600,n=0.9))

# draw a Sherman curve with optimal parameters
lines(duration,predict(nls_sherman),col="dark green")
# check the regression result
nls_sherman
