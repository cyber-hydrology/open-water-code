# hydraulic 1 at civil eng kit by Seongjin Noh
# calculation and visualization of Moody diagram

# relative roughness
e_D = 0.001

# Reynolds number
Re <- seq(3.6,8,0.1)
Re_moody <- 10^(Re)

# array for friction factor values
f_moody_1 = rep(0.01,length(Re_moody))

# estimating Colebrook equation
f = seq(0.001,0.1,0.00001)
for(i in 1:length(Re_moody)){
  fn_moody_colebrook = 1/(f^0.5) + 
    (2*log10(e_D/3.7 + 2.51/(Re_moody[i]*(f)^0.5)))
  min(abs(fn_moody_colebrook))
  f_moody_1[i] = f[which.min(abs(fn_moody_colebrook))]
}

e_D = 0.01
f_moody_2 = rep(0.01,length(Re_moody))
for(i in 1:length(Re_moody)){
  fn_moody_colebrook = 1/(f^0.5) + 
    (2*log10(e_D/3.7 + 2.51/(Re_moody[i]*(f)^0.5)))
  min(abs(fn_moody_colebrook))
  f_moody_2[i] = f[which.min(abs(fn_moody_colebrook))]
}

Re_laminar = 10^(seq(1,3.3,0.1))
f_laminar = 64/Re_laminar

plot(Re_moody,f_moody_1,log="xy",type ="l",
     xlim = c(6*10^2,10^8),
     ylim = c(0.01,0.1),
     xlab = "Re",ylab = "f",
     main = "Moody diagram")
lines(Re_moody,f_moody_2,col="blue")
lines(Re_laminar,f_laminar,col="red")

legend(x = "topright", lty = c(1,1,1), 
       text.font = 2, 
       col= c("blue","black","red"),
       text.col = c("blue","black","red"), 
       legend=c("e/D=0.01", "e/D=0.001","laminar"))
