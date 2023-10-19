options(digits = 5)
Re = 500000
e_D = 0.025
# Re = 10000
# e_D = 0.001

# Halland equation

f_halland =  (1/-(1.8*log10((e_D/3.7)^1.11 + 6.9/(Re))))^2
f_halland

# Find the friction factor f 
# by solving Swamee and Jain

f_swamee =  0.25/(log10(e_D/3.7 + 5.74/(Re)^0.9))^2
f_swamee

# Colebrook-White equation
f = seq(0.001,0.1,0.0001)
fn_colebrook = 1/(f^0.5) + (2*log10(e_D/3.7 + 2.51/(Re*(f)^0.5)))
plot(abs(fn_colebrook))
min(abs(fn_colebrook))
f_colebook = f[which.min(abs(fn_colebrook))]
f_colebook
