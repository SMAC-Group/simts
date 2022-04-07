# # test for SIN support SIMTS
library(simts)
# # define a SIN process
# 
test = FGN(sigma2 = 1, H = .9999)
test
# 
# # generate sin data
test = gen_fgn(N = 100000, sigma2 = 1, H = .9999)
test[2]
# 
model_i = FGN(sigma2 = 1, H = .9999)
# 
# # support in gen_model
Xt = gen_model(N = 1000, theta = model_i$theta, desc = model_i$desc, objdesc = model_i$obj.desc)
plot(wv::wvar(Xt))
# Xt[2]
# 
# 
# # support in gen_gts
Xt = gen_gts(n = 10000, model = FGN(sigma2 = 1, H = .9999))
plot(wv::wvar(Xt))

# 
# # support in gen_lts
Xt = gen_gts(n = 100000, model =  FGN(sigma2 = 1, H = .5) + AR1(phi = .2, sigma2 = 5) )
plot(wv::wvar(Xt))


# plot(Xt[,1], type ="l")
# 
# 
# Xt[5,1]
# 
# 
# 
# plot(wv::wvar(Xt))
# 
# 
# 
# 
# 
# 
# plot(Xt)
# 
# 
# # support in gen_gts
# Xt = gen_gts(n = 100000, model = SIN(alpha2 = 9e-04, beta = .06, U = 0) + RW(9e-10))
# plot(wv::wvar(Xt))
# 
# # support in gen_lts
# Xt = gen_lts(n = 100, model = SIN(alpha2 = 9e-06, beta = .06) + RW(9e-10))
# plot(Xt)
