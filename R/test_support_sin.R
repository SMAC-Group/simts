# # test for SIN support SIMTS
# 
# # define a SIN process
# 
# test = SIN(alpha2 = 7e-04, beta = .06, U = 0.5)
# test
# 
# # generate sin data
# test = gen_sin(N = 100000, alpha2 = 3e-06, beta = 1, U = 1.5)
# test[2]
# 
# model_i = SIN(alpha2 = 1, beta = .02, U = 1.79)
# 
# # support in gen_model
# Xt = gen_model(N = 100, theta = model_i$theta, desc = model_i$desc, objdesc = model_i$obj.desc)
# Xt[2]
# 
# 
# # support in gen_gts
# Xt = gen_gts(n = 100, model = SIN(alpha2 = 30, beta = .1, U = 4))
# plot(Xt)
# 
# # support in gen_lts
# Xt = gen_lts(n = 100, model = SIN(alpha2 = 4, beta = 1, U = 3) + RW(3) )
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
