# # test for simts support on new processes 

# When you add a new process you have to:

# - define a generating data function in gen_process.cpp
# - define a function for its definition in ts.model.R
# - add its inclusion in function gen_gts_cpp in file gen_process.cpp
# - add its inclusion in gen_lts_cpp in file gen_process.cpp



###########################
# SIN sinusoidal processes
###########################


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







###########################
# FGN Fractional gaussian noise
###########################




# # # 
# # test = FGN(sigma2 = 1, H = .9999)
# # test
# # # 
# # # # generate sin data
# # test = gen_fgn(N = 100000, sigma2 = 1, H = .9999)
# # test[2]
# # # 
# # model_i = FGN(sigma2 = 1, H = .9999)
# # # 
# # # # support in gen_model
# # Xt = gen_model(N = 1000, theta = model_i$theta, desc = model_i$desc, objdesc = model_i$obj.desc)
# # plot(wv::wvar(Xt))
# # # Xt[2]
# # # 
# # # 
# # # # support in gen_gts
# # Xt = gen_gts(n = 10000, model = FGN(sigma2 = 1, H = .9999))
# # plot(wv::wvar(Xt))
# # 
# # # 
# # # # support in gen_lts
# Xt = gen_lts(n = 100, model =  FGN(sigma2 = 1, H = .5) + AR1(phi = .2, sigma2 = 5) )
# plot(Xt)
# # 
# # plot(wv::wvar(Xt))
# # plot(Xt)
# # 
# # plot(Xt[,1], type ="l")
# # # 
# # # 
# # # Xt[5,1]
# # # 
# # # 
# # # 
# # # plot(wv::wvar(Xt))
# # # 
# # # 
# # # 
# # # 
# # # 
# # # 
# # # plot(Xt)
# # # 
# # # 
# # # # support in gen_gts
# # # Xt = gen_gts(n = 100000, model = SIN(alpha2 = 9e-04, beta = .06, U = 0) + RW(9e-10))
# # # plot(wv::wvar(Xt))
# # # 
# # # # support in gen_lts
# # # Xt = gen_lts(n = 100, model = SIN(alpha2 = 9e-06, beta = .06) + RW(9e-10))
# # # plot(Xt)


###########################
# PLP Power Law process
###########################




# set n
# n = 10000

# test = PLP(sigma2 = 1, d = .4)
# # test
# # # 
# # # # generate sin data
# test = gen_powerlaw(N = n, sigma2 = 1, d = .4)
# # test[2]
# # # 
# model_i = PLP(sigma2 = 1, d = .4)
# # # 
# # # # support in gen_model
# Xt = gen_model(N = n, theta = model_i$theta, desc = model_i$desc, objdesc = model_i$obj.desc)
# plot(wv::wvar(Xt))
# # # Xt[2]
# # # 
# # # 
# # # # support in gen_gts
# Xt = gen_gts(n = n, model = model_i )
# plot(wv::wvar(Xt))
# # 
# # # 
# # # # support in gen_lts
# Xt = gen_lts(n = 100, model =  model_i+ AR1(phi = .2, sigma2 = 5) )
# plot(Xt)
# # 
# # plot(wv::wvar(Xt))
# # plot(Xt)
# # 
# plot(Xt[,1], type ="l")
# # # 








###########################
# Matern process
###########################

# set n
n = 5000
# define a Matern process

test = MAT()
test

# generate sin data
test = gen_matern(N = n)
test[2]

model_i =MAT()

# support in gen_model
Xt = gen_model(N = 100, theta = model_i$theta, desc = model_i$desc, objdesc = model_i$obj.desc)
Xt[2]


# support in gen_gts
Xt = gen_gts(n = 100, model = model_i)
plot(Xt)

# support in gen_lts
Xt = gen_lts(n = 100, model = model_i + RW(3) )
plot(Xt)


# support in gen_gts
Xt = gen_gts(n = 100000, model = SIN(alpha2 = 9e-04, beta = .06, U = 0) + RW(9e-10))
plot(wv::wvar(Xt))

# support in gen_lts
Xt = gen_lts(n = 100, model = model_i+ RW(9e-10))
plot(Xt)




