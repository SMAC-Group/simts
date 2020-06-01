# test for SIN support SIMTS

# define a SIN process

test = SIN(alpha2 = 7e-04, beta = .06)
test

# generate sin data
gen_sin(N = 100000, alpha2 = 3e-04, beta = .05)

# support in gen_model
Xt = gen_model(N = 100000, theta = test$theta, desc = test$desc, objdesc = test$obj.desc)
plot(wv::wvar(Xt))

# support in gen_gts
Xt = gen_gts(n = 100000, model = SIN(alpha2 = 9e-04, beta = .06) + RW(9e-10))
plot(wv::wvar(Xt))
Xt

# support in gen_lts
Xt = gen_lts(n = 100000, model = SIN(alpha2 = 9e-04, beta = .06) + RW(9e-10))
plot(wv::wvar(Xt))
