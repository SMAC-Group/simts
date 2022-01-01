# load pkg
library(rhub)
library(revdepcheck)

# set emails 
validate_email()

# print validated emails
rhub::list_validated_emails()

# list platforms on rhub
platforms()

# getpath and querry last checks
mypath = getwd()
previous_checks <- rhub::list_package_checks(mypath,
                                             email="lionelvoirol@hotmail.com",
                                             howmany = 20)
previous_checks

# get last check
rhub::last_check()

# document manual
devtools::document()

# check for cran
check = rhub::check_for_cran(email = "lionelvoirol@hotmail.com")

# check with specific compilation container
check = rhub::check(platform = "linux-x86_64-rocker-gcc-san", email = "lionelvoirol@hotmail.com")

# reverse dependency
red_rev_dep_check = revdep_check(num_workers = 4)
revdepcheck::revdep_report()
revdepcheck::revdep_report_summary()

# reverse dependency cran
revdep_report_cran()
