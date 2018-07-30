library(dplyr)
library(stringr)
source('globals.R')

### pure curve optimization
methods_list <- list("discount08", 
                     "discount085",
                     "discount09") 
                     #"discount065")#, 
                     #"discount07",
                     #"discount075",
                     #"discount08")
mcc <- 8

optim_results <- lapply(methods_list, FUN = function(mth){
  rough_opt <- optim_curve(evalturn = 20, max_cc = mcc, method = mth, 
                          max_iter = 1000, iterperdeck = 100, decksize = 40)
  r_opt_c <- round(colMeans(rough_opt))
  sign_opt_c <- sign_optim_curve(evalturn = 20, max_cc = mcc, method = mth, 
                               max_iter = 2000, inititer = 100, decksize = 40,
                               max_samples = 100000,
                               initcurve = r_opt_c, conf = 0.0005)
  analyze_optim_results(sign_opt_c)
})
names(optim_results) <- methods_list

save(optim_results, file = "optimresults_08_085_09.Rdata")

### mana ramp optimization
methods_list <- list("discount07")
rampcounts <- c(1,2,3)
mcc <- 8

optim_results_withramp <- lapply(rampcounts, FUN = function(rampcount){
  optim_results <- lapply(methods_list, FUN = function(mth){
    rough_opt <- optim_curve(evalturn = 20, max_cc = mcc, ramp = rampcount, method = mth, 
                             max_iter = 1000, iterperdeck = 100, decksize = 40)
    r_opt_c <- round(colMeans(rough_opt))
    sign_opt_c <- sign_optim_curve(evalturn = 20, max_cc = mcc, ramp = rampcount, method = mth, 
                                   max_iter = 2000, inititer = 100, decksize = 40,
                                   max_samples = 100000,
                                   initcurve = r_opt_c, conf = 0.0005)
    analyze_optim_results(sign_opt_c)
  })
  names(optim_results) <- methods_list
  optim_results
})
names(optim_results_withramp) <- rampcounts

save(rampcounts, optim_results_withramp, file = "optimresults_ramp_07.Rdata")

### card draw optimization
methods_list <- list("discount08")
cdraws <- c(1,2,3)
mcc <- 8

optim_results_withdraw <- lapply(cdraws, FUN = function(drawcount){
  optim_results <- lapply(methods_list, FUN = function(mth){
    rough_opt <- optim_curve(evalturn = 20, max_cc = mcc, carddraw = drawcount, method = mth, 
                             max_iter = 1000, iterperdeck = 100, decksize = 40)
    r_opt_c <- round(colMeans(rough_opt))
    sign_opt_c <- sign_optim_curve(evalturn = 20, max_cc = mcc, carddraw = drawcount, method = mth, 
                                   max_iter = 2000, inititer = 100, decksize = 40,
                                   max_samples = 100000,
                                   initcurve = r_opt_c, conf = 0.0005)
    analyze_optim_results(sign_opt_c)
  })
  names(optim_results) <- methods_list
  optim_results
})
names(optim_results_withdraw) <- cdraws

save(cdraws, optim_results_withdraw, file = "optimresults_cdraw_08.Rdata")


