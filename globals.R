ramp_specs <- list(cost = 2)
cdraw_specs <- list(cost = 3, draw = 2)

decklist <- function(size = 40, curve = c(10,0,0), ramp = 0, carddraw = 0){
  list(size = size,
       landcount = size - sum(curve) - ramp - carddraw,
       maxcost = max(which(curve > 0)),
       curve = curve[seq(max(which(curve > 0)))],
       ramp = ramp,
       carddraw = carddraw
  )
}

shuffled_deck <- function(dl){
  stacked_deck <- c(rep(0,dl$landcount), 
                    rep(-1,dl$ramp),
                    rep(-2,dl$carddraw),
                    rep(seq_len(dl$maxcost), dl$curve))
  sample(stacked_deck, size = dl$size, replace = FALSE)
}
  
executed_curve <- function(sd, handsize = 7, t = 30, otp = TRUE){
  lib <- sd
  hs <- handsize - as.numeric(otp)
  rampsinhands <- drawsinhands <- landsinhand <- execurve <- numeric()
  spellsinhands <- list()
  
  ### draw starting hand
  hand <- list(landcount = sum(lib[seq(hs)] == 0),
               spells = lib[which(lib[seq(hs)] > 0)],
               ramp = sum(lib[seq(hs)] == -1),
               carddraw = sum(lib[seq(hs)] == -2)
               )
  lib <- lib[-seq(hs)]
  bflands <- 0
  
  ### define game actions
  drawacard <- function(){
    if(lib[1] == 0) hand$landcount <<- hand$landcount + 1
    if(lib[1] > 0) hand$spells <<- c(hand$spells, lib[1])
    if(lib[1] == -1) hand$ramp <<- hand$ramp + 1
    if(lib[1] == -2) hand$carddraw <<- hand$carddraw + 1
    lib <<- lib[-1]
  }
  playaland <- function(){
    if(hand$landcount > 0){
      hand$landcount <<- hand$landcount - 1
      bflands <<- bflands + 1
      land_drop <<- T
    }
  }
  playspells <- function(){
    repeat{
      if(length(hand$spells) > 0){
        if(min(hand$spells) <= mana){
          onlyone <- TRUE
          relspells <- hand$spells[hand$spells <= mana]
          cc_onespell <- max(relspells)
          onespell <- min(which(hand$spells == cc_onespell))
          if(cc_onespell < mana & length(relspells) > 1){
            twospells <- sapply(relspells, FUN = function(i){i + relspells}) + mana * diag(length(relspells))
            ts_index <- which(twospells <= mana)
            cc_twospell <- ifelse(length(ts_index) > 0, max(twospells[ts_index]), 0) 
            if(cc_twospell > cc_onespell){
              onlyone <- FALSE
              ts_index <- min(which(twospells == cc_twospell))
              ts_cmc1 <- relspells[1 + (ts_index-1) %% length(relspells)]
              ts_cmc2 <- relspells[1 + (ts_index-1) %/% length(relspells)]
              tsi1 <- min(which(hand$spells == ts_cmc1))
              hand$spells <<- hand$spells[setdiff(seq_len(length(hand$spells)), tsi1)]
              tsi2 <- min(which(hand$spells == ts_cmc2))
              hand$spells <<- hand$spells[setdiff(seq_len(length(hand$spells)), tsi2)]
              manaspent <<- manaspent + cc_twospell
              mana <<- mana - cc_twospell
              
            }
          }
          if(onlyone){
            manaspent <<- manaspent + cc_onespell
            mana <<- mana - cc_onespell
            hand$spells <<- hand$spells[setdiff(seq_len(length(hand$spells)), onespell)]
          }
        } else {
          break
        }
      } else {
        break
      }
    }
  }
  
  ### execute turns
  for(i in seq(t)){
    eot_ramp <- FALSE
    land_drop <- FALSE
    
    ### draw a card
    drawacard()
    
    ### for bookkeeping purposes:
    landsinhand <- c(landsinhand, hand$landcount)
    spellsinhands <- c(spellsinhands, list(hand$spells))
    rampsinhands <- c(rampsinhands, hand$ramp)
    drawsinhands <- c(drawsinhands, hand$carddraw)
      
    ### play a land
    playaland()
    
    ### available mana this turn
    mana <- bflands
    manaspent <- 0
    
    ### play mana ramp
    if(hand$ramp > 0 & mana >= ramp_specs$cost){
      if(length(hand$spells) == 0 | max(hand$spells) > mana | 
           mana >= ramp_specs$cost + sum(hand$spells)){
        hand$ramp <- hand$ramp - 1
        mana <- mana - ramp_specs$cost
        eot_ramp <- TRUE
      }
    }
    
    ### play action spells
    playspells()
    
    ### play card draw
    if(hand$carddraw > 0 & mana >= cdraw_specs$cost){
      for(i in seq(cdraw_specs$draw)){drawacard()}
      hand$carddraw <- hand$carddraw - 1
      mana <- mana - cdraw_specs$cost
    }
    
    ### again play lands and action spells after carddraw
    if(!land_drop) playaland() 
    playspells()
    
    if(eot_ramp){bflands <- bflands + 1}
    execurve <- c(execurve, manaspent)
  }
  list(bfl = bflands, lih= landsinhand, sih = spellsinhands, rih = rampsinhands, dih = drawsinhands, xcurve = execurve)
}

curvesample <- function(decklist, iter = 1000, turns = 20, ontheplay = TRUE){
  sapply(seq_len(iter), FUN = function(i){
    sd <- shuffled_deck(decklist)
    mulligan <- 0
    # mulligan rule
    if(sum(sd[1:7] == 0) %in% c(0,1,6,7)){
      sd <- shuffled_deck(decklist)
      mulligan <- 1
      if(sum(sd[1:6] == 0) %in% c(0,1,5,6)){
        sd <- shuffled_deck(decklist)
        mulligan <- 2
      }
    }
    # insert mulligan rule here
    exc <- executed_curve(sd, handsize = 7 - mulligan, t = turns, otp = ontheplay)
    exc$xcurve[seq_len(turns)]
  }) %>% rbind
} 

mean_xcurve <- function(decklist, iter = 1000, turns = 20, ontheplay = TRUE){
  curvesample(decklist = decklist, iter = iter, turns = turns, ontheplay = ontheplay) %>% 
    rowMeans
}

curve_weights <- function(turns = 20, method = "1t5"){
  if(method == "1t5") return(c(rep(1,5), rep(0, turns - 5)))
  if(method == "1t7") return(c(rep(1,7), rep(0, turns - 7)))
  if(method == "6t10")  return(c(rep(0,5), rep(1,5), rep(0, turns - 10)))
  if(method == "fullmana")  return(1/seq_len(turns))
  if(method == "discount05") return(0.5^(0:(turns-1)))
  if(method == "discount055") return(0.55^(0:(turns-1)))
  if(method == "discount06") return(0.6^(0:(turns-1)))
  if(method == "discount065") return(0.65^(0:(turns-1)))
  if(method == "discount07") return(0.7^(0:(turns-1)))
  if(method == "discount075") return(0.75^(0:(turns-1)))
  if(method == "discount08") return(0.8^(0:(turns-1)))
  if(method == "discount085") return(0.85^(0:(turns-1)))
  if(method == "discount09") return(0.9^(0:(turns-1)))
}

eval_xcurve <- function(decklist, iter = 1000, turns = 20, ontheplay = TRUE, method = "discount075"){
  weights <- curve_weights(turns = turns, method = method)
  cs <- curvesample(decklist = decklist, iter = iter, turns = turns, ontheplay = ontheplay)
  t(cs) %*% weights
}

eval_csample <- function(csample, method = "discount"){
  weights <- curve_weights(turns = dim(csample)[1], method = method)
  t(csample) %*% weights
}

optim_curve <- function(evalturn = 20, max_cc = 3, ramp = 0, carddraw = 0,
                        method = "fullmana", 
                        max_iter = 200, iterperdeck = 100, decksize = 40,
                        initcurve = NULL, proposal = "fast"){
  if(is.null(initcurve)){
    step_curve <- rep(floor((decksize - ramp - carddraw)/(2 * max_cc)), max_cc)
  } else {
    step_curve <- initcurve
  }
  dl_list <- step_curve
  for(i in 2:max_iter){
    repeat{
      if(proposal == "slow"){
        can_curve <- step_curve + sample(c(-1,0,1), size = max_cc, 
                                         replace = TRUE, prob = c(0.25, 0.5, 0.25)) 
      } else {
        can_curve <- step_curve + sample(c(-1,0,1), size = max_cc, replace = TRUE) 
      }
      if(min(can_curve) >= 0 & sum(can_curve) <= decksize - ramp - carddraw){break}
    }
    step_dl <- decklist(size = decksize, curve = step_curve, ramp = ramp, carddraw = carddraw)
    step_eval <- mean(eval_xcurve(step_dl, iter = iterperdeck, turns = evalturn, method = method))
    can_dl <- decklist(size = decksize, curve = can_curve, ramp = ramp, carddraw = carddraw)
    can_eval <- mean(eval_xcurve(can_dl, iter = iterperdeck, turns = evalturn, method = method))
    if(can_eval > step_eval){
      step_curve <- can_curve
    }
    print(c(i, step_curve, step_eval))
    dl_list <- rbind(dl_list, step_curve)
  }
  dl_list
}

sign_optim_curve <- function(evalturn = 20, max_cc = 3, ramp = 0, carddraw = 0,
                             method = "fullmana", 
                             max_iter = 200, inititer = 100, conf = 0.001,
                             max_samples = 100000,
                             decksize = 40, initcurve = NULL, proposal = "fast"){
  if(is.null(initcurve)){
    step_curve <- rep(floor((decksize - ramp - carddraw)/(2 * max_cc)), max_cc)
  } else {
    step_curve <- initcurve
  }
  dl_list <- step_curve
  curve_results <- list()
 
  step_hash <- paste0(step_curve, collapse = "#")
  step_dl <- decklist(size = decksize, curve = step_curve, ramp = ramp, carddraw = carddraw)
  step_eval <- eval_xcurve(step_dl, iter = inititer, turns = evalturn, method = method)
  curve_results[[step_hash]] <- step_eval
  
  for(i in 2:max_iter){
    ### sample new candidate
    repeat{
      if(proposal == "slow"){
        can_curve <- step_curve + sample(c(-1,0,1), size = max_cc, 
                                         replace = TRUE, prob = c(0.25, 0.5, 0.25)) 
      } else {
        can_curve <- step_curve + sample(c(-1,0,1), size = max_cc, replace = TRUE) 
      }
      if(min(can_curve) >= 0 & sum(can_curve) <= decksize -  ramp - carddraw &
         sum((can_curve - step_curve)^2) > 0){break}
    }
    print(can_curve)
    ### look for existing results of candidate or create otherwise
    can_dl <- decklist(size = decksize, curve = can_curve, ramp = ramp, carddraw = carddraw)
    can_hash <- paste0(can_curve, collapse = "#")
    if(!(paste0(can_curve, collapse = "#") %in% names(curve_results))){
      curve_results[[can_hash]] <- eval_xcurve(can_dl, iter = inititer, turns = evalturn, method = method)
    }
    
    ### test the deck performance against each other
    repeat{
      tt <- t.test(curve_results[[step_hash]], curve_results[[can_hash]])
      print(tt$p.value)
      if(tt$p.value < conf | length(curve_results[[can_hash]]) > max_samples){break}
      ### increase sample size
      step_eval <- curve_results[[step_hash]]
      can_eval <- curve_results[[can_hash]]
      if(length(can_eval) == length(step_eval)){
        step_dl <- decklist(size = decksize, curve = step_curve, ramp = ramp, carddraw = carddraw)
        new_step_eval <- eval_xcurve(step_dl, iter = length(step_eval), turns = evalturn, method = method)
        curve_results[[step_hash]] <- c(curve_results[[step_hash]], new_step_eval)
      }
      new_can_eval <- eval_xcurve(can_dl, iter = length(can_eval), turns = evalturn, method = method)
      curve_results[[can_hash]] <- c(curve_results[[can_hash]], new_can_eval)
    }
    if(mean(curve_results[[can_hash]]) > mean(curve_results[[step_hash]])){
      step_curve <- can_curve
      step_hash <- can_hash
    }
    print(c(i, step_curve, mean(step_eval), length(curve_results[[step_hash]])))
    dl_list <- rbind(dl_list, step_curve)
  }
  curve_results
}

analyze_optim_results <- function(curve_results){
  means <- lapply(curve_results, FUN = function(x)mean(x)) %>% unlist
  best_curve <- names(which(means == max(means)))
  pvals <- lapply(curve_results, FUN = function(x){
             t.test(x, curve_results[[best_curve]])$p.value
           }) %>% unlist
  data.frame("curves" = names(curve_results),
             "means" = means,
             "pvals" = pvals) %>% 
    arrange(desc(means))
}


identify_mostfrequent <- function(dllist){
  hashes <- sapply(seq_len(dim(dllist)[1]), FUN = function(i){
    paste0(dllist[i,], collapse = "#")
  })
  mf_hash <- as.data.frame(list("ids" = hashes)) %>% 
    group_by(ids) %>% 
    summarise(N = n()) %>% 
    arrange(desc(N)) %>% 
    .[[1]] %>% 
    head(n = 1) %>% 
    as.character()
  as.numeric(str_split(mf_hash, pattern = "#")[[1]]) 
}

