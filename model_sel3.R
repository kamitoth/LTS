model.sel <- function(dat=df_long, unique_t=unique_time, unique_r=unique_run, unique_u=unique_unit_no){
  # MODEL Selection step 1
  if (length(unique_u) > 2L) {
    # for u_hom calculation
    formi <- res ~ (1 | unit_no)
    if (length(unique_t) > 2L) {
      # add time if makes sense for u_stab calculation
      formi <- update.formula(formi, .~. + time)
    }
    # add run if exist
    if ("run" %in% colnames(dat)){
      # Use NESTED model if design is fully nested
      M <- table(dat$run, dat$unit_no)
      if (all(colSums(M > 0L) == 1L)) {
        # use run as fixed effect if it has less than 6 levels
        if (length(unique_r) < 6){
          #formi.alt <- update.formula(formi, .~. + run) # for comparison # same as crossed! could be simplified?
          formi <- update.formula(formi, .~. + run + (1|run:unit_no) - (1|unit_no))
          #COMPARISON, new winner?
          # add anal if exist
          if ("anal" %in% colnames(dat)) {
            formi <- update.formula(formi, .~. + run:anal)
            #significance check!
          }
        } else {
          # use run as random effect if it has more than 6 levels
          #formi.alt <- formi # for comparison, drop run if SD close to 0
          formi <- update.formula(formi, .~. + (1|run) + (1|run:unit_no) - (1|unit_no)) 
          # add anal if exist
          if ("anal" %in% colnames(dat)) {
            formi <- update.formula(formi, .~. + (anal|run) - (1|run))
            #significance check!
            #then formi is with (anal|run) if significant or (1|run/unit)
            #sign...
            #formi.alt <- update.formula(formi, .~. - (1|run:unit) + (1|unit))
            # not sign...
            #formi <- update.formula(formi, .~. - (anal|run) + (1|run)) #i.e. reverse!
            #formi.alt <- update.formula(formi, .~. - (1|run:unit) + (1|unit))
            #or 
          }
        }
      }
      # Use CROSSED model if design is not fully nested
      else {
        if (length(unique_r) < 6){
          formi <- update.formula(formi, .~. + run)
          # add anal if exist
          if ("anal" %in% colnames(dat)) {
            formi <- update.formula(formi, .~. + run:anal)
            #significance check!
          }
        } else {
          # use run as random effect if it has more than 6 levels
          formi <- update.formula(formi, .~. + (1|run)) 
          # add anal if exist
          if ("anal" %in% colnames(dat)) {
            formi <- update.formula(formi, .~. + (anal|run) - (1|run))
          }
        }
      }
    }
    else if ("anal" %in% colnames(dat) & !"run" %in% colnames(dat)){
      formi <- update.formula(formi, .~. + anal)
    }
  } 
  # if single unit was tested...
  else {
    if (length(unique_t) > 2L) {
      # add time if makes sense for u_stab calculation
      formi <- res ~ time
    }
    # add run if exist
    if ("run" %in% colnames(dat)){
        # use run as fixed effect if it has less than 6 levels
        if (length(unique_r) < 6){
          formi <- update.formula(formi, .~. + run)
          # add anal if exist
          if ("anal" %in% colnames(dat)) {
            formi <- update.formula(formi, .~. + run:anal)
          }
        } else {
          # use run as random effect if it has more than 6 levels
          #formi.alt <- formi # for comparison, drop run if SD close to 0
          formi <- update.formula(formi, .~. + (1|run)) 
          # add anal if exist
          if ("anal" %in% colnames(dat)) {
            formi <- update.formula(formi, .~. + (anal|run) - (1|run))
          }
        }
    }
    else if ("anal" %in% colnames(dat) & !"run" %in% colnames(dat)){
      formi <- update.formula(formi, .~. + anal) # or correcting for anal trend?
    }
  }
  #uc <- ifelse(grepl("|", deparse(formi)), lmer(formi, dat), lm(formi, dat))
  #uc <- lmer(formi, dat)
  if (grepl("|", deparse(formi))) {
    uc <- lmer(formi, dat)
  } else {
    uc <- lm(formi, dat)
  }
  #uc
}