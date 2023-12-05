library(ggplot2)

# For saving and loading Stan files
stan_path <- function(x, stan_model_suffix = default_stan_model_to_use) 
  paste0("analysis_output/stan/", x, "_", stan_model_suffix, ".rds")



default_theme <- theme(panel.background = element_rect(fill = NA),
                       panel.border = element_rect(fill = NA, color = "black"),
                       panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(),
                       axis.ticks = element_line(color = "gray5"),
                       axis.text = element_text(color = "black", size = 10),
                       axis.title = element_text(color = "black", size = 10,
                                                 family = "Times",
                                                 face="italic"),
                       plot.title = element_text(family = "Times",
                                                 face="italic", 
                                                 colour="black",
                                                 size=14,
                                                 hjust = 0.5)
)

interval2 <- function(a,b,d=2) gsub(",",", ",gsub(" ","",paste0("(", acr(a,d), ",", acr(b,d), ")")))
acr <- function(x, d=2) format(round(x,d), nsmall = d)

# oeb = (get) overall estimate (from) bayes, this is done a few times in the scripts
oeb <- function(bg, mlu = FALSE){
  if(!inherits(bg, "baggr")){
    res <- rep(NA, 3)
  } else {
    res <- treatment_effect(bg, trans = exp, s=T)$tau[1:3]  
    #just to follow the same convention everywhere
  }
  res <- setNames(res, c("lower", "mean", "upper"))
  
  # Ordering to match the one in oef()!
  if(mlu)
    res <- res[c("mean", "lower", "upper")]
  
  res
}

# oef = overall estimate from frequentist model
oef <- function(rma, pval = TRUE, ci = FALSE){
  if(!inherits(rma, "rma")){
    res <- rep(NA, ifelse(pval, 4, 3))
  }else {
    res <- exp(c("mean" = rma$b, "lower" = rma$ci.lb, "upper" = rma$ci.ub))
    if(pval)
      res <- c(res, "pval" = rma$pval)
  }
  # if(ci) {
  # res <- c(res, "ci" = interval2(res[["lower"]], res[["upper"]]))
  # }
  
  res
}

oef_ci <- function(res)
  interval2(res[["lower"]], res[["upper"]])


# "Manually" apply continuity correction to studies that have 0 total events
apply_cc <- function(df, cc=0.5){
  whichrows <- df$ccases + df$tcases == 0
  df %>%
    mutate(ccases    = ifelse(whichrows, ccases + cc, ccases),
           tcases    = ifelse(whichrows, tcases + cc, tcases),
           cnoncases = ifelse(whichrows, cnoncases + cc, cnoncases),
           tnoncases = ifelse(whichrows, tnoncases + cc, tnoncases))
}

# We will be running baggr a few times, so let's use a function to prep inputs
baggr_prep <- function(df, type = "bti"){
  x <- df %>%
    rename(group = trial_name, 
           a = tcases, c= ccases, b = tnoncases, d = cnoncases) %>%
    select(group, a, b, c, d) 
  
  if(type == "bti")
    return(binary_to_individual(x))
  if(type == "or")
    return(prepare_ma(x, effect = "logOR", correction_type = "all", rare = .5))
  if(type == "rd")
    return(prepare_ma(x, effect = "RD", correction_type = "all", rare = .5))
  if(type == "rr")
    return(prepare_ma(x, effect = "logRR", correction_type = "all", rare = .5))
}

# Prepare data to model
# Note that baggr() may refuse to calculate this due to non-integer values
or_calculator <- function(df, add = 0.5) {
  df %>%
    rename(group = trial, 
           a = tcases, c= ccases, b = tnoncases, d = cnoncases) %>%
    mutate(a = a + add, b = b + add, c = c + add, d = d + add) %>% 
    select(group, a, b, c, d) %>% 
    mutate(tau = log(a*d/(c*b)),
           se = sqrt(1/a + 1/b + 1/c + 1/d))
}

# Reduce sample sizes to effective s.s. by correcting for design effect (ICC)
adjust_for_icc <- function(df, default_icc = 0.01){
  df %>% 
    left_join(df_cluster, by = "trial") %>%
    mutate(design_effect = 1 + (cluster_size - 1)*default_icc) %>%
    mutate(design_effect = ifelse(is.na(design_effect), 1, design_effect)) %>%
    mutate(ccases = ccases/design_effect,
           tcases = tcases/design_effect,
           cnoncases = cnoncases/design_effect,
           tnoncases = tnoncases/design_effect) 
}


# Bubble plots for meta-regression models
bubble <- function(bg, covariate, pred=TRUE, label=TRUE) {
  if(!inherits(bg, "baggr"))
    stop("bg must be a baggr object")
  if(is.null(bg$covariates))
    stop("bg must include covariates")
  if(!(covariate %in% bg$covariate))
    stop("requested covariate not specified in the baggr object")
  
  data <- bg$data
  data$dotsize <- 1/(data$se^2)
  data$mean <- as.data.frame(group_effects(bg, summary = TRUE)[,,1])$mean
  group <- NULL
  data$.covariate <- data[[covariate]]
  fe <- fixed_effects(bg, summary=TRUE)[,"mean",1]
  te <- treatment_effect(bg,summary=TRUE)$tau[["mean"]]
  data$group <- data$trial_name
  
  ggplot2::ggplot(data, aes(x = .covariate,y=mean)) +
    ggplot2::geom_point(aes(size=dotsize)) +
    ggplot2::guides(size = "none") +
    ggplot2::xlab(covariate) + ggplot2::ylab(bg$effects) +
    # {if(pred) ggplot2::geom_smooth(method="lm", level = interval, col="black")} +
    {if(pred) ggplot2::geom_abline(slope = fe, intercept = te, lty = "dashed")} +
    {if(label) ggrepel::geom_text_repel(aes(x = .covariate,
                                            y=mean,
                                            label=group),
                                        box.padding=1)}
  
}
