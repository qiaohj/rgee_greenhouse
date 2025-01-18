
  start.time <- proc.time()
  ecospat.use <- TRUE
  occs <- as.data.frame(occs)
  algorithm = 'maxnet'
  tune.args = list(fc = c("L", "Q", "P", "LQ", "LP", "QP", "LQP"),
                   rm = c(0.01, 0.1, 0.5, c(1:10)))
  partition.settings = ps
  other.settings<-list()
  other.settings$removeduplicates = TRUE
  other.settings$abs.auc.diff <- TRUE
  other.settings$pred.type <- "cloglog"
  other.settings$validation.bg <- "full"
  other.settings <- c(other.settings, ecospat.use = ecospat.use)
  if (inherits(occs[, 1], "character") | inherits(bg[, 1], 
                                                  "character")) 
    stop("* If first column of input occurrence or background data is the taxon name, remove it and instead include the 'taxon.name' argument. The first two columns must be the longitude and latitude of the occurrence/background localities.")
  
  all.partitions <- c("jackknife", "randomkfold", "block", 
                      "checkerboard", "user", "testing", "none")
  partitions = 'randomkfold'
  overlap = TRUE
  doClamp = TRUE
  tune.args.num <- which((sapply(tune.args, class) %in% c("numeric", 
                                                          "integer")) & sapply(tune.args, length) > 1)
  for (i in tune.args.num) {
    tune.args[[i]] <- sort(tune.args[[i]])
  }
  
  lookup.enm <- function(algorithm) {
    x <- switch(algorithm, 
                maxent.jar = enm.maxent.jar,
                maxnet = enm.maxnet,
                # randomForest = enm.randomForest,
                # boostedRegressionTrees = enm.boostedRegressionTrees,
                bioclim = enm.bioclim
    )
    return(x)
  }
  enm <- lookup.enm(algorithm)
  clamp.directions = NULL
  categoricals = NULL
  enm@errors(occs, envs, bg, tune.args, partitions, algorithm, 
             partition.settings, other.settings, categoricals, doClamp, 
             clamp.directions)
  quiet<-F
  if (!is.null(envs)) {
    if (is.null(bg)) {
      if (quiet != TRUE) 
        message(paste0("* Randomly sampling ", n.bg, 
                       " background points ..."))
      bg <- as.data.frame(terra::spatSample(envs, size = n.bg, 
                                            xy = TRUE, na.rm = TRUE, values = FALSE))
      names(bg) <- names(occs)
    }
    if (other.settings$removeduplicates == TRUE) {
      occs.cellNo <- terra::extract(envs, occs, cells = TRUE, 
                                    ID = FALSE)
      occs.dups <- duplicated(occs.cellNo[, "cell"])
      if (sum(occs.dups) > 0) 
        if (quiet != TRUE) 
          message(paste0("* Removed ", sum(occs.dups), 
                         " occurrence localities that shared the same grid cell."))
      occs <- occs[!occs.dups, ]
      
      occs.z <- occs.cellNo[!occs.dups, -which(names(occs.cellNo) == 
                                                 "cell")]
    }
    else {
      occs.z <- terra::extract(envs, occs, ID = FALSE)
    }
    bg.z <- terra::extract(envs, bg, ID = FALSE)
    occs <- cbind(occs, occs.z)
    bg <- cbind(bg, bg.z)
  }
  user.grp<-NULL
  occs.z.na <- which(rowSums(is.na(occs)) > 0)
  if (length(occs.z.na) > 0) {
    if (quiet != TRUE) 
      message(paste0("* Removed ", length(occs.z.na), " occurrence points with NA predictor variable values."))
    occs <- occs[-occs.z.na, ]
    if (!is.null(user.grp)) 
      user.grp$occs.grp <- user.grp$occs.grp[-occs.z.na]
  }
  bg.z.na <- which(rowSums(is.na(bg)) > 0)
  if (length(bg.z.na) > 0) {
    if (quiet != TRUE) 
      message(paste0("* Removed ", length(bg.z.na), " background points with NA predictor variable values."))
    bg <- bg[-bg.z.na, ]
    if (!is.null(user.grp)) 
      user.grp$bg.grp <- user.grp$bg.grp[-bg.z.na]
  }
  d <- rbind(occs, bg)
  d$pb <- c(rep(1, nrow(occs)), rep(0, nrow(bg)))
  if (!is.null(user.grp)) {
    d[d$pb == 1, "grp"] <- as.numeric(as.character(user.grp$occs.grp))
    d[d$pb == 0, "grp"] <- as.numeric(as.character(user.grp$bg.grp))
    if (!all(user.grp.uniq %in% d$grp)) 
      stop("Removal of cell duplicates caused one or more user partition groups to be missing. Please make sure all partition groups are represented by at least one non-duplicate occurrence record.")
    d$grp <- factor(d$grp)
  }
  
  if (!is.null(envs)) {
    categoricals <- unique(c(categoricals, names(envs)[which(terra::is.factor(envs))]))
  }else {
    categoricals <- unique(c(categoricals, names(occs)[which(sapply(occs, 
                                                                    is.factor))]))
  }
  categoricals<-NULL
  if (!is.null(categoricals)) {
    cat.levs <- list()
    for (i in 1:length(categoricals)) {
      if (!is.null(envs)) {
        cat.levs[[i]] <- terra::levels(envs[[categoricals[i]]])[[1]][, 
                                                                     2]
      }
      else {
        cat.levs[[i]] <- levels(d[, categoricals[i]])
      }
    }
    for (i in 1:length(categoricals)) {
      if (algorithm == "maxent.jar") {
        if (quiet != TRUE) {
          message(paste0("* Assigning variable ", categoricals[i], 
                         " to categorical and changing to integer for maxent.jar..."))
        }
        d[, categoricals[i]] <- factor(as.numeric(d[, 
                                                    categoricals[i]]), levels = 1:length(cat.levs[[i]]))
      }
      else {
        if (quiet != TRUE) {
          message(paste0("* Assigning variable ", categoricals[i], 
                         " to categorical ..."))
        }
      }
      d[, categoricals[i]] <- as.factor(d[, categoricals[i]])
      if (!is.null(user.val.grps)) {
        if (algorithm == "maxent.jar") {
          user.val.grps[, categoricals[i]] <- as.numeric(user.val.grps[, 
                                                                       categoricals[i]])
        }
        user.val.grps[, categoricals[i]] <- factor(user.val.grps[, 
                                                                 categoricals[i]], levels = levels(d[, categoricals[i]]))
      }
      if (!is.null(occs.testing.z)) {
        if (algorithm == "maxent.jar") {
          occs.testing.z[, categoricals[i]] <- as.numeric(occs.testing.z[, 
                                                                         categoricals[i]])
        }
        occs.testing.z[, categoricals[i]] <- factor(occs.testing.z[, 
                                                                   categoricals[i]], levels = levels(d[, categoricals[i]]))
      }
    }
  }
  other.settings$categoricals <- categoricals
  doClamp<-F 
  if (doClamp == TRUE) {
    if (!is.null(envs)) {
      if (is.null(clamp.directions)) {
        clamp.directions$left <- names(envs)
        clamp.directions$right <- names(envs)
      }
      other.settings$clamp.directions <- clamp.directions
      envs <- clamp.vars(orig.vals = envs, ref.vals = rbind(occs.z, 
                                                            bg.z), left = clamp.directions$left, right = clamp.directions$right, 
                         categoricals = categoricals)
      if (quiet != TRUE) 
        message("* Clamping predictor variable rasters...")
    }
    else {
      if (is.null(clamp.directions)) {
        clamp.directions$left <- names(d[, 3:(ncol(d) - 
                                                1)])
        clamp.directions$right <- names(d[, 3:(ncol(d) - 
                                                 1)])
      }
    }
  }
  other.settings$doClamp <- FALSE
  d.occs <- dplyr::select(dplyr::filter(d, pb == 1), 1:2)
  d.bg <- dplyr::select(dplyr::filter(d, pb == 0), 1:2)
  grps <- switch(partitions, jackknife = get.jackknife(d.occs, 
                                                       d.bg), randomkfold = get.randomkfold(d.occs, d.bg, partition.settings$kfolds), 
                 block = get.block(d.occs, d.bg, partition.settings$orientation), 
                 checkerboard = get.checkerboard(d.occs, envs, d.bg, partition.settings$aggregation.factor), 
                 user = NULL, testing = list(occs.grp = rep(0, nrow(d.occs)), 
                                             bg.grp = rep(0, nrow(d.bg))), none = list(occs.grp = rep(0, 
                                                                                                      nrow(d.occs)), bg.grp = rep(0, nrow(d.bg))))
  parts.message <- switch(partitions, jackknife = "* Model evaluations with k-1 jackknife (leave-one-out) cross validation...", 
                          randomkfold = paste0("* Model evaluations with random ", 
                                               partition.settings$kfolds, "-fold cross validation..."), 
                          block = paste0("* Model evaluations with spatial block (4-fold) cross validation and ", 
                                         partition.settings$orientation, " orientation..."), 
                          checkerboard = ifelse(length(partition.settings$aggregation.factor) == 
                                                  1, "* Model evaluations with basic checkerboard (2-fold) cross validation...", 
                                                "* Model evaluations with hierarchical checkerboard (4-fold) cross validation..."), 
                          user = paste0("* Model evaluations with user-defined ", 
                                        length(unique(user.grp$occs.grp)), "-fold cross validation..."), 
                          testing = "* Model evaluations with testing data...", 
                          none = "* Skipping model evaluations (only calculating full model statistics)...")
  other.settings$cbi.cv <- TRUE
  d$grp <- factor(c(grps$occs.grp, grps$bg.grp))
  message(paste("\n*** Running ENMeval v2.0.5 with", 
                enm@msgs(tune.args, other.settings), "***\n"))
  
  tune.tbl <- tibble::as_tibble(expand.grid(tune.args, stringsAsFactors = FALSE))
  calc.10p.trainThresh <- function(pred.train) {
    n <- length(pred.train)
    if(n < 10) {
      pct90.train <- floor(n * 0.9)
    }else{
      pct90.train <- ceiling(n * 0.9)
    }
    pct10.train.thr <- rev(sort(pred.train))[pct90.train]
    return(pct10.train.thr)
  }
  results <- tune(d, enm, partitions, tune.tbl, doClamp, other.settings, 
                  partition.settings, NULL, occs.testing.z, numCores, 
                  F, NULL, algorithm, updateProgress, quiet)
  train.stats.all <- dplyr::bind_rows(lapply(results, function(x) x$train.stats))
  val.stats.all <- dplyr::bind_rows(lapply(results, function(x) x$cv.stats))
  tune.names <- train.stats.all$tune.args
  tune.tbl$tune.args <- factor(tune.names, levels = tune.names)
  mod.full.all <- lapply(results, function(x) x$mod.full)
  names(mod.full.all) <- tune.names
  raster.preds<-T
  if (!is.null(envs) & raster.preds == TRUE) {
    f <- function(x) enm@predict(x$mod.full, envs, other.settings)
    if (!is.null(categoricals)) {
      for (i in 1:length(categoricals)) {
        lev.df <- terra::levels(envs[[categoricals[i]]])
        lev.df[[1]][, 2] <- 1:length(cat.levs[[i]])
        levels(envs[[categoricals[i]]]) <- lev.df[[1]]
      }
    }
    if (quiet != TRUE) 
      message("Making model prediction rasters...")
    mod.full.pred.all <- terra::rast(lapply(results, f))
    names(mod.full.pred.all) <- tune.names
  }
  
  if (partitions %in% c("testing", "none")) {
    nk <- 0
  }
  else {
    nk <- length(unique(d[d$pb == 1, "grp"]))
  }
  if (nk > 0) {
    nset <- ifelse(!is.null(tune.tbl), ncol(tune.tbl), 0)
    if (partitions == "jackknife") {
      sum.list <- list(avg = mean, sd = ~sqrt(corrected.var(., 
                                                            nk)))
    }
    else {
      sum.list <- list(avg = mean, sd = sd)
    }
    if (nk == 1 | partitions == "testing") 
      sum.list <- list(function(x) {
        x
      })
    if (!is.null(tune.tbl)) 
      val.stats.all$tune.args <- factor(val.stats.all$tune.args, 
                                        levels = tune.names)
    cv.stats.sum <- dplyr::ungroup(dplyr::summarize_all(dplyr::select(dplyr::group_by(val.stats.all, 
                                                                                      tune.args), -fold), sum.list))
    names(cv.stats.sum) <- gsub("(.*)_(.*)", "\\1.\\2", names(cv.stats.sum))
    cv.stats.sum <- cv.stats.sum[, order(colnames(cv.stats.sum))]
    if (!is.null(tune.tbl)) {
      train.stats.all$tune.args <- factor(train.stats.all$tune.args, 
                                          levels = tune.names)
      eval.stats <- dplyr::left_join(dplyr::left_join(tune.tbl, 
                                                      train.stats.all, by = "tune.args"), cv.stats.sum, 
                                     by = "tune.args")
    }
    else {
      train.stats.all$tune.args <- NULL
      cv.stats.sum$tune.args <- NULL
      eval.stats <- dplyr::bind_cols(train.stats.all, cv.stats.sum)
    }
  }
  else {
    train.stats.all$tune.args <- factor(train.stats.all$tune.args, 
                                        levels = tune.names)
    eval.stats <- dplyr::left_join(tune.tbl, train.stats.all, 
                                   by = "tune.args")
    if (nrow(val.stats.all) > 0) 
      eval.stats <- dplyr::left_join(eval.stats, val.stats.all, 
                                     by = "tune.args")
    if ("fold" %in% names(eval.stats)) 
      eval.stats <- dplyr::select(eval.stats, -fold)
  }
  ncoefs <- sapply(mod.full.all, enm@ncoefs)
  if ((enm@name == "maxnet" | enm@name == "maxent.jar")) {
    pred.type.raw <- switch(enm@name, maxnet = "exponential", 
                            maxent.jar = "raw")
    aic.settings <- other.settings
    aic.settings$pred.type <- pred.type.raw
    if (!is.null(envs)) {
      pred.all.raw <- terra::rast(lapply(mod.full.all, 
                                         enm@predict, envs, aic.settings))
    }
    else {
      pred.all.raw <- NULL
    }
    occs.pred.raw <- dplyr::bind_rows(lapply(mod.full.all, 
                                             enm@predict, d[d$pb == 1, 1:(ncol(d) - 2)], aic.settings))
    aic <- aic.maxent(occs.pred.raw, ncoefs, pred.all.raw)
    eval.stats <- dplyr::bind_cols(eval.stats, aic)
  }
  eval.stats$ncoef <- ncoefs
  if (is.null(taxon.name)) 
    taxon.name <- ""
  if (is.null(tune.tbl)) 
    tune.tbl <- data.frame()
  if (is.null(occs.testing.z)) 
    occs.testing.z <- data.frame()
  if (partitions != "block") 
    partition.settings$orientation <- NULL
  if (partitions != "checkerboard") 
    partition.settings$aggregation.factor <- NULL
  if (partitions != "randomkfold") 
    partition.settings$kfolds <- NULL
  if (is.null(partition.settings) | length(partition.settings) == 
      0) 
    partition.settings <- list()
  if (is.null(clamp.directions)) 
    clamp.directions <- list()
  variable.importance.all <- lapply(mod.full.all, enm@variable.importance)
  other.settings$doClamp <- NULL
  e <- ENMevaluation(algorithm = enm@name, tune.settings = as.data.frame(tune.tbl), 
                     results = as.data.frame(eval.stats), results.partitions = val.stats.all, 
                     predictions = mod.full.pred.all, models = mod.full.all, 
                     variable.importance = variable.importance.all, partition.method = partitions, 
                     partition.settings = partition.settings, other.settings = other.settings, 
                     doClamp = doClamp, clamp.directions = clamp.directions, 
                     taxon.name = as.character(taxon.name), occs = d[d$pb == 
                                                                       1, 1:(ncol(d) - 2)], occs.testing = occs.testing.z, 
                     occs.grp = factor(d[d$pb == 1, "grp"]), bg = d[d$pb == 
                                                                      0, 1:(ncol(d) - 2)], bg.grp = factor(d[d$pb == 0, 
                                                                                                             "grp"]), rmm = list())
  e@rmm <- buildRMM(e, envs, rmm)
  if (overlap == TRUE) {
    nr <- terra::nlyr(e@predictions)
    if (nr == 0) {
      if (quiet != TRUE) 
        message("Warning: calculate range overlap without model prediction rasters.")
    }
    else if (nr == 1) {
      if (quiet != TRUE) 
        message("Warning: only 1 model prediction raster found. Need at least 2 rasters to calculate range overlap. Increase number of tuning arguments and run again.")
    }
    else {
      for (ovStat in overlapStat) {
        if (quiet != TRUE) 
          message(paste0("Calculating range overlap for statistic ", 
                         ovStat, "..."))
        predictions.noNegs <- terra::rast(lapply(e@predictions, 
                                                 function(x) {
                                                   x[x < 0] <- 0
                                                   x
                                                 }))
        overlap.mat <- calc.niche.overlap(predictions.noNegs, 
                                          ovStat, quiet)
        e@overlap[[ovStat]] <- overlap.mat
      }
    }
  }
  timed <- proc.time() - start.time
  t.min <- floor(timed[3]/60)
  t.sec <- timed[3] - (t.min * 60)
  if (quiet != TRUE) 
    message(paste("ENMevaluate completed in", t.min, "minutes", 
                  round(t.sec, 1), "seconds."))

