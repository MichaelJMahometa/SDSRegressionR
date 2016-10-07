lmDecomp <- function(obj, interest, moderator, mod.type=1, int.levels=NULL, mod.levels=NULL, mod.range=NULL, int.range=NULL, alpha.level=0.05, show.points=FALSE, ros=TRUE){

  if(is.logical(show.points) == FALSE | is.logical(ros) == FALSE){
    stop("Logical arguments not supplied. \n Check function details.")
  }
  if(missing(obj) | missing(interest) | missing(moderator)){
    stop("One or more required arguments missing. \n Required: 'obj' 'interest' 'moderator'")
  }
  if(!mod.type %in% c(1,2)){
    stop("Please select a valid moderator type.")
  }
  if(mod.type == 1 & is.null(mod.levels)) {
    stop("Please supply levels of the moderator variable.")
  }
  if(mod.type == 2 & (is.null(mod.levels) | length(mod.levels) != 2)){
    stop("Please supply valid levels of the moderator variable.")
  }
  if(!interest %in% attributes(obj$terms)$term.labels){
    stop("Variable of interest not in supplied model object.")
  }
  if(!moderator %in% attributes(obj$terms)$term.labels){
    stop("Moderator variable not in supplied model object.")
  }
  ## Start Function
  outcome <- paste(obj$terms[[2]])
  xvar <- interest
  zvar <- moderator
  xzint <- paste(xvar,":",zvar, sep="")
  if(xzint %in% names(obj$coeff)){
    xzint <- xzint
  } else {
    xzint <- paste(zvar,":",xvar, sep="")
  }
  intercept <- summary(obj)$coef["(Intercept)",1]
  betax <- summary(obj)$coef[xvar,1]
  betaz <- summary(obj)$coef[zvar,1]
  betaxz <- summary(obj)$coef[xzint,1]
  xzpval <- summary(obj)$coef[xzint,4]
  varb1 <- vcov(obj)[xvar, xvar]
  varb2 <- vcov(obj)[zvar, zvar]
  varb3 <- vcov(obj)[xzint, xzint]
  covb1b3 <- vcov(obj)[xvar, xzint]
  covb2b3 <- vcov(obj)[zvar, xzint]
  degrees <- summary(obj)$df[2]
  tcrit <- qt((1-(alpha.level/2)), degrees)
  thisdata <- obj$model

  if(xzpval > alpha.level){
    warning("Interaction term not significant. \n Results of decomposition may be noninterpretable.")
  }

  if(length(obj$xlevels) == 0){
    m <- table(thisdata[,zvar])
    if(mod.type == 1 & length(m) == 2){
      stop("Moderator has just two values. Please select the correct mod.type")
    }
  }
  if(length(obj$xlevels != 0)){
    if(length(obj$xlevels) > 0 & mod.type == 1 & names(obj$xlevels) %in% zvar){
      stop("Moderator appears to be categorical. Please select the correct mod.type")
    }
  }


  if(mod.type == 1){ #Continuous Moderator Run
    #Simple Slopes
    #At levels of Z
    z <- mod.levels
    interceptz <- (intercept + (betaz*z))
    slopez <- (betax + betaxz*z)
    sez <- sqrt(varb1 + (2*z*covb1b3) + ((z^2)*varb3))
    tz <- slopez / sez
    ptz <- (1 - (pt(abs(tz), degrees))) * 2

    modtable <- data.frame(z, interceptz, slopez, sez, tz, ptz)
    names(modtable) <- c(paste(zvar), "Intercept", "Slope", "SE", "t-value", "p-value")
    #paste("Simple Slopes of", xvar, "at levels of", zvar)
    SSTable <- modtable

    #Simple Slopes graph (user supplied mod levels)
    ssdata <- modtable #Grab the simple slopes to use
    ssdata[,1] <- factor(ssdata[,1])
    ssout <- ggplot(thisdata, aes(x=thisdata[,xvar], y=thisdata[,outcome])) +
      geom_blank() +
      geom_abline(data=ssdata, aes(intercept=Intercept, slope=Slope, linetype=ssdata[,1]), show.legend=TRUE) +
      scale_linetype_discrete(name="Moderator \n Levels") +
      labs(title="Continuous Mediator Graph", x=xvar, y=outcome) +
      theme_bw()

    #Show graph (with show.points option)
    if(show.points == TRUE){
      ssoutp <- ssout + geom_point()
      print(ssoutp)
      assign("SSlopes", ssoutp, envir=globalenv())
    } else {
      print(ssout)
      assign("SSlopes", ssout, envir=globalenv())
    }

    if(ros==TRUE){
      #RofS (to graph...)
      #X
      A <- ((tcrit^2) * varb3) - betaxz^2
      B <- 2*(((tcrit^2) * covb1b3) - (betax*betaxz))
      C <- ((tcrit^2) * varb1) - betax^2
      xlow <- (-B + sqrt((B^2) - (4*A*C))) / (2*A)
      xhig <- (-B - sqrt((B^2) - (4*A*C))) / (2*A)
      if(is.na(xlow) | is.na(xlow)){
        stop("Region of Significance not given. \n Check interaction term for significance.")
      }
      #paste("Lower threshold with respect to", zvar, "=", round(xlow, 4))
      #paste("Upper threshold with respect to", zvar, "=", round(xhig, 4))

      #Mod slope calculations
      if (is.null(mod.range)){
        moder <- thisdata[,moderator] #Grab orignal data...
        z <- seq(min(moder), max(moder), length.out = 100) #Use range for the original data
      } else {
        moder <- mod.range
        z <- seq(min(moder), max(moder), length.out = 100)
      }
      sslopes <- betax + betaxz*z
      cb1 <- sslopes + (tcrit * (sqrt(varb1 + (2*z*covb1b3) + ((z^2)*varb3))))
      cb2 <- sslopes - (tcrit * (sqrt(varb1 + (2*z*covb1b3) + ((z^2)*varb3))))

      #ggplot (RofS)
      ggdata <- data.frame(mod = z, varint = sslopes, low=cb2, hi=cb1)
      gxlow <- ifelse(xlow < min(z), -Inf, xlow) #Fix for "if RoS is out of bounds"
      gxhig <- ifelse(xhig > max(z), Inf, xhig) #Fix for "if RoS is out of bounds"
      ROS <- ggplot(ggdata, aes(x=mod, y=varint)) +
        geom_line() +
        scale_x_continuous(limits=c(min(z), max(z))) +
        geom_line(aes(x=mod, y=low), color="red") +
        geom_line(aes(x=mod, y=hi), color="red") +
        geom_vline(xintercept=c(gxlow, gxhig), color="green") +
        geom_hline(yintercept=0, color="black") +
        annotate("rect", xmin=gxlow, xmax=gxhig, ymin=-Inf, ymax=Inf, alpha=0.2) +
        labs(title="Continuous Mediator \n Regions of Signficance", x=moderator, y=paste("Slope of", xvar)) +
        theme_bw()

      #   #Show graph (with show.points option)
      #   if(show.points == TRUE){
      #     print(ssout + geom_point())
      #   } else {
      #     print(ssout)
      #   }

      #Show RofS graph (continuous)
      suppressWarnings(print(ROS)) #Suppress warnings b/c of range changes
      assign("ROS", ROS, envir=globalenv())

      out <- list(SSTable, round(xlow, 4), round(xhig, 4))
      names(out) <- c(paste("Simple Slopes of", xvar, "at levels of", zvar),
                      paste("Lower threshold with respect to", zvar),
                      paste("Upper threshold with respect to", zvar))
      out
    } else {
      #Output in console
      out <- list(SSTable)
      names(out) <- c(paste("Simple Slopes of", xvar, "at levels of", zvar))
      out
    }
  } else { #Dichotmous Moderator run
    #Simple Slopes
    #At levels of Z
    z <- mod.levels
    interceptz <- (intercept + (betaz*z))
    slopez <- (betax + betaxz*z)
    sez <- sqrt(varb1 + (2*z*covb1b3) + ((z^2)*varb3))
    tz <- slopez / sez
    ptz <- (1 - (pt(abs(tz), degrees))) * 2

    modtable <- data.frame(z, interceptz, slopez, sez, tz, ptz)
    names(modtable) <- c(paste(zvar), "Intercept", "Slope", "SE", "t-value", "p-value")
    #paste("Simple Slopes of", xvar, "at levels of", zvar)
    SSTable <- modtable

    if(ros==TRUE){
      #RofS (dichotomous switch)
      #Z (for dichotomous moderator!!!)
      A <- ((tcrit^2) * varb3) - betaxz^2
      B <- 2*(((tcrit^2) * covb2b3) - (betaz*betaxz))
      C <- ((tcrit^2) * varb2) - betaz^2
      zlow <- (-B + sqrt((B^2) - (4*A*C))) / (2*A)
      zhig <- (-B - sqrt((B^2) - (4*A*C))) / (2*A)
      if(is.na(zlow) | is.na(zlow)){
        stop("Region of Significance not given. \n Interaction most likely non-significant.")
      }
      #   paste("Lower threshold of difference between", zvar, "with respect to", xvar, "=", round(zlow, 4))
      #   paste("Upper threshold of difference between", zvar, "with respect to", xvar, "=", round(zhig, 4))

      #Graph (dichotomous)
      gzlow <- ifelse(zlow < min(x=thisdata[,xvar]), -Inf, zlow) #Fix for "if RoS is out of bounds"
      gzhig <- ifelse(zhig > max(x=thisdata[,xvar]), Inf, zhig) #Fix for "if RoS is out of
      #Fix for int.range change
      if(!is.null(int.range)){
        if(zlow < min(int.range)){
          gzlow <- -Inf
        } else {
          gzlow <- ifelse(zlow < min(x=thisdata[,xvar]), -Inf, zlow) #Fix for "if RoS is out of bounds"
        }
        if(zhig > max(int.range)){
          gzhig <- Inf
        }  else {
          gzhig <- ifelse(zhig > max(x=thisdata[,xvar]), Inf, zhig) #Fix for "if RoS is out of bounds"
        }
      }
      ROSd <- ggplot(thisdata, aes(x=thisdata[,xvar], y=thisdata[,outcome])) +
        geom_blank() +
        geom_abline(aes(intercept=interceptz[1], slope=slopez[1], linetype="a"), show.legend=TRUE) +
        geom_abline(aes(intercept=interceptz[2], slope=slopez[2], linetype="b"), show.legend=TRUE) +
        scale_linetype_manual(name="Moderator",
                              values = c("a" = "solid", "b" = "dashed"),
                              labels=c(paste("Mod.Grp.", mod.levels[1], sep=""),
                                       paste("Mod.Grp.", mod.levels[2], sep=""))) +
        geom_vline(xintercept=c(gzlow, gzhig), color="green") +
        annotate("rect", xmin=gzlow, xmax=gzhig, ymin=-Inf, ymax=Inf, alpha=0.2) +
        labs(title="Dichotomous Mediator \n Regions of Significance", x=xvar, y=outcome) +
        theme_bw()
      if(!is.null(int.range)){
        ROSd <- ROSd + xlim(int.range)
      }

      #Show dichotomous graph (with show.points option)
      if(show.points == TRUE){
        ROSDp <- ROSd + geom_point()
        suppressWarnings(print(ROSdp))
        assign("ROS", ROSdp, envir=globalenv())

      } else {
        suppressWarnings(print(ROSd))
        assign("ROS", ROSd, envir=globalenv())
      }
      out <- list(SSTable, round(zlow, 4), round(zhig, 4))
      names(out) <- c(paste("Simple Slopes of", xvar, "at levels of", zvar),
                      paste("Lower threshold of difference between", zvar, "with respect to", xvar),
                      paste("Upper threshold of difference between", zvar, "with respect to", xvar))
      out
    } else {
      #Output in console
      out <- list(SSTable)
      names(out) <- c(paste("Simple Slopes of", xvar, "at levels of", zvar))
      out
    }
  }
}
