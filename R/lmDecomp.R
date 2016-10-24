#' Decompose lm() Interactions
#'
#' This function takes the work of Bauer and Curan (2005) and applies it to a linear model object produced by lm(). Currently, the function handles two types of interactions in the lm() model: (1) Quantitative by Quantitative, which produces the Aiken and West (1991) Pick-a-Point approach simple slopes as well as Johnson-Neyman Regions-of-Significance; and (2) Quantitative by Categorical, which produces the simple slopes for the two levels of the dummy factor level as well as a quasi Region-of-Significance marking values of the Quantitative variable at which the difference of the factor levels change significance.
#'
#' All graphs produced are exported to the Global Environment as ggplot object which can be further edited.
#'
#' @param obj Model object containing either a Quantitative by Quantitative interaction or a Quantitative by Categorical interaction from lm().
#' @param interest Variable of interest in the model. Must use quotes.
#' @param moderator Variable designated as the moderator in the model. Must use quotes.
#' @param mod.type Defines the TYPE of interaction. Must be an integer of 1 or 2. 1 = Quantitative by Quantitative. 2 = Quantitative by Categorical. Defaults to 1.
#' @param int.values DEPRECIATED. Set to NULL.
#' @param mod.values Defines the values of the moderator to be used when calculating simple slopes. Only used with mod.type=1. If mod.type=2, mod.values is set to *dummy* coding (0,1), and comparison will be between indicator level and reference level in the model.
#' @param int.range Optional. Defines the range of the variable of interest to be graphed. Only used when mod.type=2.
#' @param mod.range Optional. Defines the range of the moderator to be graphed.
#' @param alpha.level Designates the alpha for the Region-of-Significnace. Defaults to 0.5.
#' @param show.points Logical. Should the original data points be shown on the produced graph? Defaults to FALSE.
#' @param print.sslopes Logical. Should the Simple Slopes graph be created? Defaults to TRUE
#' @param print.ros Logical. Should the Region-of-Significance values and graph be created? Defaults to TRUE
#'
#' @references
#' Bauer, D.J. & Curran, P.J., (2005), Probing Interactions in Fixed and Multilevel Regression: Inferential and Graphical Techniques, Multivariate Behavioral Research, 40(3), 373–400.
#'
#' @examples
#' mod <- lm(mpg ~ wt * am, data=mtcars)
#' summary(mod)
#' lmDecomp(mod, "wt", "am", mod.type = 2, mod.values=c(0,1))
#'
#' @export
lmDecomp <- function(obj, interest, moderator, mod.type=1, int.values=NULL, mod.values=NULL, int.range=NULL, mod.range=NULL, alpha.level=0.05, show.points=FALSE, print.sslopes = TRUE, print.ros=TRUE){

  if(is.logical(show.points) == FALSE | is.logical(print.ros) == FALSE){
    stop("Logical arguments not supplied. \n Check function details.")
  }
  if(missing(obj) | missing(interest) | missing(moderator)){
    stop("One or more required arguments missing. \n Required: 'obj' 'interest' 'moderator'")
  }
  if(!mod.type %in% c(1,2)){
    stop("Please select a valid moderator type.")
  }
  if(mod.type == 1 & is.null(mod.values)) {
    stop("Please supply values of the moderator variable.")
  }
  # if(mod.type == 2 & (is.null(mod.values) | length(mod.values) != 2)){
  #   stop("Please supply valid values of the moderator variable.")
  # }
  # if(!interest %in% attributes(obj$terms)$term.labels){
  #   stop("Variable of interest not in supplied model object.")
  # }
  # if(!moderator %in% attributes(obj$terms)$term.labels){
  #   stop("Moderator variable not in supplied model object.")
  # }
  if(!interest %in% names(obj$coeff)){
    stop("Variable of interest not in supplied model object.")
  }
  if(!moderator %in% names(obj$coeff)){
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
  #thisdata <- obj$model #NOT for factors
  #thisdata <- as.data.frame.matrix(obj$model)
  thisda <- data.frame(as.numeric(unlist(data.frame(model.frame(obj)[,1]))),
                       data.frame(model.matrix(obj)[,-1]))
  names(thisda) <- c(obj$terms[[2]], names(data.frame(model.matrix(obj))[-1]))
  thisdata <- as.data.frame.matrix(thisda)

  if(xzpval > alpha.level){
    warning("Interaction term not significant. \n Results of decomposition may be noninterpretable.")
  }

  if(length(unlist(obj$xlevels)) == 0){
    m <- table(thisdata[,zvar])
    if(mod.type == 1 & length(m) == 2){
      stop("Moderator variable has just two values. Please select the correct mod.type")
    }
  }
  # if(length(unlist(obj$xlevels)) != 0){
  #   if(length(obj$xlevels) > 0 & mod.type == 1 & names(obj$xlevels) %in% zvar){
  #     stop("Moderator appears to be categorical. Please select the correct mod.type")
  #   }
  # }
  if(length(unlist(obj$xlevels)) != 0){
    if(length(obj$xlevels) > 0 & mod.type == 1 & regexpr(names(obj$xlevels), zvar)[1] > 0){
      stop("Moderator appears to be categorical. Please select the correct mod.type")
    }
  }

  if(mod.type == 1){ #Continuous Moderator Run
    #Simple Slopes
    #At levels of Z
    z <- mod.values
    interceptz <- (intercept + (betaz*z))
    slopez <- (betax + betaxz*z)
    sez <- sqrt(varb1 + (2*z*covb1b3) + ((z^2)*varb3))
    tz <- slopez / sez
    ptz <- (1 - (pt(abs(tz), degrees))) * 2

    modtable <- data.frame(z, interceptz, slopez, sez, tz, ptz)
    names(modtable) <- c(paste(zvar), "Intercept", "Slope", "SE", "t-value", "p-value")
    #paste("Simple Slopes of", xvar, "at levels of", zvar)
    SSTable <- modtable

    if(print.sslopes == TRUE){
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
    }

    if(print.ros==TRUE){
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
        labs(title="Continuous Moderator \n Regions of Signficance", x=moderator, y=paste("Slope of", xvar)) +
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
    #Force dummy coding
    if(mod.type == 2){
      mod.values <- c(0,1)
    }

    #Simple Slopes
    #At levels of Z
    z <- mod.values
    interceptz <- (intercept + (betaz*z))
    slopez <- (betax + betaxz*z)
    sez <- sqrt(varb1 + (2*z*covb1b3) + ((z^2)*varb3))
    tz <- slopez / sez
    ptz <- (1 - (pt(abs(tz), degrees))) * 2

    modtable <- data.frame(z, interceptz, slopez, sez, tz, ptz)
    names(modtable) <- c(paste(zvar), "Intercept", "Slope", "SE", "t-value", "p-value")
    #paste("Simple Slopes of", xvar, "at levels of", zvar)
    SSTable <- modtable

    if(print.ros==TRUE){
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
                              labels=c(paste("Mod.Grp.", mod.values[1], sep=""),
                                       paste("Mod.Grp.", mod.values[2], sep=""))) +
        geom_vline(xintercept=c(gzlow, gzhig), color="green") +
        annotate("rect", xmin=gzlow, xmax=gzhig, ymin=-Inf, ymax=Inf, alpha=0.2) +
        labs(title="Dichotomous Moderator \n Regions of Significance", x=xvar, y=outcome) +
        theme_bw()
      if(!is.null(int.range)){
        ROSd <- ROSd + xlim(int.range)
      }

      #Show dichotomous graph (with show.points option)
      if(show.points == TRUE){
        ROSdp <- ROSd + geom_point()
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
