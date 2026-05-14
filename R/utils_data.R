#' Convert a 3D array into a long-format data frame
#'
#' Internal helper that transforms a three-dimensional array into a data frame 
#' by stacking slices along the third dimension and adding index and year 
#' columns.
#'
#' @param arr A three-dimensional array.
#'
#' @return A data frame with combined slices and additional columns identifying 
#'   the index and year.
#'
#' @keywords internal
.array_to_dataframe <- function(arr) {
  if (length(dim(arr)) != 3) {
    stop("The input must be a three-dimensional array.")
  }
  df_list <- list()
  dims <- dim(arr)
  for (k in 1:dims[3]) {
    slice <- arr[,,k]
    df <- as.data.frame(slice)
    df$Index <- dimnames(arr)[[3]][k]
    df$Year <- as.numeric(row.names(df))
    df_list[[k]] <- df
  }
  result <- do.call(rbind, df_list)
  return(result)
}

#' Replace values based on NA pattern of another data frame
#'
#' Internal helper that replaces values in one data frame with NA wherever the 
#' corresponding positions in another data frame are NA.
#'
#' @param df1 A data frame to be modified.
#' @param df2 A data frame providing the NA pattern.
#'
#' @return A data frame with values replaced by NA where applicable.
#'
#' @keywords internal
.replace_na_with_na <- function(df1, df2) {
  df1[] <- lapply(names(df1), function(col) {
    replace(df1[[col]], is.na(df2[[col]]), NA)
  })
  return(df1)
}

#' Validate list of model fits
#'
#' Internal helper that checks whether the input is a valid list of JABBA model 
#' outputs and verifies the presence and class of required components.
#'
#' @param list_fit_models A list of model outputs.
#'
#' @return Invisibly returns NULL if all validations pass, otherwise throws an 
#' error.
#'
#' @keywords internal
.validate_fits_input_data <- function(list_fit_models) {
  if(.is_fit_jabba(list_fit_models)) {
    stop("Expected a list of valid JABBA model outputs.")
  }

  if(!all(vapply(list_fit_models, .is_fit_jabba, logical(1)))) {
    stop("All elements must be a valid JABBA model output.")
  }

  # fits
  .validate_column(list_fit_models, "settings", "list")
  .validate_column(list_fit_models$settings, "I", c("matrix", "array"))
  .validate_column(list_fit_models$settings, "SE2", c("matrix", "array"))
  .validate_column(list_fit_models, "cpue.ppd", "array")
  .validate_column(list_fit_models, "cpue.hat", "array")
  .validate_column(list_fit_models, "yr", "numeric")
  .validate_column(list_fit_models, "scenario", "character")
  # runs_tests_cpue_residuals
  .validate_column(list_fit_models, "residuals", c("matrix", "array"))
  .validate_column(list_fit_models, "stats", "data.frame")
  # prios_posteriors
  .validate_column(list_fit_models$settings, "K.pr", "numeric")
  .validate_column(list_fit_models$settings, "r.pr", "numeric")
  .validate_column(list_fit_models$settings, "psi.pr", "numeric")
  .validate_column(list_fit_models$settings, "psi.dist", "character")
  .validate_column(list_fit_models$settings, "igamma", "numeric")
  .validate_column(list_fit_models, "pars_posterior", "data.frame")
}

#' Check if object is a valid JABBA model fit
#'
#' Internal helper that verifies whether an object is a list containing all 
#' required components of a JABBA model output.
#'
#' @param model An object representing a model output.
#'
#' @return A logical value indicating whether the object is a valid JABBA fit.
#'
#' @keywords internal
.is_fit_jabba <- function(model) {
  cols_fit <- c(
    "assessment", "scenario", "settings", "inputseries", 
    "pars", "estimates", "yr", "catch", "est.catch",
    "cpue.hat", "cpue.ppd", #"PPC",
    "timeseries", "refpts", 
    "pfunc", "diags", "residuals", "std.residuals", 
    "stats", "pars_posterior", "refpts_posterior", "kobe", 
    "flqs", "bppd", "kbtrj", "posteriors"#, "model"
  )
  is.list(model) && all(cols_fit %in% names(model))
}

#' Validate column class
#'
#' Internal helper that checks whether a specific column in a model object 
#' inherits from the expected class.
#'
#' @param model A model object.
#' @param column A character string indicating the column name.
#' @param class_expected A character vector of expected class names.
#'
#' @return A logical value indicating whether the column has the expected class.
#'
#' @keywords internal
.is_column_valid <- function(model, column, class_expected) {
  inherits(model[[column]], class_expected)
}

#' Validate column across model list
#'
#' Internal helper that verifies whether a specific column exists in all models 
#' and matches the expected class. If any model fails validation, an informative 
#' error is thrown.
#'
#' @param list_fit_models A list of model outputs.
#' @param column A character string indicating the column name.
#' @param class_expected A character vector of expected class names.
#'
#' @return Invisibly returns NULL if validation passes, otherwise throws an 
#' error.
#'
#' @keywords internal
.validate_column <- function(list_fit_models, column, class_expected) {
  check <- vapply(
    list_fit_models,
    function(m) .is_column_valid(m, column, class_expected),
    logical(1)
  )
  if(!all(check)) {
    invalid_idx <- which(!check)

    received_class <- vapply(
      list_fit_models[invalid_idx],
      function(m) paste(class(m[[column]]), collapse = ", "),
      character(1)
    )

    stop(
      paste0(
        "Invalid '", column, "' in model(s): ", 
        paste(invalid_idx, collapse = ", "),
        ". Expected class: ", paste(class_expected, collapse = ", "),
        ". Received class: ", paste(received_class, collapse = " | ")
      )
    )
  }
}

#' Check if object is a valid hindcast JABBA model result
#' 
#' Internal helper that verifies whether an object is a list containing all 
#' required components of a hindcast JABBA model result.
#' 
#' @param obj An object representing a JABBA model output.
#' 
#' @return A logical value indicating wheter the object is a valid hindcast 
#' JABBA result
#' 
#' @keywords internal
.is_hindcast_jabba <- function(obj) {
  if(!is.list(obj)) {
    stop()
  }

  if (length(obj) == 0) return(FALSE)
  
  elem <- obj[[1]]

  .is_fit_jabba(elem)
}

#'Validate list of hindicast models
#' 
#' Internal helper that checks whether the input is a valid list of hindcast 
#' JABBA model outputs and verifies the presence and class of required 
#' components.
#' 
#' @param list_fit_models A list of model outputs.
#' 
#' @return Invisibily returns NULL if all validation pass, otherwise throws an 
#' error.
#' @keywords internal
.validate_hcs_input_data <- function(list_fit_models) {
  
  if(.is_hindcast_jabba(list_fit_models)) {
    stop("Expected a list of valid JABBA model outputs.")
  }

  if(!all(vapply(list_fit_models, .is_hindcast_jabba, logical(1)))) {
    stop("All elements must be a valid JABBA model output.")
  }

  all(
    vapply(
      list_fit_models,
      function(hc) {
        .validate_column(hc, "scenario", "character")
        .validate_column(hc, "timeseries", "array")
        .validate_column(hc, "pfunc", "data.frame")
        .validate_column(hc, "diags", "data.frame")
        TRUE
      },
      logical(1)
    )
  )
}

#' Check if an object is a valid JABBA ensemble output
#'
#' Internal helper that verifies whether an object is a data frame containing 
#' the required columns for a JABBA model ensemble output.
#'
#' @param obj An object to be checked.
#'
#' @return A logical value indicating whether the object matches the expected 
#' structure.
#'
#' @keywords internal
.is_jbplot_ensemble <- function(obj) {
  cols <- c(
    "year", "run", "type", "iter", "stock",
    "harvest", "B", "H", "Bdev", "Catch", 
    "BB0", "BBfrac", "Bref"
  )
  is.data.frame(obj) && all(cols %in% names(obj))
}

#' Validate a JABBA ensemble object
#'
#' Internal helper that checks whether a data frame is a valid JABBA model 
#' output. It verifies both the presence of required columns and that all 
#' relevant columns are numeric.
#'
#' @param object_df A data frame representing JABBA model output.
#'
#' @return Invisibly returns \code{NULL}. Throws an error if validation fails.
#'
#' @details
#' Columns other than \code{year}, \code{run}, \code{type}, and \code{iter} are 
#' expected to be numeric. If not, an informative error is raised listing the 
#' invalid columns and their classes.
#'
#' @keywords internal
.validate_jbplot_ensemble <- function(object_df) {
  if(!.is_jbplot_ensemble(object_df)) {
    stop("Element must be a valid JABBA model output.")
  }

  cols <- setdiff(names(object_df), c("year", "run", "type", "iter"))
  check <- vapply(
    cols,
    function(col) .is_column_valid(object_df, col, "numeric"),
    logical(1)
  )
  if(!all(check)) {
    invalid_cols <- cols[!check]

    received_class <- vapply(
      invalid_cols,
      function(col) paste(class(object_df[[col]]), collapse = ", "),
      character(1)
    )

    stop(
      paste0(
        "Invalid column(s): ",
        paste(invalid_cols, collapse = ", "),
        ". Expected class: numeric",
        ". Received class: ",
        paste(received_class, collapse = " | ")
      )
    )
  }
}

#' Validate index values against available data
#'
#' Internal helper that checks whether all provided indices exist in the 
#' available data indices.
#'
#' @param data_indices A vector of valid indices present in the data.
#' @param factor_indices A vector of indices to be validated.
#'
#' @return Invisibly returns \code{NULL}. Throws an error if any index is not 
#' found in \code{data_indices}.
#'
#' @keywords internal
.validate_indices <- function(data_indices, factor_indices) {
  if (!all(factor_indices %in% data_indices)) {
    stop("All indices past in 'indices' must exist in the data.")
  }
}

#' Validate color palette
#'
#' Checks whether all elements in a character vector are valid R colors.
#'
#' @param pal A character vector of color names or hexadecimal color codes.
#'
#' @return Returns \code{TRUE} if all elements are valid colors. Otherwise, the 
#'   function stops with an error.
#'
#' @details
#' The function attempts to convert the provided values using 
#' \code{grDevices::col2rgb()}. If any element is not a valid color, an error is 
#' raised.
#'
#' @keywords internal
#' @importFrom grDevices col2rgb
.is_palette_valid <- function(pal) {
  res <- try(col2rgb(pal), silent = TRUE)
  if (inherits(res, "try-error")) {
    stop("All elements in palette are expected to be colors.")
  }
  return(TRUE)
}

#' .jbplot_ensemble2()
#'
#' Plots plots JABBA ensemble models + projections - joint or by run  
#' 
#' @param kb objects from fit_jabba(),jabba_fw(), list of fit_jabba() or 
#'   fit_jabba()$kbtrj    
#' @param subplots option choose from subplots 1:7 
#' \itemize{
#'   \item 1: stock (B/Bmsy)  
#'   \item 2: harvest (F/Fmsy) 
#'   \item 3: B (Biomass) 
#'   \item 4: H (Harvest rate)
#'   \item 5: Bdev (Process Deviations)
#'   \item 6: Catch
#'   \item 7: BB0 (B/K) 
#'   \item 8: BBfrac 
#' }    
#' @param joint if true it creates a joint ensemble from list of fit_jabba()
#' @param plotCIs Plot Credibilty Interval
#' @param quantiles default is 95CIs as c(0.025,0.975)
#' @param kbout if TRUE, produces the kb data.frame as output 
#' @param bfrac biomass fraction of Bmsy or B0 (subplot 8), default 0.5Bmsy
#' @param bref biomass fraction reference options c("bmsy","b0")
#' @param reflines if TRUE show reference point lines in absolute plots
#' @param ylabs yaxis labels for quants
#' @param ylab.bref option to only specify BBfrac plot ylab final year of values 
#' to show for each model. By default it is set to the
#' @param col Optional vector of colors to be used for lines. Input NULL
#' @param bref.col option to set color for Bref reference line (default is 
#' "red")
#' @param pch Optional vector of plot character values
#' @param lty Optional vector of line types
#' @param lwd Optional vector of line widths
#' @param tickEndYr TRUE/FALSE switch to turn on/off extra axis mark at final
#' year in timeseries plots.
#' @param ylimAdj Multiplier for ylim parameter. Allows additional white space
#' @param xlim = NULL range of years
#' @param xaxs Choice of xaxs parameter (see ?par for more info)
#' @param yaxs Choice of yaxs parameter (see ?par for more info)
#' @param type Type parameter passed to points (default 'o' overplots points on
#' top of lines)
#' @param legend Add a legend?
#' @param legendlabels Optional vector of labels to include in legend.
#' @param legend.loc Location of legend. Either a string like "topleft" or a 
#' vector of two numeric values representing the fraction of the maximum in the 
#' x and y dimensions, respectively. See ?legend for more info on the string 
#' options.
#' @param legendorder Optional vector of model numbers that can be used to have
#' the legend display the model names in an order that is different than that
#' which is represented in the summary input object.
#' @param legendncol Number of columns for the legend.
#' @param legendcex Allows to adjust legend cex
#' @param legendsp Space between legend labels
#' @param pwidth Width of plot
#' @param pheight Height of plot
#' @param punits Units for PNG file
#' @param res Resolution for PNG file
#' @param ptsize Point size for PNG file
#' @param cex.main Character expansion for plot titles
#' @param plotdir Directory where PNG or PDF files will be written. By default
#' it will be the directory where the model was run.
#' @param filenameprefix Additional text to append to PNG or PDF file names.
#' It will be separated from default name by an underscore.
#' @param par list of graphics parameter values passed to par() function
#' @param verbose Report progress to R GUI?
#' @param shadecol uncertainty shading of hcxval horizon
#' @param shadealpha Transparency adjustment used to make default shadecol
#' @param new Create new empty plot window
#' @param add surpresses par() to create multiplot figs
#' @param run name for single models or joint ensembles
#' @author Mostly adopted from ss3diags::SSplotEnsemble
#' @keywords internal
#' @examples
#' \dontrun{
#' if (requireNamespace("JABBA", quietly = TRUE)) {
#'   data("iccat", package = "JABBA")
#'   bet <- iccat$bet
#'   
#'   .jbplot_ensemble2(bet)
#' }
#' }
#' @importFrom JABBA addBfrac
#' @importFrom grDevices adjustcolor dev.off graphics.off png
#' @importFrom graphics abline arrows axis box legend lines par points polygon
#' @importFrom stats aggregate
.jbplot_ensemble2 <- function(
  kb,subplots=1:6,joint=FALSE,plotCIs=TRUE,quantiles = c(0.025,0.975),
  bfrac = 0.5,bref = c("bmsy","b0")[1],reflines = TRUE,kbout = FALSE,
  ylabs = NULL,ylab.bref = NULL,plot=TRUE,as.png=FALSE,col=NULL,
  bref.col = "red",pch=NULL, lty=1, lwd=1.5,tickEndYr=FALSE,xlim=NULL, 
  ylimAdj=1.05,xaxs="i", yaxs="i",xylabs=TRUE,type="l",legend=TRUE, 
  legendlabels="default", legend.loc="topright",legendorder="default",
  legendncol=1,legendcex=0.7,legendsp=0.8,pwidth=6.5,pheight=5.0,punits="in",
  res=300,ptsize=10,cex.main=1,plotdir=NULL,filenameprefix="",
  par=list(mar=c(5,4,1,1)+.1),verbose=FALSE,shadecol = NULL, shadealpha=0.3,
  new=TRUE,add=FALSE,single.plots = add,run=NULL,fmax=5.0){
  if(!is.null(kb$settings)){ 
    kb = kb$kbtrj
  }      
  # if a list of fit_jabba() is provided
  if(inherits(kb, "list")) {
  
    if(!is.null(kb$settings)){
      kb = list(kb)
      if(!is.null(run)) names(kb) = run
    }
  
    if(is.null(names(kb)[1])){
      run.ls = do.call(c,lapply(kb,function(x){
      x$scenario
      })) 
    } else {
      run.ls = names(kb)
    }
    run.ls = as.list(run.ls)

    
    kb = do.call(rbind,Map(function(x,y){
    z = x$kbtrj
    z$run = y
    z
    },x=kb,y=run.ls))
    
    
    if(joint & !is.null(run)) kb$run = run
    if(joint & is.null(run)) kb$run = "Joint"
  }
  
  if(!is.null(xlim)) kb =kb[kb$year<=xlim[2] & kb$year>=xlim[1],]
  
    # Constraint on F/Fmsy
    kb$harvest[kb$type=="prj"] = pmin(kb[kb$type=="prj",]$harvest,fmax)
    kb$H[kb$type=="prj"]= pmin(fmax*median(kb[kb$type=="prj",]$H/kb[kb$type=="prj",]$harvest),kb[kb$type=="prj",]$H)
   
  if(as.png==TRUE){
    add=FALSE
  }
   if(!add) graphics.off()
   
  if(is.null(ylabs)){
    if(!is.null(bfrac) & bref[1]=="bmsy") bflab = bquote(B / ~(.(bfrac)~B[MSY])) 
    if(!is.null(bfrac) & bref[1]=="b0") bflab = bquote(B / ~(.(bfrac)~B[0]))
    if(is.null(bfrac)) bflab = "Not defined"
    if(!is.null(ylab.bref)) bflab = ylab.bref
    ylab.default = TRUE
    ylabs =  c(expression(B/B[MSY]),expression(F/F[MSY]),"Biomass (t)","Fishing Mortality","Process Deviation","Catch (t)",expression(B/B[0]), bflab)
  } else {
    ylab.default = FALSE
  }
  
  refquants=c("stock","harvest","B","H","Bdev","Catch","BB0","BBfrac")
  
  # for defaul multiplot use always 6 subplots in order
  if(!single.plots){
  subplots = c(subplots,(1:8)[(1:8)%in%subplots==F])[1:6]  
  }
  
  kb = addBfrac(kb,bfrac=bfrac,bref=bref,quantiles=quantiles)$kb
  
  kbs = aggregate(cbind(stock,harvest,B,H,Bdev,Catch,BB0,BBfrac)~year+run,kb,
                   quantile, c(0.5,quantiles))
  
  n             <- length(unique(kbs$run))
  startyrs      <- min(kbs$year)
  endyrs        <- max(kbs$year)
  years         <- unique(kbs$year)
  
  if(is.null(col)){
    col = ss3col(n,1)
    shadecol <- ss3col(n,shadealpha)
  } else {
    shadecol= adjustcolor(col, alpha.f=shadealpha)
  }
  
  
  quants =  refquants[subplots]
 
  pngfun <- function(file){
    
    
    # if extra text requested, add it before extention in file name
    file <- paste0(filenameprefix, file)
    # open png file
    png(filename=file.path(plotdir,file),
        width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
    # change graphics parameters to input value
    par(par)
  }
  
  print=FALSE
  if(as.png) print <- TRUE
  if(as.png & is.null(plotdir)) plotdir = getwd()
  
  if(!single.plots){
  Par = list(mfrow=c(3,2),mai=c(0.45,0.49,0.1,.15),omi = c(0.15,0.15,0.1,0) + 0.1,mgp=c(2,0.5,0), tck = -0.02,cex=0.8)
  if(as.png==TRUE){png(filename = paste0(output.dir,"/",prefix,"_",jbs$assessment,".png"), width = 7, height = 8,
                       res = 200, units = "in")}
  par(Par)
  }
  
  plot_quants <- function(quant="Bdev"){  
   
    if(single.plots){ 
    if(as.png) print <- TRUE
    if(as.png & is.null(plotdir))
      stop("to print PNG files, you must supply a directory as 'plotdir'")
    }
    #-------------------------------------------------------------
    # plot function
    #-------------------------------------------------------------
    # get stuff from summary output (minimized)
    y = kbs[,quant]   
    Yr = kbs$year
    exp      <- y[,1]
    lower    <- y[,2]
    upper <- y[,3]
    models <- 1:n    
    nlines <- length(models) 
    runs = unique(kb$run)[models]
    
   
    # if line stuff is shorter than number of lines, recycle as needed
    if(length(col) < nlines) col <- rep(col,nlines)[1:nlines]
    if(length(pch) < nlines) pch <- rep(pch,nlines)[1:nlines]
    if(length(lty) < nlines) lty <- rep(lty,nlines)[1:nlines]
    if(length(lwd) < nlines) lwd <- rep(lwd,nlines)[1:nlines]
    
    
    # subfunction to add legend
    legendfun <- function(legendlabels,cumulative=FALSE) {
      if(cumulative){
        legend.loc="topleft"
      }
      if(is.numeric(legend.loc)) {
        Usr <- par()$usr
        legend.loc <- list(x = Usr[1] + legend.loc[1] * (Usr[2] - Usr[1]),
                          y = Usr[3] + legend.loc[2] * (Usr[4] - Usr[3]))
      }
      
      # if type input is "l" then turn off points on top of lines in legend
      legend.pch <- pch
      if(type=="l"){
        legend.pch <- rep(NA,length(pch))
      }
      legend(legend.loc, legend=legendlabels[legendorder],
             col=col[legendorder], lty=lty[legendorder],seg.len = 2,
             lwd=lwd[legendorder], pch=legend.pch[legendorder], bty="n", ncol=legendncol,pt.cex=0.7,cex=legendcex,y.intersp = legendsp)
    }
    
    
    if(!is.expression(legendlabels[1]) &&
       legendlabels[1]=="default") legendlabels <- runs
    if(legendorder[1]=="default") legendorder <- 1:(nlines)
    
    
    
    # open new window if requested
    if(single.plots){
    if(plot & as.png==FALSE){
      if(!add) par(par)
      
    } else {
      
      if(!add) par(par)
    }
    }
    
   if(is.null(xlim)) xlim = c(max(min(years)),max(years)) 
    xmin = min(xlim)
    ylim <- c(0,max(ifelse(plotCIs,max(upper[Yr>=xmin])*ylimAdj, ylimAdj*max(exp[Yr>=xmin])*1.05)))
    if(quant=="Bdev") ylim <- c(-max(ifelse(plotCIs,max(c(0.2,upper[Yr>=xmin],abs(lower[Yr>=xmin])))*ylimAdj, ylimAdj*max(abs(exp[Yr>=xmin]))*1.05)),max(0.2,ifelse(plotCIs,max(c(upper[Yr>=xmin],abs(lower[Yr>=xmin])))*ylimAdj, ylimAdj*max(abs(exp[Yr>=xmin]))*1.05)))
    
      
    if(ylab.default){
    ylab = ylabs[which(refquants%in%quant)]} else {
    ylab = ylabs[which(quants%in%quant)]  
    }
    
    
    plot(0, type = "n", xlim = xlim, yaxs = yaxs, 
         ylim = ylim, xlab = ifelse(xylabs,"Year",""), ylab = ifelse(xylabs,ylab,""), axes = FALSE,cex.lab=0.9)
    
    if(plotCIs){
    for(iline in nlines:1){
    yr <- kbs[kbs$run==runs[iline],]$year  
    if(quant%in%c("B","stock","harvest","H","Bdev","BB0","Catch","BBfrac")){  
       polygon(c(yr,rev(yr)),c(lower[kbs$run == runs[iline]],rev(upper[kbs$run == runs[iline]])),col=shadecol[iline],border=shadecol)
    } else {
      adj <- 0.2*iline/nlines - 0.1
      arrows(x0=yr+adj, y0=lower[kbs$run == runs[iline]],
      x1=yr+adj, y1=upper[kbs$run == runs[iline]],
      length=0.02, angle=90, code=3, col=col[iline])
    }}
    }
    
    for(iline in 1:nlines){
      yr <- kbs[kbs$run==runs[iline],]$year  
      if(quant%in%c("B","stock","harvest","H","Bdev","BB0","Catch","BBfrac")){
        lines(yr,exp[kbs$run == runs[iline]],col=col[iline],pch=pch[iline],lty=lty[iline],lwd=lwd[iline],type="l")
      } else {
        points(yr,exp[kbs$run == runs[iline]],col=col[iline],pch=16,cex=0.8)
      } 
   
    }  
    if(quant == "stock") abline(h=1,lty=2)
    if(quant == "harvest") abline(h=1,lty=2)
    if(quant == "Bdev") abline(h=0,lty=2)
    if(quant == "BBfrac") abline(h=1,lty=2,col=bref.col)
    
    if(reflines){
      #if(quant == "B") abline(h=median(kb$B/kb$stock),lty=2,col=1) 
      #if(quant == "B" & !is.null(bfrac)) abline(h=median(kb$Bref),lty=2,col=bref.col)
      if(quant == "stock" & !is.null(bfrac)) abline(h=median(kb$Bref/(kb$B/kb$stock)),lty=2,col=bref.col) 
      #if(quant == "stock" & !is.null(bfrac)) abline(h=median(kb$Bref/(kb$B/kb$stock)),lty=2,col=bref.col)
      #if(quant == "H") abline(h=median(kb$H/kb$harvest),lty=2,col=1) 
      #if(quant == "BB0") abline(h=median((kb$B/kb$stock)/(kb$B/kb$BB0)),lty=2,col=1) 
      #if(quant == "BB0" & !is.null(bfrac)) abline(h=median(kb$Bref/(kb$B/kb$BB0)),lty=2,col=bref.col) 
    }
    
    if(single.plots | quants[s]==quants[1]){
    if(legend){
      # add legend if requested
      
      legendfun(legendlabels)
    }
    }
    #axis(1, at=c(min(xmin,min(yr)):max(endyrvec)))
    axis(1,cex.axis=0.8)
    
    if(tickEndYr) axis(1, at=max(xlim[2]),cex.axis=0.8)
    
    axis(2,cex.axis=0.8)
    box()
  } # End of plot_quant function  
  legend.temp = legend  
  # Do plotting
  if(plot){ 
    # subplots
    for(s in 1:length(subplots)){
    if(print & single.plots){
    quant=quants[s]
    par(par)
    pngfun(paste0("ModelComp_",quant,".png",sep=""))
    plot_quants(quant)
    dev.off()
    }
    }
    # subplots
    for(s in 1:length(subplots)){
      if(verbose) cat(paste0("\n","Plot Comparison of ",quants[s],"\n"))
      
    if(!add & single.plots)par(par)
    quant=quants[s]
    plot_quants(quant)   
    }
  } # endplot
  if(kbout) return(kb)
} 

#' Extract reference points data from fitted models
#' 
#' Retrieves the data frame containing reference points (refpts) extracted from
#' one or more fitted JABBA models returned by \code{fit_jabba()}.
#' 
#' @param list_fit_models A list of fitted model objects returned by 
#'   \code{fit_jabba()}, or a single fitted model object.
#' 
#' @return A combined data frame containing reference points (refpts) for all
#'   fitted models provided in \code{list_fit_models}.
#' 
#' @details
#' If a single fitted model is provided, it is automatically wrapped into a list
#' to ensure consistent processing. The function extracts the \code{refpts}
#' component from each model and binds them by rows into a single data frame.
#' 
#' This function is a convenience accessor to facilitate comparison and further
#' analysis of reference points across multiple fitted models.
#' 
#' @export
#' @importFrom dplyr bind_rows
get_refpts <- function(list_fit_models) {
  if (.is_fit_jabba(list_fit_models)) {
    list_fit_models <- list(list_fit_models)
  }

  temp00 <- lapply(
    list_fit_models,
    function(fit) {
      fit$refpts
    }
  )
  temp00 <- bind_rows(temp00) %>%
    mutate(
      across(
        c(k, bmsy, fmsy, msy),
        ~ifelse(quant == "logse", exp(.x), .x)
      )
    )

  return(temp00)
}

#' Extract parameters data from fitted models
#' 
#' Retrieves a combined data frame containing model parameters extracted from
#' one or more fitted JABBA models returned by \code{fit_jabba()}.
#' 
#' @param list_fit_models A list of fitted model objects returned by 
#'   \code{fit_jabba()}, or a single fitted model object.
#' 
#' @return A data frame containing parameter estimates for each model, including
#'   the parameter name and associated scenario.
#' 
#' @details
#' If a single fitted model is provided, it is automatically wrapped into a list
#' to ensure consistent processing. For each model, the \code{pars} component is
#' converted to a data frame, with row names extracted as an \code{indicator}
#' column and the model \code{scenario} appended as an additional column.
#' 
#' The resulting data frames are combined by rows into a single data frame,
#' facilitating comparison of parameter estimates across scenarios or models.
#' 
#' @importFrom dplyr bind_rows
#' @export
get_pars <- function(list_fit_models) {
  if (.is_fit_jabba(list_fit_models)) {
    list_fit_models <- list(list_fit_models)
  }

  temp00 <- lapply(
    list_fit_models,
    function(fit) {
      df <- as.data.frame(fit$pars)
      df$indicator <- rownames(df)
      df$scenario <- fit$scenario

      rownames(df) <- NULL
      df[, c("scenario", "indicator", setdiff(names(df), 
      c("scenario", "indicator")))]
    }
  )
  temp00 <- bind_rows(temp00) 

  return(temp00)
}
