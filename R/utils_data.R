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

#' Format rho values into a data frame
#'
#' Internal helper that converts rho outputs into a structured 
#' data frame with index labels.
#'
#' @param rho A matrix or data frame containing rho values.
#'
#' @return A data frame with indices and corresponding rho values.
#'
#' @keywords internal
.extract_rhos <- function(rho) {
  vec01 <- as.numeric(rho[nrow(rho),])
  vec02 <- c("B", "F", "BBmsy", "FFmsy", "procB", "MSY")
  vec03 <- c(
    "Biomass", "Fishing Mortality", "B/Bmsy", "F/Fmsy",
    "Process Error on log(Biomass)", "Surplus Production"
  )
  result <- data.frame("Index" = vec02, "Index2" = vec03, "rho" = vec01)
  return(result)
}

#' Fill missing index values
#'
#' Internal helper that replaces missing values in the Index column using the 
#' set of expected indices from the input series.
#'
#' @param data A data frame containing an Index column.
#' @param index_inputseries A character vector with expected index names.
#'
#' @return The input data frame with missing Index values filled.
#'
#' @keywords internal
.fill_na_indices <- function(data, index_inputseries) {
  index_data <- unique(data$Index)
  index_inputseries <- index_inputseries[!index_inputseries == "year"]
  NA_index <- setdiff(index_inputseries, index_data)
  data$Index[is.na(data$Index)] <- NA_index
}

#' Filter data based on conditional transitions
#'
#' Internal helper that filters grouped data by identifying the first
#' occurrence of a logical condition and returning selected rows around that 
#' transition.
#'
#' @param df A data frame.
#' @param group_col A character string specifying the grouping column.
#' @param condition_col A character string specifying the logical
#'   condition column.
#' @param year_col A character string specifying the time variable.
#'
#' @return A filtered data frame containing selected observations.
#'
#' @keywords internal
#' @importFrom dplyr group_by across all_of group_modify filter ungroup
.filter_by_condition <- function(df, group_col, condition_col, year_col) {
  df %>%
    group_by(across(all_of(group_col))) %>%
    group_modify(~ {
      data_group <- .x
      first_true_index <- which(data_group[[condition_col]] == TRUE)[1]
      if (!is.na(first_true_index) && first_true_index > 1) {
          target_years <-c(data_group[[year_col]][first_true_index - 1],
            data_group[[year_col]][first_true_index])
          data_group %>% filter(data_group[[year_col]] %in% target_years)
      } else {
          data_group[0,]
      }
    }) %>%
    ungroup()
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
    return(FALSE)
  }

  if (length(obj) == 0) return(FALSE)
  
  elem <- obj[[1]]

  .is_fit_jabba(elem)
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
  new=TRUE,add=FALSE,single.plots = add,run=NULL,fmax=5.0)
  {
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

#' .jbplot_retro2() to plot retrospective pattern
#'
#' Plots retrospective pattern of B, F, BBmsy, FFmsy, BB0 and SP #'
#' @param hc output list from hindast_jabba()
#' @param type  single plot option c("B","F","BBmsy","FFmsy","BB0","SP")
#' @param forecast  includes retrospective forecasting if TRUE
#' @param ylabs yaxis labels for quants
#' @param add  add to multi plot if TRUE
#' @param output.dir directory to save plots
#' @param as.png save as png file of TRUE
#' @param single.plots if TRUE plot invidual fits else make multiplot
#' @param width plot width
#' @param height plot hight
#' @param xlim  allows to "zoom-in" requires speficiation Xlim=c(first.yr,last.yr)
#' @param cols option to add colour palette 
#' @param legend.loc location of legend
#' @param verbose if FALSE be silent
#' @param rhoout if TRUE, produces the rho data.frame as output 
#' @param plot if TRUE, produces the plots
#' @return Mohn's rho statistic for several quantaties
#' @keywords internal
#' @examples 
#' \dontrun{
#' if (requireNamespace("JABBA", quietly = TRUE)) {
#'   data("iccat", package = "JABBA")
#'   bet <- iccat$bet
#'   
#'   .jbplot_retro2(bet)
#' }
#' }
.jbplot_retro2 <- function (hc, type = c("B", "F", "BBmsy", "FFmsy", "procB", "SP"), 
    forecast = FALSE, ylabs = NULL, add = F, output.dir = getwd(), 
    as.png = FALSE, single.plots = add, width = NULL, height = NULL, 
    xlim = NULL, cols = NULL, legend.loc = "topright", verbose = TRUE,
    rhoout = TRUE, plot = TRUE) 
{
    hc.ls = hc
    peels = as.numeric(do.call(c, lapply(hc.ls, function(x) {
        x$diags$retro.peels[1]
    })))
    Ref = hc.ls[[1]]
    hc = list(scenario = Ref$scenario, yr = Ref$yr, catch = Ref$catch, 
        peels = NULL, timeseries = NULL, refpts = NULL, pfunc = NULL, 
        diags = NULL, settings = Ref$settings)
    for (i in 1:length(peels)) {
        hc.ls[[i]]$pfunc$level = peels[i]
        hc.ls[[i]]$refpts$level = peels[i]
        hc$timeseries$mu = rbind(hc$timeseries$mu, data.frame(factor = hc.ls[[i]]$diags[1, 
            1], level = peels[i], hc.ls[[i]]$timeseries[, "mu", 
            ]))
        hc$timeseries$lci = rbind(hc$timeseries$lci, data.frame(factor = hc.ls[[i]]$diags[1, 
            1], level = peels[i], hc.ls[[i]]$timeseries[, "lci", 
            ]))
        hc$timeseries$uci = rbind(hc$timeseries$uci, data.frame(factor = hc.ls[[i]]$diags[1, 
            1], level = peels[i], hc.ls[[i]]$timeseries[, "uci", 
            ]))
        hc$diags = rbind(hc$diags, hc.ls[[i]]$diags)
        hc$refpts = rbind(hc$refpts, hc.ls[[i]]$refpts[1, ])
        hc$pfunc = rbind(hc$pfunc, hc.ls[[i]]$pfunc)
    }
    if (verbose) 
        cat(paste0("\n", "><> jbplot_retro() - retrospective analysis <><", 
            "\n"))
    if (add) 
        single.plots = TRUE
    if (single.plots == F) 
        type = c("B", "F", "BBmsy", "FFmsy", "procB", "SP")
    if (is.null(ylabs)) 
        ylabs = c(paste("Biomass", hc$settings$catch.metric), 
            "Fishing mortality F", expression(B/B[MSY]), expression(F/F[MSY]), 
            expression(B/B[0]), "Process Deviations", paste("Surplus Production", 
                hc$settings$catch.metric))
    retros = unique(peels)
    runs = hc$timeseries$mu$level
    years = hc$yr
    nyrs = length(years)
    if (is.null(cols)) 
        cols = c("black", ss3col(length(peels) - 1))
    if (is.null(xlim)) {
        xlim = range(years)
    }
    FRP.rho = c("B", "F", "Bmsy", "Fmsy", "procB", "MSY")
    rho = data.frame(mat.or.vec(length(retros) - 1, length(FRP.rho)))
    colnames(rho) = FRP.rho
    fcrho = rho
    for (k in 1:length(type)) {
        j = which(c("B", "F", "BBmsy", "FFmsy", "BB0", "procB", 
            "SP") %in% type[k])
        if (type[k] %in% c("B", "F", "BBmsy", "FFmsy", "procB")) {
            y = hc$timeseries$mu[, j + 2]
            ref = hc$timeseries$mu[runs %in% retros[1], j + 
              2]
            ylc = hc$timeseries$lci[runs %in% retros[1], 
              j + 2]
            yuc = hc$timeseries$uci[runs %in% retros[1], 
              j + 2]
            for (i in 1:length(retros)) {
              if (i > 1) {
                rho[i - 1, k] = (y[runs %in% retros[i]][(nyrs - 
                  retros[i])] - ref[(nyrs - retros[i])])/ref[(nyrs - 
                  retros[i])]
                fcrho[i - 1, k] = (y[runs %in% retros[i]][(nyrs + 
                  1 - retros[i])] - ref[(nyrs + 1 - retros[i])])/ref[(nyrs + 
                  1 - retros[i])]
                if (type[k] == "procB") {
                  rho[i - 1, k] = (exp(y[runs %in% retros[i]][(nyrs - 
                    retros[i])]) - exp(ref[(nyrs - retros[i])]))/exp(ref[(nyrs - 
                    retros[i])])
                  if (single.plots == TRUE) {
                    fcrho[i - 1, k] = (exp(y[runs %in% retros[i]][(nyrs + 
                    1 - retros[i])]) - exp(ref[(nyrs + 1 - 
                    retros[i])]))/exp(ref[(nyrs + 1 - retros[i])])
                  }
                  else {
                    fcrho[i - 1, k] = (exp(y[runs %in% retros[i]][(nyrs - 
                      retros[i])]) - exp(ref[(nyrs + 1 - retros[i])]))/exp(ref[(nyrs + 
                      1 - retros[i])])
                  }
                }
              }
            }
        }
        else {
            for (i in 1:length(retros)) {
              if (i > 1) {
                rho[i - 1, 6] = (hc$refpts$msy[hc$refpts$level == 
                  retros[i]] - hc$refpts$msy[hc$refpts$level == 
                  retros[1]])/hc$refpts$msy[hc$refpts$level == 
                  retros[1]]
                fcrho[i - 1, 6] = NA
              }
            }
        }
    }
    if (plot) {
        if (single.plots == TRUE) {
            if (is.null(width)) 
                width = 5
            if (is.null(height)) 
                height = 3.5
            for (k in 1:length(type)) {
                Par = list(mfrow = c(1, 1), mar = c(3.5, 3.5, 0.5, 
                  0.1), mgp = c(2, 0.5, 0), tck = -0.02, cex = 0.8)
                if (as.png == TRUE) {
                    png(filename = paste0(output.dir, "/Retro", hc$scenario, 
                      "_", type[k], ".png"), width = width, height = height, 
                      res = 200, units = "in")
                }
                if (as.png == TRUE | add == FALSE) 
                    par(Par)
                if (type[k] %in% c("B", "F", "BBmsy", "FFmsy", "procB")) {
                    if (type[k] == "procB") 
                      ylim = c(-max(y[years >= xlim[1] & years <= 
                        xlim[2]], yuc[years >= xlim[1] & years <= 
                        xlim[2]]), max(y[years >= xlim[1] & years <= 
                        xlim[2]], yuc[years >= xlim[1] & years <= 
                        xlim[2]]))
                    if (!type[k] == "procB") 
                      ylim = c(0, max(y[years >= xlim[1] & years <= 
                        xlim[2]], yuc[years >= xlim[1] & years <= 
                        xlim[2]]))
                    plot(years, years, type = "n", ylim = ylim, ylab = ifelse(length(ylabs) > 
                      1, ylabs[j], ylabs), xlab = "Year", xlim = xlim)
                    polygon(c(years, rev(years)), c(ylc, rev(yuc)), 
                      col = "grey", border = "grey")
                    for (i in 1:length(retros)) {
                      lines(years[1:(nyrs - retros[i])], y[runs %in% 
                        retros[i]][1:(nyrs - retros[i])], col = cols[i], 
                        lwd = ifelse(i == 1, 2, 1.5), lty = 1)
                      if (forecast) {
                        lines(years[(nyrs - retros[i]):(nyrs + 1 - 
                          retros[i])], y[runs %in% retros[i]][(nyrs - 
                          retros[i]):(nyrs + 1 - retros[i])], col = cols[i], 
                          lwd = 1, lty = 2)
                        points(years[(nyrs + 1 - retros[i])], y[runs %in% 
                          retros[i]][(nyrs + 1 - retros[i])], pch = 16, 
                          col = cols[i], cex = 0.8)
                      }
                    }
                    if (type[k] %in% c("BBmsy", "FFmsy")) 
                      abline(h = 1, lty = 2)
                    if (type[k] %in% c("procB")) 
                      abline(h = 0, lty = 2)
                }
                else {
                    plot(years, years, type = "n", ylim = c(0, max(hc$pfunc$SP * 
                    1.12)), xlim = c(0, max(hc$pfunc$SB_i)), ylab = ifelse(length(ylabs) > 
                    1, ylabs[j], ylabs), xlab = ylabs[1])
                    for (i in 1:length(retros)) {
                      lines(hc$pfunc$SB_i[hc$pfunc$level %in% retros[i]], 
                        hc$pfunc$SP[hc$pfunc$level %in% retros[i]], 
                        col = cols[i], lwd = ifelse(i == 1, 2, 1.5), 
                        lty = 1)
                      points(mean(hc$pfunc$SB_i[hc$pfunc$level %in% 
                        retros[i]][hc$pfunc$SP[hc$pfunc$level %in% 
                        retros[i]] == max(hc$pfunc$SP[hc$pfunc$level %in% 
                        retros[i]])]), max(hc$pfunc$SP[hc$pfunc$level %in% 
                        retros[i]]), col = cols[i], pch = 16, cex = 1.2)
                    }
                }
                if (single.plots == TRUE | k == 1) 
                    legend(legend.loc, paste(years[nyrs - retros]), 
                      col = cols, bty = "n", cex = 0.7, pt.cex = 0.7, 
                      lwd = c(2, rep(1.5, length(retros))))
                if (!forecast) 
                    legend("top", legend = bquote(rho == .(round(mean(rho[, 
                      k]), 2))), bty = "n", x.intersp = -0.2, y.intersp = -0.3, 
                      cex = 0.8)
                if (forecast) 
                    legend("top", legend = bquote(rho == .(round(mean(rho[, 
                      k]), 2)) ~ "(" ~ .(round(mean(fcrho[, k]), 
                      2)) ~ ")"), bty = "n", x.intersp = -0.2, y.intersp = -0.3, 
                      cex = 0.8)
            }
        }
        else {
            if (is.null(width)) 
                width = 7
            if (is.null(height)) 
                height = 8
            Par = list(mfrow = c(3, 2), mai = c(0.45, 0.49, 0.1, 
                0.15), omi = c(0.15, 0.15, 0.1, 0) + 0.1, mgp = c(2, 
                0.5, 0), tck = -0.02, cex = 0.8)
            if (as.png == TRUE) {
                png(filename = paste0(output.dir, "/Retro_", hc$scenario, 
                    ".png"), width = width, height = height, res = 200, 
                    units = "in")
            }
            par(Par)
            for (k in 1:length(type)) {
                if (type[k] %in% c("B", "F", "BBmsy", "FFmsy", "procB")) {
                    if (type[k] == "procB") 
                      ylim = c(-max(y[years >= xlim[1] & years <= 
                        xlim[2]], yuc[years >= xlim[1] & years <= 
                        xlim[2]]), max(y[years >= xlim[1] & years <= 
                        xlim[2]], yuc[years >= xlim[1] & years <= 
                        xlim[2]]))
                    if (!type[k] == "procB") 
                      ylim = c(0, max(y[years >= xlim[1] & years <= 
                        xlim[2]], yuc[years >= xlim[1] & years <= 
                        xlim[2]]))
                    plot(years, years, type = "n", ylim = ylim, ylab = ylabs[j], 
                      xlab = "Year", xlim = xlim)
                    polygon(c(years, rev(years)), c(ylc, rev(yuc)), 
                      col = "grey", border = "grey")
                    for (i in 1:length(retros)) {
                      lines(years[1:(nyrs - retros[i])], y[runs %in% 
                        retros[i]][1:(nyrs - retros[i])], col = cols[i], 
                        lwd = ifelse(i == 1, 2, 1.5), lty = 1)
                      if (forecast) {
                        lines(years[(nyrs - retros[i]):(nyrs + 1 - 
                          retros[i])], y[runs %in% retros[i]][(nyrs - 
                          retros[i]):(nyrs + 1 - retros[i])], col = cols[i], 
                          lwd = 1, lty = 2)
                        points(years[(nyrs + 1 - retros[i])], y[runs %in% 
                          retros[i]][(nyrs + 1 - retros[i])], pch = 16, 
                          col = cols[i], cex = 0.8)
                      }
                    }
                    if (type[k] %in% c("BBmsy", "FFmsy")) 
                      abline(h = 1, lty = 2)
                    if (type[k] %in% c("procB")) 
                      abline(h = 0, lty = 2)
                    if (single.plots == TRUE | k == 1) 
                      legend(legend.loc, paste(years[nyrs - retros]), 
                        col = cols, bty = "n", cex = 0.7, pt.cex = 0.7, 
                        lwd = c(2, rep(1.5, length(retros))))
                    if (!forecast) 
                      legend("top", legend = bquote(rho == .(round(mean(rho[, 
                        k]), 2))), bty = "n", x.intersp = -0.2, y.intersp = -0.3, 
                        cex = 0.8)
                    if (forecast) 
                      legend("top", legend = bquote(rho == .(round(mean(rho[, 
                        k]), 2)) ~ "(" ~ .(round(mean(fcrho[, k]), 
                        2)) ~ ")"), bty = "n", x.intersp = -0.2, 
                        y.intersp = -0.3, cex = 0.8)
                }
                else {
                    plot(years, years, type = "n", ylim = c(0, max(hc$pfunc$SP * 
                      1.15)), xlim = c(0, max(hc$pfunc$SB_i)), ylab = ylabs[j], 
                      xlab = ylabs[1])
                    for (i in 1:length(retros)) {
                      lines(hc$pfunc$SB_i[hc$pfunc$level %in% retros[i]], 
                        hc$pfunc$SP[hc$pfunc$level %in% retros[i]], 
                        col = cols[i], lwd = ifelse(i == 1, 2, 1.5), 
                        lty = 1)
                      points(mean(hc$pfunc$SB_i[hc$pfunc$level %in% 
                        retros[i]][hc$pfunc$SP[hc$pfunc$level %in% 
                        retros[i]] == max(hc$pfunc$SP[hc$pfunc$level %in% 
                        retros[i]])]), max(hc$pfunc$SP[hc$pfunc$level %in% 
                        retros[i]]), col = cols[i], pch = 16, cex = 1.2)
                    }
                    legend("top", legend = bquote(rho == .(round(mean(rho[, 
                      k]), 2))), bty = "n", x.intersp = -0.2, y.intersp = -0.3, 
                      cex = 0.8)
                }
            }
        }
        if (as.png == TRUE) 
            dev.off()
    }
    rho = rbind(rho, apply(rho, 2, mean))
    rownames(rho) = c(rev(years)[retros[-1]], "rho.mu")
    fcrho = rbind(fcrho, apply(fcrho, 2, mean))
    rownames(fcrho) = c(rev(years)[retros[-1]], "forecastrho.mu")
    if (forecast) {
        out = list()
        out$Mohns.rho = rho
        out$Forecast.rho = fcrho
    }
    else {
        out = rho
    }
    if (rhoout) 
        return(out)
}

#' Extract and combine CPUE data
#'
#' Internal helper that extracts CPUE-related outputs from a list of model 
#' results, converts array-based data into data frames, and combines them into 
#' a single structure.
#'
#' @param fit_list A list of model outputs.
#' @param vars A character string indicating which CPUE output to extract.
#'   Supported values are "ppd" and "hat".
#'
#' @return A data frame containing combined CPUE data across scenarios.
#'
#' @keywords internal
#' @importFrom dplyr bind_rows
.process_cpues <- function(fit_list, vars) {
  temp00 <- lapply(fit_list, function(fit) {
    cbind.data.frame(
      Scenario = fit$scenario,
      if (vars == "ppd") {
          .array_to_dataframe(fit$cpue.ppd)
      } else {
          .array_to_dataframe(fit$cpue.hat)
      }
    )
  })
  result <- bind_rows(temp00)
  return(result)
}

#' Extract MASE data from hindcast results
#' 
#' Retrieves the data frame containing MASE (Mean Absolute Scaled Errors) values
#' for all indices and scenarios, as returned by \code{hindcast_data()}.
#' 
#' @param df_lists A named list object returned by \code{hindcast_data()}, which 
#'   must contain a component named \code{"mase_data"}.
#' 
#' @return A data frame containing Mean Absolute Scaled Error (MASE) metrics 
#' for each index and scenario.
#' 
#' @details
#' The returned data frame is in wide format, with one row per combination of 
#' Index and Scenario. This function is a convenience acessor for extracting
#' MASE results for further analysis or visualization.
#' 
#' @export
get_mase <- function(df_lists) {
  return(df_lists$mase_data)
}

#' Extract hindcast diagnostics from model outputs
#'
#' Internal helper that extracts hindcast diagnostic data from model outputs 
#' and combines them into a single data frame.
#'
#' @param fit_list A list of model outputs.
#'
#' @return A data frame containing hindcast diagnostics across scenarios.
#'
#' @keywords internal
#' @importFrom dplyr bind_rows
.process_hindcasts <- function(fit_list) {
  temp00 <- lapply(
    fit_list,
    function(fit) {
      temp01 <- lapply(
        names(fit),
        function(nm) {
          data <- fit[[nm]]
          peel <- ifelse(grepl("^-", nm), nm, "Ref")
          data.frame(
            Peel = peel,
            data$diags
          )
        }
      )
      bind_rows(temp01)
    }
  )
  bind_rows(temp00)
}

#' Extract index names from model inputs
#'
#' Internal helper that extracts CPUE index names from the input series of each 
#' model in the list and returns the unique set of indices.
#'
#' @param fit_list A list of model outputs.
#'
#' @return A character vector containing unique index names across models.
#'
#' @keywords internal
.process_index <- function(fit_list) {
  temp00 <- lapply(fit_list, function(fit) {
    names(fit$inputseries$cpue)
  })
  return(unique(unlist(temp00)))
}

#' Compute MASE diagnostics
#'
#' Internal helper that computes Mean Absolute Scaled Error (MASE) metrics from 
#' model outputs.
#'
#' @param fit_list A list of model outputs.
#'
#' @return A data frame containing MASE values for each scenario, including 
#' plotting coordinates.
#'
#' @keywords internal
#' @importFrom dplyr bind_rows everything
#' @importFrom JABBA jbmase
.process_mase <- function(fit_list) {
  temp00 <- lapply(
    fit_list,
    function(fit) {
      jbmase(fit) %>% 
        mutate(
          Scenario = fit[[1]]$scenario
        )
    }
  )
  result <- bind_rows(temp00) %>% 
    filter(Index != "joint") %>%
    select(Scenario, everything())
  return(result)
}

#' Extract surplus production data
#'
#' Internal helper that extracts surplus production function outputs from 
#' retrospective model runs.
#'
#' @param hc_list A list of retrospective model outputs.
#'
#' @return A data frame containing surplus production data.
#'
#' @keywords internal
#' @importFrom dplyr bind_rows
.process_pfunc <- function(hc_list) {
  temp00 <- lapply(
    hc_list,
    function(hc) {
      temp01 <- lapply(
        names(hc),
        function(nm) {
          peel <- ifelse(grepl("^-", nm), nm, "Ref")
          cbind.data.frame(
            id = peel,
            Scenario = hc[[nm]]$scenario,
            hc[[nm]]$pfunc
          )
        }
      )
    }
  )
  result <- bind_rows(temp00)

  return(result)
}

#' Extract posterior samples from model outputs
#'
#' Internal helper that extracts posterior parameter samples from a list of 
#' model outputs and combines them into a single data frame.
#'
#' @param fit_list A list of model outputs.
#'
#' @return A data frame containing posterior samples for each scenario.
#'
#' @keywords internal
#' @importFrom dplyr bind_rows
.process_posteriors <- function(fit_list) {
  temp00 <- lapply(
    fit_list,
    function(fit) {
      cbind.data.frame(
        Scenario = fit$scenario,
        fit$pars_posterior
      )
    }
  )
  result <- bind_rows(temp00)
  return(result)
}

#' Extract prior settings from model outputs
#'
#' Internal helper that extracts prior distribution parameters from a list of 
#' model outputs and combines them into a single data frame.
#'
#' @param fit_list A list of model outputs.
#'
#' @return A data frame containing prior parameters for each scenario.
#'
#' @keywords internal
#' @importFrom dplyr bind_rows
.process_priors <- function(fit_list) {
  temp00 <- lapply(
    fit_list,
    function(fit) {
      cbind.data.frame(
        Scenario = fit$scenario,
        K.pr = fit$settings$K.pr,
        r.pr = fit$settings$r.pr,
        psi.pr = fit$settings$psi.pr,
        psi.dist = fit$settings$psi.dist,
        proc.pr = fit$settings$igamma
      )
    }
  )
  result <- bind_rows(temp00)
  return(result)
}

#' Extract retrospective time series
#'
#' Internal helper that extracts time series data from retrospective model runs 
#' and reshapes them into a combined data frame.
#'
#' @param hc_list A list of retrospective model outputs.
#'
#' @return A data frame containing time series across scenarios and runs.
#'
#' @keywords internal
#' @importFrom dplyr bind_rows
.process_retro <- function(hc_list) {
  temp00 <- lapply(
    hc_list,
    function(hc) {
      temp01 <- lapply(
        names(hc),
        function(nm) {
          peel <- ifelse(grepl("^-", nm), nm, "Ref")
          cbind.data.frame(
            id = peel,
            Scenario = hc[[nm]]$scenario,
            .array_to_dataframe(hc[[nm]]$timeseries)
          )
        }
      )
    }
  )
  result <- bind_rows(temp00)
  return(result)
}

#' Extract and combine residuals data
#' 
#' Internal helper that extracts residuals from each model, converts them into 
#' a data frame format, and combines them across scenarios.
#'
#' @param fit_list A list of model outputs.
#'
#' @return A data frame containing residuals by year and scenario.
#' 
#' @keywords internal
#' @importFrom dplyr bind_rows
.process_runs <- function(fit_list) {
  temp00 <- lapply(
    fit_list,
    function(fit) {
      cbind.data.frame(
        Year = fit$yr,
        Scenario = fit$scenario,
        Ref = 0,
        t(fit$residuals)
      )
    }
  )
  result <- bind_rows(temp00)
  return(result)
}

#' Extract and combine scenario-level data
#'
#' Internal helper that extracts scenario-specific variables from a list of 
#' model outputs and combines them into a single data frame.
#'
#' @param fit_list A list of model outputs.
#' @param vars A character string indicating which variable to extract. 
#'   Supported values are "I" (index) and "SE2" (variance).
#'
#' @return A data frame containing year, scenario, and extracted variables.
#'
#' @keywords internal
#' @importFrom dplyr bind_rows
.process_scenarios <- function(fit_list, vars) {
  temp00 <- lapply(fit_list, function(fit) {
    cbind.data.frame(
      Year = fit$yr,
      Scenario = fit$scenario,
      if (vars == "I") {
        fit$settings$I
      } else {
        fit$settings$SE2
      }
    )
  })
  temp01 <- lapply(fit_list, function(fit) {
    c("Year", "Scenario", unique(fit$diags$name))
  })
  tmp01 <- mapply(.rename_columns, temp00, temp01, SIMPLIFY = FALSE)
  result <- bind_rows(tmp01)
  return(result)
}

#' Extract and combine model statistics
#'
#' Internal helper that extracts summary statistics from each model and 
#' combines them into a single data frame across scenarios.
#'
#' @param fit_list A list of model outputs.
#'
#' @return A data frame containing model statistics by scenario.
#' 
#' @keywords internal
#' @importFrom dplyr bind_rows
.process_stats <- function(fit_list) {
  temp00 <- lapply(
    fit_list,
    function(fit) {
      cbind.data.frame(
        Scenario = fit$scenario,
        fit$stats
      )
    }
  )
  result <- bind_rows(temp00)
  return(result)
}

#' Rename data frame columns
#'
#' Internal helper to assign new column names to a data frame.
#'
#' @param df A data frame.
#' @param col_names A character vector with new column names.
#'
#' @return The data frame with renamed columns.
#'
#' @keywords internal
#' @importFrom stats setNames 
.rename_columns <- function(df, col_names) {
  setNames(df, col_names)
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

#' Extract retrospective bias (rho) values
#'
#' Internal helper that computes retrospective bias statistics (rho) using 
#' JABBA outputs.
#'
#' @param hc_list A list of retrospective model outputs.
#'
#' @return A data frame containing rho values by index and scenario.
#'
#' @keywords internal
#' @importFrom dplyr bind_rows
#' @importFrom JABBA jbplot_retro
.rho_retro <- function(hc_list) {
  temp00 <- lapply(
    hc_list,
    function(hc) {
      cbind.data.frame(
        Scenario = names(hc[1]),
        .extract_rhos(
          .jbplot_retro2(hc, verbose = FALSE, rhoout = TRUE, plot = FALSE)
        )
      )
    }
  )
  result <- bind_rows(temp00)
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