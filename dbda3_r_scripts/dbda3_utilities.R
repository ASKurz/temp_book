# Utility R scripts for use with the book,
#   Kruschke, J. K. and Kurz, A. S. (2026). 
#   Doing Bayesian Data Analysis, Third Edition: 
#   A Tutorial with R, Stan, and the Tidyverse. Academic Press.
# This file contains several functions that are called by other programs
# or can be called directly by the user. To load all the functions into
# R's working memory, at R's command line type:
# source("DBDA3-utilities.R")

# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# !!! dbda3 package is loaded at end, to overwrite definitions here.
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#------------------------------------------------------------------------------



# MEMO TO US: HAVE ALL THE PACKAGES INSTALLED AUTOMATICALLY. CHECK IF NOT
# INSTALLED, THEN INSTALL IF NEEDED. PERHAPS IT'S EASIEST TO PUT THE DESIRED
# PACKAGES IN A want<-c(...) CHARACTER VECTOR, ETC., AS IN CODE COMMENTED OUT,
# BELOW. DO WE NEED TO LOAD JAGS AT ALL? IS IT EVER USED? IF NOT, REMOVE FROM
# LIST.

# Load JAGS first, so Stan packages can overwrite.
# # Check that required packages are installed:
# want = c("parallel","rjags","runjags","compute.es")
# have = want %in% rownames(installed.packages())
# if ( any(!have) ) { install.packages( want[!have] ) }
# Load rjags. Assumes JAGS is already installed.
try( library(rjags) )
# Load runjags. Assumes JAGS is already installed.
try( library(runjags) )
try( runjags.options( inits.warning=FALSE , rng.warning=FALSE ) )
# set default number of chains and parallelness for MCMC:
library(parallel) # for detectCores().
nCores = detectCores() # from package "parallel"
if ( !is.finite(nCores) ) { nCores = 1 } 
if ( nCores > 4 ) { 
  nChainsDefault = 4  # because JAGS has only 4 rng's.
  runjagsMethodDefault = "parallel"
}
if ( nCores == 4 ) { 
  nChainsDefault = 3  # save 1 core for other processes.
  runjagsMethodDefault = "parallel"
}
if ( nCores < 4 ) { 
  nChainsDefault = 3 
  runjagsMethodDefault = "rjags" # NOT parallel
}

#library( mvtnorm ) # Argh, conflict with standardize(), do not use.
library(phia) # load early, so other packages overwrite
library( emdbook ) # for dmvnorm()
library(MASS) # load before dplyr so dplyr functions are not masked
library(brms)
library(bayesplot)
library(bridgesampling)
library(colorspace)
library(cowplot)
library(distributional)
# devtools::install_github("mjskay/ggdist")
library(ggdist)
library(ggarrow)  # For `geom_arrow()`
library(ggforce)  # For `geom_shape()`
library(ggrepel) # for geom_label_repel
library(janitor)
library(kableExtra)
library(magick)
library(patchwork)
library(posterior)
library(rstan)
library(scales)
library(plyr) 
library(tidybayes)
library(tidyverse)
library(tinytable)
library(ellipse)

# -----------------------------------------------------------------------------
# Global colors and ggplot theme:

# Specify ggplot theme here:
# Adjust global plot theme.

## Set color palettes:

## Viridis:
# base_palette <- viridis_pal(option = "D")(5)
# palette_1 <- lighten( base_palette[2] , seq(0.4, 0, length=9) , space="HLS" )
# palette_2 <- lighten( base_palette[3] , seq(0.4, 0, length=9) , space="HLS" )

# For brewer options, run
# RColorBrewer::display.brewer.all()
# palette_1 <- brewer_pal(type = "seq", palette = "Greens")(9)
# palette_2 <- brewer_pal(type = "seq", palette = "Purples")(9)
# Also consider: brewer_pal() "Blues" "Greens" "Purples" "BuGn" "Reds" 

# domain_vec <- 1:10
# palette_1 <- col_numeric(
#   brewer_pal( type = "seq", palette = "RdBu")(11)[6:11], 
#   domain = domain_vec )(domain_vec)
# palette_2 <- col_numeric(
#   brewer_pal( type = "seq", palette = "RdBu")(11)[6:1], 
#   domain = domain_vec )(domain_vec)

palette_1 <- darken( "skyblue" , 0.5*c(0, 0.1, 0.3) , space="HCL" )
palette_2 <- darken( "plum" , 0.5*c(0, 0.1, 0.3) , space="HCL" )

fill_color_1 <- palette_1[1] 
fill_color_2 <- palette_2[1] 
border_color_1 <- palette_1[2] 
border_color_2 <- palette_2[2] 
line_color_1 <- palette_1[3] 
line_color_2 <- palette_2[3] 
point_color_1 <- line_color_1
point_color_2 <- line_color_2
facet_color_1 <- "turquoise" 
facet_color_2 <- "gold"
bg_color_1 <- lighten( "lightgray" , 0.5 , space="HCL" )
bg_color_2 <- lighten( facet_color_1 , 0.7 , space="HCL" )
text_color_1 <- "black"
text_color_2 <- darken(facet_color_1 , 0.7 , space="HCL" )

bayesplot::color_scheme_set( 
  # palette 1 at end because it is drawn last, over the previous colors:
  colorRampPalette( c( line_color_2 , line_color_1 ) )(6) 
)

show_palettes <- TRUE
if ( show_palettes ){
  bayesplot::color_scheme_view() 
}  
if ( show_palettes ){
  data.frame( color = c( palette_1 , 
                         palette_2 , 
                         facet_color_1 , facet_color_2 ,
                         bg_color_1 , bg_color_2 ,
                         text_color_1 , text_color_2 ) ,
              index = factor( c( 1:length(palette_1) ,
                                 1:length(palette_2) ,
                                 1:2 , 
                                 1:2 , 
                                 1:2 ) ) ,
              label = factor( c( rep("palette_1",length(palette_1)) ,
                                 rep("palette_2",length(palette_2)) ,
                                 rep("facet",2) , 
                                 rep("bg",2) , 
                                 rep("text",2) ) ,
                              ordered=TRUE ,
                              levels = c("palette_1", "palette_2", 
                                         "facet", "bg", "text") )
  ) %>% 
    ggplot(aes(x = index, y = label)) +
    geom_tile(aes(fill = color)) +
    scale_x_discrete(expand = expansion(mult = 0)) +
    scale_y_discrete(NULL, expand = expansion(mult = 0), limits=rev) +
    scale_fill_identity()
  
}


# base font size:
base_size <- 12  

theme_dbda <- function( base_size = 12 , ... ) {
  theme_gray( base_size = base_size , ... ) %+replace%
    theme(
      #---------------------------------------------------------------------------
      ### Uncomment one panel.background:
      ## Background only:
      panel.background = element_rect( fill=bg_color_1 , color=NA ) , 
      ## Frame only:
      # panel.background = element_rect( fill=NA , color=border_color_1 ) , 
      ## Background and Frame:
      # panel.background = element_rect( fill=bg_color_1 , color=border_color_1 ) , 
      ## Neither:
      # panel.background = element_rect( fill=NA , color=NA ) , 
      # panel.background = element_blank() , # no background
      #---------------------------------------------------------------------------
      panel.grid = element_blank(), # turn off grid
      strip.background = element_rect( fill = facet_color_1,
                                       color = "transparent" ),
      strip.text = element_text( size = base_size , 
                                 margin = margin(5,5,5,5, "pt")) 
    )
}
theme_set( theme_dbda() )


light_color <- "grey75"
mid_color   <- "gray65" 
dark_color  <- "gray55"


#------------------------------------------------------------------------------
# Useful functions:

logistic <- function( eta ){ 1 / ( 1 + exp( -eta ) ) }
logit <- function( theta ){ log( theta / ( 1 - theta ) ) }

#------------------------------------------------------------------------------
# Functions for opening and saving graphics that operate the same for 
# Windows and Macintosh and Linux operating systems. At least, that's the hope!

# NEW version of openGraph(), calling openMyWindow(), suggested by Joseph Skudlarek, 2023-04-25.
openMyWindow = function( width , height , ... ) {
  if ( Sys.info()["sysname"] == "Darwin" ) { # MacOS
    tryInfo = try( quartz ( width=width , height=height , ... ) ) ;
  } else if ( .Platform$OS.type == "unix" ) { # Linux etc.
    tryInfo = try( X11    ( width=width , height=height , type="cairo" , ... ) ) ;
  } else { # Windows OS
    tryInfo = try( windows( width=width , height=height , ... ) ) ;
  }
  return ( tryInfo );
}    
openGraph = function( width=7 , height=7 , mag=1.0 , ... ) {
  tryInfo = openMyWindow( width=width*mag , height=height*mag, ...) ;
  if ( class(tryInfo)=="try-error" ) {
    lineInput = readline("WARNING: Previous graphics windows will be closed -- too many open windows?\nTO CONTINUE, PRESS <ENTER> IN R CONSOLE.\n")
    graphics.off()
    openMyWindow( width=width*mag , height=height*mag, ...) ;
  }
}

### OLD version of openGraph(), retained in case new version breaks...
# openGraph = function( width=7 , height=7 , mag=1.0 , ... ) {
#   if ( .Platform$OS.type != "windows" ) { # Mac OS, Linux
#     tryInfo = try( X11( width=width*mag , height=height*mag , type="cairo" , 
#                         ... ) )
#     if ( class(tryInfo)=="try-error" ) {
#       lineInput = readline("WARNING: Previous graphics windows will be closed because of too many open windows.\nTO CONTINUE, PRESS <ENTER> IN R CONSOLE.\n")
#       graphics.off() 
#       X11( width=width*mag , height=height*mag , type="cairo" , ... )
#     }
#   } else { # Windows OS
#     tryInfo = try( windows( width=width*mag , height=height*mag , ... ) )
#     if ( class(tryInfo)=="try-error" ) {
#       lineInput = readline("WARNING: Previous graphics windows will be closed because of too many open windows.\nTO CONTINUE, PRESS <ENTER> IN R CONSOLE.\n")
#       graphics.off() 
#       windows( width=width*mag , height=height*mag , ... )    
#     }
#   }
# }

saveGraph = function( file="saveGraphOutput" , type="pdf" , ... ) {
  if ( .Platform$OS.type != "windows" ) { # Mac OS, Linux
    if ( any( type == c("png","jpeg","jpg","tiff","bmp")) ) {
      sptype = type
      if ( type == "jpg" ) { sptype = "jpeg" }
      savePlot( file=paste0(file,".",type) , type=sptype , ... )     
    }
    if ( type == "pdf" ) {
      dev.copy2pdf(file=paste0(file,".",type) , ... )
    }
    if ( type == "eps" ) {
      dev.copy2eps(file=paste0(file,".",type) , ... )
    }
  } else { # Windows OS
    file=paste0(file,".",type) 
    savePlot( file=file , type=type , ... )
  }
}

#------------------------------------------------------------------------------

# bfplot() from Kruschke (2021) Bayesian Analysis Reporting Guidelines. 
# Specifically BARG-Supplement.html at https://osf.io/aj3dk
#' @title bfplot
#' @description For an input Bayes factor, `bfplot()` creates a plot of the
#'   posterior model probability as a function of the prior model probability.
#' @details For an input Bayes factor, `bfplot()` plots the posterior
#'   probability of the numerator model as a function of the prior probability
#'   of the numerator model. The plot is annotated with the critical posterior
#'   probability for deciding to accept or reject the models, and with the
#'   corresponding prior probability required to reach that threshold. The
#'   function returns a list including a text message summarizing the results.
#' @param bf The Bayes factor to be plotted; a scalar greater than zero. User
#'   must specify.
#' @param numerName Name of numerator model; character string in quotes. Has default.
#' @param denomName Name of denominator model; character string in quotes. Has default.
#' @param critPost Critical value for posterior probability of numerator model
#'   to 'accept' the numerator model. Defaults to 0.95. Must be numeric > 0.50,
#'   or NA in which case no accept/reject annotation is displayed or returned.
#' @param main Title annotation for plot. Has default.
#' @param bfCol Color of Bayes factor annotation. Has default.
#' @param acceptCol Color of 'accept' annotation. Has default.
#' @param rejectCol Color of 'reject' annotation. Has default.
#' @param bfLabelNudge Displacement of BF label relative to curve. Has default.
#' @param bfLabelCex Magnification of BF label text. Has default.
#' @param noPlot If TRUE then only returns info, no plot. Defaults to FALSE.
#' @return A list containing the input values and various computed values along
#'   with a text message (character string) explaining critical prior
#'   probabilities of models.
#' @examples
#' # Example with bf>1, lean toward 'accept' numerator model:
#' bfplot( bf = 19 )
#' # Example with bf<1, lean toward 'reject' numerator model:
#' bfplot( bf = 1/19 )
#' # Example with different critical posterior probability for decision:
#' bfplot( bf = 3 , critPost = 0.75 )
#' # Example with returned output:
#' bfplotInfo = bfplot( bf = 19 )
#' print( bfplotInfo )
#' # Example for disease diagnosis:
#' falsePositiveRate = 0.07 ; specificity = 1.0-falsePositiveRate
#' falseNegativeRate = 0.03 ; sensitivity = 1.0-falseNegativeRate
#' bfPositiveTest = sensitivity / falsePositiveRate # for positive test result
#' bfNegativeTest = falseNegativeRate / specificity # for negative test result
#' par(mfrow=c(1,2))
#' bfplot( bfPositiveTest ,
#'         numerName="Have Disease" , denomName="Don't Have Dis." ,
#'         main="Test Positive" , critPost=NA )
#' bfplot( bfNegativeTest ,
#'         numerName="Have Disease" , denomName="Don't Have Dis." ,
#'         main="Test Negative" , critPost=NA )
#' par(mfrow=c(1,1)) # reset single plot panel
#' @author John K. Kruschke, johnkruschke@gmail.com,
#'   https://jkkweb.sitehost.iu.edu/, December 2020.
#' @export
bfplot = function( bf ,
                   numerName="Numerator Model" ,
                   denomName="Denominator Model" ,
                   critPost=0.95 , # or NA
                   # ornamentation:
                   main=bquote(atop(.(numerName),"/ "*.(denomName))) ,
                   bfCol = "deepskyblue" ,
                   acceptCol = "darkgreen" ,
                   rejectCol = "darkred" ,
                   bfLabelNudge = 0.2 ,
                   bfLabelCex = 1.5 ,
                   noPlot = FALSE , # if TRUE then only returns info, no plot
                   ... # additional arguments passed to plot()
) {
  if ( !noPlot ){
    oldPar = par()[c("mar","mgp","pty")]
    on.exit( par(oldPar) )
    par( mar=c(3.5,3.5,4.0,1.0) , mgp=c(2.0,0.7,0) , pty="s" )
  }
  # Check values of inputs:
  if ( bf <=0 ) { stop(" bf must be greater than zero") }
  if ( !is.na(critPost) ) {
    if ( critPost <= 0.50 ) { stop(" critPost must be greater than 0.50") }
    if ( critPost > 1 ) { stop(" critPost must not exceed 1") }
  }
  if ( !noPlot ){
    # Plot bf curve:
    prior = seq( 1e-100 , 1 - 1e-100 , length=501 ) # vector, obviously
    priorOdds = prior/(1-prior) # vector
    postOdds = bf*priorOdds # vector
    post = postOdds/(1+postOdds) # vector
    idxToInclude = which(is.finite(post))
    post = c(0,post[idxToInclude],1)
    prior = c(0,prior[idxToInclude],1)
    limMarg = 0.05
    plot( prior , post ,
          xlim=c(0-limMarg,1+limMarg) , ylim=c(0-limMarg,1+limMarg) ,
          xlab=paste0("p( ",numerName," )") ,
          ylab=paste0("p( ",numerName," | D)") ,
          type="l" , lwd=4 , col=bfCol ,
          main=main ,
          ...
    )
    # Draw limited grid:
    segments( x0=seq(0,1,0.2),y0=0,y1=1 , lty="dotted" , col="lightgray")
    segments( y0=seq(0,1,0.2),x0=0,x1=1 , lty="dotted" , col="lightgray")
    # Annotate curve with bf value:
    bfLabelNudgeCrit = 1.7
    if ( bf > 1 ) {
      bfPlotIdx = which.max( post - prior )
      text( prior[bfPlotIdx] , post[bfPlotIdx] ,
            adj=c(0-bfLabelNudge,1+bfLabelNudge+1.3*(abs(log10(bf))>bfLabelNudgeCrit)) ,
            labels=paste0("BF = ",signif(bf,3)) , col=bfCol , cex=bfLabelCex )
    } else {
      bfPlotIdx = which.min( post - prior )
      text( prior[bfPlotIdx] , post[bfPlotIdx] ,
            #adj=c(1+bfLabelNudge,0-bfLabelNudge-1.3*(abs(log10(bf))>bfLabelNudgeCrit)) ,
            adj=c(1 ,0-bfLabelNudge-1.3*(abs(log10(bf))>bfLabelNudgeCrit)) ,
            labels=paste0("BF = ",signif(bf,3)) , col=bfCol , cex=bfLabelCex )
    }
  }
  if ( !is.na(critPost) ) {
    # Compute critical values of prior prob:
    hiCritPost = critPost
    hiCritPostOdds = hiCritPost/(1-hiCritPost)
    hiCritPrior = hiCritPostOdds/(bf+hiCritPostOdds)
    loCritPost = 1-critPost
    loCritPostOdds = loCritPost/(1-loCritPost)
    loCritPrior = loCritPostOdds/(bf+loCritPostOdds)
    if ( !noPlot ){
      # Plot critical intervals of prior prob:
      # Accept interval:
      segments( y0=hiCritPost ,x0=0,x1=1, lty="dashed" , col=acceptCol )
      lines( x=c(hiCritPrior,1) , y=c(hiCritPost,hiCritPost) ,
             lwd=3 , lend=1 , col=acceptCol )
      text( x=hiCritPrior , y=hiCritPost , adj=c(0.5,-0.5) ,
            labels=round(hiCritPrior,3) , col=acceptCol )
      text( x=1 , y=hiCritPost , adj=c(1.0,1.5) ,
            labels=paste0("Prior s.t. Post>",round(hiCritPost,3)) , col=acceptCol )
      # Reject interval:
      segments( y0=loCritPost ,x0=0,x1=1, lty="dashed" , col=rejectCol )
      lines( x=c(0,loCritPrior) , y=c(loCritPost,loCritPost) ,
             lwd=3 , lend=1 , col=rejectCol )
      text( x=loCritPrior , y=loCritPost , adj=c(0.5,1.5) ,
            labels=round(loCritPrior,3) , col=rejectCol )
      text( x=0 , y=loCritPost , adj=c(0.0,-0.5) ,
            labels=paste0("Prior s.t. Post<",round(loCritPost,3)) , col=rejectCol )
    }
    # Construct message:
    message = paste0( "The Bayes factor is ", signif(bf,3), 
                      " for ", numerName, " relative to ", denomName, ".",
                      " To 'accept' ",numerName," (relative to ",denomName,")",
                      " with a posterior probability of at least ",
                      round(hiCritPost,3),", ",
                      numerName,"'s prior probability must be at least ",
                      round(hiCritPrior,3),".",
                      " To 'reject' ",numerName," (relative to ",denomName,")" ,
                      " with a posterior probability less than ",
                      round(loCritPost,3),", ",
                      numerName,"'s prior probability must be less than ",
                      round(loCritPrior,3),"." )
  } else {
    hiCritPost = NULL
    hiCritPrior = NULL
    loCritPost = NULL
    loCritPrior = NULL
    message = paste0("The Bayes factor is ", signif(bf,3), 
                     " for ", numerName, " relative to ", denomName, ".")
  }
  # Invisibly return relevant information:
  invisible( list(
    bf=bf ,
    numerName=numerName ,
    denomName=denomName ,
    critPost=critPost ,
    hiCritPost=hiCritPost ,
    hiCritPrior=hiCritPrior ,
    loCritPost=loCritPost ,
    loCritPrior=loCritPrior ,
    message=message ) )
}

# ggplot version of bfplot():
bf_plot <- function(bf,
                    crit_post = 0.95,  # or NA
                    numer_name = bquote(M[1]) ,
                    denom_name = bquote(M[2]) ,
                    title = bquote( .(numer_name) *" / "* .(denom_name) ),
                    bf_col = line_color_1, 
                    accept_col = line_color_2 , # "darkgreen",
                    reject_col = line_color_2 , # "darkred",
                    bf_label_nudge = 0.2,
                    # new
                    axis_breaks = 0:5 / 5,
                    text_mag = 1.0 ,
                    no_plot = FALSE) {
  
  # Check the inputs values
  if ( bf <= 0 ) stop("`bf` must be greater than zero (and finite).")
  if ( is.infinite( bf )) stop("`bf` must be finite (and greater than zero).")
  if (!is.na(crit_post)) {
    if (crit_post <= 0.50) stop("`crit_post` must be greater than 0.50.")
    if (crit_post > 1) stop("`crit_post` must not exceed 1.")
  }
  
  # Primary data frame for the curve
  bf_df <- data.frame(prior = seq(from = 0, to = 1, length.out = 501)) %>% 
    mutate(prior_odds = prior / (1 - prior)) %>% 
    mutate(post_odds = bf * prior_odds) %>% 
    mutate(post = post_odds / (1 + post_odds)) %>% 
    mutate(post = ifelse( prior == 1, 1, post)) 
  
  # Annotate curve with `bf` value
  bf_label_nudge_crit <- 1.7
  
  if (bf > 1) {
    # Data frame for the BF annotation
    bf_label_df <- bf_df %>% 
      slice_max(post - prior) %>% 
      mutate(label = str_c("BF==", signif(bf, digits = 3)))
    # Adjust the horizontal and vertical text alignment within geom_text()
    h_just <- 0 - bf_label_nudge
    v_just <- 1 + bf_label_nudge + 1.3 * (abs(log10(bf)) > bf_label_nudge_crit)
  } else {
    bf_label_df <- bf_df %>% 
      slice_min(post - prior, with_ties = FALSE) %>% 
      mutate(label = str_c("BF==", signif(bf, digits = 3)))
    h_just <- 1 + bf_label_nudge
    v_just <- 0 - bf_label_nudge - 1.3 * (abs(log10(bf)) > bf_label_nudge_crit)
  }
  
  # Compute critical values for prior probability
  hi_crit_post <- crit_post                                      # hiCritPost
  hi_crit_post_odds <- hi_crit_post / (1 - hi_crit_post)         # hiCritPostOdds
  hi_crit_prior <- hi_crit_post_odds / (bf + hi_crit_post_odds)  # hiCritPrior
  lo_crit_post <- 1 - crit_post                                  # loCritPost
  lo_crit_post_odds <- lo_crit_post / (1 - lo_crit_post)         # loCritPostOdds
  lo_crit_prior <- lo_crit_post_odds / (bf + lo_crit_post_odds)  # loCritPrior
  
  # Annotation related to `crit_post`
  crit_post_df <- data.frame(
    level = rep(c("hi", "lo"), each = 4),
    post = rep(c(hi_crit_post, lo_crit_post), each = 4)) %>% 
    mutate(prior = c(0, hi_crit_prior, hi_crit_prior, 1, 
                     0, lo_crit_prior, lo_crit_prior, 1)) %>% 
    mutate(decision = rep(c("reject", "accept", "accept", "reject"), each = 2))
  
  crit_value_df <- crit_post_df %>% 
    slice(c(2, 6)) %>% 
    mutate(label = round(prior, digits = 3), vjust = c(-0.4, 1.4))
  
  crit_label_df <- crit_post_df %>% 
    slice(4:5) %>% 
    mutate(label = c(str_c("Prior~s.t.~Post>", round(hi_crit_post, digits = 3)),
                     str_c("Prior~s.t.~Post<", round(lo_crit_post, digits = 3))),
           hjust = 1:0, vjust = c(1.4, -0.4))
  
  # For when `crit_post = NA`
  if (is.na(crit_post)) {
    crit_post_df <- crit_post_df %>% 
      slice(0)
    crit_value_df <- crit_value_df %>% 
      slice(0)
    crit_label_df <- crit_label_df %>% 
      slice(0)
  }
  
  # Make the plot
  the_bf_plot <- bf_df %>% 
    ggplot(aes(x = prior, y = post)) +
    geom_line(color = bf_col, linewidth = 1) +
    geom_text(data = bf_label_df,
              aes(label = label),
              color = bf_col, hjust = h_just, parse = TRUE, 
              size = text_mag*5, vjust = v_just) +
    geom_line(data = crit_post_df,
              aes(color = level, linetype = decision, linewidth = decision)) +
    geom_text(data = crit_value_df,
              aes(label = label, color = level, vjust = vjust),
              size = text_mag*3.5) +
    geom_text(data = crit_label_df,
              aes(label = label, color = level, hjust = hjust, vjust = vjust),
              parse = TRUE, size = text_mag*3.5) +
    # Note the flexible axis breaks (maybe not needed)
    scale_x_continuous(breaks = axis_breaks, expand = expansion(mult = 0.075)) +
    scale_y_continuous(breaks = axis_breaks, expand = expansion(mult = 0.075)) +
    scale_color_manual(values = c(accept_col, reject_col)) +
    scale_linetype_manual(values = c(1, 2)) +
    scale_linewidth_manual(values = c(1, 1/3)) +
    labs(x = bquote("p( "* .(numer_name) *" )"),
         y = bquote("p( "* .(numer_name) *" | D )"),
         subtitle = title) +
    coord_equal() +
    theme(legend.position = "none",
          panel.grid.major = element_line(color = "white", linewidth = 1/3),
          panel.grid.minor = element_blank(),
          plot.subtitle = element_text(hjust = 0.5,size=text_mag*12),
          axis.title.x = element_text(size=text_mag*12),
          axis.text.x = element_text(size=text_mag*12), 
          axis.title.y = element_text(size=text_mag*12),
          axis.text.y = element_text(size=text_mag*12) )
  
  the_bf_plot
  
}

#------------------------------------------------------------------------------
# Functions for computing limits of HDI's:

HDIofMCMC = function( sampleVec , credMass=0.95 ) {
  # Computes highest density interval from a sample of representative values,
  #   estimated as shortest credible interval.
  # Arguments:
  #   sampleVec
  #     is a vector of representative values from a probability distribution.
  #   credMass
  #     is a scalar between 0 and 1, indicating the mass within the credible
  #     interval that is to be estimated.
  # Value:
  #   HDIlim is a vector containing the limits of the HDI
  sortedPts = sort( sampleVec )
  ciIdxInc = ceiling( credMass * length( sortedPts ) )
  nCIs = length( sortedPts ) - ciIdxInc
  ciWidth = rep( 0 , nCIs )
  for ( i in 1:nCIs ) {
    ciWidth[ i ] = sortedPts[ i + ciIdxInc ] - sortedPts[ i ]
  }
  HDImin = sortedPts[ which.min( ciWidth ) ]
  HDImax = sortedPts[ which.min( ciWidth ) + ciIdxInc ]
  HDIlim = c( HDImin , HDImax )
  return( HDIlim )
}

hdi_of_mcmc = function( sample_vec , cred_mass=0.95 ) {
  # Computes highest density interval from a sample of representative values,
  #   estimated as shortest credible interval.
  # Arguments:
  #   sample_vec
  #     is a vector of representative values from a probability distribution.
  #   cred_mass
  #     is a scalar between 0 and 1, indicating the mass within the credible
  #     interval that is to be estimated.
  # Value:
  #   hdi_lim is a vector containing the limits of the HDI
  sorted_pts = sort( sample_vec )
  ci_idx_inc = ceiling( cred_mass * length( sorted_pts ) )
  n_cis = length( sorted_pts ) - ci_idx_inc
  ci_width = rep( 0 , n_cis )
  for ( i in 1:n_cis ) {
    ci_width[ i ] = sorted_pts[ i + ci_idx_inc ] - sorted_pts[ i ]
  }
  hdi_min = sorted_pts[ which.min( ci_width ) ]
  hdi_max = sorted_pts[ which.min( ci_width ) + ci_idx_inc ]
  hdi_lim = c( hdi_min , hdi_max )
  return( hdi_lim )
}

HDIofICDF = function( ICDFname , credMass=0.95 , tol=1e-8 , ... ) {
  # Arguments:
  #   ICDFname is R's name for the inverse cumulative density function
  #     of the distribution.
  #   credMass is the desired mass of the HDI region.
  #   tol is passed to R's optimize function.
  # Return value:
  #   Highest density iterval (HDI) limits in a vector.
  # Example of use: For determining HDI of a beta(30,12) distribution, type
  #   HDIofICDF( qbeta , shape1 = 30 , shape2 = 12 )
  #   Notice that the parameters of the ICDFname must be explicitly named;
  #   e.g., HDIofICDF( qbeta , 30 , 12 ) does not work.
  # Adapted and corrected from Greg Snow's TeachingDemos package.
  incredMass =  1.0 - credMass
  intervalWidth = function( lowTailPr , ICDFname , credMass , ... ) {
    ICDFname( credMass + lowTailPr , ... ) - ICDFname( lowTailPr , ... )
  }
  optInfo = optimize( intervalWidth , c( 0 , incredMass ) , ICDFname=ICDFname ,
                      credMass=credMass , tol=tol , ... )
  HDIlowTailPr = optInfo$minimum
  return( c( ICDFname( HDIlowTailPr , ... ) ,
             ICDFname( credMass + HDIlowTailPr , ... ) ) )
}

hdi_of_icdf = function( icdf_name , cred_mass=0.95 , tol=1e-8 , ... ) {
  # Arguments:
  #   icdf_name is R's name for the inverse cumulative density function
  #     of the distribution.
  #   cred_mass is the desired mass of the HDI region.
  #   tol is passed to R's optimize function.
  # Return value:
  #   Highest density iterval (HDI) limits in a vector.
  # Example of use: For determining HDI of a beta(30,12) distribution, type
  #   hdi_of_icdf( qbeta , shape1 = 30 , shape2 = 12 )
  #   Notice that the parameters of the icdf_name must be explicitly named;
  #   e.g., hdi_of_icdf( qbeta , 30 , 12 ) does not work.
  incred_mass =  1.0 - cred_mass
  interval_width = function( low_tail_p , icdf_name , cred_mass , ... ) {
    icdf_name( cred_mass + low_tail_p , ... ) - icdf_name( low_tail_p , ... )
  }
  opt_info = optimize( interval_width , 
                       c( 0 , incred_mass ) , 
                       icdf_name = icdf_name ,
                       cred_mass = cred_mass , tol = tol , ... )
  hdi_low_tail_p = opt_info$minimum
  return( c( icdf_name( hdi_low_tail_p , ... ) ,
             icdf_name( cred_mass + hdi_low_tail_p , ... ) ) )
}


HDIofGrid = function( probMassVec , credMass=0.95 ) {
  # Arguments:
  #   probMassVec is a vector of probability masses at each grid point.
  #   credMass is the desired mass of the HDI region.
  # Return value:
  #   A list with components:
  #   indices is a vector of indices that are in the HDI
  #   mass is the total mass of the included indices
  #   height is the smallest component probability mass in the HDI
  # Example of use: For determining HDI of a beta(30,12) distribution
  #   approximated on a grid:
  #   > probDensityVec = dbeta( seq(0,1,length=201) , 30 , 12 )
  #   > probMassVec = probDensityVec / sum( probDensityVec )
  #   > HDIinfo = HDIofGrid( probMassVec )
  #   > show( HDIinfo )
  sortedProbMass = sort( probMassVec , decreasing=TRUE )
  HDIheightIdx = min( which( cumsum( sortedProbMass ) >= credMass ) )
  HDIheight = sortedProbMass[ HDIheightIdx ]
  HDImass = sum( probMassVec[ probMassVec >= HDIheight ] )
  return( list( indices = which( probMassVec >= HDIheight ) ,
                mass = HDImass , height = HDIheight ) )
}

#------------------------------------------------------------------------------
# Function(s) for plotting properties of mcmc coda objects.

DbdaAcfPlot = function( codaObject , parName=varnames(codaObject)[1] , plColors=NULL ) {
  if ( all( parName != varnames(codaObject) ) ) { 
    stop("parName must be a column name of coda object")
  }
  nChain = length(codaObject)
  if ( is.null(plColors) ) plColors=1:nChain
  xMat = NULL
  yMat = NULL
  for ( cIdx in 1:nChain ) {
    acfInfo = acf(codaObject[,c(parName)][[cIdx]],plot=FALSE) 
    xMat = cbind(xMat,acfInfo$lag)
    yMat = cbind(yMat,acfInfo$acf)
  }
  matplot( xMat , yMat , type="o" , pch=20 , col=plColors , ylim=c(0,1) ,
           main="" , xlab="Lag" , ylab="Autocorrelation" )
  abline(h=0,lty="dashed")
  EffChnLngth = effectiveSize(codaObject[,c(parName)])
  text( x=max(xMat) , y=max(yMat) , adj=c(1.0,1.0) , cex=1.25 ,
        labels=paste("ESS =",round(EffChnLngth,1)) )
}

DbdaDensPlot = function( codaObject , parName=varnames(codaObject)[1] , plColors=NULL ) {
  if ( all( parName != varnames(codaObject) ) ) { 
    stop("parName must be a column name of coda object")
  }
  nChain = length(codaObject) # or nchain(codaObject)
  if ( is.null(plColors) ) plColors=1:nChain
  xMat = NULL
  yMat = NULL
  hdiLims = NULL
  for ( cIdx in 1:nChain ) {
    densInfo = density(codaObject[,c(parName)][[cIdx]]) 
    xMat = cbind(xMat,densInfo$x)
    yMat = cbind(yMat,densInfo$y)
    hdiLims = cbind(hdiLims,HDIofMCMC(codaObject[,c(parName)][[cIdx]]))
  }
  matplot( xMat , yMat , type="l" , col=plColors , 
           main="" , xlab="Param. Value" , ylab="Density" )
  abline(h=0)
  points( hdiLims[1,] , rep(0,nChain) , col=plColors , pch="|" )
  points( hdiLims[2,] , rep(0,nChain) , col=plColors , pch="|" )
  text( mean(hdiLims) , 0 , "95% HDI" , adj=c(0.5,-0.2) )
  EffChnLngth = effectiveSize(codaObject[,c(parName)])
  MCSE = sd(as.matrix(codaObject[,c(parName)]))/sqrt(EffChnLngth) 
  text( max(xMat) , max(yMat) , adj=c(1.0,1.0) , cex=1.25 ,
        paste("MCSE =\n",signif(MCSE,3)) )
}

diagMCMC = function( codaObject , parName=varnames(codaObject)[1] ,
                     saveName=NULL , saveType="jpg" ) {
  DBDAplColors = c("skyblue","black","royalblue","steelblue")
  openGraph(height=5,width=7)
  par( mar=0.5+c(3,4,1,0) , oma=0.1+c(0,0,2,0) , mgp=c(2.25,0.7,0) , 
       cex.lab=1.5 )
  layout(matrix(1:4,nrow=2))
  # traceplot and gelman.plot are from CODA package:
  require(coda)
  coda::traceplot( codaObject[,c(parName)] , main="" , ylab="Param. Value" ,
                   col=DBDAplColors ) 
  tryVal = try(
    coda::gelman.plot( codaObject[,c(parName)] , main="" , auto.layout=FALSE , 
                       col=DBDAplColors )
  )  
  # if it runs, gelman.plot returns a list with finite shrink values:
  if ( class(tryVal)=="try-error" ) {
    plot.new() 
    print(paste0("Warning: coda::gelman.plot fails for ",parName))
  } else { 
    if ( class(tryVal)=="list" & !is.finite(tryVal$shrink[1]) ) {
      plot.new() 
      print(paste0("Warning: coda::gelman.plot fails for ",parName))
    }
  }
  DbdaAcfPlot(codaObject,parName,plColors=DBDAplColors)
  DbdaDensPlot(codaObject,parName,plColors=DBDAplColors)
  mtext( text=parName , outer=TRUE , adj=c(0.5,0.5) , cex=2.0 )
  if ( !is.null(saveName) ) {
    saveGraph( file=paste0(saveName,"Diag",parName), type=saveType)
  }
}

diagStanFit = function( stanFit , parName ,
                        saveName=NULL , saveType="jpg" ) {
  codaFit = mcmc.list( lapply( 1:ncol(stanFit) , 
                               function(x) { mcmc(as.array(stanFit)[,x,]) } ) )
  DBDAplColors = c("skyblue","black","royalblue","steelblue")
  openGraph(height=5,width=7)
  par( mar=0.5+c(3,4,1,0) , oma=0.1+c(0,0,2,0) , mgp=c(2.25,0.7,0) , cex.lab=1.5 )
  layout(matrix(1:4,nrow=2))
  # traceplot is from rstan package
  require(rstan)
  traceplot(stanFit,pars=parName,nrow=1,ncol=1)#,main="",ylab="Param. Value",col=DBDAplColors) 
  # gelman.plot are from CODA package:
  require(coda)
  tryVal = try(
    coda::gelman.plot( codaObject[,c(parName)] , main="" , auto.layout=FALSE , 
                       col=DBDAplColors )
  )
  # if it runs, gelman.plot returns a list with finite shrink values:
  if ( class(tryVal)=="try-error" ) {
    plot.new() 
    print(paste0("Warning: coda::gelman.plot fails for ",parName))
  } else { 
    if ( class(tryVal)=="list" & !is.finite(tryVal$shrink[1]) ) {
      plot.new() 
      print(paste0("Warning: coda::gelman.plot fails for ",parName))
    }
  }
  DbdaAcfPlot(codaFit,parName,plColors=DBDAplColors)
  DbdaDensPlot(codaFit,parName,plColors=DBDAplColors)
  mtext( text=parName , outer=TRUE , adj=c(0.5,0.5) , cex=2.0 )
  if ( !is.null(saveName) ) {
    saveGraph( file=paste0(saveName,"Diag",parName), type=saveType)
  }
}

#------------------------------------------------------------------------------
# Functions for summarizing and plotting distribution of a large sample; 
# typically applied to MCMC posterior.

normalize = function( v ){ return( v / sum(v) ) }

require(coda) # loaded by rjags, but redundancy doesn't hurt

summarizePost = function( paramSampleVec , 
                          compVal=NULL , ROPE=NULL , credMass=0.95 ) {
  meanParam = mean( paramSampleVec )
  medianParam = median( paramSampleVec )
  dres = density( paramSampleVec )
  modeParam = dres$x[which.max(dres$y)]
  mcmcEffSz = round( effectiveSize( paramSampleVec ) , 1 )
  names(mcmcEffSz) = NULL
  hdiLim = HDIofMCMC( paramSampleVec , credMass=credMass )
  if ( !is.null(compVal) ) {
    pcgtCompVal = ( 100 * sum( paramSampleVec > compVal ) 
                    / length( paramSampleVec ) )
  } else {
    compVal=NA
    pcgtCompVal=NA
  }
  if ( !is.null(ROPE) ) {
    pcltRope = ( 100 * sum( paramSampleVec < ROPE[1] ) 
                 / length( paramSampleVec ) )
    pcgtRope = ( 100 * sum( paramSampleVec > ROPE[2] ) 
                 / length( paramSampleVec ) )
    pcinRope = 100-(pcltRope+pcgtRope)
  } else { 
    ROPE = c(NA,NA)
    pcltRope=NA 
    pcgtRope=NA 
    pcinRope=NA 
  }  
  return( c( Mean=meanParam , Median=medianParam , Mode=modeParam , 
             ESS=mcmcEffSz ,
             HDImass=credMass , HDIlow=hdiLim[1] , HDIhigh=hdiLim[2] , 
             CompVal=compVal , PcntGtCompVal=pcgtCompVal , 
             ROPElow=ROPE[1] , ROPEhigh=ROPE[2] ,
             PcntLtROPE=pcltRope , PcntInROPE=pcinRope , PcntGtROPE=pcgtRope ) )
}

plotPost = function( paramSampleVec , cenTend=c("mode","median","mean")[1] , 
                     compVal=NULL, ROPE=NULL, credMass=0.95, HDItextPlace=0.7, 
                     xlab=NULL , xlim=NULL , yaxt=NULL , ylab=NULL , 
                     main=NULL , cex=NULL , cex.lab=NULL ,
                     col=NULL , border=NULL , showCurve=FALSE , breaks=NULL , 
                     ... ) {
  # Override defaults of hist function, if not specified by user:
  # (additional arguments "..." are passed to the hist function)
  if ( is.null(xlab) ) xlab="Param. Val."
  if ( is.null(cex.lab) ) cex.lab=1.5
  if ( is.null(cex) ) cex=1.4
  if ( is.null(xlim) ) xlim=range( c( compVal , ROPE , paramSampleVec ) )
  if ( is.null(main) ) main=""
  if ( is.null(yaxt) ) yaxt="n"
  if ( is.null(ylab) ) ylab=""
  if ( is.null(col) ) col="skyblue"
  if ( is.null(border) ) border="white"
  
  # convert coda object to matrix:
  # if ( class(paramSampleVec) == "mcmc.list" ) { # obsolete, prior to R4.0
  if ( inherits( paramSampleVec , "mcmc.list") )  {
    paramSampleVec = as.matrix(paramSampleVec)
  }
  
  summaryColNames = c("ESS","mean","median","mode",
                      "hdiMass","hdiLow","hdiHigh",
                      "compVal","pGtCompVal",
                      "ROPElow","ROPEhigh","pLtROPE","pInROPE","pGtROPE")
  postSummary = matrix( NA , nrow=1 , ncol=length(summaryColNames) , 
                        dimnames=list( c( xlab ) , summaryColNames ) )
  
  # require(coda) # for effectiveSize function
  postSummary[,"ESS"] = effectiveSize(paramSampleVec)
  
  postSummary[,"mean"] = mean(paramSampleVec)
  postSummary[,"median"] = median(paramSampleVec)
  mcmcDensity = density(paramSampleVec)
  postSummary[,"mode"] = mcmcDensity$x[which.max(mcmcDensity$y)]
  
  HDI = HDIofMCMC( paramSampleVec , credMass )
  postSummary[,"hdiMass"]=credMass
  postSummary[,"hdiLow"]=HDI[1]
  postSummary[,"hdiHigh"]=HDI[2]
  
  # Plot histogram.
  cvCol = "darkgreen"
  ropeCol = "darkred"
  if ( is.null(breaks) ) {
    if ( max(paramSampleVec) > min(paramSampleVec) ) {
      breaks = c( seq( from=min(paramSampleVec) , to=max(paramSampleVec) ,
                       by=(HDI[2]-HDI[1])/18 ) , max(paramSampleVec) )
    } else {
      breaks=c(min(paramSampleVec)-1.0E-6,max(paramSampleVec)+1.0E-6)
      border="skyblue"
    }
  }
  if ( !showCurve ) {
    par(xpd=NA)
    histinfo = hist( paramSampleVec , xlab=xlab , yaxt=yaxt , ylab=ylab ,
                     freq=F , border=border , col=col ,
                     xlim=xlim , main=main , cex=cex , cex.lab=cex.lab ,
                     breaks=breaks , ... )
  }
  if ( showCurve ) {
    par(xpd=NA)
    histinfo = hist( paramSampleVec , plot=F )
    densCurve = density( paramSampleVec , adjust=2 )
    plot( densCurve$x , densCurve$y , type="l" , lwd=5 , col=col , bty="n" ,
          xlim=xlim , xlab=xlab , yaxt=yaxt , ylab=ylab ,
          main=main , cex=cex , cex.lab=cex.lab , ... )
  }
  cenTendHt = 0.9*max(histinfo$density)
  cvHt = 0.7*max(histinfo$density)
  ROPEtextHt = 0.55*max(histinfo$density)
  # Display central tendency:
  mn = mean(paramSampleVec)
  med = median(paramSampleVec)
  mcmcDensity = density(paramSampleVec)
  mo = mcmcDensity$x[which.max(mcmcDensity$y)]
  if ( cenTend=="mode" ){ 
    text( mo , cenTendHt ,
          bquote(mode==.(signif(mo,3))) , adj=c(.5,0) , cex=cex )
  }
  if ( cenTend=="median" ){ 
    text( med , cenTendHt ,
          bquote(median==.(signif(med,3))) , adj=c(.5,0) , cex=cex , col=cvCol )
  }
  if ( cenTend=="mean" ){ 
    text( mn , cenTendHt ,
          bquote(mean==.(signif(mn,3))) , adj=c(.5,0) , cex=cex )
  }
  # Display the comparison value.
  if ( !is.null( compVal ) ) {
    pGtCompVal = sum( paramSampleVec > compVal ) / length( paramSampleVec ) 
    pLtCompVal = 1 - pGtCompVal
    lines( c(compVal,compVal) , c(0.96*cvHt,0) , 
           lty="dotted" , lwd=2 , col=cvCol )
    text( compVal , cvHt ,
          bquote( .(round(100*pLtCompVal,1)) * "% < " *
                    .(signif(compVal,3)) * " < " * 
                    .(round(100*pGtCompVal,1)) * "%" ) ,
          adj=c(pLtCompVal,0) , cex=0.8*cex , col=cvCol )
    postSummary[,"compVal"] = compVal
    postSummary[,"pGtCompVal"] = pGtCompVal
  }
  # Display the ROPE.
  if ( !is.null( ROPE ) ) {
    pInROPE = ( sum( paramSampleVec > ROPE[1] & paramSampleVec < ROPE[2] )
                / length( paramSampleVec ) )
    pGtROPE = ( sum( paramSampleVec >= ROPE[2] ) / length( paramSampleVec ) )
    pLtROPE = ( sum( paramSampleVec <= ROPE[1] ) / length( paramSampleVec ) )
    lines( c(ROPE[1],ROPE[1]) , c(0.96*ROPEtextHt,0) , lty="dotted" , lwd=2 ,
           col=ropeCol )
    lines( c(ROPE[2],ROPE[2]) , c(0.96*ROPEtextHt,0) , lty="dotted" , lwd=2 ,
           col=ropeCol)
    text( mean(ROPE) , ROPEtextHt ,
          bquote( .(round(100*pLtROPE,1)) * "% < " * .(ROPE[1]) * " < " * 
                    .(round(100*pInROPE,1)) * "% < " * .(ROPE[2]) * " < " * 
                    .(round(100*pGtROPE,1)) * "%" ) ,
          adj=c(pLtROPE+.5*pInROPE,0) , cex=1 , col=ropeCol )
    
    postSummary[,"ROPElow"]=ROPE[1] 
    postSummary[,"ROPEhigh"]=ROPE[2] 
    postSummary[,"pLtROPE"]=pLtROPE
    postSummary[,"pInROPE"]=pInROPE
    postSummary[,"pGtROPE"]=pGtROPE
  }
  # Display the HDI.
  lines( HDI , c(0,0) , lwd=4 , lend=1 )
  text( mean(HDI) , 0 , bquote(.(100*credMass) * "% HDI" ) ,
        adj=c(.5,-1.7) , cex=cex )
  text( HDI[1] , 0 , bquote(.(signif(HDI[1],3))) ,
        adj=c(HDItextPlace,-0.5) , cex=cex )
  text( HDI[2] , 0 , bquote(.(signif(HDI[2],3))) ,
        adj=c(1.0-HDItextPlace,-0.5) , cex=cex )
  par(xpd=F)
  #
  return( postSummary )
}

#------------------------------------------------------------------------------

# Shape parameters from central tendency and scale:

betaABfromMeanKappa = function( mean , kappa ) {
  if ( mean <=0 | mean >= 1) stop("must have 0 < mean < 1")
  if ( kappa <=0 ) stop("kappa must be > 0")
  a = mean * kappa
  b = ( 1.0 - mean ) * kappa
  return( list( a=a , b=b ) )
}

betaABfromModeKappa = function( mode , kappa ) {
  if ( mode <=0 | mode >= 1) stop("must have 0 < mode < 1")
  if ( kappa <=2 ) stop("kappa must be > 2 for mode parameterization")
  a = mode * ( kappa - 2 ) + 1
  b = ( 1.0 - mode ) * ( kappa - 2 ) + 1
  return( list( a=a , b=b ) )
}

betaABfromMeanSD = function( mean , sd ) {
  if ( mean <=0 | mean >= 1) stop("must have 0 < mean < 1")
  if ( sd <= 0 ) stop("sd must be > 0")
  kappa = mean*(1-mean)/sd^2 - 1
  if ( kappa <= 0 ) stop("invalid combination of mean and sd")
  a = mean * kappa
  b = ( 1.0 - mean ) * kappa
  return( list( a=a , b=b ) )
}

gammaShRaFromMeanSD = function( mean , sd ) {
  if ( mean <=0 ) stop("mean must be > 0")
  if ( sd <=0 ) stop("sd must be > 0")
  shape = mean^2/sd^2
  rate = mean/sd^2
  return( list( shape=shape , rate=rate ) )
}

gammaShRaFromModeSD = function( mode , sd ) {
  if ( mode <=0 ) stop("mode must be > 0")
  if ( sd <=0 ) stop("sd must be > 0")
  rate = ( mode + sqrt( mode^2 + 4 * sd^2 ) ) / ( 2 * sd^2 )
  shape = 1 + mode * rate
  return( list( shape=shape , rate=rate ) )
}

beta_a_b_from_mean_kappa <- function(mean, kappa) {
  if (mean <= 0 | mean >= 1) stop("must have 0 < mean < 1")
  if (kappa <= 0) stop("kappa must be > 0")
  a <- mean * kappa
  b <- (1.0 - mean) * kappa
  return(list(a = a, b = b))
}

beta_a_b_from_mode_kappa <- function(mode, kappa) {
  if (mode <= 0 | mode >= 1) stop("must have 0 < mode < 1")
  if (kappa <= 2) stop("kappa must be > 2 for mode parameterization")
  a <- mode * (kappa - 2) + 1
  b <- (1.0 - mode) * (kappa - 2) + 1
  return(list(a = a, b = b))
}

beta_a_b_from_mean_sd <- function(mean, sd) {
  if (mean <= 0 | mean >= 1) stop("must have 0 < mean < 1")
  if (sd <= 0) stop("sd must be > 0")
  kappa <- mean * (1 - mean) / sd^2 - 1
  if (kappa <= 0) stop("invalid combination of mean and sd")
  a <- mean * kappa
  b <- (1.0 - mean) * kappa
  return(list(a = a, b = b))
}

gamma_s_r_from_mean_sd = function( mean , sd ) {
  if ( mean <=0 ) stop("mean must be > 0")
  if ( sd <=0 ) stop("sd must be > 0")
  shape = mean^2/sd^2
  rate = mean/sd^2
  return( list( shape=shape , rate=rate ) )
}

gamma_s_r_from_mode_sd = function( mode , sd ) {
  if ( mode <=0 ) stop("mode must be > 0")
  if ( sd <=0 ) stop("sd must be > 0")
  rate = ( mode + sqrt( mode^2 + 4 * sd^2 ) ) / ( 2 * sd^2 )
  shape = 1 + mode * rate
  return( list( shape=shape , rate=rate ) )
}

#------------------------------------------------------------------------------

# Make some data files for examples...
createDataFiles=FALSE
if ( createDataFiles ) {
  
  source("HtWtDataGenerator.R")
  N=300
  m = HtWtDataGenerator( N , rndsd=47405 )
  write.csv( file=paste0("HtWtData",N,".csv") , row.names=FALSE , m )
  
  
  # Function for generating normal data with normal outliers:
  genYwithOut = function( N , pcntOut=15 , sdOut=3.0 ) {
    inl = rnorm( N-ceiling(pcntOut/100*N) )
    out = rnorm(   ceiling(pcntOut/100*N) )
    inl = (inl-mean(inl))/sd(inl)
    out = (out-mean(out))/sd(out) * sdOut
    return(c(inl,out))
  }
  
  # Two-group IQ scores with outliers 
  set.seed(47405)
  y1 = round(pmax(50,genYwithOut(63,20,3.5)*17.5+106))
  y2 = round(pmax(50,genYwithOut(57,20,3.5)*10+100))
  write.csv( file="TwoGroupIQ.csv" , row.names=FALSE ,
             data.frame( Score=c(y1,y2) , 
                         Group=c(rep("Smart Drug",length(y1)),
                                 rep("Placebo",length(y2))) ) )
  
  # One-group log-normal
  set.seed(47405)
  z = rnorm(123)
  logY = (z-mean(z))/sd(z) * 0.5 + 5.5 # logY has mean 5.5 and sd 0.5
  y = round( exp(logY) , 2 )
  write.csv( file="OneGroupLogNormal.csv" , row.names=FALSE ,
             cbind(y) )
  
  # One-group gamma
  desiredMode = 250
  desiredSD = 100
  desiredRate = (desiredMode+sqrt(desiredMode^2+4*desiredSD^2))/(2*desiredSD^2)
  desiredShape = 1+desiredMode*desiredRate
  set.seed(47405)
  y = round( rgamma( 153 , shape=desiredShape , rate=desiredRate ) , 2 )
  write.csv( file="OneGroupGamma.csv" , row.names=FALSE , cbind(y) )
  
} # end if createDataFiles

#------------------------------------------------------------------------------

# Loss, expected loss, etc. for decisions with ROPE

# Loss functions for ROPE. Vectorized for param_vec.
loss_equiv <- function( param_vec , rope=NULL , 
                        rope_narrow=rope , rope_wide=rope , 
                        loss_type=c("step","halfstep","thick")[1] ){
  # check that rope or rope_narrow & rope_wide are specified:
  if ( is.null(rope) & ( is.null(rope_narrow) | is.null(rope_wide) ) ) {
    stop( "rope, or rope_narrow and rope_wide, must be specified." )
  }
  if ( is.null(rope) ) {
    rope <- colMeans( rbind( rope_narrow , rope_wide ) )
  }
  # check rope specifications for consistency:
  if ( rope[2] < rope[1] |
       rope_narrow[1] < rope[1] | rope_wide[1] > rope[1] |
       rope_narrow[2] > rope[2] | rope_wide[2] < rope[2] ) {
    stop( "ROPE specifications not consistent." )
  }
  # proceed...
  loss <- rep( 0, length(param_vec) ) # default place holder
  if ( loss_type == "step" ) {
    loss[ which( param_vec < rope[1] | 
                   param_vec > rope[2] ) ] <- 1
  }
  if ( loss_type == "thick" ) {
    loss[ which( param_vec < rope_narrow[1] | 
                   param_vec > rope_narrow[2] ) ] <- 1
  }
  if ( loss_type == "halfstep" ) {
    loss[ which( param_vec < rope_wide[1] | 
                   param_vec > rope_wide[2] ) ] <- 1
    loss[ which( param_vec >= rope_wide[1] & 
                   param_vec <= rope_narrow[1] ) ] <- (1-0)/2
    loss[ which( param_vec >= rope_narrow[2] & 
                   param_vec <= rope_wide[2] ) ] <- (1-0)/2
  }
  return( loss )
}

loss_lessthan <- function( param_vec , rope=NULL , 
                           rope_narrow=rope , rope_wide=rope , 
                           loss_type=c("step","halfstep","thick")[1] ){
  # check that rope or rope_narrow & rope_wide are specified:
  if ( is.null(rope) & ( is.null(rope_narrow) | is.null(rope_wide) ) ) {
    stop( "rope, or rope_narrow and rope_wide, must be specified." )
  }
  if ( is.null(rope) ) {
    rope <- colMeans( rbind( rope_narrow , rope_wide ) )
  }
  # check rope specifications for consistency:
  if ( rope[2] < rope[1] |
       rope_narrow[1] < rope[1] | rope_wide[1] > rope[1] |
       rope_narrow[2] > rope[2] | rope_wide[2] < rope[2] ) {
    stop( "ROPE specifications not consistent." )
  }
  # proceed...
  loss <- rep( 0, length(param_vec) ) # default place holder
  if ( loss_type == "step" ) {
    loss[ which( param_vec > rope[1] ) ] <- 1
  }
  if ( loss_type == "thick" ) {
    loss[ which( param_vec > rope_wide[1] ) ] <- 1
  }
  if ( loss_type == "halfstep" ) {
    loss[ which( param_vec > rope_narrow[1] ) ] <- 1
    loss[ which( param_vec >= rope_wide[1] & 
                   param_vec <= rope_narrow[1] ) ] <- (1-0)/2
  }
  return( loss )
}

loss_greaterthan <- function( param_vec , rope=NULL , 
                              rope_narrow=rope , rope_wide=rope , 
                              loss_type=c("step","halfstep","thick")[1] ){
  # check that rope or rope_narrow & rope_wide are specified:
  if ( is.null(rope) & ( is.null(rope_narrow) | is.null(rope_wide) ) ) {
    stop( "rope, or rope_narrow and rope_wide, must be specified." )
  }
  if ( is.null(rope) ) {
    rope <- colMeans( rbind( rope_narrow , rope_wide ) )
  }
  # check rope specifications for consistency:
  if ( rope[2] < rope[1] |
       rope_narrow[1] < rope[1] | rope_wide[1] > rope[1] |
       rope_narrow[2] > rope[2] | rope_wide[2] < rope[2] ) {
    stop( "ROPE specifications not consistent." )
  }
  # proceed...
  loss <- rep( 0, length(param_vec) ) # default place holder
  if ( loss_type == "step" ) {
    loss[ which( param_vec < rope[2] ) ] <- 1
  }
  if ( loss_type == "thick" ) {
    loss[ which( param_vec < rope_wide[2] ) ] <- 1
  }
  if ( loss_type == "halfstep" ) {
    loss[ which( param_vec < rope_narrow[2] ) ] <- 1
    loss[ which( param_vec >= rope_narrow[2] & 
                   param_vec <= rope_wide[2] ) ] <- (1-0)/2
  }
  return( loss )
}

## loss_differ should be vestigial and unused.
# loss_differ <- function( param_vec , rope=NULL , 
#                          rope_narrow=rope , rope_wide=rope , 
#                          loss_type=c("step","halfstep","thick")[1] ){
#   # check that rope or rope_narrow & rope_wide are specified:
#   if ( is.null(rope) & ( is.null(rope_narrow) | is.null(rope_wide) ) ) {
#     stop( "rope, or rope_narrow and rope_wide, must be specified." )
#   }
#   if ( is.null(rope) ) {
#     rope <- colMeans( rbind( rope_narrow , rope_wide ) )
#   }
#   # check rope specifications for consistency:
#   if ( rope[2] < rope[1] |
#        rope_narrow[1] < rope[1] | rope_wide[1] > rope[1] |
#        rope_narrow[2] > rope[2] | rope_wide[2] < rope[2] ) {
#     stop( "ROPE specifications not consistent." )
#   }
#   # proceed...
#   loss <- rep( 0, length(param_vec) ) # default place holder
#   if ( loss_type == "step" ) {
#     loss[ which( param_vec > rope[1] &
#                    param_vec < rope[2] ) ] <- 1
#   }
#   if ( loss_type == "thick" ) {
#     loss[ which( param_vec > rope_wide[1] &
#                    param_vec < rope_wide[2] ) ] <- 1
#   }
#   if ( loss_type == "halfstep" ) {
#     loss[ which( param_vec > rope_narrow[1] &
#                    param_vec < rope_narrow[2] ) ] <- 1
#     loss[ which( param_vec >= rope_wide[1] & 
#                    param_vec <= rope_narrow[1] ) ] <- (1-0)/2
#     loss[ which( param_vec >= rope_narrow[2] & 
#                    param_vec <= rope_wide[2] ) ] <- (1-0)/2
#   }
#   return( loss )
# }

loss_uncom <- function( param_vec , alpha = 0.05 , ... ){
  loss <- rep( alpha , length(param_vec) )
  return( loss )
}

# Expected loss with ROPE:
exp_loss_rope_decis <- function( 
    param_sample , rope=NULL , 
    rope_narrow=rope , rope_wide=rope , 
    loss_type=c("step","halfstep","thick")[1] ,
    alpha = 0.05 , na.rm = FALSE ) {
  # Remove NA from param_sample:
  if ( na.rm ){
    param_sample <- param_sample[ !is.na( param_sample ) ]
  }
  expleq <- mean( 
    loss_equiv( param_sample ,
                rope=rope , rope_wide=rope_wide , rope_narrow=rope_narrow ,
                loss_type=loss_type ) )
  expllt <- mean( 
    loss_lessthan( param_sample ,
                   rope=rope , rope_wide=rope_wide , rope_narrow=rope_narrow ,
                   loss_type=loss_type ) )
  explgt <- mean( 
    loss_greaterthan( param_sample ,
                      rope=rope , rope_wide=rope_wide , rope_narrow=rope_narrow ,
                      loss_type=loss_type ) )
  explunc <- mean( loss_uncom( param_sample, alpha=alpha ) )
  return( list( exp_loss_lessthan = expllt ,
                exp_loss_equiv = expleq , 
                exp_loss_greaterthan = explgt ,
                exp_loss_uncom = explunc , 
                decision = c("less than", "equivalent", "greater than", "uncommitted")[ 
                  which.min(c(expllt, expleq, explgt, alpha)) ]  
  ) )
}

# !!!!!!!!!!!!!!!!
# dbda3 package overwrites some functions above:

# install.packages("devtools")
# install.packages("httpuv")
# install.packages("xtable")
# devtools::install_github("ASKurz/dbda3")
library("dbda3")

#------------------------------------------------------------------------------

