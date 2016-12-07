#' PLOT GUI
#' @details GUI simulation for ELGP
#' @import tcltk
#' @import tkrplot
#' @param t variable exposure
#' @param y response variable
#' @param x variable
#' @export
plotGUI<-function(x,y,t){
win1 <-tktoplevel()
tktitle(win1)<-"ELGP Simulation"

#Menu bar
win1$env$menu <- tkmenu(win1) # Create a menu
tkconfigure(win1, menu = win1$env$menu) # Add it to the 'win1' window
win1$env$menuFile <- tkmenu(win1$env$menu, tearoff = FALSE)
tkadd(win1$env$menuFile,
      "command",
      label = "Quit",
      command = function() tkdestroy(win1))
tkadd(win1$env$menu, "cascade", label = "File", menu = win1$env$menuFile)

win1$env$framePlot<-tkframe(win1,
                            borderwidth = 2,relief = "flat")
win1$env$frameparam<-tkframe(win1,borderwidth=2,relief="flat")
tkgrid(win1$env$framePlot)
tkgrid(win1$env$frameparam)
p <- tclVar("0")
h <-tclVar("1")
#Plot awal
elgpplot<-function(){

  elgpres <- ELGP (x, y, t,
                   h=as.double(tclvalue(h)),
                   p=as.integer(tclvalue(p)))
  plotElgp (x,y,t, elgpres, ylab="rate", xlab="age")
}

win1$env$plot <- tkrplot(win1$env$framePlot, fun = elgpplot,
                         hscale = 1.75, vscale = 1.75)

tkgrid(win1$env$plot)

#Entry for p
onEnter<-function(...){
  tkrreplot(win1$env$plot)
}
win1$env$entP <-tkentry(win1$env$frameparam, width = "5", textvariable = p)

tkbind(win1$env$entP,"<Return>",onEnter)


#Slider for h
onChange<-function(...){
  tkrreplot(win1$env$plot)
}

win1$env$sliderH <- tkscale(win1$env$frameparam, from = 1, to = 10,resolution=0.1, variable = h, orient = "horizontal", length = 300,command=onChange)

tkgrid(tklabel(win1$env$frameparam,text="Degree of Polynomial = ", justify ="left"),
       win1$env$entP,
       tklabel(win1$env$frameparam,text="Bandwidth = ", justify ="left"),
       win1$env$sliderH,padx=5,pady=c(10,5))
}

