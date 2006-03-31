####################################################################
# Thomas Hoffmann                                                  #
# CREATED:     07/15/2006                                          #
# MODIFIED:    07/15/2006                                          #
#                                                                  #
# DESCRIPTION:                                                     #
#  The major pbat-interface commands for power & ss                #
####################################################################

####################################################################
#                                                                  #
# CONSTANTS                                                        #
#                                                                  #
####################################################################
POWERENTRYWIDTH <- 20;

####################################################################
#                                                                  #
# SET UP THE GLOBAL VARIABLES INTERFACE                            #
#                                                                  #
####################################################################
powerEnv <- new.env();
setPower <- function( x, value )
  assign( x, value, envir=powerEnv );
getPower <- function( x, mode="any" )
  get( x, envir=powerEnv, mode=mode, inherits=FALSE );

####################################################################
#                                                                  #
# SET UP THE GLOBAL VARIABLES                                      #
#                                                                  #
####################################################################
power.setglobs <- function() {
  globs <- list();
  setPower( "globs", globs );
}


pbat.power <- function() {
  loadTclTkOrDie();

  power.choiceForm();
}




power.choiceForm <- function() {
  ## Set up the global variable interface
  power.setglobs();
  
  ## Set up the modal form
  form <- tktoplevel();
  tkwm.deiconify(form);
  tkgrab.set(form); # make it modal
  tkwm.title( form, "P2BAT - Power & Sample Size" );
  
  but1 <- tkbutton(form, text="Family - Binary",
                   command=power.binFamForm);
  but2 <- tkbutton(form, text="Family - Continuous",
                   command=power.contFamForm);
  but3 <- tkbutton(form, text="Population - Case / Control",
                   command=power.caseContForm);
  but4 <- tkbutton(form, text="Population - Continuous",
                   command=power.contPopForm);

  tkgrid( but1, but2 );
  tkgrid( but3, but4 );

  ## wait for completion
  tkwait.window( form );
  return(invisible());
}

power.textEntry <- function( form, label, value="" ) {
  ## Create a frame for everything
  frame <- tkframe( form, relief="groove", borderwidth=2 );
  tkgrid( frame );
  ##tkgrid.configure( frame, sticky="news" );
  tkgrid.configure( frame, sticky="nws" );

  ## The label
  lbl <- tklabel( frame, text=label );

  ## The entry box
  teVar <- tclVar( value );
  te <- tkentry( frame, width=POWERENTRYWIDTH, textvariable=teVar );

  ## and grid it all
  tkgrid( lbl, te );
  tkgrid.configure( lbl, sticky="ew" );

  ## return the tcl variable
  return( teVar );
}

power.choicesEntry <- function( form, label, choices ){
  ## Create a frame for everything
  frame <- tkframe( form, relief="groove", borderwidth=2 );
  tkgrid( frame );
  ##tkgrid.configure( frame, sticky="news" );
  tkgrid.configure( frame, sticky="nws" );

  ## The label
  lbl <- tklabel( frame, text=label );

  ## The choices
  rb <- list();
  rb.lbl <- list();
  rb.subframe <- list();

  ## Create the subframe for each choice
  for( i in 1:length(choices) ) 
    rb.subframe[[i]] <- tkframe( frame, relief='groove', borderwidth=1 );
  
  ## grid the subframes
  if( length(choices)==2 ){
    tkgrid( lbl, rb.subframe[[1]], rb.subframe[[2]] );
  }else if( length(choices)==3 ){
    tkgrid( lbl, rb.subframe[[1]], rb.subframe[[2]], rb.subframe[[3]] );
  }else if( length(choices)==4 ){
    tkgrid( lbl, rb.subframe[[1]], rb.subframe[[2]], rb.subframe[[3]], rb.subframe[[4]] );
  }

  ## Create the variable
  rbVal <- tclVar( choices[1] );
  
  ## Create each choice
  for( i in 1:length(choices) ) {
    rb[[i]] <- tkradiobutton( rb.subframe[[i]] );
    tkconfigure( rb[[i]], variable=rbVal, value=choices[i]  );
    rb.lbl[[i]] <- tklabel( rb.subframe[[i]], text=choices[i] );
    tkgrid( rb[[i]], rb.lbl[[i]] );
  }

  ## Return the variable
  return( rbVal );
}

power.binFamForm <- function() {
  ## Set up the modal form
  form <- tktoplevel();
  tkwm.deiconify(form);
  tkgrab.set(form); # make it modal
  tkwm.title( form, "P2BAT Power & SS - Family, Binary" );

  ## get the globals
  globs <- getPower( "globs" );
  
  ## draw the stuff
  globs$numOffspring <- power.textEntry( form, "# offspring", "1" );
  globs$missingParents <- power.choicesEntry( form, "# missing parents", c("0","1","2") );
  globs$numFam <- power.textEntry( form, "# families", "100" );
  globs$addiOffspringPheno <- power.choicesEntry( form, "additional offspring phenotypes (mis parents)", c("TRUE","FALSE") );
  globs$ascertainment <- power.choicesEntry( form, "ascertainment", c("unaffected", "affected", "not applicable") );
  
  globs$model <- power.choicesEntry( form, "model", c("additive","recessive","dominant") );
  globs$model.afreq <- power.textEntry( form, "allele frequency", "0.2" );
  globs$model.incrAfreq <- power.textEntry( form, "allele freq. incr.", "0.1" );
  globs$model.disLocIsMarker <- power.choicesEntry( form, "disease loc is marker", c("TRUE","FALSE") );

  globs$model.choice <- power.choicesEntry( form, "option", c("1","2","3","4") )
  globs$model.popPrev <- power.textEntry( form, "  option 1,3,4: population prevalence", "0.3" );
  globs$model.genAF <- power.textEntry( form, "  option 1: genetic attributable fraction", "0.1" );
  globs$model.penAA <- power.textEntry( form, "  option 2: penetrance AA", "0.8" );
  globs$model.penAB <- power.textEntry( form, "  option 2: penetrance AB", "0.5" );
  globs$model.penBB <- power.textEntry( form, "  option 2: penetrance BB", "0.3" );
  globs$model.OR <- power.textEntry( form, "  option 3: OR", "2.333" );
  globs$model.aOR <- power.textEntry( form, "  option 4: allelic OR", "2.471" );

  globs$stat.sigLevel <- power.textEntry( form, "significance level", "0.01" );
  globs$stat.offset <- power.textEntry( form, "offset [default(empty) is pop prev]" );
  globs$comp <- power.choicesEntry( form, "computation", c("numerical","approximation","simulation") );
  globs$log <- power.textEntry( form, "Logfile (will be overwritten)", "pbatLog.txt" );

  globs$but <- tkbutton( form, text="Process", command=power.binFamProcess );
  tkgrid( globs$but );

  ## set the globals
  setPower( "globs", globs );
}

pbat.binaryFamily2 <-
  function(
           numOffspring=1, missingParents=0, numFam=0,
           addiOffspringPheno=1, ## only when you have missing parents
           ascertainment="unaffected",
           model="additive", model.afreq=0.2, model.incrAfreq=0,
           model.disLocIsMarker=TRUE,
           
           model.popPrev=NULL,  ## Options 1, 3, & 4
           model.genAF=NULL,    ## Option 1
           model.penAA=NULL, model.penAB=NULL, model.penBB=NULL, ## Option 2
           model.OR=NULL,       ## Option 3
           model.aOR=NULL,      ## Option 4
           
           stat.sigLevel=0.01,
           stat.offset="",  ## defaults to population prevalence
           comp="numerical",
           log="pbatLog.txt",
           
           model.choice=1)
{
  if( model.choice == 2 )
    model.popPrev <- NULL;

  if( model.choice != 2 ){
    model.penAA <- NULL;
    model.penAB <- NULL;
    model.penBB <- NULL;
  }

  if( model.choice != 3 )
    model.OR <- NULL;

  if( model.choice != 4 )
    model.aOR <- NULL;

  pbat.binaryFamily(numOffspring=numOffspring, missingParents=missingParents, numFam=numFam,
                    addiOffspringPheno=addiOffspringPheno,
                    ascertainment=ascertainment,
                    model=model, model.afreq=model.afreq, model.incrAfreq=model.incrAfreq,
                    model.disLocIsMarker=model.disLocIsMarker,
                    model.popPrev=model.popPrev,
                    model.genAF=model.genAF,
                    model.penAA=model.penAA, model.penAB=model.penAB, model.penBB=model.penBB,
                    model.OR=model.OR,
                    model.aOR=model.aOR,
                    stat.sigLevel=stat.sigLevel,
                    stat.offset=stat.offset,
                    comp=comp,
                    log=log);
}

power.binFamProcess <- function() {
  ## get the globals
  globs <- getPower( "globs" );


  ## and process the stuff
  pbat.binaryFamily2(
                     numOffspring=tclvalue(globs$numOffspring),
                     addiOffspringPheno=as.numeric(tclvalue(globs$addiOffspringPheno)==TRUE),
                     ascertainment=tclvalue(globs$ascertainment),
                     model=tclvalue(globs$model), model.afreq=tclvalue(globs$model.afreq), model.incrAfreq=tclvalue(globs$model.incrAfreq),
                     model.disLocIsMarker=as.numeric(tclvalue(globs$model.disLocIsMarker)=="TRUE"),
                     model.popPrev=tclvalue(globs$model.popPrev),
                     model.genAF=tclvalue(globs$model.genAF),
                     model.penAA=tclvalue(globs$model.penAA),
                     model.penAB=tclvalue(globs$model.penAB),
                     model.penBB=tclvalue(globs$model.penBB),
                     model.OR=tclvalue(globs$model.OR),
                     model.aOR=tclvalue(globs$model.aOR),
                     stat.sigLevel=tclvalue(globs$stat.sigLevel),
                     stat.offset=tclvalue(globs$stat.offset),
                     comp=tclvalue(globs$comp),
                     log=tclvalue(globs$log),
                     
                     model.choice = as.numeric(tclvalue(globs$model.choice)) );
}

power.contFamForm <- function() {
  ## Set up the modal form
  form <- tktoplevel();
  tkwm.deiconify(form);
  tkgrab.set(form); # make it modal
  tkwm.title( form, "P2BAT Power & SS - Family, Continuous" );

  ## get the globals
  globs <- getPower( "globs" );
  
  ## draw the stuff (almost identical to the previous!)
  globs$numOffspring <- power.textEntry( form, "# offspring", "1" );
  globs$missingParents <- power.choicesEntry( form, "# missing parents", c("0","1","2") );
  globs$numFam <- power.textEntry( form, "# families", "100" );
  globs$addiOffspringPheno <- power.choicesEntry( form, "additional offspring phenotypes (mis parents)", c("TRUE","FALSE") );
  globs$ascertainment <- power.choicesEntry( form, "ascertainment", c("unaffected", "affected", "not applicable") );
  
  globs$model <- power.choicesEntry( form, "model", c("additive","recessive","dominant") );
  globs$model.afreq <- power.textEntry( form, "allele frequency", "0.2" );
  globs$model.incrAfreq <- power.textEntry( form, "allele freq. incr.", "0.1" );
  globs$model.disLocIsMarker <- power.choicesEntry( form, "disease loc is marker", c("TRUE","FALSE") );

  globs$model.heritability <- power.textEntry( form, "heritability", "0.1" );
  globs$model.afreqMarker <- power.textEntry( form, "allele frequency of marker", "0.3" );
  globs$model.prDiseaseGivenMarker <- power.textEntry( form, "Pr(Disease|marker)", "1" );

  globs$stat.sigLevel <- power.textEntry( form, "significance level", "0.01" );
  globs$stat.offset <- power.textEntry( form, "offset [default(empty) is pop prev]" );
  globs$comp <- power.choicesEntry( form, "computation", c("numerical","approximation","simulation") );
  globs$log <- power.textEntry( form, "Logfile (will be overwritten)", "pbatLog.txt" );

  globs$but <- tkbutton( form, text="Process", command=power.contFamProcess );
  tkgrid( globs$but );

  ## set the globals
  setPower( "globs", globs );
}

power.contFamProcess <- function() {
  ## get the globals
  globs <- getPower( "globs" );

  ## and process the stuff
  pbat.continuousFamily(
                        numOffspring=tclvalue(globs$numOffspring),
                        addiOffspringPheno=as.numeric(tclvalue(globs$addiOffspringPheno)==TRUE),
                        ascertainment=tclvalue(globs$ascertainment),
                        model=tclvalue(globs$model), model.afreq=tclvalue(globs$model.afreq), model.incrAfreq=tclvalue(globs$model.incrAfreq),
                        model.disLocIsMarker=as.numeric(tclvalue(globs$model.disLocIsMarker)=="TRUE"),
                        model.heritability=tclvalue(globs$model.heritability),
                        model.afreqMarker=tclvalue(globs$model.afreqMarker),
                        model.prDiseaseGivenMarker=tclvalue(globs$model.prDiseaseGivenMarker),
                        stat.sigLevel=tclvalue(globs$stat.sigLevel),
                        stat.offset=tclvalue(globs$stat.offset),
                        comp=tclvalue(globs$comp),
                        log=tclvalue(globs$log) );
}


power.caseContForm <- function() {
  ## Set up the modal form
  form <- tktoplevel();
  tkwm.deiconify(form);
  tkgrab.set(form); # make it modal
  tkwm.title( form, "P2BAT Power & SS - Population, Case/Control" );

  ## get the globals
  globs <- getPower( "globs" );
  
  ## draw the stuff
  globs$model <- power.choicesEntry( form, "model", c("additive","recessive","dominant") );
  globs$model.minafreq <- power.textEntry( form, "min allele frequency", "0.1" );
  globs$model.incrAfreq <- power.textEntry( form, "allele freq. incr.", "0.1" );
  globs$model.prevalence <- power.textEntry( form, "prevalence", "0.1" );
  globs$model.choice <- power.choicesEntry( form, "option", c("1","2") );
  globs$model.ORofABvsBB <- power.textEntry( form, "  option 1: OR of AB to BB", "1.5" );
  globs$model.aOR <- power.textEntry( form, "  option 2: allelic OR", "1.481" );

  globs$comp.cases <- power.textEntry( form, "cases", "500" );
  globs$comp.controls <- power.textEntry( form, "controls", "500" );
  globs$comp.caseControlRatio <- power.textEntry( form, "case/control ratio", "1.5" );
  globs$comp.power <- power.textEntry( form, "power", "0.8" );
  globs$comp.sigLevel <- power.textEntry( form, "significance level", "0.05" );
  globs$comp.numSim <- power.textEntry( form, "# simulations", 1000 );

  globs$mode <- power.choicesEntry( form, "compute power or sample size", c("power", "ss") );
  globs$log <- power.textEntry( form, "Logfile (will be overwritten)", "pbatLog.txt" );

  globs$but <- tkbutton( form, text="Process", command=power.caseContProcess );
  tkgrid( globs$but );

  ## set the globals
  setPower( "globs", globs );
}

pbat.caseControl2 <-
  function(
           model="additive", model.minafreq=0.1, model.incrAfreq=0.1,
           model.prevalence=0.1,
           model.ORofABvsBB=NULL, # Option 1 - default 1.5
           model.aOR=NULL,      # Option 2 - default 1.481
           comp.cases=500, comp.controls=500, comp.caseControlRatio=1.5,
           comp.power=0.8, comp.sigLevel=0.05, comp.numSim=1000,
           mode="power",
           log="pbatLog.txt",
           
           model.choice=1) {
  if( model.choice==1 ) {
    model.aOR <- NULL;
  }else{
    model.ORofABvsBB <- NULL;
  }

  pbat.caseControl(
                   model=model, model.minafreq=model.minafreq, model.incrAfreq=model.incrAfreq,
                   model.prevalence=model.prevalence,
                   model.ORofABvsBB=model.ORofABvsBB,
                   model.aOR=model.aOR,
                   comp.cases=comp.cases, comp.controls=comp.controls, comp.caseControlRatio=comp.caseControlRatio,
                   comp.power=comp.power, comp.sigLevel=comp.sigLevel, comp.numSim=comp.numSim,
                   mode=mode, log=log );
}

power.caseContProcess <- function() {
  ## get the globals
  globs <- getPower( "globs" );

  ## and process the stuff
  pbat.caseControl2(
                    model=tclvalue(globs$model),
                    model.minafreq=tclvalue(globs$model.minafreq),
                    model.incrAfreq=tclvalue(globs$model.incrAfreq),
                    model.ORofABvsBB=tclvalue(globs$model.ORofABvsBB),
                    model.aOR=tclvalue(globs$model.aOR),
                    comp.cases=tclvalue(globs$comp.cases),
                    comp.controls=tclvalue(globs$comp.controls),
                    comp.caseControlRatio=tclvalue(globs$comp.caseControlRatio),
                    comp.power=tclvalue(globs$comp.power),
                    comp.sigLevel=tclvalue(globs$comp.sigLevel),
                    comp.numSim=tclvalue(globs$comp.numSim),
                    mode=tclvalue(globs$mode),
                    log=tclvalue(globs$log),
                    model.choice=tclvalue(globs$model.choice) );
}

power.contPopForm <- function() {
  ## Set up the modal form
  form <- tktoplevel();
  tkwm.deiconify(form);
  tkgrab.set(form); # make it modal
  tkwm.title( form, "P2BAT Power & SS - Population, Continuous" );

  ## get the globals
  globs <- getPower( "globs" );
  
  ## draw the stuff
  globs$model <- power.choicesEntry( form, "model", c("additive","recessive","dominant") );
  globs$model.minafreq <- power.textEntry( form, "min allele frequency", "0.1" );
  globs$model.incrAfreq <- power.textEntry( form, "allele freq. incr.", "0.1" );
  globs$model.heritability <- power.textEntry( form, "heritability", "0.001" );

  globs$comp.numProbands <- power.textEntry( form, "number of probands", "2000" );
  globs$comp.power <- power.textEntry( form, "power", "0.8" );
  globs$comp.power <- power.textEntry( form, "power", "0.8" );
  globs$comp.sigLevel <- power.textEntry( form, "significance level", "0.05" );
  globs$comp.numSim <- power.textEntry( form, "# simulations", 1000 );
  
  globs$mode <- power.choicesEntry( form, "compute power or sample size", c("power", "ss") );
  globs$log <- power.textEntry( form, "Logfile (will be overwritten)", "pbatLog.txt" );

  globs$but <- tkbutton( form, text="Process", command=power.contPopProcess );
  tkgrid( globs$but );

  ## set the globals
  setPower( "globs", globs );
}

power.contPopProcess <- function(){
   ## get the globals
  globs <- getPower( "globs" );

  ## and process the stuff
  pbat.popQuant(
                model=tclvalue(globs$model),
                model.minafreq=tclvalue(globs$model.minafreq),
                model.incrAfreq=tclvalue(globs$model.incrAfreq),
                model.heritability=tclvalue(globs$model.heritability),
                comp.numProbands=tclvalue(globs$comp.numProbands),
                comp.power=tclvalue(globs$comp.power),
                comp.sigLevel=tclvalue(globs$comp.sigLevel),
                comp.numSim=tclvalue(globs$comp.numSim),
                mode=tclvalue(globs$mode),
                log=tclvalue(globs$log) );
}
