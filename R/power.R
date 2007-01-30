##############################################################
## Thomas Hoffmann                                           #
## Created 7/14/2006                                         #
##                                                           #
## DESCRIPTION:                                              #
##  Wraps in the power calculations using sh.exe             #
##############################################################

## if you have incessant looping, it's worthwhile to note that
##  it is extremely important for the program to terminate normally.

matchLoc <- function( command, choices, vals=(1:length(choices))-1 ){
  ## first make sure it's within bounds
  if( sum(command==choices) != 1 )
    stop( paste( "You must select from ", csPasteVector(choices),
                "; you supplied ", command, ".", sep="" ) );
  
  ## Then return the proper value
  return( vals[which(command==choices)] );
}

pbat.start <- function(choice,log){
  return( c("","",choice,log) );
}

pbat.family <- function(numOffspring, missingParents, numFam,
                        addiOffspringPheno, ascertainment){
  cmds <- c("1","1",  ## design, change family design
            numOffspring, missingParents, numFam);
  if( missingParents != 0 )
    cmds <- c( cmds, addiOffspringPheno );
  cmds <- c(cmds, 
            matchLoc(ascertainment,c("unaffected","affected","not applicable")),
            "" ## Return back to the menu
            );

  return( cmds );
}

pbat.runpowerss <- function( cmds, log ) {
  ## The infamous pbatdata.txt file... this is needed everywhere!
  checkAndGetPbatdata();
  
  ## Now run the stuff
  cmds <- c(cmds,"","","","",-1); ## protective
  if( file.exists(log) ) file.remove(log); ## otherwise we print a whole string of them
  ##print( csPasteVector( cmds ) );
  pbatControl( cmds, intern=TRUE  );  ## intern=FALSE is debug only
  ##                                      so we can see what goes down
  printFile( log );
  return(invisible());
}

pbat.binaryFamily <-
  function(numOffspring=1, missingParents=0, numFam=0,
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
           log="pbatLog.txt")
{
  ## Initial family-based stuff
  cmds <- c(pbat.start(1,log),
            pbat.family(numOffspring,missingParents,numFam,
                        addiOffspringPheno,ascertainment));
  
  
  ## 2) Genetic model
  model.extraParms <- c();
  model.modelNum <- 1;
  ## switch the type, fill in the extraParms (dependent on type, but used later)
  if( !is.null(model.popPrev) && !is.null(model.genAF) ) {
    model.modelNum <- 1;
    model.extraParms <- c( model.popPrev, model.genAF );
  }else if( !is.null(model.penAA) && !is.null(model.penAB) && !is.null(model.penBB) ){
    model.modelNum <- 2;
    model.extraParms <- c( model.penAA, model.penAB, model.penBB );
  }else if( !is.null(model.popPrev) && !is.null(model.OR) ){
    model.modelNum <- 3;
    model.extraParms <- c( model.popPrev, model.OR );
  }else if( !is.null(model.popPrev) && !is.null(model.aOR) ){
    model.modelNum <- 4;
    model.extraParms <- c( model.popPrev, model.aOR );
  }else{
    stop( "Please see the help on how to specify the model." );
  }
  
  ## finally add to the command
  cmds <- c(cmds,
            2, ## Select genetic model from the menu
            model.modelNum,
            matchLoc( model, c("additive", "multi", "recessive", "dominant" ) ),
            model.afreq, model.incrAfreq,
            model.extraParms );
  if( model.modelNum == 2 ) cmds <- cmds[-(length(cmds)-2-length(model.extraParms))]; ## bugfix 01/29/07
  if( as.numeric(model.incrAfreq)==0 )
    cmds <- c( cmds, as.numeric(model.disLocIsMarker) );
  
  ## and return to the menu
  cmds <- c( cmds, "" );

  ## 3) Statistical parameters
  cmds <- c( cmds, "3", stat.sigLevel, stat.offset, "", "" )  ## _double_ return... horrible
  
  ## 4) and finally the power calculation!!!
  cmds <- c( cmds, 4, 
            matchLoc( comp, c("numerical", "approximation", "simulation"), 1:3 )
            );
  
  ## Lastly just kill it (nicely)
  cmds <- c( cmds, "", "-1", "", "", "-1" ); ## twice just in case

  ## -- Now, do the real processing on the list!
  pbat.runpowerss( cmds, log );
}


pbat.continuousFamily <-
  function(numOffspring=1, missingParents=0, numFam=0,
           addiOffspringPheno, ## only when you have missing parents
           ascertainment="unaffected",
           model="additive", model.afreq=0.1, model.incrAfreq=0,
           model.disLocIsMarker=FALSE,                      
           model.heritability=0.1, model.afreqMarker=0,
           model.prDiseaseGivenMarker=1,
           stat.sigLevel=0.05, stat.offset="",
           comp="numerical",
           log="pbatLog.txt")
{
  cmds <- c(pbat.start(2,log),
            pbat.family(numOffspring,missingParents,numFam,
                        addiOffspringPheno,ascertainment));

  ## The genetic model
  cmds <- c(cmds, 2,
            matchLoc( model, c("additive", "recessive", "dominant" ) ),
            model.afreq, model.incrAfreq,
            model.heritability,
            as.numeric(model.disLocIsMarker),
            model.afreqMarker, model.prDiseaseGivenMarker
            );
  ## THE ABOVE IS GETTING REALLY SCREWY!!!
  ## and return to the menu
  cmds <- c( cmds, "", "" );
  
  ## 3) Statistical parameters
  cmds <- c( cmds, "3", stat.offset, stat.sigLevel, "", "" ) ## backwards

  ## 4) and finally the power calculation!!!
  cmds <- c( cmds, 4,
            matchLoc( comp, c("numerical", "approximation", "simulation"), 1:3 ) );
  
  ## Lastly just kill it (nicely)
  cmds <- c( cmds, "", "", "-1", "", "", "-1" );  ## do it twice in case

  ## -- Now, do the real processing on the list!
  pbat.runpowerss( cmds, log );
}

pbat.caseControl <-
  function(model="additive", model.minafreq=0.1, model.incrAfreq=0.1,
           model.prevalence=0.1,
           model.ORofABvsBB=NULL, # Option 1 - default 1.5
           model.aOR=NULL,      # Option 2 - default 1.481
           comp.cases=500, comp.controls=500, comp.caseControlRatio=1.5,
           comp.power=0.8, comp.sigLevel=0.05, comp.numSim=1000,
           mode="power",
           log="pbatLog.txt")
{
  cmds <- pbat.start(3,log);

  ## The genetic model
  model.loc <- matchLoc( model, c("additive", "multi", "recessive", "dominant" ) );

  if( !(
        ( is.null(model.ORofABvsBB) && !is.null(model.aOR))
      || ( !is.null(model.ORofABvsBB) && is.null(model.aOR)) ) ){
    ## exclusive or
    stop( "Either model.ORofABvsBB or model.aOR must be non-null, but not both. See ?pbat.caseControl for more details." );
  }
  if( !is.null(model.ORofABvsBB) ){
    cmds <- c(cmds, 1, ## specifies the first option
              model.loc,
              model.minafreq, model.incrAfreq,
              model.prevalence,
              model.ORofABvsBB );
  }else{
    cmds <- c(cmds, 2, ## specifies the second option
              model.loc,
              model.minafreq, model.incrAfreq,
              model.prevalence,
              model.aOR );
  }
  ## and return to the menu
  ##cmds <- c( cmds, "" ); ## AUTOMATICALLY RETURNS!!! Arghh!
  
  ## computational parameters
  cmds <- c(cmds, 3, ## specifies the comp parameters menu
            comp.cases, comp.controls, comp.caseControlRatio,
            comp.power, comp.sigLevel, comp.numSim,
            ##""  ## and return to the main menu - NO AUTO RETURNS AGAIN
            );

  ## now process the data
  if( mode=="power" ){
    cmds <- c(cmds,4,"","",-1,"","","-1");
  }else if(mode=="ss" ){
    cmds <- c(cmds,5,"","",-1,"","","-1");
  }

  ## -- Now, do the real processing on the list!
  pbat.runpowerss( cmds, log );
}

pbat.popQuant <-
  function(model="additive",
           model.minafreq=0.2, model.incrAfreq=0.1,
           model.heritability=0.001,
           comp.numProbands=2000,
           comp.power=0.8, comp.sigLevel=0.05, comp.numSim=10000,
           mode="power",
           log="pbatLog.txt")
{
  cmds <- pbat.start(4,log);

  cmds <- c(cmds, 1, ## model
            matchLoc( model, c("additive", "recessive", "dominant" ) ),
            model.minafreq, model.incrAfreq,
            model.heritability,
            ##0, ## return to the main menu ## auto-returns
            2, ## specify computational parameters
            comp.numProbands,
            comp.power, comp.sigLevel, comp.numSim,
            ##"" ## return to the main menu -- auto-returns
            );

  ## now process the data
  if( mode=="power" ){
    cmds <- c(cmds,3,"","",-1);
  }else if(mode=="ss" ){
    cmds <- c(cmds,4,"","",-1);
  }

  ## -- Now, do the real processing on the list!
  pbat.runpowerss( cmds, log );
}
           
