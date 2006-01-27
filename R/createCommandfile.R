#####################################################################
## Thomas Hoffmann                                                  #
## EXPORTED:    07/08/2005                                          #
## MODIFIED:    01/25/2006                                          #
## DESCRIPTION: Creating the command file to pass to pbat.          #
#####################################################################

PBATDATAURL <- "http://www.biostat.harvard.edu/~clange/pbatdata.zip";

#####################################################################
## getPbatdata()                                                    #
## Gets the 'pbatdata.txt' file, from the internet.                 #
#####################################################################
getPbatdata <- function() {
  ## Give the user a chance to say yes or no:
  msgStr <- paste("Can I attempt to download 'pbatdata.txt' from '",
                  PBATDATAURL,
                  "'? This file is needed.", sep="");
  if( "yes" != tclvalue(tkmessageBox(title="pbatdata.txt",message=msgStr,icon="question",type="yesno")) )
    return();

  ## Carry on with downloading
  pbatpath <- str.getpath(pbat.get());
  zipfile <- paste( pbatpath, "/pbatdata.zip", sep="" );
  if( pbatpath=="" ) zipfile <- "./pbatdata.zip";
  ###download.file( PBATDATAURL, zipfile );
  download.file( PBATDATAURL, "./pbatdata.zip" );
  Sys.sleep(1); ## Just in case this is why it was getting corrupted
  pbatdatafile <- zip.file.extract( file="pbatdata.txt", zipname="pbatdata.zip" );
  destfile <- paste( pbatpath, "/pbatdata.txt", sep="" );
  if( pbatpath=="" ) destfile <- "pbatdata.txt";
  if( file.exists(pbatdatafile) ){
    file.copy(pbatdatafile, destfile);
  }
}

#####################################################################
## getTimeStamp(...)                                                #
## Gets a unique time-stamp string for the output!                  #
#####################################################################
getTimeStamp <- function() {
  zpad <- function(n, pad=2) {
    if( nchar(n)<pad )
      return( paste( rep("0",pad-nchar(n)), n, sep="" ) );
    return(n);
  }
  
  d <- as.POSIXlt( Sys.time() );
  return( paste( 1900+d$year, zpad(d$mon), zpad(d$mday), zpad(d$hour), zpad(d$min), zpad(d$sec), sep="" ) );
}

isTimeStamped <- function( str, extLen=3 ) {
  timeLen <- strlen(getTimeStamp());
  strLen <- strlen(str);
  strPbatLen <- strlen("pbat")+1;  # 10/07/2005
  if( strLen < 5+timeLen ) return( FALSE ); # too short
  possTimeStr <- substring( str, strPbatLen, strPbatLen+timeLen-1 );
  #print( possTimeStr );
  if( !is.na( as.numeric( possTimeStr ) ) )
    return(TRUE);
  return(FALSE);
}

# Assumes isTimeStamped( str ) _already_ returned TRUE
extractTimeStamp <- function( str, extLen=3 ) {
  timeLen <- strlen(getTimeStamp());
  strLen <- strlen(str);
  strPbatLen <- strlen("pbat")+1;  # 10/07/2005
  if( strLen < 5+timeLen ) return( FALSE ); # too short
  possTimeStr <- substring( str, strPbatLen, strPbatLen+timeLen-1 );
  return( possTimeStr );
}


## Moved outside of pbat.create.commandfile(...) 9/?/05
############################################
## Whether 'subcol' is contained in 'col'. #
############################################
isVecContained <- function( subcol, col ) {
  for( i in 1:length(subcol) ) {
    if( sum( subcol[i]==col ) != 1 )
      return( FALSE );
  }
  return( TRUE );
}

# Moved outside of pbat.create.commandfile(...) 9/20/05
#################################################################
## paste a vector of strings together                           #
## SQUOTE    if TRUE, surrounds each string with a single quote #
## COMMASEP  if TRUE, comma seperates the values                #
#################################################################
pasteVector <- function( vector, SQUOTE=FALSE, COMMASEP=FALSE ) {
  if( length(vector) < 1 ) return("");
  
  squote <- function(str) {return(str)};
  if( SQUOTE==TRUE )
    squote <- function(str){return(paste("'",str,"'",sep=""));};
  
  if( length(vector) == 1 ) return( squote(vector[1]) );
  strRet = squote(vector[1]);
  for( i in 2:length(vector) ) {
    if( COMMASEP==FALSE ) {
      strRet <- paste(strRet, squote(vector[i]));
    }else{
      strRet <- paste(strRet, ", ", squote(vector[i]), sep="");
    }
  }
  return(strRet);
} # Status: debugged
##### And just a wrapper to make commands simpler later #######
csPasteVector <- function( vector ){
  return( pasteVector( vector, SQUOTE=TRUE, COMMASEP=TRUE ) );
} # Status: debugged

pasteVector2 <- function( vector, sep=" " ){
  if( length(vector) < 1 ) return("");

  if(length(vector) == 1 ) return(vector);
  strRet <- vector[1];
  for( i in 2:length(vector) )
    strRet <- paste(strRet, sep, vector[i], sep="");
  return(strRet);
}


#####################################################################
## pbat.create.commandfile(...)                                     #
## LOTS of options and info! See .Rd file!                          #
##                                                                  #
## This includes all of the debugging code and everything.          #
## Hopefully some good debugging here :).                           #
####################################################################
pbat.create.commandfile <- function(
       pedfile, phefile="",
       snps="",
       phenos="", time="",
       preds="", preds.order="",
       inters="",
       groups.var="", groups="",
       fbat="gee",
       censor="",
       max.pheno=1, min.pheno=1,
       null="no linkage, no association", alpha=0.05,
       trans.pheno="none", trans.pred="none", trans.inter="none",
       scan.pred="all", scan.inter="all",
       scan.genetic="additive",
       offset="default",
       screening="conditional power", distribution="continuous",
       logfile="",
       max.gee=1,
       max.ped=7, min.info=20,
       haplos=NULL, incl.ambhaplos=TRUE, infer.mis.snp=TRUE,
       sub.haplos=FALSE, length.haplos=2, adj.snps=TRUE,
       overall.haplo=FALSE, cutoff.haplo=FALSE,
       output="normal",
       max.mating.types=10000,
       commandfile="",
       future.expansion=NULL )
{
  ##-----------------------------
  ## fix up extensions / naming -
  ##-----------------------------
  pedfile <- str.file.extension( pedfile, ".ped" );
  if( phefile=="" ) {
    phefile <- paste( substring(pedfile,1,strlen(pedfile)-3), "phe", sep="" );
  }else{
    phefile <- str.file.extension( phefile, ".phe" );
  }
  if( logfile=="" ) {
    if( isTimeStamped( pedfile ) ) {
      logfile <- paste( substring(pedfile,1,strlen(pedfile)-4),
                        "", sep="" );
    }else{
      logfile <- paste( substring(pedfile,1,strlen(pedfile)-4),
                       getTimeStamp(), "", sep="" );
    }
  }
  if( commandfile=="" )
    commandfile <- paste( substring(logfile,1,strlen(logfile)-3), "txt", sep="" );
  #print( "COMMANDFILE" );
  #print( commandfile );
  #print( "LOGFILE" );
  #print( logfile );

  #warning( "COMMented out this part of logfile stuff too..." );
#  if( file.exists(logfile) )
#    stop( paste("Logfile '",logfile,"' already exists!  (Note, the default name for the 'logfile' option is the prefix of the pedigree file with a .txt suffix.", sep="" ) );

  # Take certain strings to lowercase (but NOT the phenos stuff!)
  fbat <- tolower(fbat);
  null <- tolower(null);
  trans.pheno <- tolower(trans.pheno);
  trans.pred <- tolower(trans.pred);
  trans.inter <- tolower(trans.inter);
  scan.pred <- tolower(scan.pred);
  scan.inter <- tolower(scan.inter);
  scan.genetic <- tolower(scan.genetic);
  offset <- tolower(offset);
  screening <- tolower(screening);
  distribution <- tolower(distribution);
  output <- tolower(output);

  # Note: these functions are included within this function
  #        because they are operating on variables in this
  #        function.  Slightly confusing, but I think it makes
  #        the most sense this way.
  
  ####################################
  # write the command (unless empty) #
  ####################################
  writeCommand <- function( commandStr, vals, end=FALSE ) {
    if(  !( length(vals)==1 && (is.na(vals)||vals=="") )  ) {
      if( end==FALSE ) {
        writeLines( paste( commandStr, pasteVector(vals) ), con=outfile );
      }else{
        writeLines( paste( commandStr, pasteVector(vals), "end" ), con=outfile );
      }
    }
  } # Status: debugged

  ###################################################################
  # Prints helpful error msg if 'subcol' is not contained in 'col'. #
  # if AT.MOST.SINGLETON=TRUE, then 'subcol' can be of at           #
  #  most length 1                                                  #
  ###################################################################
  errorVecNotContained <- function( command, subcol, col,
                                 AT.MOST.SINGLETON=FALSE ){
    if( length(subcol)==1 && subcol=="" ) return(); # empty set is contained!
       
    if( !isVecContained(subcol,col) ) {
      stop( paste( "For the option '", command,
                   "', the values that you specified {",
                   csPasteVector(subcol),
                   "} did not match the possible values {",
                   csPasteVector(col),
                   "}.",
                   sep=""
                   ) );
    }
    if( length(subcol)>1 && AT.MOST.SINGLETON==TRUE ) {
      stop( paste("For the option '", command,
                  "', the values that you specified {",
                  csPasteVector(subcol),
                  "} was of too long a length.  Keep in mind it must take at most a single string value from the following collection {",
                  csPasteVector(col), "}.",
                  sep=""
                  ) );
    }
  }

  #########################################################################
  # Prints out a useful error message if their is a nonempty intersection #
  #  between 'col1' and 'col2'                                            #
  #########################################################################
  errorIfAnyMatch <- function( col1, col2, nameCol1, nameCol2 ) {
    if( length(col1)==1 && col1=="" ) return(); # collection is empty
    if( length(col2)==1 && col2=="" ) return();
    
    for( i in 1:length(col1) )
      if( sum(col1[i]==col2)>0 )
        stop( paste("There should not be any overlap in the following two option's collections: ",
                    nameCol1, "{", csPasteVector(col1), "}, ",
                    nameCol2, "{", csPasteVector(col2), "}.",
                    sep="" ) );
  }

  ###############################################
  # write the command from human-readable input #
  # The beauty of this function is it includes  #
  #   the error handling routines within.       #
  # stop() is like throwing an exception, which #
  #   if not caught goes all the way to the user#
  #   and halts the program.                    #
  ###############################################
  writeCommandStrMatch <- function( commandStr, str, strVec, vals=c(0:(length(strVec)-1)) ){
    if( sum(str==strVec)!=1 ) {
      # Error! New: print out a message here for ease!
      stop( paste( "'", commandStr, "' can only take on the following values: ",
                   csPasteVector( strVec ),
                   ".  You passed the invalid value '", str, "'.",
                   sep="" ) );
    }

    writeCommand( commandStr, vals[which(str==strVec)] );
    return( TRUE ); # success!
  }
  ###### Again just another wrapper to make life a little easier. #######
  writeCommandStrMatch1 <- function( commandStr, str, strVec ) {
    return(  writeCommandStrMatch( commandStr, str, strVec, vals=c(1:length(strVec)) )  );
  } # NOT DEBUGGED

  ###############################################################
  # Prints error messages if outside a range (Closed interval). #
  #  If the value is NULL or "", then there is no max/min       #
  ###############################################################
  errorRangeCheck <- function( commandStr, value, min=1, max=NULL, IS.INTEGER=TRUE ) {
    hasMin <- !is.null(min) && min!="";
    hasMax <- !is.null(max) && max!="";
    if( (hasMin && value<min) || (hasMax && value>max) ) {
      minStr <- as.character(min); if(!hasMin) minStr <- "INF";
      maxStr <- as.character(max); if(!hasMax) maxStr <- "INF";
      stop( paste("For the option '", commandStr, "', the value must be in the range [",
                  minStr, ",", maxStr, "]. The value you supplied was ", value, ".",
                  sep="") );
    }

    #if( IS.INTEGER && value!=floor(value) )
    if( IS.INTEGER && as.numeric(value)!=floor(as.numeric(value)) )
      stop( paste("For the option '", commandStr,
                  "', the value must be _an integer_ in the range [",
                  minStr, ",", maxStr, "]. The value you supplied was ", value, ".",
                  sep="") );
  }

  #-----------------
  # Some debugging -
  #-----------------

  # first make sure certain files exist

  ## First try to copy from the pbat directory.
  pbatdatafile <- paste( str.getpath(pbat.get()), "/pbatdata.txt", sep="" );
  if( file.exists( pbatdatafile ) )
    file.copy( from=pbatdatafile, to=paste(getwd(),"/pbatdata.txt",sep="") );

  ## If we can't find it, then try other things
  if( !file.exists( paste(getwd(),"/pbatdata.txt",sep="") ) ) {

    ## See if it's anywhere in the path
    newLoc <- pathFindFile("pbatdata.txt");
    if( newLoc != "" ){
      ## found it! copy it over!
      file.copy( from=newLoc, to=paste(getwd(),"/pbatdata.txt",sep="") ); ## seems to have to be in cwd - more than just the path somewhere
    }else{
      ## Last thing to try is to download from the internet
      getPbatdata(); ## but puts it in pbat dir first
      if( file.exists( pbatdatafile ) ) {
        file.copy( from=pbatdatafile, to=paste(getwd(),"/pbatdata.txt",sep="") );
      }
    }

    ## So make sure that it finally got copied in
    if( !file.exists( paste(getwd(),"/pbatdata.txt",sep="") ) ) {
      stop( paste("'pbatdata.txt was not found in the current",
                  " working directory '", getwd(),
                  "', or in the pbat directory '", pbatdatafile, "'",
                  ", or anywhere in your current path,",
                  " and it could not be downloaded online. ",
                  " Please see",
                  " http://www.biostat.harvard.edu/~clange/Downloading%20PBAT.htm",
                  " for more details.", sep="" ) );
    }

    ## Old coding below (changed order to look)
    
    ## Now, see if we can find it in the pbat directory location, and go from there...
    ##getPbatdata();
    ##if( file.exists( pbatdatafile ) )
    ##  file.copy( from=pbatdatafile, to=paste(getwd(),"/pbatdata.txt",sep="") );
    ##if( !file.exists( paste(getwd(),"/pbatdata.txt",sep="") ) ) {
    ##  if( pathFindFile( "pbatdata.txt" ) == "" ){
    ##    stop( paste("'pbatdata.txt was not found in the current",
    ##                " working directory '", getwd(),
    ##                "', or in the pbat directory '", pbatdatafile, "'",
    ##                ", or anywhere in your current path,",
    ##                " and it could not be downloaded online. ",
    ##                " Please see",
    ##                " http://www.biostat.harvard.edu/~clange/Downloading%20PBAT.htm",
    ##                " for more details.", sep="" ) );
    ##  }
    ##}
  }
  
  
  if( !file.exists(pedfile) )
    stop( paste("The pedigree file '",
                pedfile, "' does not exist.  Current working directory is '",
                getwd(), "'.", sep="") );
  if( !file.exists(phefile) )
    stop( paste("The phenotype file '",
                phefile, "' does not exist.  Current working directory is '",
                getwd(), "'.", sep="") );

  # other debugging
  if( phenos[1]!="" & time[1]!="" )
    stop( "Both 'phenos' and 'time' cannot have values set to them.  See the help file for more details." );

  # much more advanced debugging!

  # pedigree file information
  #ped.b <- read.badheader( pedfile );
  #posSnps <- ped.b$header[3:length(ped.b$header)];
  posSnps <- read.badheader( pedfile )$header;

  # phenotype file information
  phe <- read.phe( phefile );
  posPhenos <- names(phe);

  # check containment of various options...
  errorVecNotContained( "snps", snps, posSnps );
  
  errorVecNotContained( "phenos", phenos, posPhenos );
  errorVecNotContained( "time", time, posPhenos, AT.MOST.SINGLETON=TRUE );
  ##errorVecNotContained( "inters", inters, phenos );
  errorVecNotContained( "inters", inters, preds );  ## 01/18/2006
  errorVecNotContained( "groups", groups, posPhenos );
  
  errorIfAnyMatch( groups.var, phenos, "groups", "phenos" );
  errorIfAnyMatch( groups.var, time, "groups", "time");
  errorIfAnyMatch( groups.var, censor, "groups", "censor" );
  errorIfAnyMatch( groups.var, preds, "groups", "preds" );
  errorIfAnyMatch( preds, censor, "preds", "censor" );
  errorIfAnyMatch( preds, time, "preds", "time" );
  errorIfAnyMatch( preds, phenos, "preds", "phenos" );
  errorIfAnyMatch( censor, time, "censor", "time" );
  errorIfAnyMatch( censor, phenos, "censor", "phenos" );
  errorIfAnyMatch( time, phenos, "time", "phenos" );

  ## Enforce haplotype mode for multiprocessing
  ## 01/18/2006 rewrite - this should _always_ be done!
  ##if( pbat.getNumProcesses() > 1 && is.null(haplos) ) {
  if( is.null(haplos) ) {
    haplos <- list();
    if( is.null(snps) || snps[1]=="" ) {
      # simple hack - we need to insert the names into the haplotypes...
      # - it's really not pretty but it solves our problem so simply
      #   now that ew have the addition of multiple processing built
      #   in to pbat.
      # - on a second note, it's really the only way to be able to fix
      #   this on such a low level to guarantee that the command-line
      #   stuff will also work without a massive changes?
      junk <- read.ped( pedfile, lowercase=FALSE ); ## that lowercase...
      allSnps <- names( as.pedlist( junk ) );
      haplos[[1]] <- allSnps[7:length(allSnps)];  ## 01/18/06 fix - list
    }else{
      haplos[[1]] <- snps;
    }
    snps <- "";
    sub.haplos <- TRUE;
    length.haplos <- 1;
    adj.snps <- TRUE;
  }  

  # Verification of the haplos structure
  if( !is.null(haplos) && !is.list(haplos) )
    stop( "Haplos must be a list of string vectors (or objects that can coerced into strings." );
  if( !is.null(haplos) && is.list(haplos) ) {
    # check to make sure the snps are in, and no overlap.
    for( i in 1:length(haplos) ){
      # make sure the snps are in the list
      errorVecNotContained( "haplos dataframe", haplos[[i]], posSnps );

      # then make sure there is no overlap (can't have a snp in more than one block!)
      ##print( length(haplos) );
      if( i<length(haplos) ) {
        for( j in (i+1):length(haplos) ) {
          errorIfAnyMatch(haplos[[i]], haplos[[j]],
                          paste("Haplotype block ",i," (",names(haplos)[i],") ",sep="" ),
                          paste("Haplotype block ",j," (",names(haplos)[j],") ",sep="" ) );
        }
      }
    }
  }
      
  # Some simple range checking...
  errorRangeCheck( "max.pheno", max.pheno, min=min.pheno );
  errorRangeCheck( "min.pheno", min.pheno, max=max.pheno );
  errorRangeCheck( "alpha", alpha, min=0, max=1, IS.INTEGER=FALSE );
  errorRangeCheck( "max.gee", max.gee );
  errorRangeCheck( "max.ped", max.ped );
  errorRangeCheck( "min.info", min.info );
  errorRangeCheck( "length.haplos", length.haplos );
  errorRangeCheck( "max.mating.types", max.mating.types );
  ##warning( "Range checking for 'max.mating.types' is _NOT_ realistic!" );
  
  #-------------------------
  # now do the actual work -
  #-------------------------

  outfile <- file( commandfile, "w" );
  on.exit( close(outfile) );

  if( logfile!="" )
    writeCommand( "logfile",logfile );
  
  writeCommand( "pedfile", pedfile);     # (1)
  writeCommand( "phenofile", phefile );  # (3)

  #if( snps!="" )
  writeCommand( "snps", c(snps), end=TRUE ); # (2)
  
  writeCommand( "censor", c(censor), end=TRUE ); # (4)
  if( time=="" && fbat=="logrank" ) { # (5)
    stop( "time-to-onset variable is required for pbat-logrank." );
  }else{
    writeCommand( "phenos", c(time), end=TRUE );
  }
  writeCommand( "phenos", phenos, end=TRUE );

  if( !is.null(preds) && preds[1]!="" ) { # (6)   ## 01/27/2006
    ##if( length(preds)!=length(preds.order) ) {
    ##  warning("'preds' and 'preds.order' must be of the same length. This information will be ignored.");
    ##}else{
    ##  writeCommand( "preds", pasteVector(c(preds,preds.order,"end")) );
    ##}

    ## 01/18/2006 bugfix - alteration in the pbat syntax?
    if( (length(preds)!=length(preds.order)) || preds.order[1]=="" ) {
      warning("'preds' and 'preds.order' are not of the same length; all order's will be forced to 1.'");
      preds.order = rep(1,length(preds));
    }
    writeCommand( "preds", pasteVector( c( preds, "end", preds.order, "end" ) ) );
  }

  ## 01/18/2006 bugfix - didn't have end
  writeCommand( "inters", inters, end=TRUE );      # (7)
  
  if( groups.var!="" ){                    # (8)
    if( is.null(groups) || groups[1]=="" ) {
      ## Then we have to extract the groups values from the file!
      junk.phe <- read.phe( phefile );
      groups <- unique( junk.phe[[groups.var]] );
    }
    
    writeCommand( "groups", c(groups.var, "end", groups, "end") );
  }

  writeCommandStrMatch1( "fbat", fbat, c("gee","pc","logrank") );

  if( censor=="" && fbat=="logrank" ) stop( "Need a censoring variable." );
  
  # (10-11)
  if( fbat!="logrank" ) {
    ## 01/25/2005 - amazing - this bug only shows up in multiprocessing mode
    writeCommand( "max", max.pheno );
    writeCommand( "min", min.pheno );
  }
  
  writeCommandStrMatch( "null", null, c("no linkage, no association", "linkage, no association"),
                        vals=c(1,2) );  ## 01/25/2006
  writeCommand( "alpha", alpha );        # (13)

  writeCommandStrMatch( "transpheno", trans.pheno, c("none","ranks","normal score") );
  writeCommandStrMatch( "transpred",  trans.pred , c("none","ranks","normal score") );
  writeCommandStrMatch( "transinter", trans.inter, c("none","ranks","normal score") );

  if( fbat!="logrank" ) {
    writeCommandStrMatch( "scanpred", scan.pred, c("all","subsets") );
    writeCommandStrMatch( "scaninter", scan.inter, c("all","subsets") );
  }
  
  writeCommandStrMatch( "scangenetic", scan.genetic,
                        c("additive","dominant","recessive",
                          "heterozygous advantage","all") );

  if( offset!="default" & offset!="" )
    writeCommandStrMatch("offset", offset, c("none","max power","gee + marker score","gee") );

  writeCommandStrMatch1( "screening", screening, c("conditional power","wald") );
  #warning( "replace with is.factor??" );
  writeCommandStrMatch( "distribution", distribution, c("continuous","categorical") );

  #warning( "I commented out logfile." );
  #writeCommand( "logfile", logfile );  # (23)
  
  # (24) NA
  if( fbat=="gee" )
    writeCommand( "maxgee", max.gee );
  
  writeCommand( "maxped", max.ped );  # (25)
  writeCommand( "mininfo", min.info );  # (26)

  if( is.list(haplos) ) {
    vec <- c(  as.character(length(haplos))  );
    for( colnum in 1:length(haplos) ) {
      for( rownum in 1:length(haplos[[colnum]]) ) {
        vec <- c(vec, as.character(haplos[[colnum]][rownum]));
      }
      vec <- c(vec,"end");
    }
    writeCommand( "haplos", vec );
  }

  writeCommandStrMatch( "ambhaplos", incl.ambhaplos, c(TRUE,FALSE) ); #backwards
  writeCommandStrMatch( "infermissnp", infer.mis.snp, c(FALSE,TRUE) );

  if( sub.haplos==TRUE ) {
    writeCommandStrMatch( "subhaplos", sub.haplos, c(FALSE,TRUE) );
    writeCommand( "lengthhaplos", length.haplos );  # (31)
    writeCommandStrMatch( "adjsnps", adj.snps, c(FALSE,TRUE) );
  }
  
  writeCommandStrMatch( "overallhaplo", overall.haplo, c(FALSE,TRUE) );
  writeCommandStrMatch( "cutoffhaplo", cutoff.haplo, c(FALSE,TRUE) );
  
  # (35-36)
  if( output=="short" )
    writeCommand( "shortoutput", "1" );
  if( output=="detailed" )
    writeCommand( "detailedoutput", "1" );
  
  writeCommand( "maxmatingtypes", max.mating.types );  # (37)

  if( fbat=="logrank" )
    writeCommand( "splus", "1" ); # (38)

  if( !is.null(future.expansion) ) {
    for( i in 1:length(future.expansion) )
      writeLines( future.expansion[i], con=outfile );
  }
  
  return( logfile );  # for future processing!
}
