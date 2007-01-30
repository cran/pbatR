\name{pbat}
\alias{pbat}
\alias{pbat.last}
\alias{pbat.last.rawResults}
\alias{pbat.m}
\alias{pbat.obj}
\alias{pbat.files}
\alias{pbat.create.commandfile}
\title{PBAT Graphical and Command Line Interface}
\description{
  The following routines are for the graphical and command line pbat
  interface.  The command line interfaces are listed in an order of
  suggested usage.  Most users of the command line will only want to use
  \code{pbat.m}.

  
  \code{pbat} runs a GUI (Graphical User Interface) for pbat.

  \code{pbat.last} returns an object of class \code{pbat} of the last
  command file run from running \code{pbat()}. Note this is also
  returned from \code{pbat}.
  However, this command is provided because rerunning a command in
  pbat can be a very time-consuming process).

  \code{pbat.last.rawResults} prints out the raw text file of the
  output (particularly useful if the output of pbat cannot be parsed
  properly, in the unexpected event the output could not be parsed
  correctly). This should work even with the new option of not loading
  the output in.

  
  \code{pbat.m} runs pbat according to an expression, from \code{phe} class
  (phenotype information), \code{ped} class (pedigree information), and
  various options.
  
  \code{pbat.obj} runs pbat with a \code{ped} class object (pedigree
  information), a `phe' class object (phenotype information), and
  various other options.
  
  \code{pbat.files} runs pbat according to a set of filenames and
  commands.

  \code{pbat.create.commandfile} creates a command file for Christoph
  Lange's pbat software with respect to two files on disk (.phe, .ped).

  
  Some options are only available for the respective pbat-gee (G),
  pbat-pc (P), pbat-logrank (L).
  If a parameter is `R'equired for a specific version, it will be denoted,
  for example, by (G-R).
}
\usage{
pbat()

pbat.last()

pbat.last.rawResults()

pbat.m( formula, phe, ped, fbat="",
        max.pheno=1, min.pheno=1,
        null="no linkage, no association", alpha=0.05,
        trans.pheno="none", trans.pred="none", trans.inter="none",
        scan.pred="all", scan.inter="all",
        scan.genetic="additive",
        offset="default",
        screening="conditional power", distribution="continuous",
        logfile="",
        max.gee=1,
        max.ped=14, min.info=20,
        incl.ambhaplos=TRUE, infer.mis.snp=FALSE,
        sub.haplos=FALSE, length.haplos=2, adj.snps=TRUE,
        overall.haplo=FALSE, cutoff.haplo=FALSE,
        output="normal",
        max.mating.types=10000,
        commandfile="",
        future.expansion=NULL,
        LOAD.OUTPUT=TRUE,
        monte=0,
        mminsnps=NULL, mmaxsnps=NULL,
        mminphenos=NULL, mmaxphenos=NULL,
        env.cor.adjust=FALSE,
        gwa=FALSE,
        snppedfile=FALSE,
        extended.pedigree.snp.fix=FALSE )

pbat.obj( phe, ped, file.prefix, LOAD.OUTPUT=TRUE, ... )

pbat.files( pedfile, phefile, fbat="gee",
            commandfile="",
            logrank.outfile="",
            preds="", preds.order="",
            LOAD.OUTPUT=TRUE,
            ... )

pbat.create.commandfile( pedfile, phefile="",
       snps="",
       phenos="", time="", # (only one of 'phenos' and 'time' can be set)
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
       haplos=NULL, incl.ambhaplos=TRUE, infer.mis.snp=FALSE,
       sub.haplos=FALSE, length.haplos=2, adj.snps=TRUE,
       overall.haplo=FALSE, cutoff.haplo=FALSE,
       output="normal",
       max.mating.types=10000,
       commandfile="",
       future.expansion=NULL,
       LOGFILE.OVERRIDE=TRUE,
       monte=0,
       mminsnps=NULL, mmaxsnps=NULL,
       mminphenos=NULL, mmaxphenos=NULL,
       env.cor.adjust=FALSE,
       gwa=FALSE,
       snppedfile=FALSE,
       extended.pedigree.snp.fix=FALSE )
}
\arguments{
  \item{formula}{Symbolic expression describing what should be
    processed.  See `examples' for more information.}
  \item{phe}{`phe' object as described in \code{\link{write.phe}}. If
    you do not have a phe file set this to NULL (i.e when you are only
    using AffectionStatus from the pedigree).}
  \item{ped}{`ped' object as described in \code{\link{write.ped}}.}

  \item{file.prefix}{Prefix of the output datafile (phe & ped must match)}

  \item{pedfile}{Name of the pedigree file (.ped) in PBAT-format
    (extension `.ped' is optional).}
  \item{phefile}{Name of the phenotype file (.phe) in PBAT-format.
    The default assumes the same prefix as that in 'pedfile'. Leave
    empty or set to the empty string "" if you do not have a phenotype
    file (i.e. you are only using AffecitonStatus).}

  \item{...}{Options in higher level functions to be passed to
    'pbat.create.commandfile'.}
  
  \item{fbat}{Selects the fbat statistic used the data analysis.
    
    \code{"gee"} = The FBAT-GEE statistic simplifies to the standard
    univariate FBAT-statistic. If several phenotypes are selected, all
    phenotypes are tested simultaneously, using FBAT-GEE. The FBAT-GEE
    statistic can handle any type of multivariate data.
    
    \code{"pc"} = FBAT extension for longitudinal phenotypes and repeated
    measurements.
    
    \code{"logrank"} = FBAT-extensions of the classical LOGRANK and WILCOXON
    tests for time-on-onset data. Kaplan-Meier plots for the
    analyzed data set will be generated and plotted.
  }

  \item{max.pheno}{(G,P) The maximum number of phenotypes that will be
    analyzed in the FBAT-statistic.}
  \item{min.pheno}{(G,P) The minimum number of phenotypes that will be
    analyzed in the FBAT-statistic.}
  
  \item{null}{Specification of the null-hypothesis.
    
    \code{"no linkage, no association"} = Null-hypothesis of no linkage and no
    association.
    
    \code{"linkage, no association"} = Null-hypothesis of linkage, but no
    association.
  }
  \item{alpha}{Specification of the significance level.}

  \item{trans.pheno}{Transformation of the selected phenotypes.
    
    \code{"none"} = no transformation
    
    \code{"ranks"} = transformation to ranks
    
    \code{"normal score"} = transformation to normal score
    
    The default choice is \code{"none"}, although it recommended to use
    transformation to normal scores for quantitative phenotypes.
  }
  \item{trans.pred}{Transformation of the selected predictor
    variables/covariates:
    
    \code{"none"} = no transformation
    
    \code{"ranks"} = transformation to ranks
    
    \code{"normal score"} = transformation to normal score
    
    The default choice is \code{"none"}, although it recommended to use
    transformation to normal scores for quantitative covariates.
  }
  \item{trans.inter}{Transformation of the selected interaction variables
    
    \code{"none"} = no transformation
    
    \code{"ranks"} = transformation to ranks
    
    \code{"normal score"} = transformation to normal score
    
    The default choice is \code{"none"}, although it recommended to use
    transformation to normal scores for quantitative interaction
    variables.
  }

  \item{scan.pred}{(G,P) Computation of all covariate sub-models:

    \code{"all"} = The selected FBAT statistic is computed with
    adjustment for all selected covariates/predictors.

    \code{"subsets"} = The selected FBAT statistic is computed for all
    possible subsets of the selected covariates/predictor variables.  The
    command is particularly useful to examine the dependence of
    significant results on the selection of a covariate model.
  }
  \item{scan.inter}{(G,P) Computation of all interaction sub-models:

    \code{"all"} = The selected FBAT statistic is computed including all
    selected interaction variables.

    \code{"subsets"} = The selected FBAT statistic is computed for all
    posible subsets of the interaction variables.
  }

  \item{scan.genetic}{Specification of the mode of inheritance:
    
    \code{"additive"} = Additive model
    
    \code{"dominant"} = Dominant model
    
    \code{"recessive"} = Recessive model
    
    \code{"heterozygous advantage"} = Heterozygous advantage model
    
    \code{"all"} = The FBAT-statistics are computed for all 4 genetic models
  }
  
  \item{offset}{Specification of the covariate/predictor variables
    adjustment:
    
    \code{"none"} = No adjustments for covariates/predictor variables
    
    \code{"max power"} = Offset (=FBAT adjustment for covariates and interaction
    variables) that maximizes the power of the FBAT-statistic
    (computationally slow, efficiency dependent on the correct choice of
    the mode of inheritance)
    
    \code{"gee + marker score"} = Offset (=FBAT adjustment for covariates and interaction
    variables) based on standard phenotypic residuals obtained by
    GEE-estimation including the expected marker score (E(X|H0)), all
    covariates and interaction variables.
    
    \code{"gee"} = Offset (=FBAT adjustment for covariates and interaction
    variables) based on standard phenotypic residuals obtained by
    GEE-estimation including all covariates and interaction variables.
    
    \code{"default"} = The default choice is \code{"gee"} (\code{"none"}
    for dichotomous traits).
  }
  
  \item{screening}{
    Specification of the screening methods to handle the multiple
    comparison problem for multiple SNPs/haplotypes and a set of
    phenotypes.
    
    \code{"conditional power"} = Screening based on conditional power
    (parametric approach)
    
    \code{"wald"} = Screening based on Wald-tests (non-parametric approach)
  }
  \item{distribution}{
    Specification of the phenotypic distribution
    
    \code{"continuous"} = Phenotypes are treated as continuous phenotypes in the power
    calculation
    
    \code{"categorical"} = Phenotypes are treated as categorical/integer variables 
    This option is especially recommended for analysis of time-to-onset
    data and affection status.
  }
  
  \item{logfile}{Specification of the log-file. By default, PBAT
    selects an unique file-name for the log-file, i.e. "pbatlog...".}

  \item{max.gee}{(G) Specification of the maximal number of iterations
    in the GEE-estimation procedure.}
  
  \item{max.ped}{Specification of the maximal number of proband in one
    extended pedigrees.
  }
  \item{min.info}{Specification of the minimum number of informative
    families required for the computation of the FBAT-statistics.}
  
  \item{incl.ambhaplos}{This command defines the handling of ambiguous
    haplotypes in the haplotypes analysis. Choices: 

    \code{TRUE} = Ambiguous haplotypes (phase can not be inferred) are included
    in the analysis and are weighted according to their estimated
    frequencies in the probands.
    
    \code{FALSE} = Ambiguous haplotypes are excluded from the analysis.
  }
  \item{infer.mis.snp}{Handling of missing genotype information in the
    haplotypes analysis.

    \code{FALSE} = Individuals with missing genotype information are
    excluded from the analysis. This is the analysis also implemented in
    the HBAT option of the FBAT-program.

    \code{TRUE} = Individuals with missing genotype information are
    included in the analysis. The algorithm of Horvath et al (2004) is
    applied to all individuals, even if they have missing genotype
    information. This results in more ambiguous haplotypes.
  }
  
  \item{sub.haplos}{
    \code{FALSE} = The haplotypes defined by the all SNPs given in the
    haplotype-block definition are analyzed.

    \code{TRUE} = All haplotypes are analyzed that are defined by any subset of
    SNPs in the haplotypes block definition.
  }
  \item{length.haplos}{Defines the haplotype length when subhaplos=\code{TRUE}.}
  \item{adj.snps}{Takes effect when subhaplos=\code{TRUE}.

    \code{FALSE} = All sub-haplotypes are analyzed

    \code{TRUE} = Only the sub-haplotypes are analyzed for which the first
    constituting SNPs are adjacent.
  }
  
  \item{overall.haplo}{Specification of an overall haplotypes test. When
    this command is included in the batch-file, only one level of the
    \code{"groups"} variable can be specified.

    \code{FALSE} = no overall test

    \code{TRUE} = an overall test is computed testing all haplotypes defined by
    the same set of SNPs simultaneously. This option can not be applied
    when sub.haplos=\code{TRUE}.
  }
  \item{cutoff.haplo}{The minimum haplotypes frequency so that a
    haplotypes is included in the overall test.}

  \item{output}{
    \code{"normal"} = Normal PBAT output.
    
    \code{"short"} = Shorter output.  This is mostly for use in
    conjunction with 'gwa', where there is a lot of output.
    
    \code{"detailed"} = Detailed output for each family is created.
  }
  
  \item{max.mating.types}{Maximal number of mating types in the haplotype
    analysis.}
  
  \item{commandfile}{Name of the temporary command file that will
    be created to send to the pbat.  It is suggested to leave
    this blank, and an appropriate name will be chosen with a time
    stamp.}

  \item{future.expansion}{(Only included for future expansion of pbat.)
    A vector of strings for extra lines to write to the batchfile for
    pbat.
  }


  \item{logrank.outfile}{(L) Name of the file to store the R source code to
    generate the plots for logrank analysis.}


  \item{snps}{Vector of strings for the SNPs to process. Default
    processes all of the SNPs.}
  
  \item{phenos}{(G,P) Vector of strings for the phenotypes/traits for the
    analysis.  If none are specified, then all are analyzed.  (Note:
    this \emph{must be left empty for logrank analysis}, instead specify
    the time to onset with the time variable. }
  \item{time}{(L-R) Time to onset variable.  `phenos' cannot be specified
    when this is used, but it \emph{must be set for logrank}.}
  
  \item{preds}{Vector of strings for the covariates for the test
    statistic.}
  \item{preds.order}{Vector of integers indicating the order of 'preds'
    - the order for the vector of covariates for the test
    statistic.}
  
  \item{inters}{Vector of strings for the interaction variables.}

  \item{groups.var}{String for the grouping variable.}
  \item{groups}{Vector of strings corresponding to the groups of the
    grouping variable (groupsVar).}

  \item{censor}{(L-R) String of the censoring variables. In the
    corresponding data, this variable has to be binary.}

  \item{haplos}{List of string vectors representing the haplotype
    blocks for the haplotype analysis.
    For example, \code{list( block1=c("m1","m2"), block2=c("m3","m4") )}
    defines 2 
    haplotype-blocks where the first block is defined by SNPs m1 and m2,
    and the second by SNPs m3 and m4.
  }

  \item{LOGFILE.OVERRIDE}{When using the 'sym' option in read.ped and
    read.phe, when this is set to TRUE (default), the PBAT logfile is
    put in the current working directory; if FALSE, then it is put in
    the same directory as the datafile.}
  \item{LOAD.OUTPUT}{When TRUE, loads the output into R (generally
    recommended). When FALSE, it leaves it in the output left from PBAT
    (in case output is too large to load into memory).}

  \item{monte}{When this is nonzero, monte-carlo based methods are used
    to compute the p-values instead, according to the number of
    iterations supplied. 1000 iterations is suggested.}

  \item{mminsnps}{Multi-marker multi-phenotype tests: the minimum number
    of snps to be tested.}
  \item{mmaxsnps}{Multi-marker multi-phenotype tests: the maximum number
    of snps to be tested.}
  \item{mminphenos}{Multi-marker multi-phenotype tests: the minimum number
    of phenotypes to be tested.}
  \item{mmaxphenos}{Multi-marker multi-phenotype tests: the maximum number
    of phenotypes to be tested.}

  \item{env.cor.adjust}{Whether to adjust for environmental
    correlation.}

  \item{gwa}{Whether to use (g)enome (w)ide (a)cceleration mode.  This
    is faster for genome-wide association tests, and has slightly less
    output.}

  \item{snppedfile}{Whether the pedigree file contains just snps. When
    this is true, it employs a more optimal storage technique and uses
    much less memory. It is especially advantageous for genome-wide
    studies.}

  \item{extended.pedigree.snp.fix}{Set to TRUE when you are using a
    dataset with large extended pedigrees. This will not work with any
    mode but `single' mode currently [see pbat.set(...)].
  }
}
\value{
  `pbat', `pbat.last', `pbat.m', `pbat.obj', and `pbat.files'
  return an object of class \code{pbat}.  Methods supported by  this include
  \code{plot(...)}, \code{summary(...)}, and \code{print(...)}.  Follow
  the first three links in the 'see also' section of this file for more
  details.
}
\details{
  These commands require `pbatdata.txt' to be in the working directory;
  if not found, the program will attempt to (1) copy the file from the
  directory where pbat is, (2) copy it from anywhere in the path, or (3)
  download it from the internet.

  It is recommended to set 'LOAD.OUTPUT' to 'FALSE' when dealing with
  large numbers of SNPs.

  These commands will also generate a lot of output files in the current
  working directory when interfacing with pbat.  These files will be
  time-stamped so concurrent analysis in the same directory can be run.
  \emph{Race condition}: if two logrank analysis finish at \emph{exactly the
    same time}, then the plots for one might be lost and/or get linked
  to the wrong analysis.  This should be a rather rare occurence, and is
  an unpreventable result of pbat always sending this output to only one
  filename.  Workaround to race condition: create another directory and
  use that as your current working directory instead.

  Note that multi-marker / multi-phenotype mode is not supported in
  parallel at this time, so if you are having problems try running the
  command \code{pbat.setmode("single")}, or setting it to single from
  the graphical interface before running these tests.

  WARNING: Note the 'extended.pedigree.snp.fix' option, which is
  important for getting more accurate results in very extended
  pedigrees.  It uses a slower but more accurate pedigree reconstruction
  method.
}
\examples{

##########################
## pbat.m(...) examples ##
##########################

\dontrun{
# Note that none of these can be run as they are verbatim.
# They are just meant to be examples.

# load in the data
# Here we assume that:
#  data.phe contains 'preds1', 'preds2', 'preds3', 'time',
#                     'censor', 'phenos1', ... 'phenos4'
#  data.ped contains 'snp1', 'snp2', 'snp3',
#                     'block1snp1','block1snp2',
#                     'block2snp1','block2snp2'
data.phe <- read.phe( "data" )
data.ped <- read.ped( "data" )

# This model does just the affection status (always given as
#  AffectionStatus) as the phenotype, no predictor covariates, and all
#  the snps for a snps analysis.
# Since affection status is dichotomous, we additionally set
#  distribution='categorical'
#  offset='none'
# NONE is a special keyword to indicate none, and can be only used in
#  this case (note that it is _case_ _sensative_);
#  otherwise one specifies values from the phenotype object, after and
#  including AffectionStatus.
res <- pbat.m( AffectionStatus ~ NONE, phe, ped, fbat="gee",
               distribution='categorical', offset='none', ... )
summary( res )
res  # equivalent to print(res)

# basic model with one phenotype, does all snps (if none specified)
pbat.m( phenos1 ~ preds1, phe, ped, fbat="gee" )

# same model, but with more phenotypes; here we test them all at once
pbat.m( phenos1 + phenos2 + phenos3 ~ preds1, phe, ped, fbat="gee" )

# same model as just before, but now supposing that these phenotypes are
#  instead from a longitudinal study
pbat.m( phenos1 + phenos2 + phenos3 ~ preds1, phe, ped, fbat="pc" )

# like our second model, but the mi() tells it should be a marker
#  interaction
pbat.m( phenos1 ~ mi(preds1), phe, ped, fbat="gee" )

# logrank analysis - fbat need not be set
# uses more than one predictor variable
res <- pbat.m( time & censor ~ preds1 + preds2 + preds3, phe, ped )
plot( res )

# single snp analysis (because each snp is seperated by a vertical bar
#  '|'), and stratified by group (presence of censor auto-indicates
#  log-rank analysis).  Note that the group is at the end of the
#  expression, and _must_ be at the end of the expression
res <- pbat.m( time & censor ~ preds1^3 + preds2 | snp1 | snp2 |
         snp3 / group, temp )
plot( res )

# haplotype analysis, stratified by group
pbat.m( time & censor ~ preds1^2 + preds2^3 | block1snp1
        + block1snp2 | block2snp1 + block2snp2 / group, temp )

# set any of the various options
pbat.m( phenos ~ preds, phe, ped, fbat="pc",
        null="linkage, no association", alpha=0.1 )

## New multimarker test (as described above)
# mmaxphenos and mmaxsnps are set to the minimum if not specified
pbat.m( phenos1 + phenos2 + phenos3 ~ preds | m1 | m2 | m3 | m4, phe,
        ped, fbat="pc", mminphenos=2, mminsnps=2 )
        
}


############################
## pbat.obj(...) examples ##
############################

\dontrun{
# These will not function; they only serve as examples.

# ... just indicates there are various options to be put here!
res <- pbat.obj("pedfile", snps=c("snp1,snp2"), preds="pred1", ... ) 
summary(res)
res

# plot is only available for "logrank"
res <- pbat.obj(..., fbat="logrank")
plot( res )
}


##############################
## pbat.files(...) examples ##
##############################

\dontrun{
# These will not function, but only serve as examples.

# Note in the following example, both "pedfile.ped" and "pedfile.phe"
#  must exist.  If the names differed, then you must specify the
#  option 'phe="phefile.phe"', for example.
res <- pbat.files( "pedfile", phenos=c("phenos1","phenos2"),
                   screening="conditional power" )
summary(res)
res
}

}
\references{
  This was taken with only slight modification to accomodate the
  interface from Christoph Lange's description of the commands for the
  pbat program, (which was available with the software at the time of
  this writing), available at the PBAT webpage:
  \url{http://www.biostat.harvard.edu/~clange/default.htm}

  P2BAT webpage:
  \url{http://www.people.fas.harvard.edu/~tjhoffm/pbatR.html}

  FBAT webpage (lists a lot of references in relation to both of these
  programs):
  \url{http://biosun1.harvard.edu/~fbat/fbat.htm}

  More pbat references:

  Hoffmann, T. and Lange, C. (2006) P2BAT: a massive parallel
  implementation of PBAT for genome-wide association studies in
  R. Bioinformatics. Dec 15;22(24):3103-5.

  Jiang, H., et al. (2006) Family-based association test for
  time-to-onset data with time-dependent differences between the hazard
  functions. Genet. Epidemiol, 30, 124-132.
  
  Laird, N.M. and Lange, C. (2006) Family-based designs in the age of
  large-scale gene-association studies. Nat. Rev. Genet, 7.
  
  Lange, C., et al. (2003) Using the noninformative families in
  family-based association tests: a powerful new testing
  strategy. Am. J. Hum. Genet, 73, 801-811.
  
  Lange, C., et al. (2004a) A family-based association test for
  repeatedly measured quantitative traits adjusting for unknown
  environmental and/or polygenic effects. Stat. Appl. Genet. Mol. Biol,
  3.
  
  Lange, C., et al. (2004b) Family-based association tests for survival
  and times-to-onset analysis. Stat. Med, 23, 179-189.
  
  Van Steen, K., et al. (2005) Genomic screening and replication using
  the same data set in family-based association testing. Nat. Genet, 37,
  683-691.

}
\seealso{
  \code{\link{summary.pbat}},
  \code{\link{plot.pbat}},
  \code{\link{print.pbat}},
  
  \code{\link{as.ped}},
  \code{\link{as.pedlist}},
  \code{\link{read.ped}}

  \code{\link{as.phe}},
  \code{\link{read.phe}}
}
\keyword{interface}
