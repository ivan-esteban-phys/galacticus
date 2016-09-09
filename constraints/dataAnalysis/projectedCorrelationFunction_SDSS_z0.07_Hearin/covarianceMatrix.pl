#!/usr/bin/env perl
use strict;
use warnings;
use Cwd;
use lib exists($ENV{'GALACTICUS_ROOT_V094'}) ? $ENV{'GALACTICUS_ROOT_V094'}.'/perl' : cwd().'/perl';
use Galacticus::Path;

# Find the maximum likelihood estimate of the covariance matrix for the Hearin et al. (2014)
# SDSS stellar mass selected projected correlation functions.
# Andrew Benson (22-July-2014)

# Simply run the generic script with our config file as argument.
system(&galacticusPath()."constraints/dataAnalysis/scripts/covarianceMatrixProjectedCorrelation.pl ".&galacticusPath()."constraints/dataAnalysis/projectedCorrelationFunction_SDSS_z0.07_Hearin/covarianceMatrixControl.xml");

exit;
