#!/bin/csh

# Build the Galacticus documentation.
# Andrew Benson (20-February-2011)

# Extract source code data.
scripts/doc/Extract_Data.pl source doc/data
if ( $? != 0 ) then
 echo Failed to extract source code data
 exit
endif

# Analyze source code.
scripts/doc/Code_Analyzer.pl source doc/source_documentation.tex
if ( $? != 0 ) then
 echo Failed to analyze source code
 exit
endif

# Compile the manual.
cd doc
@ iPass = 1
while( $iPass <= 3 )
 # Run pdflatex.
 pdflatex Galacticus
 if ( $? != 0 ) then
  echo pdflatex failed
  exit
 endif

 # Run bibtex.
 bibtex Galacticus
 if ( $? != 0 ) then
  echo bibtex failed
  exit
 endif

 # Run makeindex.
 makeindex -s Galacticus.ist Galacticus
 if ( $? != 0 ) then
  echo makeindex failed
  exit
 endif

 @ iPass++
end

exit
