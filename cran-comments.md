## submission 1/4/2024

I've included `#define R_NO_REMAP` and manually remapped `methodname` to `Rf_methodname`, and included R API headers after system headers.

Many thanks to Uwe who has been patient and helpful, hopefully this finally works.

## submission 29/03/2024

Address helpful comments by Uwe Ligges, regarding the use of R_NO_REMAP.

I simply remove the usage of error() from R_ext/Error.h which is not a necessity at the moment for the package.

## submission 28/03/2024

Addressed helpful comments raised by Uwe Ligges.

* removed superfluous \*.o object files
* removed superfluous \*.log file
* removed VignetteBuilder field from DESCRIPTION

## submission 27/03/2024

This package was archived due to me being slow to address a comment by Brian Ripley, my apologies.

Minor patch to address a R_NO_REMAP issue Brian Raised.

There are two NOTEs, one saying the package was archived, the other minor html related documentation warnings, which according to CRAN mailing list discussions can be ignored.

### submission 9/03/2024

This is a resubmission, I have responded to Benjamin Altmann's helpful comments in the following ways.

* I favour message() over print() when outputting to the console
* I have removed problematic scripts in inst/scripts that were actually not meant to be included
* I can confirm that at present, we do not have a reference for this work, I will update DESCRIPTION with a patch when a manuscript is ready

### submission 7/03/2024

Running R CMD check --as-cran produces the following note, which I believe is routine and can be disregarded.

\* checking CRAN incoming feasibility ... NOTE
Maintainer: ‘Samuel Unicomb <samuelunicomb@gmail.com>’ 

New submission
