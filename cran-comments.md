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
