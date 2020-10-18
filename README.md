# GRIP - Analysis of Motion Capture Data from Co-Speech Gesture and Sign

This project is supported by the [Center for Gesture Sign and Language at the University of Chicago](https://voices.uchicago.edu/cgsl/). This github repository details the analyses relevant to the recent publication:

Brown, A., Pouw, W., Brentari, D., Goldin-Meadow, S., (under review). When hands are used to communicate they are less susceptible to illusion than when they are used to estimate.

Brief description: Thirty-two English-speakers and 13 ASL-signers used their hands to act on, estimate, and describe sticks eliciting the Müller-Lyer illusion. Their hand movements were recorded using motion capture technology. Here we ask whether the hands are susceptible to illusion when used, not to act on objects, but to describe them in spontaneous co-speech gestures or in conventional sign languages.

More information related to this project can be found on [our page](https://osf.io/3rb6u/) on the Open Science Framework. 

On this site you will find:
*[project data for grasping apertures](/data)
*[Markdown file for current analysis](/R code/Grip_Github.rmd)

The grasping data was extracted from motion capture time series data using the package [mocapGrip](https://rdrr.io/github/jonkeane/mocapGrip/). More about the data processing pipeline can be found on [the package github site](https://github.com/jonkeane/mocapGrip/blob/master/README.md). Although the mocapGrip package contains built-in analytic functions, the data for this paper were analyzed separately using [lme4](https://rdrr.io/cran/lme4/).