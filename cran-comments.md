This is a resubmission that addresses warnings due to using sprintf in compiled code. All instances of sprintf were replaced with snprintf and the result checked using R hub on R-dev debian+fedora (clang|gcc) and 

I also removed the vignette describing the methods, instead linking to the published manuscript. 
