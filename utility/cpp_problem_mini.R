library(inline)
library(Rcpp)
src <- '
 std::vector<std::string> s; 
 s.push_back("hello");
 s.push_back("world");
 return Rcpp::wrap(s);
 '
hellofun <- cxxfunction(body = src, includes = '', plugin = 'Rcpp', verbose = FALSE)
cat(hellofun(), '\n') 

# Should get this:

# hello world

# But instead get this:

# Error in compileCode(f, code, language = language, verbose = verbose) : 
#   Compilation ERROR, function(s)/method(s) not created! sh: line 6: C:/RBUILD~1/3.5/mingw_64/bin/nm: Permission denied
# sh: line 6: /cygdrive/c/RBuildTools/3.5/bin/sed: Permission denied
# sh: line 6: /cygdrive/c/RBuildTools/3.5/bin/sed: Permission denied
# sh: line 6: /cygdrive/c/RBuildTools/3.5/bin/sed: Permission denied
# sh: line 8: C:/RBUILD~1/3.5/mingw_64/bin/g++: Permission denied
# sh: line 9: /cygdrive/c/RBuildTools/3.5/bin/rm: Permission denied
# make: *** [C:/Users/DANIEL~1.FLY/DOCUME~1/R/R-36~1.0/share/make/winshlib.mk:13: file32c4300b4ef8.dll] Error 126
# In addition: Warning message:
#   In system(cmd, intern = !verbose) :
#   running command 'C:/Users/DANIEL~1.FLY/DOCUME~1/R/R-36~1.0/bin/x64/R CMD SHLIB file32c4300b4ef8.cpp 2> file32c4300b4ef8.cpp.err.txt' had status 1