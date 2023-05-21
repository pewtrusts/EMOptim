R --vanilla < run-roxygen.R
R CMD build --force EMoptim
R CMD INSTALL EMoptim_0.1.tar.gz
