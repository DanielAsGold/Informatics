FindData= function(directory="/Users/danielgold/Documents/Bren/Spring 2015/Informatics/clim.txt")
{
Test=read.table(directory, header=TRUE)

assign("clim", Test, .GlobalEnv)}
