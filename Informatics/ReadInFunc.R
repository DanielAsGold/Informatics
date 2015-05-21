#Names the Function which will read my chosen file and upload it to the R console
#Tells my function what to do, which is to read/upload the table that I have specified as my directory. I also indicate that this table has headers
#Tells the function where to place the table that it has called up, put it in the global environment. 

FindData= function(directory="/Users/danielgold/Documents/Bren/Spring 2015/Informatics/clim.txt")
{Test=read.table(directory, header=TRUE)
assign("clim", Test, .GlobalEnv)}
