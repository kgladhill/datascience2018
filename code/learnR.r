#CECS Path to RGui -->P:\Programs\R\R-26~1.2\bin\Rgui.exe
##########################################################
# K. Gladhill
#--------------------------------------------------------
# Basics
#--------------------------------------------------------
# pound sign makes all that follows on that line a comment
# R is case sensitive but ignores line breaks and spaces.
# You can wrap code around lines. Line continuation:  use + at start of second line; sometimes this is optional
# R has built in help functions. At the R console type
  ? plot
# You can also see examples of the function by typing
  example(plot)

rp1 <- 3 * 4^2      #assign value
rp1                 #display value
rp1 <- 3 * 
  +  4^2            #assign value
rp1                 #display value

Year <- c(1800, 1850, 1900, 1950, 2000)
Carbon <- c(8,54,534,1639,6611)
plot (Carbon ~ Year, pch = 1)   # plot carbon as a function of year, using empty black dots (pch = 1)
#pch = 15 open circle
#pch = 2  open triangle
#pch = 3  plus sign
#pch = 4  X
#pch = 5  open diamond
#pch = 15 solid black box
#pch = 16 solid black diamond
#pch = 17 solid triangle
#pch = 20 solid circle

# you can group data in a dataframe
fossilFuel <- data.frame(year=Year, carbon=Carbon)
fossilFuel                 #display
fossilFuel[3,2]            #display row 3, column 2 value
fossilFuel [3,]            #display row 3
fossilFuel$carbon[3]       #display row 3 of field "carbon"
summary(fossilFuel)        #display summary stats on each data column
range(fossilFuel$carbon)   #display min,max this column
# since fossilFuel$year = Year, you can remove redundant value
fossilFuel$year  
class(fossilFuel)          #data type = data frame in this case
class(Carbon)              #data type for Carbon
nrow(fossilFuel)
ncol(fossilFuel) 
fossilFuel.20c <- subset(fossilFuel,Year > 1899)
fossilFuel.20c

Year

ls()                       #list workspace contents
rm(Year)                   #remove Year
ls()                       #list workspace contents
getwd()                    #locate your working directory
#--------------------------------------------------------
# Packages
#--------------------------------------------------------
# In the R console, under packages select the mirror site, then select the package. 
#To load the package in your current R session the following is needed:
library(lattice)
library(RODBC)
##notes from Monsere
#First set the option CRAN to your nearest CRAN mirror using
chooseCRANmirror()
installed.packages(lib.loc = NULL, priority = NULL, noCache = FALSE, fields = NULL)

#install package and any packages it's dependent on
install.packages("rgl", dependencies = TRUE)
install.packages("rgl", lib = file.path("c:/Program Files (x86)/R/R-2.10.1/library"), dependencies = TRUE)
install.packages("rgl",  "C:/PROGRA~2/R/R-210~1.1/library", dependencies = TRUE)
install.packages("rgl", lib = file.path("c:", "Program Files (x86)","R","R-2.10.1","library",fsep = "/"), dependencies = TRUE)
?file.path
?install.packages

#to ensure that all packages are up to date
update.packages()  
remove.packages(c("pkg1", "pkg2"), lib = file.path("c:", "Program Files (x86)","R","R-2.10.1","library")
#"C:\Program Files (x86)\R\R-2.10.1\library"
#On most systems
#install.packages()? will allow packages to be selected from a list box.

#--------------------------------------------------------
# Data connections
#--------------------------------------------------------
library(RODBC)
#Read the data file 
#washington <- read.table("http://web.cecs.pdx.edu/~monserec/courses/safety/r_files/WashingtonetalData.csv", header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
#access <- odbcConnectAccess("//stash.cecs.pdx.edu/marston/Active_Projects//09-02 OTREC HSM Calib//_Data//ODOT ITIS//HSMcalib_dev00_FE.mdb") 
#access <- odbcConnectAccess("s://Active_Projects//09-02 OTREC HSM Calib//_Data//ODOT ITIS//HSMch12_calib_00_FE.mdb")
access <- odbcConnectAccess("s://Student_Research//Gladhill//SafetyUrbanForm//_4_Data//GIS//GeoDatabase//SafetyUrban Form.mdb")
##Access 2007 connection:
#access <- odbcConnectAccess2007("ODOT_Intersections_BE.accdb")  cannot link to query, must link to table
GridData <- sqlFetch(access,"tblGridData")
names(GridData)
summary (GridData)

mileposts <- c(29.1, 29.2, 26.3)
mileposts[1]
for (j in 1:5) {
  if (mileposts[j]<29) {
    print (mileposts[j])
    }
}

#write a function
trialFunction <- function(str1,str2)
{
paste(str1,str2,sep = "")
}
#call and run the function
trialFunction("test","case")