if(!require("readxl")) install.packages("readxl")
library(readxl)
rm(list = ls(all.names = TRUE))

exceltable<-NULL
instruction<-NULL
prevalence<-NULL

readfile<- function(path){
  return (read_excel(path))
}

inc <- function(x)
{
  eval.parent(substitute(x <- x + 1))
}

get_col_index <-function(question){
  continue=TRUE
  index=0
  print(exceltable)
  while (continue){
    index=readline(question)
    index=as.integer(index)
    print(exceltable[index],n=10)
    if(readline("Reselect column? (y/n): ")=="n")
      continue=FALSE
  }
  return(exceltable[index])
}

get_firsttable<-function(){
  table=array(c(0,0,0,0,0,0,0,0,0),dim=c(3,3))
  for (i in 1:nrow(instruction)){
    x<-as.integer(instruction[i,1])
    y<-as.integer(prevalence[i,1])
    inc(table[x,y])
  }
  table[1,3]<-table[1,1]+table[1,2]
  table[2,3]<-table[2,1]+table[2,2]
  table[3,1]<-table[1,1]+table[2,1]
  table[3,2]<-table[1,2]+table[2,2]
  table[3,3]<-table[3,1]+table[3,2]
  return(table)
}

file_path=readline("Enter excel file path: ")
if (file.exists(file_path)){

exceltable<-readfile(file_path)
instruction=get_col_index("Enter instruction column number ")
prevalence=get_col_index("Enter prevalence column number ")
firsttable=get_firsttable()
df<-data.frame("GROUP"=c("Recieved Instructions","No Instructions","Total"),
               "Present"=firsttable[,1],
               "Absent"=firsttable[,2],
               "Total"=firsttable[,3])

print(df)

propwithprevalence=firsttable[3,1]/firsttable[3,3]
propwithoutprevalence=firsttable[3,2]/firsttable[3,3]

instpresent=firsttable[1,3]*propwithprevalence
instnotpresent=firsttable[1,3]*propwithoutprevalence

notinstpresent=firsttable[2,3]*propwithprevalence
notinstnotpresent=firsttable[2,3]*propwithoutprevalence


recieved_present=c(firsttable[1,1],instpresent,firsttable[1,1]-instpresent)
recieved_notpresent=c(firsttable[1,2],instnotpresent,firsttable[1,2]-instnotpresent)
notrecieved_present=c(firsttable[2,1],notinstpresent,firsttable[2,1]-notinstpresent)
notrecieved_notpresent=c(firsttable[2,2],notinstnotpresent,firsttable[2,2]-notinstnotpresent)

secondtable=array(c(recieved_present,recieved_notpresent,notrecieved_present,notrecieved_notpresent),dim=c(1,3,4))
ds<-data.frame("GROUP"=c("","With Instructions","","","Without Instructions",""),
               "Present"=c((secondtable[,,1]),secondtable[,,2])
               ,
               "Not Present"=c(secondtable[,,2],secondtable[,,3]))
print(ds)

chisquare=0
for (i in 1:4){
  chisquare=chisquare+((secondtable[1,3,i]^2)/secondtable[1,2,i])
}
print("chisquare is:")
print(chisquare)
}