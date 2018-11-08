# script to boxplot and t.test ispla.

# Revised 22-10-2018, Coded by: CT

#Remember to change the path of file names to your PC or MAC
# For future datasets, it is important all the varibles
# have exactly the same names across excel or txt files.

#data ispla 1
is1 <- read.table("/Users/cati7218/Dropbox/ispla1.csv", 
                  header=TRUE,
                  sep=";")
#data ispla 2
is2 <- read.table("/Users/cati7218/Dropbox/ispla2.csv", 
                  header=TRUE,
                  sep=";")
#data ispla 3
is3 <- read.table("/Users/cati7218/Dropbox/ispla3.csv", 
                  header=TRUE,
                  sep=";")
# new ispla 4

is4<-read.table("/Users/cati7218/Desktop/isplaN.csv", 
                header=TRUE,
                sep=";")
# new ispla 5

is5<-read.table("/Users/cati7218/Dropbox/ispla5.csv", 
                header=TRUE,
                sep=";")


# order factors (hours) for boxplot to plot points in chronological order.

is1$Hour<-factor(is1$Hour,unique(is1$Hour))
is2$Hour<-factor(is2$Hour,unique(is2$Hour))
is3$Hour<-factor(is3$Hour,unique(is3$Hour))
is4$Hour<-factor(is4$Hour,unique(is4$Hour))
# different names so it had to be done away from the rest.
is5$Time.point<-factor(is5$Time.point,unique(is5$Time.point))

# Ispla function: it ony needs the time points, hours, and plot title to run.
ispla<- function (points, hours,title) {
            plot(points~hours, 
            col ="grey", xlab="Hours after serum shock",
            ylab="FU",las=1,main=title)
  
}

# plot each ispla separately
ispla(is1$Points, is1$Hour,"name") # ispla 1

ispla(is2$Points, is2$Hour,"name") # ispla 2

ispla(is3$Points, is3$Hour,"name") # ispla 3

ispla(is4$Points, is4$Hour,"name") # ispla 4

# variables names are different because the names are different in csv file
ispla(is5$Mean, is5$Time.point,"name") # ispla 5


# function to calculate t.test. Where x= dataset, y=first time point, and
# z=the second time point.
UT<- function(x,y,z) {
          a<-t.test(x$Points[x$Hours==y],
          x$Points[x$Hours==z],
          paired = FALSE) # the different time points

           print (a) # print t.test

}

# ispla 1
UT(is1,"8h","16h")
UT(is1,"16h","32h")

# ispla 2
UT(is2,"8h","16h")
UT(is2,"16h","32h")

# ispla 3
UT(is3,"8h","16h")
UT(is3,"16h","32h")

# ispla 4, the variable names changed slightly (important to standarized names!)
UT(is4,"8 hrs","16 hrs")
UT(is4,"16 hrs","32 hrs")
UT(is4,"32 hrs","40 hrs")


#ispla 5 also has totally different variable names.It didn't seem woth it 
#to modify the previous function for 1 t.test

t.test(is5$Mean[is5$Time.point=="16h"],is5$Mean[is5$Time.point=="32h"]
       ,paired = FALSE) #16 and 32
