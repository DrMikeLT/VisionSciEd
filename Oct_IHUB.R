#Libraries
library(plyr)

#
#previously worked on data from lines 6 through 400ish:
setwd("~/Dropbox/000-Bill and Katie Work")
load(file="IHUB_Oct2018.rdata" )
#############
#Load raw data

setwd("~/Dropbox/000-Bill and Katie Work")
a1<-read.csv(file="testfile_oct2018.csv", header = T, stringsAsFactors = F)

#Functions

testsum<-function(dat){ #Sum the columns for a student
  ct<-0
  for(i in 1:ncol(dat)){
    ifelse(dat[,i]>0, ct<-ct+dat[,i], ct)
  }
  ct
}

isblank<-function(dat){#was the test not even administred and thus is all NAs
  b<-FALSE
  ifelse(rowSums(is.na(dat))==ncol(dat), b<-TRUE, 
         b<-FALSE)
  b
}

rejec<-function(dat){#Did the student leave 100% of the adinistered items blank
  b<-FALSE
  ifelse(rowSums(dat)==ncol(dat)*-9, b<-TRUE, 
         b<-FALSE)
  b
}

sumorreject<-function(dat, tot=10){
  ret<-0
  ifelse(isblank(dat)==T, ret<-NA,
         ifelse(rejec(dat)==T, ret<-(-0.3), 
                ret<-testsum(dat)))
  t<-tot
  #ifelse(dat[,ncol(dat)]==-99, t<-10, t<-t)
  round(ret/t,3)
}


#Cross Talk Table 
#This will use the codes in the Pre and Post Form columns to tell the code
#which test to sum for pre and post

#A1 = Species/Finches
#A2 = Swallow/Tibet
#B1 = Species/Swallows
#b2 = Finches/Tibet
#C1 = Species/Tibet
#C2 = Finches Swallows

#All tests have a max score of 10 individually

###########
croswalk<-function(dat1){
  ret<-NA
  ret<-ddply(dat1, .(ID), function(x){
    h<-NA
    ifelse(x$FORM=="A1", h<-sumorreject(dat=x[,c("SP2_PRE","SP3_PRE","SP4_PRE",
                                                  "SP5_PRE","SP6_PRE",
                                                  "F1_PRE", "F2_PRE","F3_PRE",
                                                  "F4_PRE", "F5_PRE",
                                                  "F6_PRE")], tot=20), ifelse(x$FORM=="A2",
           h<-sumorreject(dat=x[,c("SW1_PRE", "SW2a_PRE",
                                   "SW2b_PRE","SW2c_PRE",
                                   "SW3_PRE", "T1_PRE",
                                   "T2_PRE", "T3_PRE", "T4_PRE" )], tot=20),
           ifelse(x$FORM=="B1",
                  h<-sumorreject(dat=x[,c("SP2_PRE","SP3_PRE","SP4_PRE",
                                          "SP5_PRE","SP6_PRE","SW1_PRE", "SW2a_PRE",
                                          "SW2b_PRE","SW2c_PRE",
                                          "SW3_PRE")], tot=20),
                  ifelse(x$FORM=="B2",
                         h<-sumorreject(dat=x[,c("F1_PRE", "F2_PRE","F3_PRE",
                                                 "F4_PRE", "F5_PRE",
                                                 "F6_PRE",
                                                 "T1_PRE",
                                                 "T2_PRE", "T3_PRE", "T4_PRE")], tot=20), 
                         ifelse(x$FORM=="C1",
                                h<-sumorreject(dat=x[,c("SP2_PRE","SP3_PRE","SP4_PRE",
                                                        "SP5_PRE","SP6_PRE",
                                                        "T1_PRE",
                                                        "T2_PRE", "T3_PRE", "T4_PRE")], tot=20),
                                h<-sumorreject(dat=x[,c("F1_PRE", "F2_PRE","F3_PRE",
                                                        "F4_PRE", "F5_PRE",
                                                        "F6_PRE",
                                                        "SW1_PRE", "SW2a_PRE",
                                                        "SW2b_PRE","SW2c_PRE",
                                                        "SW3_PRE")], tot=20))))))
    return(h)
  })
  ret1<-NA
  
  ret1<-ddply(dat1, .(ID), function(x){
    h<-NA
    ifelse(x$FORM__P=="A1", h<-sumorreject(dat=x[,c("SP2__POST","SP3__POST","SP4__POST",
                                                 "SP5__POST","SP6__POST",
                                                 "F1__POST", "F2__POST","F3__POST",
                                                 "F4__POST", "F5__POST",
                                                 "F6__POST")], tot=20), ifelse(x$FORM=="A2",
                                                                             h<-sumorreject(dat=x[,c("SW1__POST", "SW2a__POST",
                                                                                                     "SW2b__POST","SW2c__POST",
                                                                                                     "SW3__POST", "T1__POST",
                                                                                                     "T2__POST", "T3__POST", "T4__POST" )], tot=20),
                                                                             ifelse(x$FORM=="B1",
                                                                                    h<-sumorreject(dat=x[,c("SP2__POST","SP3__POST","SP4__POST",
                                                                                                            "SP5__POST","SP6__POST","SW1__POST", "SW2a__POST",
                                                                                                            "SW2b__POST","SW2c__POST",
                                                                                                            "SW3__POST")], tot=20),
                                                                                    ifelse(x$FORM=="B2",
                                                                                           h<-sumorreject(dat=x[,c("F1__POST", "F2__POST","F3__POST",
                                                                                                                   "F4__POST", "F5__POST",
                                                                                                                   "F6__POST",
                                                                                                                   "T1__POST",
                                                                                                                   "T2__POST", "T3__POST", "T4__POST")], tot=20), 
                                                                                           ifelse(x$FORM=="C1",
                                                                                                  h<-sumorreject(dat=x[,c("SP2__POST","SP3__POST","SP4__POST",
                                                                                                                          "SP5__POST","SP6__POST",
                                                                                                                          "T1__POST",
                                                                                                                          "T2__POST", "T3__POST", "T4__POST")], tot=20),
                                                                                                  h<-sumorreject(dat=x[,c("F1__POST", "F2__POST","F3__POST",
                                                                                                                          "F4__POST", "F5__POST",
                                                                                                                          "F6__POST",
                                                                                                                          "SW1__POST", "SW2a__POST",
                                                                                                                          "SW2b__POST","SW2c__POST",
                                                                                                                          "SW3__POST")], tot=20))))))
    return(h)
  })
  
  ret1<-rename(ret1, replace=c("V1" ="posttest"))
  ret<-merge(ret, ret1, by="ID")
  ret<-rename(ret, replace = c("V1"= "pretest"))
  ret
  #data.frame(ID=ID, pretest=ret)       
}
a<-a1

test<-croswalk(dat1=a)
test
##############

a<-merge(a, test, by="ID")
save(a, file="IHUB_Oct2018.rdata")
#

a$diff<-a$posttest-a$pretest
save(a, file="IHUB_Oct2018.rdata")
a1<-a
a<-a[a$ID!=678745,]
a<-a[a$ID!=641856,]
a<-a[a$ID!=661750,]
a<-a[a$ID!=681570,]
a<-a[a$ID!=707305,]
a<-a[a$ID!=759708 ,]
a<-a[a$ID!=854958,]
save(a, file="IHUB_Oct2018.rdata")

#############
#Now Individual Tests

save(a, file="IHUB_Oct2018.rdata")

test<-ddply(a[a$FORM!="A1" & 
                a$FORM!="A2" &
                a$FORM!="B1" &
                a$FORM!="C1",][,c("ID","F1_PRE", "F2_PRE","F3_PRE",
                 "F4_PRE", "F5_PRE",
                 "F6_PRE")],.(ID),function(x){
  data.frame(FinchPreT1=sumorreject(x[,c("F1_PRE", "F2_PRE","F3_PRE",
                                         "F4_PRE", "F5_PRE",
                                         "F6_PRE")],
                                    tot=10))})
a<-merge(a, test, by="ID", all.x=T)

test<-ddply(a[a$FORM!="A2" & 
                a$FORM!="B2" &
                a$FORM!="B1" &
                a$FORM!="C1" &a$FORM!="C2",][,c("ID","F1_PRE", "F2_PRE","F3_PRE",
                                  "F4_PRE", "F5_PRE",
                                  "F6_PRE")],.(ID),function(x){
                                    data.frame(FinchPreT2=sumorreject(x[,c("F1_PRE", "F2_PRE","F3_PRE",
                                                                           "F4_PRE", "F5_PRE",
                                                                           "F6_PRE")],
                                                                      tot=10))})
a<-merge(a, test, by="ID", all.x=T)

test<-ddply(a[a$FORM__P!="A1" & 
                a$FORM__P!="A2" &
                a$FORM__P!="B1" &
                a$FORM__P!="C1",][,c("ID","F1__POST", "F2__POST","F3__POST",
                 "F4__POST", "F5__POST",
                 "F6__POST")],.(ID),function(x){
  data.frame(FinchPostT1=sumorreject(x[,c("F1__POST", "F2__POST","F3__POST",
                                          "F4__POST", "F5__POST",
                                          "F6__POST")],
                                     tot=10))})
a<-merge(a, test, by="ID", all.x=T)

test<-ddply(a[a$FORM__P!="A2" & 
                a$FORM__P!="B2" &
                a$FORM__P!="B1" &
                a$FORM__P!="C1"& a$FORM__P!="C2",][,c("ID","F1__POST", "F2__POST","F3__POST",
                                     "F4__POST", "F5__POST",
                                     "F6__POST")],.(ID),function(x){
                                       data.frame(FinchPostT2=sumorreject(x[,c("F1__POST", "F2__POST","F3__POST",
                                                                               "F4__POST", "F5__POST",
                                                                               "F6__POST")],
                                                                          tot=10))})
a<-merge(a, test, by="ID", all.x=T)


##########
#Swallows
test<-ddply(a[a$FORM!="A1" & 
                a$FORM!="B1" &
                a$FORM!="B2" &
                a$FORM!="C1" &
                a$FORM!="C2",][,c("ID","SW1_PRE", "SW2a_PRE",
                 "SW2b_PRE","SW2c_PRE",
                 "SW3_PRE")],.(ID),function(x){
                  data.frame(SwallowsPreT1=sumorreject(x[,c("SW1_PRE", "SW2a_PRE",
                                                            "SW2b_PRE","SW2c_PRE",
                                                            "SW3_PRE")],
                                                       tot=10))})
a<-merge(a, test, by="ID", all.x=T)

test<-ddply(a[a$FORM!="A1" & 
                a$FORM!="A2" &
                a$FORM!="B2" &
                a$FORM!="C1"
                ,][,c("ID","SW1_PRE", "SW2a_PRE",
                                  "SW2b_PRE","SW2c_PRE",
                                  "SW3_PRE")],.(ID),function(x){
                                    data.frame(SwallowsPreT2=sumorreject(x[,c("SW1_PRE", "SW2a_PRE",
                                                                              "SW2b_PRE","SW2c_PRE",
                                                                              "SW3_PRE")],
                                                                         tot=10))})
a<-merge(a, test, by="ID", all.x=T)

test<-ddply(a[a$FORM__P!="A1" & 
                a$FORM__P!="B1" &
                a$FORM__P!="B2" &
                a$FORM__P!="C1" &
                a$FORM__P!="C2",][,c("ID","SW1__POST", "SW2a__POST",
                                                   "SW2b__POST","SW2c__POST",
                                                   "SW3__POST")],.(ID),function(x){
                  data.frame(SwallowsPostT1=sumorreject(x[,c("SW1__POST", "SW2a__POST",
                                                             "SW2b__POST","SW2c__POST",
                                                             "SW3__POST")],
                                                        tot=10))})
a<-merge(a, test, by="ID", all.x=T)

test<-ddply(a[a$FORM__P!="A1" & 
                a$FORM__P!="A2" &
                a$FORM__P!="B2" &
                a$FORM__P!="C1",][,c("ID","SW1__POST", "SW2a__POST",
                                     "SW2b__POST","SW2c__POST",
                                     "SW3__POST")],.(ID),function(x){
                                       data.frame(SwallowsPostT2=sumorreject(x[,c("SW1__POST", "SW2a__POST",
                                                                                  "SW2b__POST","SW2c__POST",
                                                                                  "SW3__POST")],
                                                                             tot=10))})
a<-merge(a, test, by="ID", all.x=T)

############
#Species

test<-ddply(a[a$FORM!="A2" & 
                a$FORM!="B2" &
                a$FORM!="C1" &
                a$FORM!="C2",][,c("ID","SP2_PRE","SP3_PRE","SP4_PRE",
                 "SP5_PRE","SP6_PRE" )],.(ID),function(x){
  data.frame(SpeciesPreT1=sumorreject(x[,c("SP2_PRE","SP3_PRE","SP4_PRE",
                                          "SP5_PRE","SP6_PRE")],                                    
                                      tot=10))})
a<-merge(a, test, by="ID", all.x=T)

test<-ddply(a[a$FORM__P!="A2" & 
                a$FORM__P!="B2" &
                a$FORM__P!="C1" &
                a$FORM__P!="C2",][,c("ID","SP2__POST","SP3__POST","SP4__POST",
                 "SP5__POST","SP6__POST" )],.(ID),function(x){
                   data.frame(SpeciesPostT1=sumorreject(x[,c("SP2__POST","SP3__POST","SP4__POST",
                                                             "SP5__POST","SP6__POST")],                                    
                                                       tot=10))})
a<-merge(a, test, by="ID", all.x=T)

############
#Tibet

test<-ddply(a[a$FORM!="A1" & 
                a$FORM!="B1" &
                a$FORM!="C2",][,c("ID","T1_PRE",
                 "T2_PRE", "T3_PRE", "T4_PRE" )],.(ID),function(x){
                   data.frame(TibetPreT2=sumorreject(x[,c("T1_PRE",
                                                            "T2_PRE", "T3_PRE", "T4_PRE")],                                    
                                                       tot=10))})
a<-merge(a, test, by="ID", all.x=T)

test<-ddply(a[a$FORM__P!="A1" & 
                a$FORM__P!="B1" &
                a$FORM__P!="C2",][,c("ID","T1__POST",
                 "T2__POST", "T3__POST", "T4__POST" )],.(ID),function(x){
                   data.frame(TibetPostT2=sumorreject(x[,c("T1__POST",
                                                           "T2__POST", "T3__POST", "T4__POST")],                                    
                                                        tot=10))})
a<-merge(a, test, by="ID", all.x=T)

#########
#Droppping blanks
a1<-a
#No blanks for Finch Pre1
test<-a$ID[which(a$FinchPreT2==(-0.03))]
a<-a[-which(a$ID %in% test),]   #21 blanks

test<-a$ID[which(a$FinchPostT1==(-0.03))] #1 blank
a<-a[-which(a$ID %in% test),]

test<-a$ID[which(a$FinchPostT2==(-0.03))] #7 blank
a<-a[-which(a$ID %in% test),]

test<-a$ID[which(a$SwallowsPreT1==(-0.03))] #0 blank
a<-a[-which(a$ID %in% test),]

test<-a$ID[which(a$SwallowsPreT2==(-0.03))] #34 blank
a<-a[-which(a$ID %in% test),]

test<-a$ID[which(a$SwallowsPostT1==(-0.03))] #2 blank
a<-a[-which(a$ID %in% test),]

test<-a$ID[which(a$SwallowsPostT2==(-0.03))] #14 blank
a<-a[-which(a$ID %in% test),]

test<-a$ID[which(a$SpeciesPreT1==(-0.03))] #2 blank
a<-a[-which(a$ID %in% test),]

test<-a$ID[which(a$SpeciesPostT1==(-0.03))] #0 blank
a<-a[-which(a$ID %in% test),]

test<-a$ID[which(a$TibetPostT2==(-0.03))] #14 blank
a<-a[-which(a$ID %in% test),]

test<-a$ID[which(a$TibetPreT2==(-0.03))] #44 blank
a<-a[-which(a$ID %in% test),]

#############
#Testing the previous code from May
b<-a[,c(6:7,54:65)]
prenam<-names(b)[grep("PreT", names(b))]
postnam<-names(b)[grep("PostT", names(b), fixed=T)]
bsave<-b

substrRight <- function(x, n){
  sapply(x, function(xx)
    substr(xx, (nchar(xx)-n+1), nchar(xx))
  )
}
for (i in 1:length(postnam)){
  for(j in 1:length(prenam)){
    pretesnum<-substrRight(prenam[j], 1)
    #ifelse(pretesnum=="B", "1_3B", pretesnum)
    postesnum<-substrRight(postnam[i], 1)
    b[, paste(sub("\\P.*","",prenam)[j],ifelse(pretesnum=="B", "1_3B", pretesnum),
              "_",sub("\\P.*","",postnam)[i],ifelse(postesnum=="B", "1_3B", postesnum) , sep="")]<-b[postnam[i]]-b[prenam[j]]
  }}

b<-b[,-c(15:18,21:22,24:25,27,29,30,32, 34:37,40,42:43,47, 50 )]
bsave<-b

sumtable<-setNames(data.frame(matrix(ncol=length(names(b[,15:29]))+1)),
                   c("Statistic", names(b[,15:29])))

cnt<-2
for(i in 15:29){
  sumtable[1,cnt]<-round(mean(b[,i], na.rm=T),3)
  cnt<-cnt+1
}
sumtable[1,1]<-"Mean (%)"
sumtable[2,1]<-"SD (%)"
cnt<-2
for(i in 15:29){
  sumtable[2,cnt]<-round(sd(b[,i], na.rm=T),3)
  cnt<-cnt+1
}
sumtable[3,1]<-"Pre Task\nMean (%)"
preme<-c(prenam[5],prenam[6],prenam[3], prenam[6],prenam[2],
         prenam[5],prenam[1],prenam[6], prenam[1],prenam[3],
         prenam[6],prenam[1],prenam[2], prenam[4],prenam[5])
for(i in 2:16){
  
  sumtable[3,i]<-round(mean(as.numeric(b[preme[i-1]][[1]]), na.rm=T),3)
}
sumtable[4,1]<-"Post Task\nMean (%)"
posme<-c(postnam[1],postnam[1],postnam[2],postnam[2],postnam[3],
         postnam[3],postnam[4],postnam[4],postnam[5],postnam[5],
         postnam[5],postnam[6],postnam[6],postnam[6],postnam[6])
for(i in 2:16){
  
  sumtable[4,i]<-round(mean(as.numeric(b[posme[i-1]][[1]]), na.rm=T),3)
}

sumtable[5,1]<-"N"
for(i in 2:16){
  
  sumtable[5,i]<-table(is.na(b[preme[i-1]][[1]]), is.na(b[posme[i-1]][[1]]))[1,1]
}

library(gridExtra)
sumtable[1:4,2:16]<-sumtable[1:4,2:16]*100
sumtable[5,2:16]<-as.character(round(sumtable[5,2:16],0))
names(sumtable)<-gsub("_", "\n", names(sumtable))
sumtable<-sumtable[,c(1, order(as.numeric(sumtable[1,2:16]))+1)]
save(sumtable, file="sumtable_IHUB_Oct2018.rdata")

library(reshape)
b$id<-1:nrow(b)
c<-melt(b[,16:length(b)], id="id")
d<-c[is.na(c$value)==F,]
table(d$variable)#Drop levels with no obsrvations
varlav<-names(table(d$variable))
d$variable<-factor(d$variable) #levels= varlav[c(7,10,1,4,11,2,5,8,3,6,9)])
aov1<-aov(value~variable, data=d)
aov1
summary(aov1)
TukeyHSD(aov1)



#########
#Graphs
library(ggplot2)
library(gtable)



bplot_macro<-ggplot(data=a, aes(x=COMBO, y=diff))+geom_boxplot()+
theme(
  axis.text=element_text(size=12, color="black"),
  axis.text.x = element_text(angle=90),
  axis.title=element_text( size=12, color="black"),
  axis.ticks=element_line(size=2, color="black"),
  title=element_text( size=12, face="bold"),
  plot.title = element_text(hjust=0),
  axis.line=element_line(size=1, color="black"),
  legend.text =element_text(size=9),
  legend.key.size =  unit(0.25, "in"),
  plot.margin = unit(c(6,1,6,1), "pt"),
  legend.box = "horizontal", legend.position = "bottom"
)+
  scale_y_continuous(name="Percent Improvement Pre to Post Test",limits=c(-0.7,0.6),
                     breaks=c(-0.6,-0.3,0,0.3,0.6 ),
                     labels=c("-60%","-30%", "0%", "30%", "60%"))+
  scale_x_discrete(name="")

ggsave(bplot_macro,file="Boxplot_FullTest_Oct2018.pdf", width=12, height=11)


d$variable<-reorder(d$variable, d$value, mean)
bplot<-
  ggplot(data=d, aes(x=variable, y=value))+
  geom_boxplot()+
  theme(
    axis.text=element_text(size=12, color="black"),
    axis.text.x = element_text(angle=90),
    axis.title=element_text( size=12, color="black"),
    axis.ticks=element_line(size=2, color="black"),
    title=element_text( size=12, face="bold"),
    plot.title = element_text(hjust=0),
    axis.line=element_line(size=1, color="black"),
    legend.text =element_text(size=9),
    legend.key.size =  unit(0.25, "in"),
    plot.margin = unit(c(6,1,6,1), "pt"),
    legend.box = "horizontal", legend.position = "bottom"
  )+
  scale_y_continuous(name="Percent Improvement Pre to Post Test",limits=c(-0.9,0.9),
                     breaks=c(-0.9,-0.6,-0.3,0,0.3,0.6, 0.9),
                     labels=c("-90%","-60%","-30%", "0%", "30%", "60%", "90%"))+
  scale_x_discrete(name="")

library(gtable)
library(grid)
#names(sumtable)[2:12]<-gsub("_","\n",names(sumtable)[2:12])
tt<-ttheme_minimal(core=list(fg_params=list(cex=0.8)),
                   colhead=list(fg_params=list(cex=0.7,  fontface=2L))
)
tbl<-tableGrob(sumtable, theme=tt, rows=NULL)
tbl<-gtable_add_grob(tbl,
                     grobs=segmentsGrob(  x0 = unit(0,"npc"),
                                          y0 = unit(0,"npc"),
                                          x1 = unit(1,"npc"),
                                          y1 = unit(0,"npc"),
                                          gp = gpar(lwd = 2.0)),
                     t = 1, b = 1, l = 1, r = ncol(tbl))
#grid.arrange(bplot, tbl, nrow=2, as.table=T, heights=c(3,1))
tbl<-tbl<-gtable_add_grob(tbl,
                          grobs=segmentsGrob(  x0 = unit(0,"npc"),
                                               y0 = unit(0,"npc"),
                                               x1 = unit(1,"npc"),
                                               y1 = unit(0,"npc"),
                                               gp = gpar(lwd = 2.0)),
                          t = 6, b = 6, l = 1, r = ncol(tbl))
tbl<-tbl<-gtable_add_grob(tbl,
                          grobs=segmentsGrob(  x0 = unit(1,"npc"),
                                               y0 = unit(0,"npc"),
                                               x1 = unit(0,"npc"),
                                               y1 = unit(0,"npc"),
                                               gp = gpar(lwd = 2.0)),
                          t = 1, b = 1, l = 1, r = ncol(tbl))

boxplot_w_table<-grid.arrange(bplot, tbl, nrow=2, as.table=T,
                              heights=c(2,1))
library(cowplot)
boxwtab<-plot_grid(
  bplot,
  tbl, nrow=2,
  ncol=1,
  rel_widths = c(1,0.1), rel_heights = c(2,1)
)
setwd("~/Dropbox/000-Bill and Katie Work")
pdf(file="Boxplot_TaskOrder_Oct2018.pdf", height=10,width=14)
boxwtab
dev.off()
ggsave(boxplot_w_table,file="Boxplot_TaskOrder_Oct2018.pdf", width=12, height=11)
print(boxplot_w_table)


###########
#Mean and SD of scores

#Mean and sd when top and bottom three task combos are removed
mean(d$value[d$variable!="Tibet2_Finch2" & d$variable!="Swallows1_Finch2" & 
               d$variable!="Species1_Finch1" & d$variable!="Finch1_Tibet2" &
               d$variable!="Finch2_Swallows2" & d$variable!="Finch2_Tibet2"], na.rm = T)
sd(d$value[d$variable!="Tibet2_Finch2" & d$variable!="Swallows1_Finch2" & 
             d$variable!="Species1_Finch1" & d$variable!="Finch1_Tibet2" &
             d$variable!="Finch2_Swallows2" & d$variable!="Finch2_Tibet2"], na.rm = T)

#Without top and bottom tasks removed
mean(d$value, na.rm = T)
sd(d$value, na.rm = T)
0.118/0.26


#What's the sd of just pre tests
sd(a$pretest, na.rm = T)
sd(a$posttest, na.rm = T)


################
#Checking Ethnicity and Gender Differences
a<-a[a$Gender!=(-9),]
save(a, file="IHUB_Oct2018.rdata")

gentest<-t.test(diff~Gender, data=a)
gentest

a$coleth<-a$Ethnicity
a$coleth[a$coleth==2|a$coleth==6]<-1
table(a$coleth)
save(a, file="IHUB_Oct2018.rdata")
ethtest<-lm((diff*100)~factor(coleth), data=a)
ethtest
summary(ethtest)
gentest
