library(plyr)
library(readr)
library(ggfortify)
library(MASS)
library(dplyr)
library(plotrix)
library(tidyverse)
library(ggord)
library(ggforce)

#Bioacoustic Analysis for Classification of Bird Calls

#Importing monosyllabic calls
#1. sentinel trill
sentrillfiles <- list.files(path = "Sentinel trill",
                                 pattern = "*.txt",
                                 full.names = TRUE)

sentrill <- ldply(sentrillfiles,
                  read.table,
                  sep="\t",
                  fill=TRUE,
                  header=TRUE)

sentimefiles <- list.files(path = "sentinel trill time",
                           pattern="*.txt",
                           full.names=TRUE)

sentime <- as.data.frame(ldply(sentimefiles,
                               read.table,
                               sep="\t",
                               fill=TRUE,
                               header=TRUE))

sentime <- cbind(sentime,
                 callduration = sentime$End.Time..s. - sentime$Begin.Time..s.)
call <- rep(1, nrow(sentrill))
calltype <- rep(1, nrow(sentrill))
notes <- rep(1, nrow(sentrill))

sentrill <- cbind(sentrill, 
                  sentime[6], 
                  calltype, 
                  notes, 
                  call)
#-----------------------------------------------------------------------------

#2. soft trill
softtrillfiles <- list.files(path = "Soft trill",
                             pattern="*.txt",
                             full.names=TRUE)

softtrill <- ldply(softtrillfiles,
                   read.table,
                   sep="\t",
                   fill=TRUE,
                   header=TRUE)

softtimefiles <- list.files(path = "soft time",
                            pattern="*.txt",
                            full.names=TRUE)

softtime <- as.data.frame(ldply(softtimefiles,
                                read.table,
                                sep="\t",
                                fill=TRUE,
                                header=TRUE))
softtime <- softtime[1:5]

softtime <- cbind(softtime,
                  callduration = softtime$End.Time..s. - softtime$Begin.Time..s.)
call <- rep(2, nrow(softtrill))
calltype <- rep(1, nrow(softtrill))
notes <- rep(1, nrow(softtrill))

softtrill <- cbind(softtrill, softtime[6], calltype, notes, call)
#-----------------------------------------------------------------------------

#3. loud trill
loudtrillfiles <- list.files(path = "Loud trill",
                             pattern="*.txt",
                             full.names=TRUE)

loudtrill <- ldply(loudtrillfiles,
                   read.table,
                   sep="\t",
                   fill=TRUE,
                   header=TRUE)

loudtimefiles <- list.files(path = "loud trill time",
                            pattern="*.txt",
                            full.names=TRUE)

loudtime <- as.data.frame(ldply(loudtimefiles,
                                read.table,
                                sep="\t",
                                fill=TRUE,
                                header=TRUE))

loudtime <- cbind(loudtime,
                  callduration = loudtime$End.Time..s. - loudtime$Begin.Time..s.)

call <- rep(3, nrow(loudtrill))
calltype <- rep(1, nrow(loudtrill))
notes <- rep(1, nrow(loudtrill))

loudtrill <- cbind(loudtrill, 
                   loudtime[6], 
                   calltype, 
                   notes, 
                   call)
#-----------------------------------------------------------------------------

#creating a dataframe containing all the monosyllabic calls
monosyllabic_calls <- rbind(sentineltrill,
                            softtrill,
                            loudtrill)

#subsetting dataframe to include only parameters and call types
monosyllabic_calls <- monosyllabic_calls[,6:13]

#creating a vector of call types column from dataframe
mono_calls <- as.vector(monosyllabic_calls$call)

#substituting 1,2,3,4 as call names
for(i in 1:length(mono_calls)){
  if(mono_calls[i] == 1){
    calls[i] <- "sentinel trill"
  } else if (mono_calls[i] == 2){
    calls[i] <- "soft trill"
  } else {
    calls[i] <- "loud trill"
  }
  i <- i+1
}

#factoring call names
monosyllabic_calls$call<- as.factor(monosyllabic_calls$call)
levels(monosyllabic_calls$call) <- c("sentinel trill",
                                     "soft trill",
                                     "loud trill")

#running lda model for monosyllabic calls
dfa_mono <- lda(call ~ log(BW.90...Hz.) + log(Freq.5...Hz.) + log(Freq.95...Hz.) + log(Peak.Freq..Hz.) + callduration,
                  data = monosyllabic_calls)

#visualising groups
dfa_plot_mono <- ggord(model_mono,
                       monosyllabic_calls$call,
                       veccol='transparent',
                       vec_lab=(''),
                       grp_title='Monosyllabic Calls',
                       txt=NULL,
                       cols=c("grey", "#DDAA33", "#5C0A98"))

#changing aesthetics of the plot
dfa_plot_mono +
  theme_classic() +
  theme(legend.position = "bottom") +
  labs(title = "Vocalisation Clasification Based on Call Structure",
       subtitle = "Linear Discriminant Analysis of the first two linear discrimnants for monosyllabic calls") +
  coord_cartesian(ylim = c(-7,5), xlim = c(-6,6)) +
  theme(plot.margin = margin(0.5,0.5,0.5,0.5, "cm")) +
  scale_shape_manual("Monosyllabic Calls",
                     values=c(16,17,18))

#saving the plot as an image
ggsave("dfa_plot_mono.png")
#-----------------------------------------------------------------------------

#Multisyllabic calls
#1. location call
locationfiles <- list.files(path = "Location call",
                            pattern="*.txt",
                            full.names=TRUE)

location <- ldply(locationfiles,
                  read.table,
                  sep="\t",
                  fill=TRUE,
                  header=TRUE)

locationtimefiles <- list.files(path = "location time",
                                pattern="*.txt",
                                full.names=TRUE)

locationtime <- as.data.frame(ldply(locationtimefiles,
                                    read.table,
                                    sep="\t",
                                    fill=TRUE,
                                    header=TRUE))

locationtime <- cbind(locationtime,
                      callduration = locationtime$End.Time..s. - locationtime$Begin.Time..s.)
call <- rep(1, nrow(location))
calltype <- rep(1, nrow(location))
notes <- rep(3, nrow(location))

location <- cbind(location, 
                  locationtime[6], 
                  calltype, 
                  notes, 
                  call)
#-----------------------------------------------------------------------------

#2. twittering call
twitfiles <- list.files(path="Twittering call",
                        pattern="*.txt",
                        full.names=TRUE)

twitcall <- ldply(twitfiles,
                  read.table,
                  sep="\t",
                  fill=TRUE,
                  header=TRUE)

twittimefiles <- list.files(path = "twittering time",
                            pattern="*.txt",
                            full.names=TRUE)

twittime <- as.data.frame(ldply(twittimefiles,
                                read.table,
                                sep="\t",
                                fill=TRUE,
                                header=TRUE))

twittime <- cbind(twittime,
                  callduration = twittime$End.Time..s. - twittime$Begin.Time..s.)
call <- rep(2, nrow(twitcall))
calltype <- rep(1, nrow(twitcall))
notes <- rep(6, nrow(twitcall)) #not sure

twitcall <- cbind(twitcall, 
                  twittime[6], 
                  calltype, 
                  notes, 
                  call)
#-----------------------------------------------------------------------------

#3. flight call
flightcallfiles <- list.files(path = "Flight call",
                              pattern="*.txt",
                              full.names=TRUE)

flightcall <- ldply(flightcallfiles,
                    read.table,
                    sep="\t",
                    fill=TRUE,
                    header=TRUE)

flighttimefiles <- list.files(path = "flight time",
                              pattern="*.txt",
                              full.names=TRUE)

flighttime <- as.data.frame(ldply(flighttimefiles,
                                  read.table,
                                  sep="\t",
                                  fill=TRUE,
                                  header=TRUE))

flighttime <- cbind(flighttime,
                    callduration = flighttime$End.Time..s. - flighttime$Begin.Time..s.)

call <- rep(3, nrow(flightcall))
calltype <- rep(1, nrow(flightcall))
notes <- rep(3, nrow(flightcall))

flightcall <- cbind(flightcall, 
                    flighttime[6], 
                    calltype, 
                    notes, 
                    call)
#-----------------------------------------------------------------------------

#4. Alert call
alertcallfiles <- list.files(path = "Alert call",
                             pattern="*.txt",
                             full.names=TRUE)

alertcall <- ldply(alertcallfiles,
                   read.table,
                   sep="\t",
                   fill=TRUE,
                   header=TRUE)

alerttimefiles <- list.files(path = "alert time",
                             pattern="*.txt",
                             full.names=TRUE)

alerttime <- as.data.frame(ldply(alerttimefiles,
                                 read.table,
                                 sep="\t",
                                 fill=TRUE,
                                 header=TRUE))

alerttime <- cbind(alerttime, 
                   callduration = alerttime$End.Time..s. - alerttime$Begin.Time..s.)

call <- rep(4, nrow(alertcall))
calltype <- rep(2, nrow(alertcall))
notes <- rep(3, nrow(alertcall))

alertcall <- cbind(alertcall, 
                   alerttime[6], 
                   calltype, 
                   notes, 
                   call)
#-----------------------------------------------------------------------------

#5. intermediate alert call
intalertcallfiles <- list.files(path = "Intermediate alert",
                                pattern="*.txt",
                                full.names=TRUE)

intalertcall <- ldply(intalertcallfiles,
                      read.table,
                      sep="\t",
                      fill=TRUE,
                      header=TRUE)

intalerttimefiles <- list.files(path = "intermediate time",
                                pattern="*.txt",
                                full.names=TRUE)

intalerttime <- as.data.frame(ldply(intalerttimefiles,
                                    read.table,
                                    sep="\t",
                                    fill=TRUE,
                                    header=TRUE))

intalerttime <- cbind(intalerttime, 
                      callduration = intalerttime$End.Time..s. - intalerttime$Begin.Time..s.)

call <- rep(5, nrow(intalertcall))
calltype <- rep(2, nrow(intalertcall))
notes <- rep(2, nrow(intalertcall))

intalertcall <- cbind(intalertcall, 
                      intalerttime[6], 
                      calltype, 
                      notes, 
                      call)
#-----------------------------------------------------------------------------

#6. movement call
movementcallfiles <- list.files(path = "Movement call",
                                pattern="*.txt",
                                full.names=TRUE)

movementcall <- ldply(movementcallfiles,
                      read.table,
                      sep="\t",
                      fill=TRUE,
                      header=TRUE)

movementtimefiles <- list.files(path = "movement time",
                                pattern="*.txt",
                                full.names=TRUE)

movementtime <- as.data.frame(ldply(movementtimefiles,
                                    read.table,
                                    sep="\t",
                                    fill=TRUE,
                                    header=TRUE))

movementtime <- cbind(movementtime, 
                      callduration = movementtime$End.Time..s. - movementtime$Begin.Time..s.)

call <- rep(6, nrow(movementcall))
calltype <- rep(1, nrow(movementcall))
notes <- rep(2, nrow(movementcall))

movementcall <- cbind(movementcall, 
                      movementtime[6], 
                      calltype, 
                      notes, 
                      call)
#-----------------------------------------------------------------------------

#creating a dataframe containing all the multisyllabic calls
multisyllabic_calls <- rbind(location,
                             twitcall,
                             flightcall,
                             alertcall,
                             intalertcall,
                             movementcall)

#subsetting dataframe to include only parameters and call types
multisyllabic_calls <- multisyllabic_calls[,6:13]

#creating a vector of call types column from dataframe
multi_calls <- as.vector(multisyllabic_calls$call)

#substituting 1,2,3,4,5 as call names
for(i in 1:length(multi_calls)) {
  if(multi_calls[i] == 1){
    calls[i] <- "location call"
  } else if (multi_calls[i] == 2) {
    calls[i] <- "twittering call"
  } else if (multi_calls[i] == 3) {
    calls[i] <- "flight call"
  } else if (multi_calls[i] == 4) {
    calls[i] <- "alert call"
  } else if (multi_calls[i] == 5) {
    calls[i] <- "intermediate alert call"
  } else {
    calls[i] <- "movement call"
  }
  i <- i+1
}

#factoring call names
multisyllabic_calls$call<- as.factor(multisyllabic_calls$call)
levels(multisyllabic_calls$call) <- c("location call",
                                      "twittering call",
                                      "flight call",
                                      "alert call",
                                      "intermediate alert call",
                                      "movement call")

#running lda model for multisyllabic calls
dfa_multi <- lda(call ~ log(BW.90...Hz.) + log(Freq.5...Hz.) + log(Freq.95...Hz.) + log(Peak.Freq..Hz.) + callduration,
                 data = multisyllabic_calls)

#visualising groups
dfa_plot_multi <- ggord(model_multi,
                        multisyllabic_calls$call,
                        veccol='transparent',
                        vec_lab=(''),
                        grp_title='Multisyllabic Calls',
                        txt=NULL,
                        cols=c("#BB5566", "#004488", "#F7EDE2", "#E28869", "#09A4DB", "#B7C235"))

#changing plot aesthetics
dfa_plot_multi +
  theme_classic() +
  theme(legend.position = "bottom") +
  labs(title = "Vocalisation Clasification Based on Call Structure",
       subtitle = "Linear Discriminant Analysis of the first two linear discrimnants for multisyllabic calls") +
  coord_cartesian(ylim = c(-5,5), xlim = c(-6,6)) +
  theme(plot.margin = margin(0.5,0.5,0.5,0.5, "cm")) +
  scale_shape_manual("Multisyllabic Calls",
                     values=c(3,10,15,16,17,18))

#saving plot as an image
ggsave("dfa_plot_multi.png")
#-----------------------------------------------------------------------------

#combining all monosyllabic and multisyllabic calls in one dataframe
#this is done to run a DFA model on all the calls
all_calls <- rbind(monosyllabic_calls, multisyllabic_calls)

#running lda model for all calls
dfa_all <- lda(call~log(BW.90...Hz.) + log(Freq.5...Hz.) + log(Freq.95...Hz.) + log(Peak.Freq..Hz.) + callduration,
              data = allcalls)

#visualizing groups 
dfa_plot <- ggord(model2,
                  allcalls$call,
                  veccol='transparent',
                  vec_lab=(''),
                  grp_title='Whiteheaded Babbler calls',
                  txt=NULL,
                  cols=c("grey", "#DDAA33", "#BB5566", "#004488", "#F7EDE2", "#E28869", "#09A4DB", "#B7C235", "#5C0A98"))

#changing aesthetics of the plot
dfa_plot +
  theme_classic() +
  theme(legend.position = "bottom") +
  labs(title = "Vocalisation Clasification Based on Call Structure",
       subtitle = "Linear Discriminant Analysis of the first two linear discrimnants") +
  coord_cartesian(ylim = c(-7,5), xlim = c(-8,6)) +
  theme(plot.margin = margin(0.5,0.5,0.5,0.5, "cm"))

#saving the plot as an image
ggsave("dfa_plot.png")


