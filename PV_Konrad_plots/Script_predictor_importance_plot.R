pacman::p_load(lme4,MuMIn,readxl,dplyr,data.table,ggpubr)

df_NAm  <- read_xlsx('/vol/milkunB/jbosmans/PV_Konrad_NAm/df_PI_NAm.xlsx')
df_SAm  <- read_xlsx('/vol/milkunB/jbosmans/PV_Konrad_SAm/df_PI_SAm.xlsx')
df_Eur  <- read_xlsx('/vol/milkunB/jbosmans/PV_Konrad_Eur/df_PI_Eur.xlsx')
df_Asia <- read_xlsx('/vol/milkunB/jbosmans/PV_Konrad_Asia/df_PI_Asia.xlsx')
df_Afr  <- read_xlsx('/vol/milkunB/jbosmans/PV_Konrad_Afr/df_PI_Afr.xlsx')
df_Oc   <- read_xlsx('/vol/milkunB/jbosmans/PV_Konrad_Oc/df_PI_Oc.xlsx')

# change 'sign' to characters (somehow gets changed into integer when saving / loading csv)
df_NAm$sign  <- as.character(df_NAm$sign)
df_SAm$sign  <- as.character(df_SAm$sign)
df_Eur$sign  <- as.character(df_Eur$sign)
df_Asia$sign <- as.character(df_Asia$sign)
df_Afr$sign  <- as.character(df_Afr$sign)
df_Oc$sign   <- as.character(df_Oc$sign)

# includes country in first row, remove for plotting
df_NAm  <- df_NAm[-c(1),]
df_SAm  <- df_SAm[-c(1),]
df_Eur  <- df_Eur[-c(1),]
df_Asia <- df_Asia[-c(1),]
df_Afr  <- df_Afr[-c(1),]
df_Oc   <- df_Oc[-c(1),]


pdf(file='Plot_predictor_importance.pdf',width=10,height=14)

par(mfrow=c(3,2),mar=c(2,3,1,3),cex=1.4)

par(mar=c(3,7.2,1,3))
barplot(df_NAm$PI,names.arg=df_NAm$predictor,horiz=TRUE,las=1,col=df_NAm$sign,border=df_NAm$sign,xlim=c(0,0.4))
title('North America')

par(mar=c(3,0,1,10.2))
barplot(df_Eur$PI,names.arg=df_Eur$predictor,horiz=TRUE,las=1,col=df_Eur$sign,border=df_Eur$sign,xlim=c(0,0.4),yaxt='n')
title('Europe')
legend("bottomright", inset=.04,c('+','-'), fill=c('#3399FF','#cc3300'),border=c('#3399FF','#cc3300'), cex=1.4,box.lty = 0)

par(mar=c(3,7.2,1,3))
barplot(df_SAm$PI,names.arg=df_SAm$predictor,horiz=TRUE,las=1,col=df_SAm$sign,border=df_SAm$sign,xlim=c(0,0.4))
title('South America')

par(mar=c(3,0,1,10.2))
barplot(df_Afr$PI,names.arg=df_Afr$predictor,horiz=TRUE,las=1,col=df_Afr$sign,border=df_Afr$sign,xlim=c(0,0.4),yaxt='n')
title('Africa')

par(mar=c(3,7.2,1,3))
barplot(df_Asia$PI,names.arg=df_Asia$predictor,horiz=TRUE,las=1,col=df_Asia$sign,border=df_Asia$sign,xlim=c(0,0.4))
title('Asia')

par(mar=c(3,0,1,10.2))
barplot(df_Oc$PI,names.arg=df_Oc$predictor,horiz=TRUE,las=1,col=df_Oc$sign,border=df_Oc$sign,xlim=c(0,0.75),yaxt='n')
title('Oceania')


dev.off()
system('gv Plot_predictor_importance.pdf')


#legend(x=0.4,y=NULL,legend=c('+','-'),col=c('#3399FF','#cc3300'))
#barplot(df_Asia$PI,names.arg=df_Asia$predictor,horiz=TRUE,las=1,col=df_Asia$sign,border=df_Asia$sign,xlim=c(0,0.55),yaxt='n', annotate("text",x=0.1,y=as.character('Wetland'),label='Asia'))
