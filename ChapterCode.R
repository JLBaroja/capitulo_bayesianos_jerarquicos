rm(list=ls())
library('extrafont')
library('R2jags')

setwd("~/Documents/Luis/Courses and Workshops/Modelos Estocásticos - feat. Federico Sanabria/Chapter - Modelos Jerárquicos")



# Ejemplos de distribuciones Poisson
pdf(file='poisson.pdf',family= "CM Roman" ,width=4,height=2.5)
par(mai=c(.45,.2,0,0.0))
par(lend=1)
plot(0,axes=F,type='n',xlim=c(0,20),ylim=c(0,.3))
lines(0:20,dpois(0:20,lambda=2),type='o',pch=21,bg='white',cex=1)
lines(0:20,dpois(0:20,lambda=5),type='o',pch=21,bg='gray70',cex=1)
lines(0:20,dpois(0:20,lambda=10),type='o',pch=21,bg='black',cex=1)
legend(15,.25,yjust=0.5,xjust=0.5,
       legend=c(expression(paste(lambda,' = 2')),
                expression(paste(lambda,' = 5')),
                expression(paste(lambda,' = 10'))),
       pch=21,lwd=1,
       pt.bg=c('white','gray70','black'),
       box.lty='blank')
mtext(expression(paste("Posibles valores de ",italic(c[f]))),1,cex=0.8,line=1.25)
mtext('Probabilidad',2,line=0,cex=0.8)
axis(1,at=0:20,tck=-.02,labels=F)
axis(1,at=seq(0,20,1),cex.axis=0.54,lwd=0,padj=-3)
dev.off() 
embed_fonts('poisson.pdf',outfile='poisson.pdf')




# Datos
persona_1 <- c(6,8,8,9,7,11,6,6,10,13,13,15)
persona_2 <- c(8,8,9,13,8,7,11,9,8,12,10,8)
persona_3 <- c(2,6,4,10,4,5,9,5,8,7,9,7)
persona_4 <- c(2,1,1,2,0,1,2,1,3,1,5,1)
persona_5 <- c(3,1,2,2,1,2,2,2,5,2,4,0)
persona_6 <- c(1,5,4,5,4,3,4,2,5,8,3,5)
persona_7 <- c(10,12,17,17,9,11,18,10,13,20,17,16)
persona_8 <- c(3,4,6,9,7,8,11,3,7,8,10,11)
persona_9 <- c(3,5,7,3,2,6,6,3,4,7,5,6)
persona_10 <- c(2,3,1,3,1,2,2,2,5,6,4,5)
persona_11 <- c(6,9,10,13,5,5,10,9,11,16,9,13)
persona_12 <- c(10,2,14,8,5,6,11,6,10,8,6,11)
persona_13 <- c(7,4,5,8,0,3,3,6,5,10,5,7)
persona_14 <- c(5,3,4,7,4,5,5,4,5,7,7,6)
persona_15 <- c(2,3,3,4,3,0,1,3,1,3,9,6)
baile <- cbind(persona_1,persona_2,persona_3,persona_4,persona_5,
               persona_6,persona_7,persona_8,persona_9,persona_10,
               persona_11,persona_12,persona_13,persona_14,persona_15)
n_fiestas <- nrow(baile)
n_personas <- ncol(baile)
datos_jags <- list('baile','n_personas','n_fiestas')



pdf_name <- 'data_1.pdf'
pdf(file=pdf_name,family='CM Roman',width=4,height=2.5)
layout(1)
par(mai=c(0.05,0.25,0.35,0))
plot(0,type='n',
     axes=F,
     xlim=c(0.75,n_personas+0.25),
     ylim=c(-n_fiestas-0.5,-0.5))
max_cex <- 1.8
for(s in 1:n_personas){
  for(i in 1:n_fiestas){
    coord_X <- s
    coord_Y <- -i
    text(coord_X,coord_Y,
         paste(baile[i,s]),
         cex=.75,adj=0.5,family="CM Typewriter")
  }
}
lines(x=c(0.5,n_personas+0.5),
      y=c(-n_fiestas-.8,-n_fiestas-0.8))
lines(x=c(0.5,n_personas+0.5),
      y=c(-.2,-0.2))
lines(x=c(0.5,0.5),
      y=c(-n_fiestas-.8,-.2))
lines(x=c(n_personas+0.5,n_personas+0.5),
      y=c(-n_fiestas-.8,-0.2))
axis(3,at=1:n_personas,cex.axis=0.65,padj=-1,lwd=0,pos=-1.75)
axis(2,at=-n_fiestas:-1,cex.axis=0.65,lwd=0,pos=1.15,las=1,
     labels=abs(-n_fiestas:-1))
mtext('Personas',3,line=.75,cex=.8)
mtext('Fiestas',2,line=.35,cex=.8)
dev.off()
embed_fonts(pdf_name)




pdf_name <- 'data_2.pdf'
pdf(file=pdf_name,family='CM Roman',width=4,height=2.5)
layout(1)
par(mai=c(0.05,0.25,0.35,0))
plot(0,type='n',
     axes=F,
     xlim=c(0.75,n_personas+0.25),
     ylim=c(-n_fiestas-0.5,-0.5))
max_cex <- 1.8
for(s in 1:n_personas){
  for(i in 1:n_fiestas){
    coord_X <- s
    coord_Y <- -i
    local_cex <- baile[i,s]*max_cex/max(baile)  
    points(coord_X,coord_Y,
           pch=1,lwd=1,
           cex=local_cex)
  }
}
lines(x=c(0.5,n_personas+0.5),
      y=c(-n_fiestas-.8,-n_fiestas-0.8))
lines(x=c(0.5,n_personas+0.5),
      y=c(-.2,-0.2))
lines(x=c(0.5,0.5),
      y=c(-n_fiestas-.8,-.2))
lines(x=c(n_personas+0.5,n_personas+0.5),
      y=c(-n_fiestas-.8,-0.2))
axis(3,at=1:n_personas,cex.axis=0.65,padj=-1,lwd=0,pos=-1.75)
axis(2,at=-n_fiestas:-1,cex.axis=0.65,lwd=0,pos=1.15,las=1,
     labels=abs(-n_fiestas:-1))
mtext('Personas',3,line=.75,cex=.8)
mtext('Fiestas',2,line=.35,cex=.8)
dev.off()
embed_fonts(pdf_name)





# m0
write('model{
      # Datos (nivel 0)
      for(f in 1:n_fiestas){
        baile[f,14]~dpois(lambda_m0)
      }

      # Priors (nivel 1)
      lambda_m0~dunif(0,50)

      }','m0.bug')

parametros <- c('lambda_m0')

valores_iniciales <- list(list(lambda_m0=runif(1,0,50)),
                          list(lambda_m0=runif(1,0,50)),
                          list(lambda_m0=runif(1,0,50)))

m0 <- jags(data=datos_jags,
           inits=valores_iniciales,
           parameters.to.save=parametros,
           model.file='m0.bug',
           n.chains=3,
           n.iter=2000,
           n.burnin=1000,
           n.thin=1)
unlink('m0.bug')
summary(m0$BUGSoutput$summary)

lambda_m0 <- m0$BUGSoutput$sims.list$lambda_m0




pdf_name <- 'lambda_m0.pdf'
pdf(file=pdf_name,family='CM Roman',width=4,height=2.5)
nodo <- lambda_m0
q_inf <- quantile(nodo,0.025)
q_sup <- quantile(nodo,0.975)
par(mai=c(.45,.2,0,0.0))
par(lend=1)
plot(0,type='n',
     xlab='',ylab='',
     axes=F,
     xlim=c(0,10),ylim=c(0,max(density(nodo)$y*1.1)))
polygon(x=c(density(nodo)$x[1],
            density(nodo)$x,
            density(nodo)$x[length(density(nodo)$x)]),
        y=c(0,density(nodo)$y,0),
        border=NA,
        col='gray80')
lines(c(q_inf,q_sup),c(0,0),lwd=5)
lines(c(mean(nodo),mean(nodo)),
      c(max(density(nodo)$y)*.2,max(density(nodo)$y)),
      lty='dashed')
text(q_inf,max(density(nodo)$y*.075),
     paste(round(q_inf,2)),adj=0.75)
text(q_sup,max(density(nodo)$y*.075),
     paste(round(q_sup,2)),adj=0.25)
text(q_inf+(q_sup-q_inf)/2,max(density(nodo)$y*.1),
     '95% MDP',cex=0.65)
text(mean(nodo),max(density(nodo)$y*1.05),
     paste(round(mean(nodo),2)),adj=.5,cex=0.8)
mtext(expression(paste("Posibles valores de ",lambda)),1,cex=0.8,line=1.25)
mtext('Probabilidad',2,line=0,cex=0.8)
axis(1,at=0:10,tck=-.02,labels=F)
axis(1,at=seq(0,10,1),cex.axis=0.75,lwd=0,padj=-2)
dev.off()
embed_fonts(pdf_name)






# m1 
write('model{
      # Datos (nivel 0)
      for(p in 1:n_personas){
        for(f in 1:n_fiestas){
          baile[f,p]~dpois(lambda_m1[p])
          baile_pred_m1[f,p]~dpois(lambda_m1[p])
        }
      }
      
      # Priors (nivel 1)
      for(p in 1:n_personas){
        lambda_m1[p]~dunif(0,50)
      }

      # Distribucion posterior predictiva
      lambda_pred_new_m1~dunif(0,50)
      for(f in 1:n_fiestas){
        baile_pred_new_m1[f]~dpois(lambda_pred_new_m1)
      }

      }','m1.bug')

parametros <- c('lambda_m1',
                'lambda_pred_new_m1',
                'baile_pred_new_m1',
                'baile_pred_m1')

valores_iniciales <- list(list(lambda_m1=runif(n_personas,0,50),lambda_pred_new_m1=runif(1,0,50)),
                          list(lambda_m1=runif(n_personas,0,50),lambda_pred_new_m1=runif(1,0,50)),
                          list(lambda_m1=runif(n_personas,0,50),lambda_pred_new_m1=runif(1,0,50)))

m1 <- jags(data=datos_jags,
           inits=valores_iniciales,
           parameters.to.save=parametros,
           model.file='m1.bug',
           n.chains=3,
           n.iter=2000,
           n.burnin=1000,
           n.thin=1)
unlink('m1.bug')
summary(m1$BUGSoutput$summary)

lambda_m1 <- m1$BUGSoutput$sims.list$lambda_m1
lambda_pred_new_m1 <- m1$BUGSoutput$sims.list$lambda_pred_new_m1
baile_pred_m1 <- m1$BUGSoutput$sims.list$baile_pred_m1
baile_pred_new_m1 <- m1$BUGSoutput$sims.list$baile_pred_new_m1




pdf_name <- 'lambda_m1.pdf'
pdf(file=pdf_name,family='CM Roman',width=5.5,height=7.5)
layout(1:3)
par(omi=c(0,0,0,0))
par(mai=c(0.5,0,0,0))
par(lend=1)
plot(0,type='n',
     axes=F,
     xlim=c(0,20),
     ylim=c(0,1.2),
     xlab='',ylab='')
for(p in c(1:3,5:6,8:15,7,4)){
  if(p==7|p==4){
    local_color='gray40'
    local_lwd=3
  }
  else{
    local_color='gray70'
    local_lwd=2
  }
  lines(density(lambda_m1[,p]),col=local_color,lwd=local_lwd)
  text(mean(lambda_m1[,p]),max(density(lambda_m1[,p])$y)+.085,
       paste('P',p,sep=''),col=local_color,cex=1.5)
}
axis(1,at=seq(0,20,5),tck=-.02,labels=F)
axis(1,at=seq(0,20,5),lwd=0,padj=-1)
mtext(expression(paste("Posibles valores de ",lambda[p])),1,line=2.2)
# Plot lambda P4
nodo <- lambda_m1[,4]
q_inf <- quantile(nodo,0.025)
q_sup <- quantile(nodo,0.975)
plot(0,type='n',
     xlab='',ylab='',
     axes=F,
     xlim=c(0,20),ylim=c(0,1.2))
polygon(x=c(density(nodo)$x[1],
            density(nodo)$x,
            density(nodo)$x[length(density(nodo)$x)]),
        y=c(0,density(nodo)$y,0),
        border=NA,
        col='gray80')
lines(c(q_inf,q_sup),c(0,0),lwd=5)
lines(c(mean(nodo),mean(nodo)),
      c(max(density(nodo)$y)*0,max(density(nodo)$y)),
      lty='dashed')
text(q_inf,.05,
     paste(round(q_inf,2)),adj=c(0,0),srt=90,cex=1.5)
text(q_sup,.05,
     paste(round(q_sup,2)),adj=c(0,1),srt=90,cex=1.5)
text(mean(nodo),max(density(nodo)$y+.065),
     paste(round(mean(nodo),2)),adj=.5,cex=1.3)
text(17.5,.5,'P4',cex=10,col='gray97')
axis(1,at=seq(0,20,5),tck=-.02,labels=F)
axis(1,at=seq(0,20,5),lwd=0,padj=-1)
mtext(expression(paste("Posibles valores de ",lambda[4])),1,line=2.2)
# Plot lambda P7
nodo <- lambda_m1[,7]
q_inf <- quantile(nodo,0.025)
q_sup <- quantile(nodo,0.975)
plot(0,type='n',
     xlab='',ylab='',
     axes=F,
     xlim=c(0,20),ylim=c(0,1.2))
polygon(x=c(density(nodo)$x[1],
            density(nodo)$x,
            density(nodo)$x[length(density(nodo)$x)]),
        y=c(0,density(nodo)$y,0),
        border=NA,
        col='gray80')
lines(c(q_inf,q_sup),c(0,0),lwd=5)
lines(c(mean(nodo),mean(nodo)),
      c(max(density(nodo)$y)*0,max(density(nodo)$y)),
      lty='dashed')
text(q_inf,.05,
     paste(round(q_inf,2)),adj=c(0,0),srt=90,cex=1.5)
text(q_sup,.05,
     paste(round(q_sup,2)),adj=c(0,1),srt=90,cex=1.5)
text(mean(nodo),max(density(nodo)$y+.065),
     paste(round(mean(nodo),2)),adj=.5,cex=1.3)
text(2.5,.5,'P7',cex=10,col='gray97')
axis(1,at=seq(0,20,5),tck=-.02,labels=F)
axis(1,at=seq(0,20,5),lwd=0,padj=-1)
mtext(expression(paste("Posibles valores de ",lambda[7])),1,line=2.2)
dev.off()
embed_fonts(pdf_name)




pdf_name <- 'data_pos_pred_m1.pdf'
pdf(file=pdf_name,family='CM Roman',width=4,height=2.5)
layout(1)
par(mai=c(0.05,0.25,0.35,0))

plot(0,type='n',
     axes=F,
     xlim=c(0.75,n_personas+1+0.25),
     ylim=c(-n_fiestas-0.5,-0.5))
max_cex <- 1.8
for(s in 1:(n_personas+1)){
  for(i in 1:n_fiestas){
    coord_X <- s
    coord_Y <- -i
    if(s<=n_personas){
      local_cex_inf <- quantile(baile_pred_m1[,i,s],0.025)*max_cex/max(baile)
      local_cex_sup <- quantile(baile_pred_m1[,i,s],0.975)*max_cex/max(baile)
      points(coord_X,coord_Y,
             pch=16,
             col=rgb(150,150,150,maxColor=255,alpha=100),
             cex=local_cex_sup)
      points(coord_X,coord_Y,
             pch=16,
             col='white',
             cex=local_cex_inf)
    }
    else if(s==(n_personas+1)){
      clip(0.5,(n_personas+1+0.5),(-n_fiestas-.8),-0.2)
      local_cex_inf <- quantile(baile_pred_new_m1[,i],0.025)*max_cex/max(baile)
      local_cex_sup <- quantile(baile_pred_new_m1[,i],0.975)*max_cex/max(baile)
      points(coord_X,coord_Y,
             pch=16,
             col=rgb(150,150,150,maxColor=255,alpha=100),
             cex=local_cex_sup)
      points(coord_X,coord_Y,
             pch=16,
             col='white',
             cex=local_cex_inf)
    }
  }
}
for(s in 1:n_personas){
  for(i in 1:n_fiestas){
    coord_X <- s
    coord_Y <- -i
    local_cex <- baile[i,s]*max_cex/max(baile)  
    points(coord_X,coord_Y,
           pch=1,lwd=1,
           cex=local_cex)
  }
}
clip(-1000,1000,-1000,1000)
# Line 1
lines(x=c(0.5,n_personas+1+0.5),
      y=c(-n_fiestas-.8,-n_fiestas-0.8))
# Line 2
lines(x=c(0.5,0.5),
      y=c(-n_fiestas-.8,-.2))
# Line 3
lines(x=c(0.5,n_personas+1+0.5),
      y=c(-.2,-0.2))
# Line 4
lines(x=c(n_personas+1+0.5,n_personas+1+0.5),
      y=c(-n_fiestas-.8,-0.2))
axis(3,at=1:(n_personas+1),labels=c(1:15,expression(paste(italic('p'['n'])))),
     cex.axis=0.65,padj=-1,lwd=0,pos=-1.75)
axis(2,at=-n_fiestas:-1,cex.axis=0.65,lwd=0,pos=1.15,las=1,
     labels=abs(-n_fiestas:-1))
mtext('Personas',3,line=.75,cex=.8)
mtext('Fiestas',2,line=.35,cex=.8)
dev.off()
embed_fonts(pdf_name)






# m2 
write('model{
      # Datos (nivel 0)
      for(p in 1:n_personas){
        for(f in 1:n_fiestas){
          baile[f,p]~dpois(lambda_m2[p])
          baile_pred_m2[f,p]~dpois(lambda_m2[p])
        }
      }
      
      # Parametros individuales (nivel 1)
      for(p in 1:n_personas){
        lambda_m2[p]~dlnorm(mu_lambda_m2,tau_lambda_m2)
      }
      
      # Priors (nivel 2)
      mu_lambda_m2~dunif(0,20)
      tau_lambda_m2 <- pow(sigma_lambda_m2,-2)
      sigma_lambda_m2~dunif(0.001,20)

      # Distribucion posterior predictiva
      lambda_pred_new_m2~dlnorm(mu_lambda_m2,tau_lambda_m2)
      for(f in 1:n_fiestas){
        baile_pred_new_m2[f]~dpois(lambda_pred_new_m2)
      }

      }','m2.bug')

parametros <- c('lambda_m2','mu_lambda_m2','sigma_lambda_m2',
                'lambda_pred_new_m2',
                'baile_pred_new_m2',
                'baile_pred_m2')

valores_iniciales <- list(list(mu_lambda_m2=runif(1,0,20),
                               sigma_lambda_m2=runif(1,0,20)),
                          list(mu_lambda_m2=runif(1,0,20),
                               sigma_lambda_m2=runif(1,0,20)),
                          list(mu_lambda_m2=runif(1,0,20),
                               sigma_lambda_m2=runif(1,0,20)))

m2 <- jags(data=datos_jags,
           inits=valores_iniciales,
           parameters.to.save=parametros,
           model.file='m2.bug',
           n.chains=3,
           n.iter=20000,
           n.burnin=10000,
           n.thin=10)
unlink('m2.bug')
summary(m2$BUGSoutput$summary)

lambda_m2 <- m2$BUGSoutput$sims.list$lambda_m2
mu_lambda_m2 <- m2$BUGSoutput$sims.list$mu_lambda_m2
sigma_lambda_m2 <- m2$BUGSoutput$sims.list$sigma_lambda_m2
lambda_pred_new_m2 <- m2$BUGSoutput$sims.list$lambda_pred_new_m2
baile_pred_new_m2 <- m2$BUGSoutput$sims.list$baile_pred_new_m2
baile_pred_m2 <- m2$BUGSoutput$sims.list$baile_pred_m2
mean_lambda_m2 <- exp(mu_lambda_m2+(sigma_lambda_m2^2/2))
sd_lambda_m2 <- ((exp(sigma_lambda_m2^2)-1)*exp(2*mu_lambda_m2+sigma_lambda_m2^2))^(1/2)


pdf_name <- 'lambda_m2.pdf'
pdf(file=pdf_name,family='CM Roman',width=5.5,height=7.5)
layout(1:3)
par(omi=c(0,0,0,0))
par(mai=c(0.5,0,0,0))
par(lend=1)
plot(0,type='n',
     axes=F,
     xlim=c(0,20),
     ylim=c(0,1.2),
     xlab='',ylab='')
for(p in c(1:3,5:6,8:15,7,4)){
  if(p==7|p==4){
    local_color='gray40'
    local_lwd=3
  }
  else{
    local_color='gray70'
    local_lwd=2
  }
  lines(density(lambda_m2[,p]),col=local_color,lwd=local_lwd)
  text(mean(lambda_m2[,p]),max(density(lambda_m2[,p])$y)+.085,
       paste('P',p,sep=''),col=local_color,cex=1.5)
}
axis(1,at=seq(0,20,5),tck=-.02,labels=F)
axis(1,at=seq(0,20,5),lwd=0,padj=-1)
mtext(expression(paste("Posibles valores de ",lambda[p])),1,line=2.2)
# Plot lambda P4
nodo_1 <- lambda_m1[,4]
nodo_2 <- lambda_m2[,4]
q_inf_1 <- quantile(nodo_1,0.025)
q_sup_1 <- quantile(nodo_1,0.975)
q_inf_2 <- quantile(nodo_2,0.025)
q_sup_2 <- quantile(nodo_2,0.975)
plot(0,type='n',
     xlab='',ylab='',
     axes=F,
     xlim=c(0,20),ylim=c(0,1.2))
# Plot distribucion M1
lines(density(nodo_1),
      col='gray80',lwd=2)
lines(c(q_inf_1,q_sup_1),c(0,0),lwd=5,col='gray80')
lines(c(mean(nodo_1),mean(nodo_1)),
      c(max(density(nodo_1)$y)*0,max(density(nodo_1)$y)),
      lty='dashed',col='gray80')
text(q_inf_1,.05,
     paste(round(q_inf_1,2)),adj=c(0,0),srt=90,cex=1.5,
     col='gray70')
text(q_sup_1,.05,
     paste(round(q_sup_1,2)),adj=c(0,1),srt=90,cex=1.5,
     col='gray70')
text(mean(nodo_1),max(density(nodo_1)$y+.065),
     paste(round(mean(nodo_1),2)),adj=.5,cex=1.3,
     col='gray70')
# Plot distribution M2
lines(density(nodo_2),
      col='black',lwd=2)
lines(c(q_inf_2,q_sup_2),c(0,0),lwd=5,col='black')
lines(c(mean(nodo_2),mean(nodo_2)),
      c(max(density(nodo_2)$y)*0,max(density(nodo_2)$y)),
      lty='dashed',col='black')
text(q_inf_2,.2,
     paste(round(q_inf_2,2)),adj=c(0,0),srt=90,cex=1.7,
     col='black')
text(q_sup_2,.2,
     paste(round(q_sup_2,2)),adj=c(0,1),srt=90,cex=1.7,
     col='black')
text(mean(nodo_2),max(density(nodo_2)$y+.14),
     paste(round(mean(nodo_2),2)),adj=.5,cex=1.5,
     col='black')
text(17.5,.5,'P4',cex=10,col='gray97')
line_1 <- .65
line_2 <- .45
segments(x0=c(9,9),x1=c(10,10),y0=c(line_1,line_2),y1=c(line_1,line_2),
         col=c('gray80','black'),lwd=4)
text(10.75,line_1,'M',family='cmsy10',cex=1.6)
text(10.75,line_1,'1',family='CM Roman',cex=1.2,adj=c(-1.6,1))
text(10.75,line_2,'M',family='cmsy10',cex=1.6)
text(10.75,line_2,'2',family='CM Roman',cex=1.2,adj=c(-1.6,1))

axis(1,at=seq(0,20,5),tck=-.02,labels=F)
axis(1,at=seq(0,20,5),lwd=0,padj=-1)
mtext(expression(paste("Posibles valores de ",lambda[4])),1,line=2.2)
# Plot lambda P7
nodo_1 <- lambda_m1[,7]
nodo_2 <- lambda_m2[,7]
q_inf_1 <- quantile(nodo_1,0.025)
q_sup_1 <- quantile(nodo_1,0.975)
q_inf_2 <- quantile(nodo_2,0.025)
q_sup_2 <- quantile(nodo_2,0.975)
plot(0,type='n',
     xlab='',ylab='',
     axes=F,
     xlim=c(0,20),ylim=c(0,1.2))
# Plot distribucion M1
lines(density(nodo_1),
      col='gray80',lwd=2)
lines(c(q_inf_1,q_sup_1),c(0,0),lwd=5,col='gray80')
lines(c(mean(nodo_1),mean(nodo_1)),
      c(max(density(nodo_1)$y)*0,max(density(nodo_1)$y)),
      lty='dashed',col='gray80')
text(q_inf_1,.05,
     paste(round(q_inf_1,2)),adj=c(0,0),srt=90,cex=1.5,
     col='gray70')
text(q_sup_1,.05,
     paste(round(q_sup_1,2)),adj=c(0,1),srt=90,cex=1.5,
     col='gray70')
text(mean(nodo_1),max(density(nodo_1)$y+.065),
     paste(round(mean(nodo_1),2)),adj=.5,cex=1.3,
     col='gray70')
# Plot distribution M2
lines(density(nodo_2),
      col='black',lwd=2)
lines(c(q_inf_2,q_sup_2),c(0,0),lwd=5,col='black')
lines(c(mean(nodo_2),mean(nodo_2)),
      c(max(density(nodo_2)$y)*0,max(density(nodo_2)$y)),
      lty='dashed',col='black')
text(q_inf_2,.2,
     paste(round(q_inf_2,2)),adj=c(0,0),srt=90,cex=1.7,
     col='black')
text(q_sup_2,.2,
     paste(round(q_sup_2,2)),adj=c(0,1),srt=90,cex=1.7,
     col='black')
text(mean(nodo_2),max(density(nodo_2)$y+.14),
     paste(round(mean(nodo_2),2)),adj=.5,cex=1.5,
     col='black')
text(2.5,.5,'P7',cex=10,col='gray97')
axis(1,at=seq(0,20,5),tck=-.02,labels=F)
axis(1,at=seq(0,20,5),lwd=0,padj=-1)
mtext(expression(paste("Posibles valores de ",lambda[7])),1,line=2.2)
dev.off()
embed_fonts(pdf_name)




pdf_name <- 'data_pos_pred_m2.pdf'
pdf(file=pdf_name,family='CM Roman',width=4,height=2.5)
layout(1)
par(mai=c(0.05,0.25,0.35,0))
plot(0,type='n',
     axes=F,
     xlim=c(0.75,n_personas+1+0.25),
     ylim=c(-n_fiestas-0.5,-0.5))
max_cex <- 1.8
for(s in 1:(n_personas+1)){
  for(i in 1:n_fiestas){
    coord_X <- s
    coord_Y <- -i
    if(s<=n_personas){
      local_cex_inf <- quantile(baile_pred_m2[,i,s],0.025)*max_cex/max(baile)
      local_cex_sup <- quantile(baile_pred_m2[,i,s],0.975)*max_cex/max(baile)
      points(coord_X,coord_Y,
             pch=16,
             col=rgb(150,150,150,maxColor=255,alpha=100),
             cex=local_cex_sup)
      points(coord_X,coord_Y,
             pch=16,
             col='white',
             cex=local_cex_inf)
    }
    else if(s==(n_personas+1)){
      clip(0.5,(n_personas+1+0.5),(-n_fiestas-.8),-0.2)
      local_cex_inf <- quantile(baile_pred_new_m2[,i],0.025)*max_cex/max(baile)
      local_cex_sup <- quantile(baile_pred_new_m2[,i],0.975)*max_cex/max(baile)
      points(coord_X,coord_Y,
             pch=16,
             col=rgb(150,150,150,maxColor=255,alpha=100),
             cex=local_cex_sup)
      points(coord_X,coord_Y,
             pch=16,
             col='white',
             cex=local_cex_inf)
    }
  }
}
for(s in 1:n_personas){
  for(i in 1:n_fiestas){
    coord_X <- s
    coord_Y <- -i
    local_cex <- baile[i,s]*max_cex/max(baile)  
    points(coord_X,coord_Y,
           pch=1,lwd=1,
           cex=local_cex)
  }
}
clip(-1000,1000,-1000,1000)
# Line 1
lines(x=c(0.5,n_personas+1+0.5),
      y=c(-n_fiestas-.8,-n_fiestas-0.8))
# Line 2
lines(x=c(0.5,0.5),
      y=c(-n_fiestas-.8,-.2))
# Line 3
lines(x=c(0.5,n_personas+1+0.5),
      y=c(-.2,-0.2))
# Line 4
lines(x=c(n_personas+1+0.5,n_personas+1+0.5),
      y=c(-n_fiestas-.8,-0.2))
axis(3,at=1:(n_personas+1),labels=c(1:15,expression(paste(italic('p'['n'])))),
     cex.axis=0.65,padj=-1,lwd=0,pos=-1.75)
axis(2,at=-n_fiestas:-1,cex.axis=0.65,lwd=0,pos=1.15,las=1,
     labels=abs(-n_fiestas:-1))
mtext('Personas',3,line=.75,cex=.8)
mtext('Fiestas',2,line=.35,cex=.8)
dev.off()
embed_fonts(pdf_name)







pdf_name <- 'hierarchical_lambda_m2.pdf'
pdf(file=pdf_name,family='CM Roman',width=4,height=2.5)
layout(matrix(1:2,ncol=2))
# Mean of Lambda
nodo <- mean_lambda_m2
q_inf <- quantile(nodo,0.025)
q_sup <- quantile(nodo,0.975)
par(mai=c(.45,.1,0,0.1))
plot(0,type='n',
     xlab='',ylab='',
     axes=F,
     xlim=c(0,20),ylim=c(0,max(density(nodo)$y*1.2)))
polygon(x=c(density(nodo)$x[1],
            density(nodo)$x,
            density(nodo)$x[length(density(nodo)$x)]),
        y=c(0,density(nodo)$y,0),
        border=NA,
        col='gray80')
par(lend=1)
lines(c(q_inf,q_sup),c(0,0),lwd=5)
lines(c(mean(nodo),mean(nodo)),
      c(0,max(density(nodo)$y)),
      lty='dashed')
par(lend=0)
text(q_inf,max(density(nodo)$y*.1),
     paste(round(q_inf,2)),adj=0.75)
text(q_sup,max(density(nodo)$y*.1),
     paste(round(q_sup,2)),adj=0.25)
text(mean(nodo),max(density(nodo)$y*1.05),
     paste(round(mean(nodo),2)),adj=.5,cex=0.8)
mtext(expression(paste("Posibles valores de ",mu^lambda)),1,cex=0.8,line=1.25)
axis(1,at=seq(0,20,5),tck=-.025,cex.axis=0.75,padj=-2)
# SD of Lambda
nodo <- sd_lambda_m2
q_inf <- quantile(nodo,0.025)
q_sup <- quantile(nodo,0.975)
plot(0,type='n',
     xlab='',ylab='',
     axes=F,
     xlim=c(0,20),ylim=c(0,max(density(nodo)$y*1.2)))
polygon(x=c(density(nodo)$x[1],
            density(nodo)$x,
            density(nodo)$x[length(density(nodo)$x)]),
        y=c(0,density(nodo)$y,0),
        border=NA,
        col='gray80')
par(lend=1)
lines(c(q_inf,q_sup),c(0,0),lwd=5)
lines(c(mean(nodo),mean(nodo)),
      c(0,max(density(nodo)$y)),
      lty='dashed')
par(lend=0)
text(q_inf,max(density(nodo)$y*.1),
     paste(round(q_inf,2)),adj=0.75)
text(q_sup,max(density(nodo)$y*.1),
     paste(round(q_sup,2)),adj=0.25)
text(mean(nodo),max(density(nodo)$y*1.05),
     paste(round(mean(nodo),2)),adj=.5,cex=0.8)
mtext(expression(paste("Posibles valores de ",sigma^lambda)),1,cex=0.8,line=1.25)
axis(1,at=seq(0,20,5),tck=-.025,cex.axis=0.75,padj=-2)
dev.off()
embed_fonts(pdf_name)






# m3
write('model{
      # Datos (nivel 0)
      for(p in 1:n_personas){
        for(f in 1:n_fiestas){
          baile[f,p]~dbinom(theta_m3[p],n_m3[f])
          baile_pred_m3[f,p]~dbinom(theta_m3[p],n_m3[f])
        }
        baile_pred_m3[(n_fiestas+1),p]~dbinom(theta_m3[p],n_pred_new_m3)
      }
      
      # Parametros primer nivel
      for(p in 1:n_personas){
        theta_m3[p]~dbeta(alpha_theta_m3,beta_theta_m3)
      }
      for(f in 1:n_fiestas){
        n_m3[f]~dcat(pi[])
      }
      for(i in 1:60){
        pi[i] <- 1/60
      }

      # Parametros segundo nivel
      alpha_theta_m3 <- mu_theta_m3/pow(sigma_theta_m3,2)
      beta_theta_m3 <- (1-mu_theta_m3)/pow(sigma_theta_m3,2)
      mu_theta_m3~dunif(0,1)
      sigma_theta_m3~dunif(0.01,20)

      # Distribucion posterior predictiva nuevo sujeto
      theta_pred_new_m3~dbeta(alpha_theta_m3,beta_theta_m3)
      n_pred_new_m3~dcat(pi[])
      for(f in 1:n_fiestas){
        baile_pred_new_m3[f]~dbinom(theta_pred_new_m3,n_m3[f])
      }
      baile_pred_new_m3[(n_fiestas+1)]~dbinom(theta_pred_new_m3,n_pred_new_m3)

      }','m3.bug')

parametros <- c('theta_m3','n_m3',
                'mu_theta_m3','sigma_theta_m3',
                'alpha_theta_m3','beta_theta_m3',
                'theta_pred_new_m3','n_pred_new_m3',
                'baile_pred_m3','baile_pred_new_m3')

valores_iniciales <- list(list(mu_theta_m3=runif(1,0,1),
                               sigma_theta_m3=runif(1,0.01,20),
                               n_m3=rep(30,n_fiestas),
                               n_pred_new_m3=30),
                          list(mu_theta_m3=runif(1,0,1),
                               sigma_theta_m3=runif(1,0.01,20),
                               n_m3=rep(30,n_fiestas),
                               n_pred_new_m3=30),
                          list(mu_theta_m3=runif(1,0,1),
                               sigma_theta_m3=runif(1,0.01,20),
                               n_m3=rep(30,n_fiestas),
                               n_pred_new_m3=30))

m3 <- jags(data=datos_jags,
           inits=valores_iniciales,
           parameters.to.save=parametros,
           model.file='m3.bug',
           n.chains=3,
           n.iter=50000,
           n.burnin=10000,
           n.thin=40)
unlink('m3.bug')
summary(m3$BUGSoutput$summary)

theta_m3 <- m3$BUGSoutput$sims.list$theta_m3
mu_theta_m3 <- m3$BUGSoutput$sims.list$mu_theta_m3
sigma_theta_m3 <- m3$BUGSoutput$sims.list$sigma_theta_m3
n_m3 <- m3$BUGSoutput$sims.list$n_m3
theta_pred_new_m3 <- m3$BUGSoutput$sims.list$theta_pred_new_m3
n_pred_new_m3 <- m3$BUGSoutput$sims.list$n_pred_new_m3
baile_pred_m3 <- m3$BUGSoutput$sims.list$baile_pred_m3
baile_pred_new_m3 <- m3$BUGSoutput$sims.list$baile_pred_new_m3




pdf_name <- 'data_pos_pred_m3.pdf'
pdf(file=pdf_name,family='CM Roman',width=4,height=2.5)
layout(1)
par(mai=c(0.05,0.25,0.35,0))
plot(0,type='n',
     axes=F,
     xlim=c(0.75,n_personas+1+0.25),
     ylim=c(-n_fiestas-1-0.5,-0.5))
max_cex <- 1.8
for(s in 1:(n_personas+1)){
  for(i in 1:(n_fiestas+1)){
    coord_X <- s
    coord_Y <- -i
    if(s<=n_personas){
      local_cex_inf <- quantile(baile_pred_m3[,i,s],0.025)*max_cex/max(baile)
      local_cex_sup <- quantile(baile_pred_m3[,i,s],0.975)*max_cex/max(baile)
      points(coord_X,coord_Y,
             pch=16,
             col=rgb(150,150,150,maxColor=255,alpha=100),
             cex=local_cex_sup)
      points(coord_X,coord_Y,
             pch=16,
             col='white',
             cex=local_cex_inf)
    }
    else if(s==(n_personas+1)){
      clip(0.5,(n_personas+1+0.5),(-n_fiestas-1-.8),-0.2)
      local_cex_inf <- quantile(baile_pred_new_m3[,i],0.025)*max_cex/max(baile)
      local_cex_sup <- quantile(baile_pred_new_m3[,i],0.975)*max_cex/max(baile)
      points(coord_X,coord_Y,
             pch=16,
             col=rgb(150,150,150,maxColor=255,alpha=100),
             cex=local_cex_sup)
      points(coord_X,coord_Y,
             pch=16,
             col='white',
             cex=local_cex_inf)
    }
  }
}
for(s in 1:n_personas){
  for(i in 1:n_fiestas){
    coord_X <- s
    coord_Y <- -i
    local_cex <- baile[i,s]*max_cex/max(baile)  
    points(coord_X,coord_Y,
           pch=1,lwd=1,
           cex=local_cex)
  }
}
clip(-1000,1000,-1000,1000)
# Line 1
lines(x=c(0.5,n_personas+1+0.5),
      y=c(-n_fiestas-1-.8,-n_fiestas-1-0.8))
# Line 2
lines(x=c(0.5,0.5),
      y=c(-n_fiestas-1-.8,-.2))
# Line 3
lines(x=c(0.5,n_personas+1+0.5),
      y=c(-.2,-0.2))
# Line 4
lines(x=c(n_personas+1+0.5,n_personas+1+0.5),
      y=c(-n_fiestas-1-.8,-0.2))
axis(3,at=1:(n_personas+1),
     labels=c(1:15,expression(paste(italic('p'['n'])))),
     cex.axis=0.65,padj=-1,lwd=0,pos=-1.75)
axis(2,at=(-n_fiestas-1):-1,
     labels=c(expression(paste(italic('f'['n']))),abs((-n_fiestas):-1)),
     cex.axis=0.65,lwd=0,pos=1.15,las=1)
     
mtext('Personas',3,line=.75,cex=.8)
mtext('Fiestas',2,line=.35,cex=.8)
dev.off()
embed_fonts(pdf_name)






# m4
write('model{
      # Datos (nivel 0)
      for(p in 1:n_personas){
        for(f in 1:n_fiestas){
          baile[f,p]~dbinom(theta_m4[p],n_m4[f])
          baile_pred_m4[f,p]~dbinom(theta_m4[p],n_m4[f])
        }
        baile_pred_m4[(n_fiestas+1),p]~dbinom(theta_m4[p],n_pred_new_m4)
      }
      
      # Parametros primer nivel
      for(p in 1:n_personas){
        theta_m4[p]~dbeta(alpha_theta_m4,beta_theta_m4)
      }
      for(f in 1:n_fiestas){
        n_m4[f]~dpois(lambda_n_m4)
      }
      
      # Parametros segundo nivel (priors)
      alpha_theta_m4 <- mu_theta_m4/pow(sigma_theta_m4,2)
      beta_theta_m4 <- (1-mu_theta_m4)/pow(sigma_theta_m4,2)
      mu_theta_m4~dunif(0,1)
      sigma_theta_m4~dunif(0.01,20)
      lambda_n_m4~dunif(0,60)
      
      # Distribucion posterior predictiva nuevo sujeto
      theta_pred_new_m4~dbeta(alpha_theta_m4,beta_theta_m4)
      n_pred_new_m4~dpois(lambda_n_m4)
      for(f in 1:n_fiestas){
        baile_pred_new_m4[f]~dbinom(theta_pred_new_m4,n_m4[f])
      }
      baile_pred_new_m4[(n_fiestas+1)]~dbinom(theta_pred_new_m4,n_pred_new_m4)
      
      }','m4.bug')

parametros <- c('theta_m4','n_m4',
                'mu_theta_m4','sigma_theta_m4','lambda_n_m4',
                'alpha_theta_m4','beta_theta_m4',
                'theta_pred_new_m4','n_pred_new_m4',
                'baile_pred_m4','baile_pred_new_m4')

valores_iniciales <- list(list(mu_theta_m4=runif(1,0,1),
                               sigma_theta_m4=runif(1,0.01,20),
                               lambda_n_m4=30),
                          list(mu_theta_m4=runif(1,0,1),
                               sigma_theta_m4=runif(1,0.01,20),
                               lambda_n_m4=40),
                          list(mu_theta_m4=runif(1,0,1),
                               sigma_theta_m4=runif(1,0.01,20),
                               lambda_n_m4=50))

m4 <- jags(data=datos_jags,
           inits=valores_iniciales,
           parameters.to.save=parametros,
           model.file='m4.bug',
           n.chains=3,
           n.iter=50000,
           n.burnin=10000,
           n.thin=40)
unlink('m4.bug')
summary(m4$BUGSoutput$summary)

theta_m4 <- m4$BUGSoutput$sims.list$theta_m4
mu_theta_m4 <- m4$BUGSoutput$sims.list$mu_theta_m4
sigma_theta_m4 <- m4$BUGSoutput$sims.list$sigma_theta_m4
n_m4 <- m4$BUGSoutput$sims.list$n_m4
theta_pred_new_m4 <- m4$BUGSoutput$sims.list$theta_pred_new_m4
n_pred_new_m4 <- m4$BUGSoutput$sims.list$n_pred_new_m4
baile_pred_m4 <- m4$BUGSoutput$sims.list$baile_pred_m4
baile_pred_new_m4 <- m4$BUGSoutput$sims.list$baile_pred_new_m4




pdf_name <- 'data_pos_pred_m4.pdf'
pdf(file=pdf_name,family='CM Roman',width=4,height=2.5)
layout(1)
par(mai=c(0.05,0.25,0.35,0))
plot(0,type='n',
     axes=F,
     xlim=c(0.75,n_personas+1+0.25),
     ylim=c(-n_fiestas-1-0.5,-0.5))
max_cex <- 1.8
for(s in 1:(n_personas+1)){
  for(i in 1:(n_fiestas+1)){
    coord_X <- s
    coord_Y <- -i
    if(s<=n_personas){
      local_cex_inf <- quantile(baile_pred_m4[,i,s],0.025)*max_cex/max(baile)
      local_cex_sup <- quantile(baile_pred_m4[,i,s],0.975)*max_cex/max(baile)
      points(coord_X,coord_Y,
             pch=16,
             col=rgb(150,150,150,maxColor=255,alpha=100),
             cex=local_cex_sup)
      points(coord_X,coord_Y,
             pch=16,
             col='white',
             cex=local_cex_inf)
    }
    else if(s==(n_personas+1)){
      clip(0.5,(n_personas+1+0.5),(-n_fiestas-1-.8),-0.2)
      local_cex_inf <- quantile(baile_pred_new_m4[,i],0.025)*max_cex/max(baile)
      local_cex_sup <- quantile(baile_pred_new_m4[,i],0.975)*max_cex/max(baile)
      points(coord_X,coord_Y,
             pch=16,
             col=rgb(150,150,150,maxColor=255,alpha=100),
             cex=local_cex_sup)
      points(coord_X,coord_Y,
             pch=16,
             col='white',
             cex=local_cex_inf)
    }
  }
}
for(s in 1:n_personas){
  for(i in 1:n_fiestas){
    coord_X <- s
    coord_Y <- -i
    local_cex <- baile[i,s]*max_cex/max(baile)  
    points(coord_X,coord_Y,
           pch=1,lwd=1,
           cex=local_cex)
  }
}
clip(-1000,1000,-1000,1000)
# Line 1
lines(x=c(0.5,n_personas+1+0.5),
      y=c(-n_fiestas-1-.8,-n_fiestas-1-0.8))
# Line 2
lines(x=c(0.5,0.5),
      y=c(-n_fiestas-1-.8,-.2))
# Line 3
lines(x=c(0.5,n_personas+1+0.5),
      y=c(-.2,-0.2))
# Line 4
lines(x=c(n_personas+1+0.5,n_personas+1+0.5),
      y=c(-n_fiestas-1-.8,-0.2))
axis(3,at=1:(n_personas+1),
     labels=c(1:15,expression(paste(italic('p'['n'])))),
     cex.axis=0.65,padj=-1,lwd=0,pos=-1.75)
axis(2,at=(-n_fiestas-1):-1,
     labels=c(expression(paste(italic('f'['n']))),abs((-n_fiestas):-1)),
     cex.axis=0.65,lwd=0,pos=1.15,las=1)

mtext('Personas',3,line=.75,cex=.8)
mtext('Fiestas',2,line=.35,cex=.8)
dev.off()
embed_fonts(pdf_name)

cat("\014") 
cat(paste('Las graficas se encuentran en:\n',getwd()))
# Fin del Codigo #