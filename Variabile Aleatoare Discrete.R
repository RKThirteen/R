#Generez repartitia, dar nu voi mai pune probabilitatile sub Yi si Xj
#In plus, voi scoate cateva elemente Pi(i,j) de pe linii separate

frepcomgen<-function(n,m){
    
    #generez spatiul matricii pentru repartitii
    repartitie<-matrix(rep(0,(n+2)*(m+2)),nrow=n+2,ncol=m+2)
    #schimb numele coloanelor
    nume_coloane <- paste0("Y", 0:(m+1))
    nume_linii <- paste0("X", 0:(n+1))
    colnames(repartitie) <- nume_coloane
    rownames(repartitie) <- nume_linii
    #repartitie[1,1]<-"X/Y"
    #generez valori random pt Xi, Yj
    repartitie[1,-c(1,m+2)]<-round(runif(m,-5,5),digits=6)
    repartitie[-c(1,n+2),1]<-round(runif(n,-5,5),digits=6)
    
    #generez matricea de probabilitati random(suma lor va fi 1)
    probabilitati <- matrix(runif(n * m), nrow = n, ncol = m)
    probabilitati <- probabilitati / sum(probabilitati)
    
    #introducere probabilitati in repartitie
    
    repartitie[-c(1,n+2),-c(1,m+2)]<-probabilitati
    
    #pun la final valorile pi si qj
    #repartitie[1,m+2]="Pi"
    #repartitie[n+2,1]="Qj"
    for (i in 2:(m+1))
      repartitie[n+2,i]=sum(repartitie[2:(n+1),i])
    for (i in 2:(n+1))
      repartitie[i,m+2]=sum(repartitie[i,2:(m+1)])
    repartitie[n+2,m+2]=1
    
    #Eliminam n numere, cate unul de pe fiecare linie(in acest caz am ales elementele (i,i))
    for (i in 2:(n+1))
      repartitie[i,i]=Inf
    
    return(repartitie)

}

n=9
m=10
bidim<-frepcomgen(n,m)
bidim[2,4]=Inf
bidim[bidim[,4]==Inf,4]

#Idee: Gasim o pozitie in matrice unde avem numai o valoare lipsa pe coloana sau pe linie
#Acolo, putem sa scadem din probabilitatea marginala suma celorlalte prob completate
#Actualizam matricea cu prob gasite, apoi reluam toata aceasta operatie
#Ne oprim cand nu gasim pozitii unde avem o valoare lipsa pe coloana/linie
fcomplrepcom<-function(repartitie)
{
  ok<-0
  n=nrow(repartitie)-1
  m=ncol(repartitie)-1
  #Cat timp mai exista numere de completat
  while (ok==0)
  {
    #Presupunem ca vor fi completate toate numerele
    ok<-1
    #Check Coloane
    for (i in 2:n)
    {
      #Daca avem o singura variabila de inlocuit pe linie
      check_row<-repartitie[i,repartitie[i,]==Inf]
      if (length(check_row)==1)
      {
        #Determinam suma numerelor de pe linie si pozitia unde se afla Inf
        ok<-0
        poz<-0
        suma<-0
        for (j in 2:m)
          if (repartitie[i,j]!=Inf)
            suma<-suma+repartitie[i,j]
          else
            poz<-j
        #Apoi scadem din Pi suma celorlalte probabilitati
        repartitie[i,poz]=repartitie[i,m+1]-suma
      }
     
    }
    #In aceeasi maniera verificam si coloanele
    for (i in 2:m)
    {
      check_col<-repartitie[repartitie[,i]==Inf,i]
      if (length(check_col)==1)
      {
        ok<-0
        poz<-0
        suma<-0
        for (j in 2:n)
          if (repartitie[j,i]!=Inf)
            suma<-suma+repartitie[j,i]
        else
          poz<-j
        repartitie[poz,i]=repartitie[n+1,i]-suma
      }
    }
       
  }
  return(repartitie)
}

bidim<-fcomplrepcom(bidim)

sum(bidim[2:10,4])
bidim[11,4]

#Scoatem matricea de probabilitati
matriceprob<-function(repartitie)
{
  
  n=nrow(repartitie)-2
  m=ncol(repartitie)-2
  probabilitati<-matrix(rep(0,n*m),nrow=n,ncol=m)
  probabilitati<-repartitie[-c(1,n+2),-c(1,m+2)]
}

probs<-matriceprob(bidim)
probs

#Generez o matrice cu 4 coloane
#Coloanele 1 si 2 vor contine Xi si Pi
#Coloanele 3 si 4 vor contine Yj si Qj
frepmarginal<-function(repartitie){
  
  #iau valorile Yj si Xi din repartitie(xi->repartitie[i,1] yj->repartitie[1,j])
  n=nrow(repartitie)
  m=ncol(repartitie)
  valori_y=repartitie[1,2:(m-1)]
  valori_x=repartitie[2:(n-1),1]
  
  #Pi si Qj sunt luate de pe ultima coloana, respectiv ultima linie
  
  prob_marg_x=repartitie[-c(1,n),m]
  
  prob_marg_y=repartitie[n,-c(1,m)]
  
  #Asamblam matricea cu repartitiile comune
  rep_marginale=matrix(rep(0,4*max(m-2,n-2)),nrow=max(n-2,m-2),ncol=4)
  for (i in 1:(n-2))
  {
    rep_marginale[i,1]=valori_x[i]
    rep_marginale[i,2]=prob_marg_x[i]
  }
  for (i in 1:(m-2))
  {
    rep_marginale[i,3]=valori_y[i]
    rep_marginale[i,4]=prob_marg_y[i]
  }

  return(rep_marginale)
}

rep_marginale<-frepmarginal(bidim)
colSums(rep_marginale)


#functie pentru covarianta unei v.a bidimensionale
fcov<-function(repartitie)
{
  #luam repartitiile_marginale si matr de probabilitati
  rep_marg<-frepmarginal(repartitie)
  probabilitati<-matriceprob(repartitie)
  n=nrow(probabilitati)
  m=ncol(probabilitati)
  
  #calculez E[X] si E[Y]
  Ex<-0
  Ey<-0
  for (i in 1:n)
    Ex<-Ex+rep_marg[i,1]*rep_marg[i,2]
  
  for (i in 1:m)
    Ey<-Ey+rep_marg[i,3]*rep_marg[i,4]
  
  #Calculez E[XY]
  Exy<-0
  for (i in 1:n)
    for (j in 1:m)
      Exy<-Exy+rep_marg[i,1]*rep_marg[j,3]*probabilitati[i,j]
  return(Exy-Ex*Ey)
}

#functie pentru Var(X),Var(Y), stocate intr-un vector cu elementele {Var(X),Var(Y)}
fvar<-function(repartitie)
{
  rep_marg<-frepmarginal(repartitie)
  n=nrow(rep_marg)
  #calculez E[X] si E[Y]
  Ex<-0
  Ey<-0
  for (i in 1:n)
    Ex<-Ex+rep_marg[i,1]*rep_marg[i,2]
  
  for (i in 1:m)
    Ey<-Ey+rep_marg[i,3]*rep_marg[i,4]
  #Calculez E[X^2] si E[Y^2]
  #Merge sa fac suma de Xi*Xi*Pi pentru ca in cazul in care un Xi si Xj ar avea patratul egal
  #Ar fi (Xi*Xi*(Pi+Pj))=(Xi*Xi*Pi)+(Xi*Xi*Pj), iar Xi*Xi=Xj*Xj
  Ex2<-0
  Ey2<-0
  for (i in 1:n)
    Ex2<-Ex2+rep_marg[i,1]*rep_marg[i,1]*rep_marg[i,2]
  
  for (i in 1:m)
    Ey2<-Ey2+rep_marg[i,3]*rep_marg[i,3]*rep_marg[i,4]
  
  variante=c((Ex2-Ex^2),(Ey2-Ey^2))
  return(variante)
}

var<-fvar(bidim)
var

#cov(aX+bY,cX+dY)=ac???cov(X,X)+ad???cov(X,Y)+bc???cov(Y,X)+bd???cov(Y,Y)
#=ac*Var(X)+ad*cov(X,Y)+bc*cov(Y,X)+bd*Var(Y)
fpropcov<-function(repartitie,a=1,b=1,c=1,d=1)
{
  var=fvar(repartitie)
  return(fcov(repartitie)*(a*d+b*c)+var[1]*+a*c*var[1]+b*d*var[2])
}

rez<-fpropcov(bidim,1,2,3,4)
rez

#Daca x e -Inf, se cere P(x/y=valoare)
#Daca y e -Inf, P(y/x=valoare)
#Altfel nu fac nimic(ar fi functia fPcomun)
fPcond<-function(repartitie,x=-Inf,y=-Inf)
{
  n=nrow(repartitie)-1
  m=ncol(repartitie)-1
  index<-Inf
  if (x==-Inf && y==-Inf)
    return("Nu se calculeaza")
  else if (x==-Inf && y!=-Inf)
  {
    #Caut pozitia unde Yj=valoare specificata
    for (i in 2:m)
      if (repartitie[1,i]==y)
        index<-i
    #Daca nu gasesc, probabilitatea va fi 0
    if (index==Inf)
      return(0)
    #Altfel returnez vectorul ce contine Pij/Qj, unde j=index
    else
    {
      probabilitati<-matriceprob(repartitie)
      return(probabilitati[1:nrow(probabilitati),index]/sum(probabilitati[1:nrow(probabilitati),index]))
    }
   
  }
  else
  #Simetric, caut xi=valoare specificata si returnez vectorul Pij/Xi
  {
    for (i in 2:n)
      if (repartitie[i,1]==x)
        index<-i
    if (index==Inf)
      return(0)
    else
    {
      probabilitati<-matriceprob(repartitie)
      return(probabilitati[1:nrow(probabilitati),index]/sum(probabilitati[1:nrow(probabilitati),index]))
    }
  }
}

y<-bidim[1,3]
prob_cont<-fPcond(bidim,-Inf,y)
prob_cont

#Caut probabilitatea(x=valoare,y=valoare in repartitie)
#Daca nu exista-->prob 0
fPcomun <- function(repartitie, x, y) 
{
  n=nrow(repartitie)-1
  m=ncol(repartitie)-1
  for (i in 2:n)
    for (j in 2:m)
    {
      if (repartitie[1,i]==x && repartitie[j,1]==y)
        return(repartitie[i,j])
    }

  return(0)
    
}

x<-bidim[1,3]
y<-bidim[3,1]

p<-fPcomun(bidim,x,y)
p
#punctul g --------------
#Cov(5X+9,-3Y-2)-->5*(-3)*Cov(X,Y)
cerinta_1<-5*(-3)*fcov(bidim)
cerinta_1
#P(0<X<0.8|Y>0.3)=P(0<X<0.8,Y>0.3)/P(Y>0.3)
#caut probabilitatile unde Xi e cuprins intre 0 si 0.8 si Yj>0.3
#apoi fac calculul
cerinta_2<-function(repartitie)
{
  p<-0
  probabilitati<-matriceprob(repartitie)
  rep_marg<-frepmarginal(repartitie)
  n=nrow(probabilitati)
  m=ncol(probabilitati)
  for (i in 1:n)
    if (rep_marg[i,1]<0.8 && rep_marg[i,1]>0 && rep_marg[i,1]!=Inf)
      for (j in 1:m)
        if (rep_marg[j,3]>0.3 && rep_marg[j,3]!=Inf)
          p<-p+probabilitati[i,j]
  suma_prob <- sum(rep_marg[rep_marg[, 3] > 0.3, 3])
  if (suma_prob==0)
    return(0)
  p<-p/suma_prob
  return(p)
}
cerinta_2(bidim)
#P(X>0.2,Y<1.7)
#Caut probabilitatile unde Xi>0.2 si Yj<1.7
cerinta_3<-function(repartitie)
{
  p<-0
  probabilitati<-matriceprob(repartitie)
  rep_marg<-frepmarginal(repartitie)
  n=nrow(probabilitati)
  m=ncol(probabilitati)
  for (i in 1:n)
    if (rep_marg[i,1]>0.2)
      for (j in 1:m)
        if (rep_marg[j,3]<1.7)
          p<-p+probabilitati[i,j]
  return(p)
}
cerinta_3(bidim)

#-----------------------

#Transform Pi(i,j) In P(i)*Q(j)
ftransfind<-function(repartitie)
{
  n=nrow(repartitie)
  m=ncol(repartitie)
  for (i in 2:(n-1))
    for (j in 2:(m-1))
  repartitie[i,j]=repartitie[i,m]*repartitie[n,j]
  return(repartitie)
}

bidim<-ftransfind(bidim)
bidim

fverind<-function(repartitie)
{
  probabilitati<-matriceprob(repartitie)
  rep_marg<-frepmarginal(repartitie)
  n=nrow(probabilitati)
  m=ncol(probabilitati)
  print(probabilitati)
  print(rep_marg)
  #verific daca Pi(i,j)!=p(i)*q(j) pentru fiecare pereche(i,j) a matricii
  for (i in 1:n)
    for (j in 1:m)
        if (probabilitati[i,j]!=rep_marg[i,2]*rep_marg[j,4])
          return(FALSE)
  return(TRUE)
}

fverind(bidim)
#verific daca cov(X,Y)==0, pt ca X si Y sunt necorelate daca au covalenta 0(coef da 0)
fvernecor<-function(repartitie)
{

  if (fcov(repartitie)==0)
    return(TRUE)
  else
    return(FALSE)
}

fvernecor(bidim)

#Idee de reprezentare v.a tridimensionala: cu matrice
simulare3D<-function(n,m,k)
{
  
   #generam valori pentru Xi, Yj, Rl
   valori_x=runif(n,-1,5)
   valori_y=runif(m,-1,5)
   valori_z=runif(k,-1,5)
   #generam probabilitati
   probn<-runif(n)
   probn<-probn/sum(probn)
   probm<-runif(m)
   probm<-probm/sum(probm)
   probk<-runif(k)
   probk<-probk/sum(probk)
   
   #prima si a 2a coloana: Xi cu Pi
   #a 3a si a 4a coloana: Yj cu Qj
   #a 5a si a 6a coloana: Zl cu Rl
   #a 7a coloana: Pi*Qj*Rl
   tridim<-matrix(rep(0,n*m*k),nrow=n*m*k,ncol=7)
   ct<-1
   for (i in 1:n) {
     for (j in 1:m) {
       for (q in 1:k) {
         tridim[ct,1] <- valori_x[i]
         tridim[ct,2] <- probn[i]
         tridim[ct,3] <- valori_y[j]
         tridim[ct,4] <- probm[j]
         tridim[ct,5] <- valori_z[q]
         tridim[ct,6] <- probk[q]
         tridim[ct,7] <- probn[i] * probm[j] * probk[q]
         ct <- ct + 1
       }
       
     }
   }
   return(tridim)
}

tridim<-simulare3D(2,3,4)
tridim

#repartitia marginala a v.a tridimensionale
repartitii_marg_3D<-function(tridim)
{
  #pentru a extrage valorile lui X/Y/Z, putem extrage toate valorile de pe liniile lor
  #apoi eliminam duplicatele
  tridim_x=unique(tridim[,1])
  tridim_y=unique(tridim[,3])
  tridim_z=unique(tridim[,5])
  
  probabilitati<-matrix(rep(0,n*m*k),nrow=n*m*k,ncol=7)
  
  probabilitati[1:length(tridim_x),1]=tridim_x
  probabilitati[1:length(tridim_y),3]=tridim_y
  probabilitati[1:length(tridim_z),5]=tridim_z
  
  #pentru a afla repartitiile marginale:
  #Pentru un Pi(i,j,k), acesta ar face parte din Pi,Qj si Rl
  #Deci, daca de ex am aduna toate probabilitatile de forma Pi(i,j,l), cu un i FIXAT
  #Am obtine acel Pi
  #Side-note: Evident putem obtine si scotand probabilitatile unice de pe fiecare coloana
  #Voi calcula insa cu suma de probabilitati
  n=length(tridim_x)
  m=length(tridim_y)
  k=length(tridim_z)
  ct<-1
  for (i in 1:n)
    for (j in 1:m)
      for (l in 1:k)
      {
        probabilitati[i,2]=probabilitati[i,2]+tridim[ct,7]
        probabilitati[j,4]=probabilitati[j,4]+tridim[ct,7]
        probabilitati[l,6]=probabilitati[l,6]+tridim[ct,7]
        ct<-ct+1
      }
 return(probabilitati)
}

rep_marg_3D=repartitii_marg_3D(tridim)

rep_marg_3D