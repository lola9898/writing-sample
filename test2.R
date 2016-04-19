setwd("~/Downloads/DataAnalystTest2/")
databfco=read.csv('before_co.csv')

conversionbf=databfco[,4]
sum(conversionbf)/dim(databfco)[1]


datatestco=read.csv('test_co.csv')

conversiontest=datatestco[,4] #the reg-success column


sum(conversiontest)# number of users creating an account 

indico=rep(NA,dim(datatestco)[1]) # create an indicator that records the number ofuser using social link
                                  # to create an account or create an actual account.
for(i in 1:dim(datatestco)[1] )
{
  if(datatestco[i,4]==1)
  {indico[i]=1}else if(datatestco[i,4]==0&datatestco[i,7]!=0){
    indico[i]=1
  }else{
    indico[i]=0
  }
}

sum(indico)/dim(datatestco)[1]


}

datatestco=cbind(datatestco,indico)
prop.test(c(sum(conversionbf),sum(indico)),c(dim(databfco)[1],dim(datatestco)[1]))# a/b testing

dataafco=read.csv('after_co.csv') # loading after test checkout page data

conversionaf=dataafco[,4]

sum(conversionaf)/dim(dataafco)[1]




mobilebfco=databfco[which(databfco[,6]=='MOBILE'),] #seperate data by device 

sum(mobilebfco$reg_success)/dim(mobilebfco)[1]    # calculate the converaion rate of mobile user

deskbfco=databfco[which(databfco[,6]=='DESKTOP'),] #seperate data by device 

sum(deskbfco$reg_success)/dim(deskbfco)[1]        # calculate the converaion rate of desktop user

tabletbfco=databfco[which(databfco[,6]=='TABLET'),] #seperate data by device 

sum(tabletbfco$reg_success)/dim(tabletbfco)[1]    # calculate the converaion rate of tablet user

prop.test(c(sum(deskbfco$reg_success==1),sum(mobilebfco$reg_success==1),sum(tabletbfco$reg_success==1)),c(dim(deskbfco)[1],dim(mobilebfco)[1],dim(tabletbfco)[1]))



####for test period####
mobiletestco=datatestco[which(datatestco[,6]=='MOBILE'),]

sum(mobiletestco$indico)/dim(mobiletestco)[1]

desktestco=datatestco[which(datatestco[,6]=='DESKTOP'),]

sum(desktestco$indico)/dim(desktestco)[1]

tablettestco=datatestco[which(datatestco[,6]=='TABLET'),]

sum(tablettestco$indico)/dim(tablettestco)[1]

prop.test(c(sum(desktestco$indico==1),sum(mobiletestco$indico==1),sum(tablettestco$indico==1)),c(dim(desktestco)[1],dim(mobiletestco)[1],dim(tablettestco)[1]))



####after test period######
mobileafco=dataafco[which(dataafco[,6]=='MOBILE'),]

sum(mobileafco$reg_success)/dim(mobileafco)[1]

deskafco=dataafco[which(dataafco[,6]=='DESKTOP'),]

sum(deskafco$reg_success)/dim(deskafco)[1]

tabletafco=dataafco[which(dataafco[,6]=='TABLET'),]

sum(tabletafco$reg_success)/dim(tabletafco)[1]

prop.test(c(sum(deskafco$reg_success==1),sum(mobileafco$reg_success==1),sum(tabletafco$reg_success==1)),c(dim(deskafco)[1],dim(mobileafco)[1],dim(tabletafco)[1]))







