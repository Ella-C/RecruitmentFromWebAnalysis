library(tidyverse)
library(showtext)
showtext::showtext_auto()
library(jsonlite)
library(rvest)

path <- '~/PycharmProjects/WebScraping/Project001/recruitment_engineer/recruitment_engineer/spiders/'

files <- c()
for (i in 1:70){
    file <- paste(path,'前程_电子工程师招聘_',i,'.html',sep='')
    files <- c(files,file)
    #print(files[i])
}

# compose the 1st dataframe
page <- read_html(files[1],encoding = 'utf-8')  # read html
list_js <- page %>% html_elements('script[type="text/javascript"]') %>% html_text() # Pick up the elements with desired data
js <- gsub('\nwindow.__SEARCH_RESULT__ = ','',list_js[4]) # Replace useless strings to read json
df <- fromJSON(js)$engine_jds %>% select(c(5,6,9:17,19:22,25,26,28:30)) # Select meaningful columns
for (p in 1:dim(df)[1]){
    df[['jobwelf_list1']][p]<- df[['jobwelf_list']][p] %>% unlist %>% toString
    df[['attribute_text1']][p] <- df[['attribute_text']][p] %>% unlist %>% toString
} # convert list to character as list unable to store in database
df <- df %>% select(-c('jobwelf_list','attribute_text')) # remove columns with list
df %>% glimpse # check the output

for (ii in 2:length(files)){
    page <- read_html(files[ii],encoding='utf-8')
    if (length(page)==2){
        list_js <- page %>% html_elements('script[type="text/javascript"]') %>% html_text()
        js <- gsub('\nwindow.__SEARCH_RESULT__ = ','',list_js[4])
        x <- fromJSON(js)$engine_jds %>% select(c(5,6,9:17,19:22,25,26,28:30))
        for (p in 1:dim(x)[1]){
            x[['jobwelf_list1']][p]<- x[['jobwelf_list']][p] %>% unlist %>% toString
            x[['attribute_text1']][p] <- x[['attribute_text']][p] %>% unlist %>% toString
        }
        x <- x %>% select(-c('jobwelf_list','attribute_text'))
        df <- rbind(df,x)
    } else {
        print(paste(ii,'Invalid Page with 0B'))
    }
}

# Check the length of values before splitting salary
d <- c()
v <- c()
for (e in 1:dim(df)[1]){
    x <- df[['providesalary_text']][e] %>% str_split('-') %>% unlist
    if(length(x)< 2){
        print(paste(e,x,sep=','))
        d <- c(d,e)
    }
}
v <- rep('0-0',length(d))  # As length discrapency occurred,fill up for same length
df[['providesalary_text']]<- replace(df[['providesalary_text']],d,v)

# Extract Min and Max salary to calculate average salary
df$salaryMin <- df$providesalary_text %>% str_split('-') %>% sapply('[[',1) %>% as.numeric()
df$salaryMax <- df$providesalary_text %>% str_split('-') %>% sapply('[[',2) %>% parse_number()

# Convert values for the same unit Monthly salary
for (index in grep('万/月',df$providesalary_text)){
    df$salaryMin[index] <- df$salaryMin[index]*10000
    df$salaryMax[index] <- df$salaryMax[index]*10000
}
for (index in grep('千/月',df$providesalary_text)){
    df$salaryMin[index] <- df$salaryMin[index]*1000
    df$salaryMax[index] <- df$salaryMax[index]*1000
}
for (index in grep('万/年',df$providesalary_text)){
    df$salaryMin[index] <- df$salaryMin[index]*10000/12
    df$salaryMax[index] <- df$salaryMax[index]*10000/12
}

# Calculate the average monthly salary
df$salaryAvg <- round((df$salaryMin+df$salaryMax)/2,0)

# Extract city
df$city <- df$workarea_text %>% str_split('-') %>% sapply('[[',1)

# Check the length of attribute_text1 for each value,if the length is 
# less than 3, fill up the blank is required to ensure the length is 
# no less than 3, as we need 3 values including location,experience and education.
y <- c()
for (i in 1:dim(df)[1]){
   x <-  df$attribute_text1[i] %>% str_split(',') %>% unlist
   if (length(x) < 3){
       print(paste(i,x))
       y <- c(y,i)
   }
}
y1 <- c(539,2368,2704,2748,3278) # lack of experience and education
y2 <- c(306,433,688,898,1138,1753,1761,1817,2567,2672,3216) # lack of experience
y3 <- setdiff(y,c(y1,y2)) # lack of education

df$attribute_text1[y1]<- paste(df$attribute_text1[y1],'无需经验','学历不限',sep=',')
v2 <- c()
v3 <- c()
for(i in 1:length(y2)){
    x <- df$attribute_text1[y2[i]] %>% str_split(',') %>% unlist
    x <- paste(x[1],'无需经验',x[2],sep=',')
    v2 <- c(v2,x)
}
for(i in 1:length(y3)){
    x <- df$attribute_text1[y3[i]] %>% str_split(',') %>% unlist
    x <- paste(x[1],x[2],'学历不限',sep=',')
    v3 <- c(v3,x)
}
df$attribute_text1 <- replace(df$attribute_text1,y2,v2)
df$attribute_text1 <- replace(df$attribute_text1,y3,v3)

df$experience <- df$attribute_text1 %>% str_split(',')%>% sapply('[[',2)
df$education <- df$attribute_text1 %>% str_split(',')%>% sapply('[[',3)


# Plot salary
df %>% group_by(city) %>% summarise(n=n(),salary=mean(salaryAvg)) %>% arrange(-n) %>%
    head %>% mutate_if(is.character,as.factor) %>% ggplot(aes(fct_reorder(city,-salary)))+
    geom_col(aes(y=round(salary,0)),fill='green',alpha=.5)+
    geom_point(aes(y=n*1e+01),color='red',alpha=.5)+
    geom_line(aes(y=n*1e+01),group=1,color='red',alpha=.5)+
    geom_text(aes(label=format(round(salary),format='f',big.mark=',',scientific=FALSE),y=salary+2e+02),color='blue',alpha=.8)+
    geom_text(aes(label=n,y=n*13),color='red',alpha=.8)+
    labs(title='电子工程师市场月薪/元',x='城市',y='平均月薪/元')+
    scale_y_continuous(sec.axis=sec_axis(~./10))

df[['jobwelf_list1']] %>% str_split(',') %>% unlist %>% trimws %>% table %>%
    data.frame() %>% rename('benefits'='.') %>% arrange(-Freq) %>% head(10) %>%
    mutate_if(is.character,as.factor) %>%
    ggplot(aes(fct_reorder(benefits,-Freq),Freq))+geom_col(fill='green',alpha=.7)+
    labs(title='电子工程师常见福利',x='福利',y='岗位数量')

# Check if there's null value in the columns
sapply(df,function(x) sum(is.na(x)))
df$companyind_text[is.na(df$companyind_text)] <-'未提供'

# Plot Industry,size and salary
df %>% select(c('companysize_text','companyind_text','companytype_text','salaryAvg')) %>% 
    filter(companysize_text !="") %>%
    group_by(companysize_text,companytype_text,companyind_text) %>% summarize(n=n(),salary=mean(salaryAvg)) %>%
    filter(n > 40) %>%
    ggplot(aes(companysize_text,salary))+geom_col(fill='steelblue',alpha=.6)+facet_grid(companytype_text~companyind_text)+labs(x='公司规模',y='平均月薪/元',title='电子工程师在不同行业、公司规模和公司性质的月薪分布')+coord_flip()






