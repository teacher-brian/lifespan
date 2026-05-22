
library(clipr)
library(tidyverse)
ha_raw<- read_clip_tbl()
ha <- ha_raw
ha[6,1] <- 71


hist(ha$age.in.years,breaks = 3)
hist(ha$height.in.inches,breaks=8)

mean(ha$height.in.inches)
abline(v=mean(ha$height.in.inches),col='blue')
abline(v=median(ha$height.in.inches),col='orange')
median(ha$height.in.inches)

sort(ha$height.in.inches)
quantile(ha$height.in.inches)



plot(ha$age.in.years,ha$height.in.inches)

ha %>% mutate(sex=as.factor(sex)) %>% group_by(sex) %>%
 summarise(avg=mean(height.in.inches))

