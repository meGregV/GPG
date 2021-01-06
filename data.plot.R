library(tidyverse)
library(gridExtra)
if(!require(ggcorrplot)) install.packages('ggcorrplot', repos = "http://cran.us.r-project.org")

# grab data
if (!file.exists('cleaned.RDS')){
  source('data.clean.R')} else {
      cleaned <- readRDS('cleaned.RDS')}

# 1 histograms of two response variables

(P1 <- cleaned %>% 
  ungroup() %>% 
  select(c(DiffMedianHourlyPercent,DiffMeanHourlyPercent)) %>%
  pivot_longer(everything()) %>%
  #Filter
  group_by(name) %>%
  filter(between(value,-100,100)) %>%
  ggplot(aes(x = value)) + 
  geom_histogram(aes(fill = value<=0),
                 breaks=seq(-100,100, by=2)) +
  guides(fill=FALSE)+
  scale_fill_brewer(palette="Set2") + 
  theme_bw() +
  theme(legend.position="none") +
  facet_wrap(.~name,scales = 'free_x') +
  labs(x = 'Differences in mean and median wages (%)', y = "Number of reporting companies") +
  theme(strip.background = element_rect(color = 'black', fill = 'white')))

  
# 2  plot quartiles 
# 2.1 sample 3 companies

sampled <- cleaned %>% 
  filter(MinEmployees >= 1000) %>% 
  filter(DiffMeanHourlyPercent == max(DiffMeanHourlyPercent) |
           DiffMeanHourlyPercent == min(DiffMeanHourlyPercent) |
           (DiffMeanHourlyPercent == 0)) %>% 
  group_by(DiffMeanHourlyPercent) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(EmployerName, DiffMeanHourlyPercent) %>% 
  arrange(desc(DiffMeanHourlyPercent))

plot_quartiles <- function(CompanyName, DiffMean) {
cleaned %>% 
            filter(str_detect(EmployerName, paste('^', str_to_title(CompanyName), sep=''))) %>% # pick a company
            select(matches('\\bMale\\w+le', perl=TRUE)) %>% # grab male quartiles
            pivot_longer(everything()) %>% 
            extract(name, c('gender', 'quartile'), '(\\bMale)(\\w+\\b)') %>% 
            mutate(men=round(value), women = 100 - men) %>% 
            select(-c(gender, value)) %>% 
            pivot_longer(c('men','women'), names_to='gender', values_to='value') %>% 
            mutate(quartile = str_replace(quartile,'(^\\w+?)(Middle)', '\\2\\1')) %>%  #sort quartiles
            uncount(value) %>% # uncount counts
            group_by(quartile) %>% 
            mutate(row = (row_number() -1)%/% 10 + 1,
                   col = (row_number() -1) %% 10 + 1) %>% # create matrices
            ggplot() + 
            ggtitle(paste(str_trunc(CompanyName,21),
                          ', MeanPayDiff = ',
                          paste0(DiffMean,'%'),   sep = ' ')) + 
            aes(col, row, color=gender) + 
            geom_point(shape=15) + 
            facet_grid(~quartile) +
            coord_equal()+
            scale_color_brewer(palette="Set2") + 
            theme_bw() +
            theme(plot.title = element_text(size=10, hjust = 0.5),
                  strip.background = element_rect(color = 'black', fill = 'white'),
                  strip.text.x = element_text(size=7),
                  legend.title = element_blank(),
                  axis.ticks = element_blank(),
                  axis.text = element_blank(),
                  axis.title = element_blank())
 }

p1 <- plot_quartiles(sampled[[1]][1], sampled[[2]][1])
p2 <- plot_quartiles(sampled[[1]][2], sampled[[2]][2])
p3 <- plot_quartiles(sampled[[1]][3], sampled[[2]][3])

grid.arrange(p1, p2, p3)

# 2.2 sample more of negative GPG to see if p3 is an outlier
sampled2 <- cleaned %>% 
  filter(MinEmployees >= 250) %>% 
  arrange(DiffMeanHourlyPercent) %>% 
  group_by(MinEmployees) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(EmployerName, DiffMeanHourlyPercent) 

p3.1 <- plot_quartiles(sampled2[[1]][1], sampled2[[2]][1])
p3.2 <- plot_quartiles(sampled2[[1]][2], sampled2[[2]][2])

grid.arrange(p3.1, p3.2, p3)

# 3 Mean Pay by INDUSTRY
df <- cleaned %>% 
  group_by(section) %>%
  drop_na(section) %>% # remove NA section
  mutate(section = str_extract(section, '^[\\S]+(?: +[\\S]+){0,3}')) %>%  # extract first 4 words https://regex101.com/r/BeiFAc/1/
  summarize(cnt = n(),
            avg = mean(DiffMeanHourlyPercent),
            se = sd(DiffMeanHourlyPercent)/sqrt(cnt)) %>%
  filter(cnt>10) %>% 
  mutate(section = reorder(section, avg)) %>%
  arrange(avg) %>% 
  ungroup()

p1 <- df %>% 
        ggplot(aes(x=section, color = section, y=avg, ymin = avg - 2*se, ymax = avg + 2*se)) +
        geom_point() +
        geom_errorbar() +
        scale_fill_brewer(palette='Set2') + 
        theme_bw() +
        theme(legend.position='none')

p1 +  ggtitle('95% CI bands of Mean Hourly Pay by Industry') +
  labs(y = 'Mean Hourly Pay') +
  theme(text = element_text(size=10),
        axis.text.x = element_text(angle = 90, hjust = 1),
        axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.5))


# 3.1 looking at samples for the industry with highest gap (Financial Svcs)
sampled3 <- cleaned %>% 
  filter(MinEmployees >= 250, str_detect(section, '^Financial')) %>% 
  arrange(desc(DiffMeanHourlyPercent)) %>% 
  group_by(MinEmployees) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(EmployerName, DiffMeanHourlyPercent) 

p3.1i <- plot_quartiles(sampled3[[1]][1], sampled3[[2]][1])
p3.2i <- plot_quartiles(sampled3[[1]][2], sampled3[[2]][2])
p3.3i <- plot_quartiles(sampled3[[1]][3], sampled3[[2]][3])

grid.arrange(p3.1i, p3.2i, p3.3i)

# 4.  By count

H1 <- cleaned %>% 
  ggplot(aes(x=DiffMeanHourlyPercent,  fill= factor(MinEmployees))) +
  geom_density(alpha=.4, size=.5) 

H1 + scale_fill_brewer(palette="YlOrBr") +
  ggtitle("GPG distribution by company size") +
  theme_minimal() +
  labs(fill = "Min # Employees") +
  theme(legend.position=c(.3,.4), plot.title = element_text(hjust=0.5))

# 4.  By CEO gender

H2_hat <- cleaned %>% 
  drop_na(Gender) %>% 
  group_by(Gender) %>% 
  summarize(avg = round(mean(DiffMeanHourlyPercent), 2))

cleaned %>% 
  drop_na(Gender) %>% 
    ggplot(aes(x=Gender, y=DiffMeanHourlyPercent, fill=Gender))+
    geom_violin(scale='count') +
    stat_summary(fun=mean, geom="point", fill='white', shape=21, size=4) +
    geom_text(data=H2_hat, aes(label= avg, y = avg+6))+
    ggtitle("GPG distribution by CEO gender, with means shown") +
    theme_minimal() +
    theme(axis.title.x = element_blank(), axis.text.x = element_text(size = 13),
          legend.position="none", plot.title = element_text(hjust = 0.5))+
    scale_colour_manual(values=c("#CC6666", "#3399FF"))
  
# 5.  by skew

# (H3 <- cleaned %>%   
#   ggplot(aes(x = pctMaleLQ, y=DiffMeanHourlyPercent, colour=pctMaleLMQ))+
#   geom_point()+
#   geom_smooth())

# 6. by pct
cleaned <- cleaned %>% 
  mutate(pctMaleLQ = MaleLowerQuartile/(MaleLowerQuartile + MaleLowerMiddleQuartile + MaleUpperMiddleQuartile + MaleTopQuartile),
         pctMaleLMQ = MaleLowerMiddleQuartile/(MaleLowerQuartile + MaleLowerMiddleQuartile + MaleUpperMiddleQuartile + MaleTopQuartile),
         pctMaleUMQ = MaleUpperMiddleQuartile/(MaleLowerQuartile + MaleLowerMiddleQuartile + MaleUpperMiddleQuartile + MaleTopQuartile),
         pctMaleTQ = MaleTopQuartile/(MaleLowerQuartile + MaleLowerMiddleQuartile + MaleUpperMiddleQuartile + MaleTopQuartile),
         pctFemaleLQ = FemaleLowerQuartile/(FemaleLowerQuartile + FemaleLowerMiddleQuartile + FemaleUpperMiddleQuartile + FemaleTopQuartile),
         pctFemaleLMQ = FemaleLowerMiddleQuartile/(FemaleLowerQuartile + FemaleLowerMiddleQuartile + FemaleUpperMiddleQuartile + FemaleTopQuartile),
         pctFemaleUMQ = FemaleUpperMiddleQuartile/(FemaleLowerQuartile + FemaleLowerMiddleQuartile + FemaleUpperMiddleQuartile + FemaleTopQuartile),
         pctFemaleTQ = FemaleTopQuartile/(FemaleLowerQuartile + FemaleLowerMiddleQuartile + FemaleUpperMiddleQuartile + FemaleTopQuartile),
         SkewGenderLQ = pctMaleLQ-pctFemaleLQ,
         SkewGenderLMQ = pctMaleLMQ-pctFemaleLMQ,
         SkewGenderUMQ = pctMaleUMQ-pctFemaleUMQ,
         SkewGenderTQ = pctMaleTQ-pctFemaleTQ)



(H4 <- cleaned %>% 
  pivot_longer(cols=starts_with('pct') & ends_with('Q'),
               names_to = c('gender', 'quartile'),
               names_pattern = '(Male|Female)(LQ|LMQ|UMQ|TQ)',
               values_to = 'value') %>% 
  ggplot(aes(x=factor(quartile, levels= c('LQ', 'LMQ', 'UMQ', 'TQ')), y=value*DiffMeanHourlyPercent))+
  geom_boxplot(aes(fill=gender)) + 
  labs(title='Distribution of GPG by quartile', 
       y='Mean Hourly GPG') +
  theme_minimal() +
  theme(axis.title.x = element_blank(),  plot.title = element_text(hjust = 0.5)))

# 7. correlation matrix
## check for highly correlated predictors and exclude
cleanedCor <- cleaned %>%
  select(where(is.numeric)) %>%
  cor() %>% 
  round(2)

ggcorrplot(cleanedCor, type='lower')

ggcorrplot(cleanedCor)

#strip out redundant predictors
stripped <- cleaned %>% 
  select(EmployerName, DiffMeanHourlyPercent,
          SkewGenderLQ, SkewGenderLMQ, SkewGenderUMQ, SkewGenderTQ,
          MinEmployees, Gender, section) %>% 
  drop_na()

strippedCor <- stripped %>%
  select(where(is.numeric)) %>%
  cor() %>% 
  round(1)
ggcorrplot(strippedCor, type='lower', lab=TRUE)

