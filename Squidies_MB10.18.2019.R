##### WR Squid pop Analysis #####
## 10.1.2019 ##

## After some thought I think we should simplify this. The overall goal is the same thoughLets ignor partials baits. That is assume 
## partials are present. I also think at least initially, that we should move away from the multinomial approac that I initially suggested.
## Sorry for any wild goose chase I may have put you on. We may still end up back there though. So the new approach will really just be a 
## t-test bewtween the proportion missing between low and high sea otter areas.

## Below I am going to build a skeleton script that you can fill in/edit.

## Libraries ##
library(ggplot2)
library(dplyr)
library(VGAM)

### Data ###
sq1 <- read.csv("squid_pops_data_R_4.11.2019MB.csv")
sq1 <- sq1[1:23, ] # remove rows that just have NAs in them

str(sq1) # lets you look at shit, looks good

### Data prep ###
## Sea otter levels ##
sq1$sea_otter_levels <- ordered(sq1$sea_otter_level, levels = c("low", "high")) ## it does not really matter what order they are in. What this does
## changes the type of data the sea otter levels are to an 'ordered' factor. All you are telling it is that there are two levels (high/low)
## and that one (high) is at a higher level or different in a specifc way from the other (low)
sq1$sea_otter_levels ## see when you do this it says 'levels: low < high" this is what you want. what order the dataframe is in does not matter.

## Calculate proportions ##
## So the goal is to calculate the probability of a bait missing so that will be the number
## missing divided by the total number that were put out there. For example if there were 25 pops put out and 10 bottoms where missing then it
## would be 10/25 = 0.4. Make sense? We will actual structure the data differnetly for the stats but this is good practice and we can
## take a look at the data.

# Bottoms #
sq1$bots_prop <- sq1$full_bottoms / sq1$bottom_pop
sq1$bots_prop

# Tops #
# YOUR TRUN! DO THE SAME AS ABOVE BUT FOR TOPS #
sq1$tops_prop <- sq1$full_tops / sq1$top_pop
sq1$tops_prop

## Summary ##
## Lets mame a quick summary of what we are looking at here ##
summary <- sq1 %>% # this %>% thing is used in the 'dplyr' package called a pipe.
  group_by(sea_otter_levels) %>% 
  summarise(mean_bottoms = mean(bots_prop),
            sd_bottoms = sd(bots_prop),
            mean_tops = mean(tops_prop),
            sd_tops = sd(tops_prop))
summary

## Model building ##

mod.bots <- t.test(sq1$bots_prop[sq1$sea_otter_levels == "high"], sq1$bots_prop[sq1$sea_otter_levels == "low"])
mod.bots

## NOW DO THE SAME FOR TOPS
mod.tops <- t.test(sq1$tops_prop[sq1$sea_otter_levels == "high"], sq1$tops_prop[sq1$sea_otter_levels == "low"])
mod.tops


## ANOVA of location and sea otters ##
# This will test the effect of top v bottom and sea otters but we need to do some data manipulation first #
sq2 <- cbind(sq1[, c(1:5)])
sq2 <- rbind(sq2, sq2)
sq2$loc <- c(rep("top", 23), rep("bottom", 23))
sq2$prop_missing <- c(sq1$tops_prop, sq1$bots_prop)

mod.all <- lm(prop_missing ~ loc*sea_otter_level, data = sq2)
anova(mod.all)

mod.all <- lm(prop_missing ~ loc + sea_otter_level, data = sq2)
anova(mod.all)
