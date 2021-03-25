## ----message=FALSE, warning=FALSE, include=FALSE------------------------------
library(knitr)
options(knitr.kable.NA = '')
knitr::opts_chunk$set(comment=">")
options(digits=2)


pkgs <- c("effectsize", "dplyr", "bayestestR", "see", "ggplot2", "parameters", "lme4", "KernSmooth")
if (!all(sapply(pkgs, requireNamespace, quietly = TRUE))) {
  knitr::opts_chunk$set(eval = FALSE)
}

## ---- fig.width=7, fig.height=4.5, results='hide', fig.align='center', comment=NA, message=FALSE, warning=FALSE----
library(dplyr)

# Download the 'emotion' dataset
load(url("https://raw.github.com/neuropsychology/psycho.R/master/data/emotion.rda"))  
  
# Discard neutral pictures (keep only negative)
df <- emotion %>% 
  filter(Emotion_Condition == "Negative")  

# Summary
df %>% 
  group_by(Participant_ID) %>% 
  summarise(n_Trials = n(),
            Valence_Mean = mean(Subjective_Valence, na.rm=TRUE),
            Valence_SD = sd(Subjective_Valence, na.rm=TRUE),
            Autobiographical_Link_Mean = mean(Autobiographical_Link, na.rm=TRUE),
            Autobiographical_Link_SD = sd(Autobiographical_Link, na.rm=TRUE))

## ---- fig.width=7, fig.height=4.5, results='markup', fig.align='center', comment=NA, message=FALSE, warning=FALSE----
library(effectsize)

Z_VariableWise <- df %>% 
  standardize()

Z_ParticipantWise <- df %>% 
  group_by(Participant_ID) %>% 
  standardize() 

Z_Full <- df %>% 
  group_by(Participant_ID) %>% 
  standardize() %>% 
  ungroup() %>% 
  standardize() 

## ---- fig.width=7, fig.height=4.5, results='markup', fig.align='center', comment=NA, message=FALSE, warning=FALSE----
# Create a convenient function to print
print_summary <- function(data){
  paste0(
    paste0(deparse(substitute(data)), ": "), 
    paste(round(mean(data[["Subjective_Valence"]]), 3), 
          "+-", 
          round(sd(data[["Subjective_Valence"]]), 3)),
    paste0(" [", 
           round(min(data[["Subjective_Valence"]]), 3),
           ",", 
           round(max(data[["Subjective_Valence"]]), 3),
           "]")
    )
}

# Check the results
print_summary(Z_VariableWise)
print_summary(Z_ParticipantWise)
print_summary(Z_Full)

## ---- fig.width=7, fig.height=4.5, results='markup', fig.align='center', comment=NA, message=FALSE, warning=FALSE----
library(bayestestR)
library(see)

data.frame(VarWise = Z_VariableWise$Subjective_Valence,
           ParWise = Z_ParticipantWise$Subjective_Valence,
           Full = Z_Full$Subjective_Valence) %>% 
  estimate_density(method="kernSmooth") %>% 
  plot() +
  see::theme_modern()

## ---- fig.width=7, fig.height=4.5, results='markup', fig.align='center', comment=NA, message=FALSE, warning=FALSE----
# Create convenient function
print_participants <- function(data){
  data %>% 
    group_by(Participant_ID) %>% 
    summarise(Mean = mean(Subjective_Valence), 
              SD = sd(Subjective_Valence)) %>% 
    mutate_if(is.numeric, round, 2) %>% 
    head(5) 
    
}

# Check the results
print_participants(Z_VariableWise)
print_participants(Z_ParticipantWise)
print_participants(Z_Full)

## ---- fig.width=7, fig.height=4.5, results='markup', fig.align='center', comment=NA, message=FALSE, warning=FALSE----
library(ggplot2)

cor.test(Z_VariableWise$Subjective_Valence, Z_ParticipantWise$Subjective_Valence)


data.frame(Original = df$Subjective_Valence,
           VariableWise = Z_VariableWise$Subjective_Valence,
           ParticipantWise = Z_ParticipantWise$Subjective_Valence) %>% 
  ggplot(aes(x=VariableWise, y=ParticipantWise, colour=Original)) +
  geom_point() +
  geom_smooth(method="lm") +
  see::theme_modern()

## ---- fig.width=7, fig.height=4.5, results='markup', fig.align='center', comment=NA, message=FALSE, warning=FALSE----
library(lme4)
library(parameters)

# Convenient function
print_model <- function(data){
  type_name <- deparse(substitute(data)) 
  lmer(Subjective_Valence ~ Autobiographical_Link + (1|Participant_ID), data=data) %>% 
    parameters() %>% 
    filter(Parameter == "Autobiographical_Link") %>% 
    mutate(Type = type_name,
           Coefficient = round(Coefficient, 3),
           p = round(p, 3)) %>% 
    select(Type, Coefficient, p)
}

# Run the model on all datasets
rbind(print_model(df), 
      print_model(Z_VariableWise),
      print_model(Z_ParticipantWise),
      print_model(Z_Full))

