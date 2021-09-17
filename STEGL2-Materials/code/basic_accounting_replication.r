#==============================================================================
# Intro:  This is a replication in R of the main figures and results in the
# STEG Lecture 2 by Julieta Caunedo. Her main code in Stata can be found in her
# GitHub Repostory.
#------------------------------------------------------------------------------
# Rafael Serrano-Quintero
# 21-02-2021
#==============================================================================

# Remove everything
rm(list = ls())

# Load necessary libraries
library(readstata13)
library(tidyverse)
library(dplyr)
library(ggplot2)

# Set working directory
user <- 3
if (user == 1) {
    setwd("D:/TEACHING/steg-macro-course/STEGL2-Materials")
} else if (user == 2) {
    setwd(
    "C:/Users/rafse/Documents/TEACHING/steg-macro-course/STEGL2-Materials")
} else if (user == 3) {
    setwd(
    "/home/rafserqui/Documents/teaching/steg-macro-course/STEGL2-Materials")
}

# Import the data
pwtfile <- "./data/pwt_10_dload_02032021.dta"
natfile <- "./data/natural_resources.dta"

pwt <- read.dta13(pwtfile)
nat <- read.dta13(natfile)

# Merge the data
df <- merge(pwt, nat, by = c("year", "countrycode"),
            all.x = TRUE)

# Notice the all.x argument to keep unmatched obs in pwt

#=========================
# Relevant variables (IMPORTANT: adjustment for natural
# resources on GDP. PWT capital corresponds to reproducible capital)

# real PPP (in mil 2017US$)

# Output: rgdpo
# Human Capital (per worker): hc
# Labor Inputs/Workers: emp
# Average hours avh
# Capital stock: cn
# Capital denominated at current PPP $
# so capital-output ratio computed using cgdpo
# Can use Capital services instead of stocks is desired.

#=========================
# parameters of interest
#=========================
# capital share H&Jones
alpha <- 0.33

# relevant year of study
year_base <- 2017

# adjustment GDP (assuming one uses the same deflator for GDP and nat_res)
df <- df %>%
    mutate(rgdpo = (1 - natural_res / 100) * rgdpo,
           cgdpo = (1 - natural_res / 100) * cgdpo)
#=========================

# Compute main variables
df <- df %>%
    mutate(output_per_worker    = log(rgdpo /  (emp * avh)),
        output_per_worker_PPPc  = log(cgdpo / (emp)),
        output_per_worker_XRc   = log((cgdpo * pl_gdpo) / emp),
        capital_output_ratio    = log(cn / cgdpo) * (alpha / (1 - alpha)),
        human_capital           = log(hc),
        capital_output_ratio_sh = ((1 - labsh) / labsh) * log(cn / cgdpo))

# Compute TFP residuals
df <- df %>%
    mutate(tfp_resid =
                output_per_worker - capital_output_ratio - human_capital,
            tfp_resid_labsh =
                output_per_worker - capital_output_ratio_sh - human_capital)

# Compute average capital by year
df <- df %>%
          group_by(year) %>%
                          mutate(us_cap_level = mean(cn, na.rm = TRUE))

# Compute capital services to output ratio
df <- df %>%
    mutate(capserv_output_ratio =
        log((us_cap_level * ck) / cgdpo) * (alpha / (1 - alpha)))

# Normalized levels wrt USA, (USA = 1)
varstomani <- c("output_per_worker", "capital_output_ratio",
    "capital_output_ratio_sh", "capserv_output_ratio",
    "human_capital", "tfp_resid", "tfp_resid_labsh")

for (ii in varstomani) {
    # Turn into exponential
    df[, ii] <- exp(df[, ii])

    # Compute value only if it is USA
    df <- df %>% mutate(
        tmp_out =
            ifelse(countrycode == "USA",
                (!!as.symbol(ii)), NA))

    # Compute mean for US by year
    df <- df %>%
        group_by(year) %>%
        mutate(output_norm = mean(tmp_out, na.rm = TRUE))

    # Generate normalize variable
    vname <- paste0("norm_", ii, sep = "")
    df <- df %>%
        mutate(across((!!as.symbol(ii)),
            function(x) x / output_norm,
            .names = vname))

    # Drop auxiliary variables
    df <- select(df, -c("tmp_out", "output_norm"))
}

#=========================
# Balassa-Samuelson effect, L=employment in the slides
#=========================
dfbase <- df %>%
                filter(year == year_base) %>%
                    filter(countrycode == "ARG" |
                        countrycode == "CHN" |
                        countrycode == "FIN")

dfbase <- dfbase %>% mutate(ccode = as.factor(countrycode))
dfbase <- pivot_longer(dfbase, cols =
            c("output_per_worker_PPPc", "output_per_worker_XRc"),
            names_to = "variable",
            values_to = "value")

plot <- 0
if (plot == 1) {
    # Plot
    pbalassa <- ggplot(dfbase, aes(x = ccode, y = value,
                        fill = variable)) +
        geom_bar(stat = "identity", position = "dodge") +
        scale_fill_manual(" ",
                        labels = c("PPP", "Exchange Rates"),
                        values = c("#999999", "#E69F00")) +
        labs(x = "Country",
            y = ~ atop("Log Output per worker (2017),",
                    "current 2017 US$ (USA=1)")) +
        coord_cartesian(ylim = c(0, 15)) +
        theme_light()

    pbalassa

    pdf("./figures/balassa-samuelson.pdf",
    height = 6, width = 10)
    print(pbalassa)
    dev.off()
}

#=========================
# Contributions
#=========================
df <- df %>%
        mutate(overall_diff = 1 / norm_output_per_worker,
        contrib_capital = 1 / norm_capital_output_ratio,
        contrib_capital_sh = 1 / norm_capital_output_ratio_sh,
        contrib_capital_serv = 1 / norm_capserv_output_ratio,
        contrib_labor = 1 / norm_human_capital,
        contrib_TFP = 1 / norm_tfp_resid)

df <- df %>%
        mutate(share_due_to_TFP =
            ifelse(countrycode != "USA",
            contrib_TFP / (contrib_TFP + contrib_labor * contrib_capital), NA))

# Sort by output per worker
df <- df %>% arrange(output_per_worker)

#=========================
# Jones table
#=========================
dftab <- df %>% filter(year == year_base)

vlist <- c("norm_output_per_worker",
        "norm_capital_output_ratio",
        "norm_human_capital",
        "norm_tfp_resid")

for (ii in vlist) {
    vname <- paste0("mean_", ii, sep = "")
    vname2 <- paste0("mean_contrib_", ii, sep = "")

    # Compute average contrib for the full sample
    dftab <- dftab %>%
                mutate(across((!!as.symbol(ii)),
                    function(x) mean(x, na.rm = TRUE),
                    .names = vname))

    dftab <- dftab %>%
                mutate(across(!!as.symbol(vname),
                    function(x) 1 / x, .names = vname2))
}


clist <- c("USA", "HKG", "SGP",
        "FRA", "DEU", "GBR", "JPN",
        "KOR", "ARG", "MEX", "BWA",
        "ZAF", "BRA", "THA", "CHN",
        "IDN", "IND", "KEN", "MWI")
dftab <- dftab %>%
            filter(countrycode %in% clist)

vlist <- c("countrycode", "country", "year",
        "norm_output_per_worker", "norm_capital_output_ratio",
        "norm_human_capital", "norm_tfp_resid", "share_due_to_TFP",
        "mean_contrib_norm_output_per_worker",
        "mean_contrib_norm_capital_output_ratio",
        "mean_contrib_norm_human_capital",
        "mean_contrib_norm_tfp_resid",
        "mean_norm_output_per_worker", "mean_norm_capital_output_ratio",
        "mean_norm_human_capital", "mean_norm_tfp_resid")

dftab <- select(dftab, all_of(vlist))

#=========================
# Plot correlations
#=========================
# Drop outliers
dfcorr <- df %>%
            filter(!(countrycode %in% c("MAC", "IRL"))) %>%
                    filter(year == year_base)

pcorr <- dfcorr %>% ggplot(aes(x = log(norm_output_per_worker),
                y = share_due_to_TFP)) +
        geom_point(shape = 21, colour = "black",
                    fill = "#E69F00", size = 3) +
        ggrepel::geom_text_repel(aes(label = countrycode),
                                    size = 3) +
        labs(x = ~ atop("Log Output per worker (2017),",
                    "PPP Current 2017 US$ (USA=1)"),
            y = "Share due to TFP") +
        theme_light()

pcorr


pdf("./figures/corr-tfp-output.pdf",
height = 6, width = 10)
print(pcorr)
dev.off()

#=========================
# Plot median contribution of TFP over time
#=========================
df <- df %>%
    mutate(denom = ((contrib_labor + contrib_capital)),
        share_due_to_hcap = (1 - share_due_to_TFP) * (contrib_labor / denom),
        share_due_to_cap = (1 - share_due_to_TFP) * (contrib_capital / denom))

dfct <- df %>%
        filter(year >= 1970) %>%
            group_by(year) %>%
            mutate(median_contrib_tfp = median(share_due_to_TFP, na.rm = TRUE),
                median_contrib_cap = median(share_due_to_cap, na.rm = TRUE),
                median_contrib_hcap = median(share_due_to_hcap, na.rm = TRUE))

dfct <- pivot_longer(dfct, cols =
            c("median_contrib_tfp", "median_contrib_cap",
            "median_contrib_hcap"),
            names_to = "variable",
            values_to = "value")
            
pcon <- dfct %>% ggplot(aes(x = year,
                y = value, group = variable)) +
        geom_line(aes(color = variable)) +
        geom_point(aes(color = variable)) +
        scale_color_manual(" ",
                    labels = c("Physical Capital", "Human Capital", "TFP"),
                    values = c("#999999", "#E69F00", "#258794")) +
        labs(x = "Year",
            y = "Median Contribution of Factor") +
        theme_light()

pcon

pdf("./figures/median-tfp-time.pdf",
height = 6, width = 10)
print(pcon)
dev.off()