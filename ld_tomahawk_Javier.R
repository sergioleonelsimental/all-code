#cd  tomahawk
#git clone --recursive https://github.com/mklarqvist/tomahawk
#sse4.2 
#linuxplink1.9 --vcf Mexiko_MAC_FIN.recode.vcf --make-bed --allow-extra-chr --keep-allele-order --out binary_file
# linux plink1.9 --bfile binary_file_serch --allow-extra-chr --keep-allele-order --recode vcf --out MAF_CONTIGS_SERCH

library("vcfR")
vcf<-read.vcfR("MAF_CONTIGS_SERCH.vcf")
write.csv(vcf@meta,file="contigs.csv")
# linux vcftools --vcf Mexiko_MAC_FIN.recode.vcf --recode-bcf --recode-INFO-all --contigs CONTINGs_ld.txt --out MAC1401

##para calcular LD con tomahawk
#./tomahawk import -i MAC1401.recode.bcf -o snp_MAC1401
#./tomahawk calc -i snp_MAC1401.twk -o snp -r 0.1 -P 0.0001 ##-C 1 ##-t 1
#./tomahawk view -i snp.two > snphoy.ld

# load libraries
library(dplyr)
library(ggplot2)

# define plotting functions
#' @title plotPairwiseLD
#' @description Plots R2 heatmap across the chromosome (like Haploview)
#' @param dfr A data.frame with minimum CHROM_A, POS_A, CHROM_B, POS_B and R2.
#' An output from tomahawk works.
#' @param chr A chromosome name.
#' @param xlim A two number vector specifying min and max x-axis limits. Any one or both can be defaulted by specifying NA.
#' @param ylim A two number vector specifying min and max y-axis limits. Any one or both can be defaulted by specifying NA.
#' @param minr2 A value between 0 and 1. All SNPs with R2 value below this
#' value is excluded from plot.
#'
plotPairwiseLD <- function(dfr, chr, xlim = c(NA, NA), ylim = c(NA, NA), minr2) {
  if (missing(dfr)) stop("Input data.frame 'dfr' missing.")
  
  if (!missing(chr)) {
    ld <- filter(ld, ridA == get("chr") & ridB == get("chr"))
  }
  ld <- filter(ld, posA < posB)
  
  if (!missing(minr2)) {
    ld <- filter(ld, R2 > get("minr2"))
  }
  
  ld <- ld %>% arrange(R2)
  
  ld$x <- ld$posA + ((ld$posB - ld$posA) / 2)
  ld$y <- ld$posB - ld$posA
  ld$r2c <- cut(ld$R2, breaks = seq(0, 1, 0.2), labels = c(
    "0-0 - 0.2", "0.2 - 0.4",
    "0.4 - 0.6", "0.6 - 0.8",
    "0.8 - 1.0"
  ))
  ld$r2c <- factor(ld$r2c, levels = rev(c(
    "0-0 - 0.2", "0.2 - 0.4",
    "0.4 - 0.6", "0.6 - 0.8",
    "0.8 - 1.0"
  )))
  
  ggplot(ld, aes(x = x, y = y, col = r2c)) +
    geom_point(shape = 20, size = 0.1, alpha = 0.8) +
    scale_color_manual(values = c("#ca0020", "#f4a582", "#d1e5f0", "#67a9cf", "#2166ac")) +
    scale_x_continuous(limits = xlim) +
    scale_y_continuous(limits = ylim) +
    guides(colour = guide_legend(title = "R2", override.aes = list(shape = 20, size = 8))) +
    labs(x = "Chromosome (Bases)", y = "") +
    theme_bw(base_size = 14) +
    theme(
      panel.border = element_blank(),
      axis.ticks = element_blank()
    ) %>%
    return()
}

#' @title plotDecayLD
#' @description Plots R2 heatmap across the chromosome (like Haploview)
#' @param dfr A data.frame with minimum CHROM_A, POS_A, CHROM_B, POS_B and R2.
#' An output from tomahawk works.
#' @param chr A chromosome name.
#' @param xlim A two number vector specifying min and max x-axis limits. Any one or both can be defaulted by specifying NA.
#' @param ylim A two number vector specifying min and max y-axis limits.  Any one or both can be defaulted by specifying NA.
#' @param avgwin An integer specifying window size. Mean R2 is computed within windows.
#' @param minr2 A value between 0 and 1. All SNPs with R2 value below this
#' value is excluded from plot.
#'
plotDecayLD <- function(dfr, chr, xlim = c(NA, NA), ylim = c(NA, NA), avgwin = 0, minr2) {
  if (missing(dfr)) stop("Input data.frame 'dfr' missing.")
  
  if (!missing(chr)) {
    ld <- filter(ld, ridA == get("chr") & ridB == get("chr"))
  }
  ld <- filter(ld, posA < posB)
  
  if (!missing(minr2)) {
    ld <- filter(ld, R2 > get("minr2"))
  }
  
  ld <- ld %>% arrange(R2)
  
  ld$dist <- ld$posB - ld$posA
  
  if (avgwin > 0) {
    ld$distc <- cut(ld$dist, breaks = seq(from = min(ld$dist), to = max(ld$dist), by = avgwin))
    ld <- ld %>%
      group_by(distc) %>%
      summarise(dist = mean(dist), R2 = mean(R2))
  }
  
  ggplot(ld, aes(x = dist, y = R2)) +
    geom_point(shape = 20, size = 0.15, alpha = 0.7) +
    geom_smooth() +
    scale_x_continuous(limits = xlim) +
    scale_y_continuous(limits = ylim) +
    labs(x = "Distance (Bases)", y = expression(LD ~ (r^{
      2
    }))) +
    theme_bw(base_size = 14) +
    theme(
      panel.border = element_blank(),
      axis.ticks = element_blank()
    ) %>%
    return()
}


ld <- read.delim("snp.ld", sep="\t", comment.char="#")
head(ld)

#Plot pairwise LD plot.
#Pairwise LD plot across a chromosome.
plotPairwiseLD(ld)

#Pairwise LD plot with a min R2 limit of 0.2.
COPYplotPairwiseLD(ld, minr2=0.2)

#Pairwise LD plot with a min R2 limit of 0.2 and y-axis max limit set to 10^7.
COPYplotPairwiseLD(ld, minr2=0.2, ylim=c(0, 10^7))

#LD decay plot with a min R2 limit of 0.25.
COPYplotDecayLD(ld, minr2=0.25)

#LD decay plot with a min R2 limit of 0.25 and R2 values averaged within 10 Kb windows.
COPYplotDecayLD(ld, minr2=0.25, avgwin=100000)