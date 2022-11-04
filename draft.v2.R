
orig_data <- read.csv("C:/Users/arun/OneDrive - Iowa State University/OrganizedDocuments/TutejaLab/small_RNASeq/proteomics/orig_data.csv", row.names=NULL, stringsAsFactors=TRUE)
View(orig_data)
df <- orig_data
head(df)
df$ï..UniProtIds
df$log2fc <- log(df$Ratio, base = 2)
df$negLog10p = -log10(df$Qvalue)
df$diffexpressed[df$log2fc <= -0.584962501 & df$Qvalue <= 0.05] <- "up in Diff"
df$diffexpressed[df$log2fc >= 0.584962501 & df$Qvalue <= 0.05] <- "up in Undiff"
df
df$diffexpressed[df$diffexpressed == "NA"] <- "other genes"
df
df$diffexpressed[df$log2fc >= -0.584962501 & df$log2fc <= 0.584962501 ] <- "other genes"
df
df$diffexpressed[df$log2fc >= -0.584962501 & df$log2fc <= 0.584962501 || df$Qvalue > 0.05] <- "other genes"
df
df$diffexpressed[df$Qvalue > 0.05] <- "other genes"
df
num.clusters=2
paste('clus', 1:num.clusters,'.five.twenty$delabel[clus', 1:num.clusters,'.five.twenty$log2fc <= -0.584962501 & clus', 1:num.clusters,'.five.twenty$p_val_adj <= 0.05]', sep="")
lhs.p  <- paste('clus', 1:num.clusters,'.five.twenty$delabel[clus', 1:num.clusters,'.five.twenty$log2fc <= -0.584962501 & clus', 1:num.clusters,'.five.twenty$p_val_adj <= 0.05]', sep="")
rhs.p  <- paste('clus', 1:num.clusters,'.five.twenty$Gene[clus', 1:num.clusters,'.five.twenty$log2fc <= -0.584962501 & clus', 1:num.clusters,'.five.twenty$p_val_adj <= 0.05]', sep="")
commands.p <- paste(paste(lhs.p, rhs.p, sep="<-"), collapse=";")
commands.p
df$delabel <- "NA"
df$diffexpressed[df$diffexpressed != "other genes"] <- df$Genes
df
df <- orig_data
df$log2fc <- log(df$Ratio, base = 2)
df$negLog10p = -log10(df$Qvalue)
df$diffexpressed[df$log2fc <= -0.584962501 & df$Qvalue <= 0.05] <- "up in Diff"
df$diffexpressed[df$log2fc >= 0.584962501 & df$Qvalue <= 0.05] <- "up in Undiff"
df$diffexpressed[df$log2fc >= -0.584962501 & df$log2fc <= 0.584962501 || df$Qvalue > 0.05] <- "other genes"
df$diffexpressed[df$Qvalue > 0.05] <- "other genes"
df$delabel <- "NA"
df$delabel[df$diffexpressed != "other genes"] <- df$Genes
df
df$diffexpressed[df$log2fc >= -0.584962501 & df$log2fc <= 0.584962501] <- "other genes"
df
df$delabel[df$diffexpressed != "other genes"] <- df$Genes
df
df$Genes
df
ggplot(data=df, aes(x=log2fc, y=negLog10p)) +
geom_point(alpha = 0.5) +
theme_classic() +
scale_color_manual(name = "Expression", values=c("#4d4d4d", "#ca0020", "#0571b0")) +
ggtitle(paste("Diff vs. Undiff (proteomics)")) +
xlab("Log2 fold change") +
ylab("-log10 pvalue") +
theme(legend.text.align = 0)
ggplot(data=df, aes(x=log2fc, y=negLog10p)) +
geom_point(alpha = 0.5) +
theme_classic() +
scale_color_manual(name = "Expression", values=c("#c6007b", "#a0b600", "grey")) +
ggtitle(paste("Diff vs. Undiff (proteomics)")) +
xlab("Log2 fold change") +
ylab("-log10 pvalue") +
theme(legend.text.align = 0)
ggplot(data=df, aes(x=log2fc, y=negLog10p), col=diffexpressed) +
geom_point(alpha = 0.5) +
theme_classic() +
scale_color_manual(name = "Expression", values=c("#c6007b", "#a0b600", "grey")) +
ggtitle(paste("Diff vs. Undiff (proteomics)")) +
xlab("Log2 fold change") +
ylab("-log10 pvalue") +
theme(legend.text.align = 0)
ggplot(data=df, aes(x=log2fc, y=negLog10p, col=diffexpressed) +
geom_point(alpha = 0.5) +
theme_classic() +
scale_color_manual(name = "Expression", values=c("#c6007b", "#a0b600", "grey")) +
ggtitle(paste("Diff vs. Undiff (proteomics)")) +
xlab("Log2 fold change") +
ylab("-log10 pvalue") +
theme(legend.text.align = 0)
))
ggplot(data=df, aes(x=log2fc, y=negLog10p, col=diffexpressed)) +
geom_point(alpha = 0.5) +
theme_classic() +
scale_color_manual(name = "Expression", values=c("#c6007b", "#a0b600", "grey")) +
ggtitle(paste("Diff vs. Undiff (proteomics)")) +
xlab("Log2 fold change") +
ylab("-log10 pvalue") +
theme(legend.text.align = 0)
ggplot(data=df, aes(x=log2fc, y=negLog10p, col=diffexpressed)) +
geom_point(alpha = 0.5) +
theme_classic() +
scale_color_manual(name = "Expression", values=c("#c6007b", "grey", "#a0b600")) +
ggtitle(paste("Diff vs. Undiff (proteomics)")) +
xlab("Log2 fold change") +
ylab("-log10 pvalue") +
theme(legend.text.align = 0)
ggplot(data=df, aes(x=log2fc, y=negLog10p, col=diffexpressed)) +
geom_point(alpha = 0.5) +
theme_classic() +
scale_color_manual(name = "Expression", values=c("blue", "grey", "#a0b600")) +
ggtitle(paste("Diff vs. Undiff (proteomics)")) +
xlab("Log2 fold change") +
ylab("-log10 pvalue") +
theme(legend.text.align = 0)
ggplot(data=df, aes(x=log2fc, y=negLog10p, col=diffexpressed)) +
geom_point(alpha = 0.5) +
theme_classic() +
scale_color_manual(name = "Expression", values=c("blue", "#c6007b", "#a0b600")) +
ggtitle(paste("Diff vs. Undiff (proteomics)")) +
xlab("Log2 fold change") +
ylab("-log10 pvalue") +
theme(legend.text.align = 0)
ggplot(data=df, aes(x=log2fc, y=negLog10p, col=diffexpressed)) +
geom_point(alpha = 0.5) +
theme_classic() +
scale_color_manual(name = "Expression", values=c("grey", "#c6007b", "#a0b600")) +
ggtitle(paste("Diff vs. Undiff (proteomics)")) +
xlab("Log2 fold change") +
ylab("-log10 pvalue") +
theme(legend.text.align = 0)
ggsave("prot_volc.png", dpi=900, width = 8, height = 6)
getwd()
View(df)
df$delabel[df$diffexpressed != "other genes"]
df$diffexpressed[df$log2fc >= -0.584962501 & df$log2fc <= 0.584962501]
df$delabel[df$log2fc <= -0.584962501 & df$Qvalue <= 0.05] <- df$Genes
df$delabel[df$log2fc <= -0.584962501 & df$Qvalue <= 0.05]
df$log2fc <= -0.584962501 & df$Qvalue <= 0.05]
df$Genes[df$log2fc <= -0.584962501 & df$Qvalue <= 0.05]
df$newGenes <- str_replace_all(string=df$Genes,pattern="\\;.*$",replacement="")
df$newGenes
df$log2fc <= -0.584962501 & df$Qvalue <= 0.05
df$newGenes[df$log2fc <= -0.584962501 & df$Qvalue <= 0.05]
diff <- df$newGenes[df$log2fc <= -0.584962501 & df$Qvalue <= 0.05]
undiff <- df$newGenes[df$log2fc >= 0.584962501 & df$Qvalue <= 0.05]
diff
undiff
library(TissueEnrich)
plotTE <- function(inputGenes = gene.list,
myColor = "color") {
gs <-
GeneSet(geneIds = inputGenes,
organism = "Mus Musculus",
geneIdType = SymbolIdentifier())
output <- teEnrichment(inputGenes = gs, rnaSeqDataset = 3)
en.output <-
setNames(data.frame(assay(output[[1]]),
row.names = rowData(output[[1]])[, 1]),
colData(output[[1]])[, 1])
en.output$Tissue <- rownames(en.output)
logp <- -log10(0.05)
en.output <-
mutate(en.output,
significance = ifelse(Log10PValue > logp,
"colored", "nocolor"))
en.output$Sig <- "NA"
ggplot(en.output, aes(reorder(Tissue, Log10PValue),
Log10PValue,
fill = significance)) +
geom_bar(stat = 'identity') +
theme_clean() + ylab("- log10 adj. p-value") + xlab("") +
scale_fill_manual(values = c("colored" = myColor, "nocolor" = "gray")) +
scale_y_continuous(expand = expansion(mult = c(0, .1)),
breaks = scales::pretty_breaks()) +
coord_flip()
}
plotTE(diff)
plotTE(unique(diff)
)
setwd("C:/Users/arun/OneDrive - Iowa State University/OrganizedDocuments/github/mouse.trophoblast.smallRNAseq")
source(  "assets/theme_clean.R")
plotTE(unique(diff)
)
plotTE(unique(diff), myColor = "#c6007b")
plotTE(unique(undiff), myColor = "#a0b600")
plotTE(unique(undiff), myColor = "#a0b600")
ggsave("undiff_TE.png", dpi=900, width = 8, height = 10)
plotTE(unique(diff), myColor = "#c6007b")
ggsave("diff_TE.png", dpi=900, width = 8, height = 10)
ggsave("diff_TE.png", dpi=900, width = 6, height = 8)
ggsave("diff_TE.png", dpi=900, width = 6, height = 6)
plotTE(unique(undiff), myColor = "#a0b600")
ggsave("undiff_TE.png", dpi=900, width = 6, height = 6)
df$delabel <- df$newGenes[df$diffexpressed != "other genes"]
df$newGenes[df$diffexpressed != "other genes"]
df$delabel <- "NA"
df$newGenes[df$diffexpressed != "other genes"]
df$delabel <- df$newGenes[df$diffexpressed != "other genes"]
dat_tidy
df.diff <- dat_tidy %>%
rowwise() %>%
mutate(Diff = mean(c(
Diff_rep1,   Diff_rep2,  Diff_rep3,  Diff_rep4,  Diff_rep5
))) %>%
dplyr::select(proteinID, Diff) %>%
dplyr::filter(Diff > 0)  %>%
ungroup() %>%
mutate(quart = ntile(Diff, 4)) %>%
mutate(decile = ntile(Diff, 10))
df.diff
df.undiff <- dat_tidy %>%
rowwise() %>%
mutate(Undiff = mean(c(
Undiff_rep1, Undiff_rep2, Undiff_rep3, Undiff_rep4, Undiff_rep5
))) %>%
dplyr::select(proteinID, Undiff) %>%
dplyr::filter(Undiff > 0)  %>%
ungroup() %>%
mutate(quart = ntile(Undiff, 4)) %>%
mutate(decile = ntile(Undiff, 10))
df.undiff
filterCuts <- function(dataIn = df.undiff,
cutOff = 4,
type = decile) {
type <- enquo(type)
dataIn %>%
dplyr::filter(!!type == cutOff) %>%
dplyr::select(proteinID)
}
undiff.75pc <- filterCuts(df.undiff, 4, type = quart)
undiff.75pc
diff.75pc <- filterCuts(df.diff, 4, type = quart)
undiff.90pc <- filterCuts(df.undiff, 10, type = decile)
diff.90pc <- filterCuts(df.diff, 10, type = decile)
diff.75pc
undiff.90pc
diff.90pc
plotTE(diff.75pc$proteinID, "#c6007b")
diff.75pc$proteinID
plotTE(as.data.frame(diff.75pc$proteinID), "#c6007b")
as.list(diff.75pc$proteinID)
diff.75pc$proteinID
as.data.frame(diff.75pc)
unique(as.data.frame(diff.75pc))
diff.75pc <- as.data.frame(diff.75pc)
diff.75pc <- as.data.frame(diff.75pc)
diff.90pc <- as.data.frame(diff.90pc)
undiff.75pc <- as.data.frame(undiff.75pc)
undiff.90pc <- as.data.frame(undiff.90pc)
plotTE(diff.75pc$proteinID, "#c6007b")
diff.75pc
diff.75pc$proteinID
as.data.frame(diff.75pc$proteinID)
as.data.frame(diff.75pc$proteinID)[1]
Diff75PC <- as.data.frame(diff.75pc$proteinID)
Diff75PC[1]
Diff75PC$`diff.75pc$proteinID`
plotTE(as.character(diff.75pc$proteinID), "#c6007b")
ggsave("diff_TE_75pc.png", dpi=900, width = 6, height = 6)
plotTE(as.character(diff.90pc$proteinID), "#c6007b")
ggsave("diff_TE_90pc.png", dpi=900, width = 6, height = 6)
plotTE(as.character(undiff.75pc$proteinID), "#a0b600")
ggsave("undiff_TE_75pc.png", dpi=900, width = 6, height = 6)
plotTE(as.character(undiff.90pc$proteinID), "#a0b600")
ggsave("undiff_TE_90pc.png", dpi=900, width = 6, height = 6)

