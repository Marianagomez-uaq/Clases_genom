# phyloseq

install.packages ("kableExtra") # Solo se usa en .rmd, en R normal las tablas generadas salen feas
BiocManager::install('DESeq2')


library (phyloseq)
library (vegan)
library (ggplot2)
library (kable)
library (RColorBrewer)
library (ggpubr)
library (scales)
library (knitr)


data("GlobalPatterns")
ps <- GlobalPatterns
ps
#> phyloseq-class experiment-level object
#> otu_table()   OTU Table:         [ 19216 taxa and 26 samples ]
#> sample_data() Sample Data:       [ 26 samples by 7 sample variables ]
#> tax_table()   Taxonomy Table:    [ 19216 taxa by 7 taxonomic ranks ]
#> phy_tree()    Phylogenetic Tree: [ 19216 tips and 19215 internal nodes ]

cat("=== Resumen del objeto phyloseq ===\n")
#> === Resumen del objeto phyloseq ===
cat("Numero de taxa (OTUs):", ntaxa(ps), "\n")
#> Numero de taxa (OTUs): 19216
cat("Numero de muestras:  ", nsamples(ps), "\n")
#> Numero de muestras:   26
cat("Rangos taxonomicos:  ", paste(rank_names(ps), collapse = " > "), "\n")
#> Rangos taxonomicos:   Kingdom > Phylum > Class > Order > Family > Genus > Species
cat("Variables de metadatos:", paste(sample_variables(ps), collapse = ", "), "\n")
#> Variables de metadatos: X.SampleID, Primer, Final_Barcode, Barcode_truncated_plus_T, Barcode_full_length, SampleType, Description
 
# R base: indexar columnas sin dplyr
kable(
  head(data.frame(sample_data(ps))[, c("SampleType", "Description")], 10),
  caption = "Primeras 10 filas de los metadatos de GlobalPatterns"
)
kable(
  data.frame(tax_table(ps)[1:6, ]),
  caption = "Primeros 6 OTUs y su clasificacion taxonomica"
)
kable(
  data.frame(otu_table(ps)[1:5, 1:5]),
  caption = "Subconjunto de la tabla de abundancias (5 OTUs x 5 muestras)"
)


sample_sums_vec <- sample_sums(ps)
summary(sample_sums_vec)
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>   58688  567103 1106849 1085257 1527330 2357181

df_reads <- data.frame(
  Sample     = names(sample_sums_vec),
  TotalReads = sample_sums_vec,
  SampleType = sample_data(ps)$SampleType
)

ggplot(df_reads, aes(x = reorder(Sample, TotalReads), # No es necesario hacer esta gráfica
                     y = TotalReads, fill = SampleType)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_y_continuous() + # Aquí había algo de labels pero me salía error entonces lo quité
  scale_fill_brewer(palette = "Set3") +
  labs(title = "Total de Lecturas por Muestra",
       x = "Muestra", y = "Numero de lecturas", fill = "Tipo") +
  theme_bw(base_size = 11) +
  theme(legend.position = "bottom")


# Paso 1: OTUs con menos de 10 lecturas totales
ps_filtered <- prune_taxa(taxa_sums(ps) > 10, ps)

# Paso 2: OTUs presentes en al menos el 5% de las muestras
prevalence_threshold <- 0.05 * nsamples(ps_filtered)
taxa_prevalence      <- rowSums(otu_table(ps_filtered) > 0)
ps_filtered <- prune_taxa(taxa_prevalence >= prevalence_threshold, ps_filtered)

cat("OTUs originales:", ntaxa(ps), "\n")
#> OTUs originales: 19216
cat("OTUs tras filtrado:", ntaxa(ps_filtered), "\n")
#> OTUs tras filtrado: 11829
cat("OTUs removidos:", ntaxa(ps) - ntaxa(ps_filtered), "\n")
#> OTUs removidos: 7387




min_depth <- min(sample_sums(ps_filtered))
cat("Profundidad minima:", min_depth, "\n")
#> Profundidad minima: 58109

ps_rare <- rarefy_even_depth(
  ps_filtered,
  sample.size = min_depth,
  rngseed     = 123,
  replace     = FALSE,
  trimOTUs    = TRUE,
  verbose     = FALSE
)

cat("Muestras tras rarefaccion:", nsamples(ps_rare), "\n")
#> Muestras tras rarefaccion: 26
cat("OTUs tras rarefaccion:", ntaxa(ps_rare), "\n")
#> OTUs tras rarefaccion: 10911




ps_rel <- transform_sample_counts(ps_filtered, function(x) x / sum(x))
head(round(sample_sums(ps_rel), 4))
#>     CL3     CC1     SV1 M31Fcsw M11Fcsw M31Plmr 
#>       1       1       1       1       1       1




alpha_div <- estimate_richness(
  ps_rare,
  measures = c("Observed", "Chao1", "Shannon", "Simpson", "InvSimpson")
)
alpha_div$SampleType <- sample_data(ps_rare)$SampleType
alpha_div$Sample     <- rownames(alpha_div)

# R base: seleccionar columnas sin dplyr
kable(
  head(alpha_div[, c("Sample","SampleType","Observed","Chao1","Shannon","Simpson")], 8),
  digits  = 3,
  caption = "Indices de diversidad alfa por muestra"
)




n_types <- length(unique(sample_data(ps_rare)$SampleType))
colores  <- brewer.pal(min(n_types, 9), "Set1")

plot_richness(ps_rare, x = "SampleType",
              measures = c("Shannon", "InvSimpson"),
              color = "SampleType") +
  geom_boxplot(alpha = 0.5) +
  geom_jitter(width = 0.2, size = 2) +
  scale_color_manual(values = colores) +
  labs(title = "Diversidad Alfa por Tipo de Muestra",
       x = NULL, y = "Indice de Diversidad", color = "Tipo") +
  theme_bw(base_size = 11) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")





plot_richness(ps_rare, x = "SampleType",
              measures = c("Observed", "Chao1"),
              color = "SampleType") +
  geom_boxplot(alpha = 0.3) +
  geom_point(size = 2) +
  scale_color_manual(values = colores) +
  labs(title = "Riqueza de OTUs por Tipo de Muestra",
       x = NULL, y = "Numero de OTUs", color = "Tipo") +
  theme_bw(base_size = 11) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")







dist_bray     <- phyloseq::distance(ps_rel,  method = "bray")
dist_wunifrac <- phyloseq::distance(ps_rare, method = "wunifrac")
dist_unifrac  <- phyloseq::distance(ps_rare, method = "unifrac")

cat("Dimensiones Bray-Curtis:", attr(dist_bray, "Size"), "x",
    attr(dist_bray, "Size"), "\n")
#> Dimensiones Bray-Curtis: 26 x 26





ord_bray <- ordinate(ps_rel, method = "PCoA", distance = dist_bray)
eig      <- ord_bray$values$Eigenvalues
var_exp  <- round(eig / sum(eig) * 100, 1)
cat("Eje 1:", var_exp[1], "% | Eje 2:", var_exp[2], "%\n")
#> Eje 1: 14.7 % | Eje 2: 12.2 %

plot_ordination(ps_rel, ord_bray,
                color = "SampleType", shape = "SampleType") +
  geom_point(size = 4, alpha = 0.9) +
  stat_ellipse(type = "norm", linetype = 2) +
  scale_color_manual(values = colores) +
  labs(title = "PCoA - Distancia de Bray-Curtis",
       x = paste0("PCoA1 [", var_exp[1], "%]"),
       y = paste0("PCoA2 [", var_exp[2], "%]"),
       color = "Tipo", shape = "Tipo") +
  theme_bw(base_size = 12)






ord_wunifrac <- ordinate(ps_rare, method = "PCoA", distance = dist_wunifrac)
eig2         <- ord_wunifrac$values$Eigenvalues
var_exp2     <- round(eig2 / sum(eig2) * 100, 1)

plot_ordination(ps_rare, ord_wunifrac,
                color = "SampleType", shape = "SampleType") +
  geom_point(size = 4, alpha = 0.9) +
  stat_ellipse(type = "norm", linetype = 2) +
  scale_color_manual(values = colores) +
  labs(title = "PCoA - UniFrac Ponderada",
       x = paste0("PCoA1 [", var_exp2[1], "%]"),
       y = paste0("PCoA2 [", var_exp2[2], "%]"),
       color = "Tipo", shape = "Tipo") +
  theme_bw(base_size = 12)






ord_nmds <- ordinate(ps_rel, method = "NMDS", distance = "bray")
#> Run 0 stress 0.1669567 
#> Run 1 stress 0.1669565 
#> ... New best solution
#> ... Procrustes: rmse 0.0001824874  max resid 0.0006772835 
#> ... Similar to previous best
#> Run 2 stress 0.1769018 
#> Run 3 stress 0.1979557 
#> Run 4 stress 0.1769018 
#> Run 5 stress 0.2056517 
#> Run 6 stress 0.1664157 
#> ... New best solution
#> ... Procrustes: rmse 0.01245612  max resid 0.05321689 
#> Run 7 stress 0.181594 
#> Run 8 stress 0.1669567 
#> Run 9 stress 0.1805407 
#> Run 10 stress 0.1990443 
#> Run 11 stress 0.1709367 
#> Run 12 stress 0.1709367 
#> Run 13 stress 0.1769018 
#> Run 14 stress 0.1769018 
#> Run 15 stress 0.1769018 
#> Run 16 stress 0.1709367 
#> Run 17 stress 0.1769018 
#> Run 18 stress 0.1769018 
#> Run 19 stress 0.1805118 
#> Run 20 stress 0.1709367 
#> *** Best solution was not repeated -- monoMDS stopping criteria:
#>     15: stress ratio > sratmax
#>      5: scale factor of the gradient < sfgrmin
cat("NMDS Stress:", round(ord_nmds$stress, 4), "\n")
#> NMDS Stress: 0.1664
cat("(Stress < 0.20 indica buena representacion 2D)\n")
#> (Stress < 0.20 indica buena representacion 2D)

plot_ordination(ps_rel, ord_nmds,
                color = "SampleType", shape = "SampleType") +
  geom_point(size = 4, alpha = 0.9) +
  annotate("text", x = Inf, y = -Inf,
           label = paste("Stress =", round(ord_nmds$stress, 3)),
           hjust = 1.1, vjust = -0.5, size = 3.5) +
  scale_color_manual(values = colores) +
  labs(title = "NMDS - Bray-Curtis",
       color = "Tipo", shape = "Tipo") +
  theme_bw(base_size = 12)









ps_phylum   <- tax_glom(ps_rel, taxrank = "Phylum", NArm = TRUE)
top10_phyla <- names(sort(taxa_sums(ps_phylum), decreasing = TRUE))[1:10]
ps_top10    <- prune_taxa(top10_phyla, ps_phylum)

# R base: ordenar sin dplyr::arrange
df_bar        <- psmelt(ps_top10)
df_bar        <- df_bar[order(df_bar$SampleType, df_bar$Phylum), ]
colores_phyla <- brewer.pal(10, "Paired")

ggplot(df_bar, aes(x = Sample, y = Abundance, fill = Phylum)) +
  geom_bar(stat = "identity", position = "stack", width = 0.9) +
  facet_wrap(~ SampleType, scales = "free_x", nrow = 2) +
  scale_fill_manual(values = colores_phyla) +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "Composicion Taxonomica — Phylum",
       x = NULL, y = "Abundancia Relativa", fill = "Phylum") +
  theme_bw(base_size = 9) +
  theme(axis.text.x     = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 7),
        legend.position = "bottom",
        legend.key.size = unit(0.4, "cm"))







top30    <- names(sort(taxa_sums(ps_rel), decreasing = TRUE))[1:30]
ps_top30 <- prune_taxa(top30, ps_rel)

plot_heatmap(ps_top30, method = "NMDS", distance = "bray",
             taxa.label = "Genus", sample.label = "SampleType",
             low = "#f7fbff", high = "#08306b", na.value = "white") +
  labs(title = "Heatmap - Top 30 OTUs") +
  theme_bw(base_size = 9) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))









top50   <- names(sort(taxa_sums(ps_rare), decreasing = TRUE))[1:50]
ps_tree <- prune_taxa(top50, ps_rare)

plot_tree(ps_tree,
          color = "SampleType", shape = "Phylum",
          size = "abundance", label.tips = "Genus",
          plot.margin = 0.5) +
  labs(title = "Arbol Filogenetico - Top 50 OTUs",
       color = "Tipo", shape = "Phylum", size = "Abundancia") +
  theme(legend.position = "right")







ig <- make_network(ps_rel, type = "samples",
                   distance = "bray", max.dist = 0.8)

plot_network(ig, ps_rel,
             color = "SampleType", shape = "SampleType",
             label = NULL) +
  scale_color_manual(values = colores) +
  labs(title = "Red de Co-ocurrencia de Muestras",
       color = "Tipo", shape = "Tipo") +
  theme_bw(base_size = 11)








meta     <- data.frame(sample_data(ps_rel))
mat_bray <- as.matrix(dist_bray)
mat_bray <- mat_bray[rownames(meta), rownames(meta)]

set.seed(123) # Esto hace que los números generados al azar sean siempre los mismos
permanova_result <- vegan::adonis2(
  mat_bray ~ SampleType,
  data = meta, permutations = 999, method = "bray"
)
print(permanova_result)
#> Permutation test for adonis under reduced model
#> Permutation: free
#> Number of permutations: 999
#> 
#> vegan::adonis2(formula = mat_bray ~ SampleType, data = meta, permutations = 999, method = "bray")
#>          Df SumOfSqs      R2      F Pr(>F)    
#> Model     8   8.2499 0.73147 5.7884  0.001 ***
#> Residual 17   3.0286 0.26853                  
#> Total    25  11.2785 1.00000                  
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

cat("\nR2 SampleType:", round(permanova_result$R2[1] * 100, 1), "%\n")
#> 
#> R2 SampleType: 73.1 %
if (permanova_result$`Pr(>F)`[1] < 0.05) {
  cat("Diferencia significativa en composicion entre tipos de muestras\n")
} else {
  cat("No hay diferencia significativa\n")
}
#> Diferencia significativa en composicion entre tipos de muestras








set.seed(123)
anosim_result <- vegan::anosim(
  mat_bray, grouping = meta$SampleType, permutations = 999
)
summary(anosim_result)
#> 
#> Call:
#> vegan::anosim(x = mat_bray, grouping = meta$SampleType, permutations = 999) 
#> Dissimilarity: user supplied square matrix 
#> 
#> ANOSIM statistic R: 0.9877 
#>       Significance: 0.001 
#> 
#> Permutation: free
#> Number of permutations: 999
#> 
#> Upper quantiles of permutations (null model):
#>   90%   95% 97.5%   99% 
#> 0.145 0.188 0.233 0.273 
#> 
#> Dissimilarity ranks between and within classes:
#>                    0%    25%   50%    75% 100%   N
#> Between            18 101.50 176.0 250.50  325 299
#> Feces              10  15.25  20.5  27.25   39   6
#> Freshwater         27  27.00  27.0  27.00   27   1
#> Freshwater (creek)  4   4.50   5.0   5.50    6   3
#> Mock                1   1.50   2.0   2.50    3   3
#> Ocean              11  14.00  17.0  18.50   20   3
#> Sediment (estuary)  7   8.00   9.0  11.50   14   3
#> Skin                8  15.00  22.0  25.50   29   3
#> Soil               12  19.00  26.0  28.00   30   3
#> Tongue             13  13.00  13.0  13.00   13   1









set.seed(123)
betadisp <- vegan::betadisper(dist_bray, meta$SampleType)
bd_test  <- vegan::permutest(betadisp, permutations = 999)
print(bd_test)
#> 
#> Permutation test for homogeneity of multivariate dispersions
#> Permutation: free
#> Number of permutations: 999
#> 
#> Response: Distances
#>           Df  Sum Sq  Mean Sq      F N.Perm Pr(>F)   
#> Groups     8 0.49964 0.062454 5.7902    999  0.005 **
#> Residuals 17 0.18337 0.010786                        
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

plot(betadisp,
     main    = "Dispersion Multivariante por Grupo",
     col     = colores[seq_along(unique(meta$SampleType))],
     hull    = FALSE,
     ellipse = TRUE)







if (requireNamespace("DESeq2", quietly = TRUE)) {
  library(DESeq2)
  
  ps_sub <- subset_samples(ps, SampleType %in% c("Feces", "Freshwater"))
  ps_sub <- prune_taxa(taxa_sums(ps_sub) > 5, ps_sub)
  dds    <- phyloseq_to_deseq2(ps_sub, ~ SampleType)
  dds    <- DESeq2::estimateSizeFactors(dds, type = "poscounts")
  dds    <- DESeq2::DESeq(dds, fitType = "mean", quiet = TRUE)
  
  res     <- DESeq2::results(dds, cooksCutoff = FALSE)
  res_sig <- res[which(!is.na(res$padj) & res$padj < 0.05), ]
  cat("OTUs diferencialmente abundantes (FDR < 5%):", nrow(res_sig), "\n\n")
  
  # R base: ordenar sin dplyr::arrange
  res_df     <- as.data.frame(res_sig)
  res_df$OTU <- rownames(res_df)
  res_df     <- res_df[order(res_df$padj), ]
  kable(
    head(res_df[, c("OTU","baseMean","log2FoldChange","padj")], 10),
    digits  = 4,
    caption = "Top 10 OTUs diferencialmente abundantes (Feces vs Freshwater)"
  )
} else {
  cat("Instala DESeq2: BiocManager::install('DESeq2')\n")
}
#> OTUs diferencialmente abundantes (FDR < 5%): 1361









if (requireNamespace("DESeq2", quietly = TRUE)) {
  # R base: sin tibble::rownames_to_column ni dplyr::filter
  res_all      <- as.data.frame(DESeq2::results(dds, cooksCutoff = FALSE))
  res_all$OTU  <- rownames(res_all)
  res_all      <- res_all[!is.na(res_all$padj), ]
  
  res_all$Significativo <- ifelse(
    res_all$padj < 0.05 & res_all$log2FoldChange > 0,  "Mas en Feces",
    ifelse(res_all$padj < 0.05 & res_all$log2FoldChange <= 0, "Mas en Freshwater",
           "No significativo")
  )
  
  ggplot(res_all, aes(x = log2FoldChange, y = -log10(padj),
                      color = Significativo)) +
    geom_point(alpha = 0.7, size = 2) +
    geom_hline(yintercept = -log10(0.05), linetype = "dashed", color = "grey50") +
    geom_vline(xintercept = c(-1, 1),     linetype = "dashed", color = "grey50") +
    scale_color_manual(values = c("Mas en Feces"      = "#e41a1c",
                                  "Mas en Freshwater" = "#377eb8",
                                  "No significativo"  = "grey60")) +
    labs(title = "Volcano Plot - Abundancia Diferencial",
         x = "log2(Fold Change)", y = "-log10(p ajustado)", color = NULL) +
    theme_bw(base_size = 12) +
    theme(legend.position = "top")
}










kw_shannon <- kruskal.test(Shannon ~ SampleType, data = alpha_div)
cat("=== Kruskal-Wallis: Diversidad Shannon ===\n")
#> === Kruskal-Wallis: Diversidad Shannon ===
print(kw_shannon)
#> 
#>  Kruskal-Wallis rank sum test
#> 
#> data:  Shannon by SampleType
#> Kruskal-Wallis chi-squared = 22.125, df = 8, p-value = 0.004689

if (kw_shannon$p.value < 0.05) {
  cat("\n=== Post-hoc (Wilcoxon, correccion BH) ===\n")
  print(pairwise.wilcox.test(
    alpha_div$Shannon, alpha_div$SampleType, p.adjust.method = "BH"
  ))
}
#> 
#> === Post-hoc (Wilcoxon, correccion BH) ===
#> 
#>  Pairwise comparisons using Wilcoxon rank sum exact test 
#> 
#> data:  alpha_div$Shannon and alpha_div$SampleType 
#> 
#>                    Feces Freshwater Freshwater (creek) Mock Ocean
#> Freshwater         0.62  -          -                  -    -    
#> Freshwater (creek) 1.00  0.85       -                  -    -    
#> Mock               0.48  0.26       0.26               -    -    
#> Ocean              0.23  0.26       0.23               0.23 -    
#> Sediment (estuary) 0.23  0.26       0.23               0.23 0.23 
#> Skin               0.23  0.26       0.23               0.23 0.48 
#> Soil               0.23  0.26       0.23               0.23 0.23 
#> Tongue             0.85  1.00       0.85               0.26 0.26 
#>                    Sediment (estuary) Skin Soil
#> Freshwater         -                  -    -   
#> Freshwater (creek) -                  -    -   
#> Mock               -                  -    -   
#> Ocean              -                  -    -   
#> Sediment (estuary) -                  -    -   
#> Skin               0.26               -    -   
#> Soil               0.23               0.23 -   
#> Tongue             0.26               0.26 0.26
#> 
#> P value adjustment method: BH








saveRDS(ps_filtered, "phyloseq_filtrado.rds")
saveRDS(ps_rel,      "phyloseq_rel.rds")
# Cargar: ps <- readRDS("phyloseq_filtrado.rds")








write.csv(as.data.frame(otu_table(ps_filtered)), "tabla_otu.csv")
write.csv(as.data.frame(tax_table(ps_filtered)), "taxonomia.csv")
write.csv(as.data.frame(sample_data(ps_filtered)), "metadatos.csv")
write.csv(alpha_div, "diversidad_alfa.csv")
write.csv(psmelt(ps_top10), "composicion_phylum.csv", row.names = FALSE)







dir.create("figuras_phyloseq", showWarnings = FALSE)

p_shannon <- plot_richness(ps_rare, x = "SampleType",
                           measures = "Shannon", color = "SampleType") +
  geom_boxplot(alpha = 0.5) + theme_bw()

ggsave("figuras_phyloseq/diversidad_shannon.png", p_shannon,
       width = 10, height = 6, dpi = 300)
ggsave("figuras_phyloseq/diversidad_shannon.pdf", p_shannon,
       width = 10, height = 6)




###############################################################################

# Ejercicio 4 — Construccion manual de un objeto phyloseq


set.seed(42)
otu_mat_syn <- matrix(
  sample(0:500, 60, replace = TRUE), nrow = 10,
  dimnames = list(paste0("OTU", 1:10), paste0("Muestra", 1:6))
)

tax_mat_syn <- matrix(
  c("Firmicutes","Bacilli","Lactobacillus",
    "Bacteroidetes","Bacteroidia","Bacteroides",
    "Proteobacteria","Gammaproteobacteria","Escherichia",
    "Firmicutes","Clostridia","Clostridium",
    "Actinobacteria","Actinobacteria","Bifidobacterium",
    "Bacteroidetes","Bacteroidia","Prevotella",
    "Firmicutes","Bacilli","Streptococcus",
    "Proteobacteria","Betaproteobacteria","Burkholderia",
    "Firmicutes","Erysipelotrichia","Erysipelothrix",
    "Tenericutes","Mollicutes",NA),
  nrow = 10, byrow = TRUE,
  dimnames = list(paste0("OTU", 1:10), c("Phylum","Class","Genus"))
)

meta_syn <- data.frame(
  Grupo   = rep(c("Control","Tratamiento"), each = 3),
  Replica = rep(1:3, times = 2),
  row.names = paste0("Muestra", 1:6)
)

ps_syn <- phyloseq(
  otu_table(otu_mat_syn, taxa_are_rows = TRUE),
  tax_table(tax_mat_syn),
  sample_data(meta_syn)
)
validObject(ps_syn)
#> [1] TRUE
ps_syn
#> phyloseq-class experiment-level object
#> otu_table()   OTU Table:         [ 10 taxa and 6 samples ]
#> sample_data() Sample Data:       [ 6 samples by 2 sample variables ]
#> tax_table()   Taxonomy Table:    [ 10 taxa by 3 taxonomic ranks ]


# Calcular diversidad alfa con este nuevo objeto phyloseq

cat("Variables de metadatos:", paste(sample_variables(ps_syn), collapse = ", "), "\n")
#> Variables de metadatos: Grupo, Replica 

# PENDIENTE DE TERMINAR