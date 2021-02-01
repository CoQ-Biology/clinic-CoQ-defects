
library(dplyr)
library(assertr)
library(ggplot2)


datos <- gsheet::gsheet2tbl("https://docs.google.com/spreadsheets/d/1zCc9r7Uy_6ujOU5qXc7VOXuN4OSvIJg5Tx9iqWw6swY/edit#gid=0") %>%
  arrange(group)

npatients <- gsheet::gsheet2tbl("https://docs.google.com/spreadsheets/d/1zCc9r7Uy_6ujOU5qXc7VOXuN4OSvIJg5Tx9iqWw6swY/edit?usp=sharing#gid=1452337852") 

datos %>%
  # verify(ncol(.) == 12) %>%
  verify(names(datos)[1:2] == c("group", "symptom")) %>%
  verify(all.equal(names(datos)[3:ncol(datos)], names(npatients)))

npatients <- as.numeric(npatients)


percent <- datos

for (j in 3:ncol(datos)) {
  percent[, j] <- 100*(percent[, j]/npatients[j - 2])
}




datos.long <- percent %>%
  tidyr::pivot_longer(cols = 3:ncol(datos), names_to = "gene") %>%
  mutate(gene = factor(gene, levels = names(datos)[3:ncol(datos)]))




plotfun <- function(df = datos.long, grupo = NULL, npat = npatients) {
  
  datos.sub <- df %>%
    dplyr::filter(group == grupo) %>%
    # dplyr::filter(value > 0) %>% 
    mutate(symptom = factor(symptom, 
                            levels = sort(unique(symptom), decreasing = TRUE)))
  
  ## Sort rows by prevalence
  df2 <- datos.sub %>%
    # dplyr::filter(value > 0) %>%
    group_by(symptom) %>%
    summarise(n.site = sum(value)) %>%
    arrange(n.site) 
  
  datos.sub$symptom <- factor(datos.sub$symptom, levels = df2$symptom)
  
  
  patients <- data.frame(gene = unique(df$gene),
                         n.pat = npat)
  
  heatm <- ggplot(datos.sub) +
    aes(x = gene, y = symptom) +
    geom_tile(aes(fill = value), colour = "grey80",
              height = 1, width = 1) +
    #scale_fill_viridis(name = "% patients", option = "magma") +
    scale_fill_distiller(name = "% patients\n", type = "seq",
                         palette = "YlGnBu", direction = 1,
                         limits = c(0.001, 100), na.value = "grey99") +
    coord_equal(clip = "off") +
    scale_x_discrete(position = "top") +
    theme(axis.text.x.top = element_text(angle = 90, hjust = 0, vjust = 0.5)) +
    xlab("") +
    ylab("") +
    theme(plot.margin = unit(c(0, 0, 0.5, 0), "cm")) +
    theme(panel.background = element_blank()) +
    geom_text(data = patients, aes(y = 0, label = n.pat), size = 3) +
    annotate("text", x = 5.5, y = -1, label = "Number of patients per gene", 
             size = 3, colour = "grey20")
  
  heatm
  
}


