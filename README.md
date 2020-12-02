# markdown_desc

---
title: "teste5"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


```{r cars, echo=FALSE, message=FALSE, warning=FALSE, results="asis"}


library(sas7bdat)
library(dplyr)
library(ggplot2)
library(PerformanceAnalytics)
library(kableExtra)
library(questionr)
library(knitr)
library(descr)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(huxtable)
library(xtable)
library(anomalize)
library(rlang)

#library(fabriciogiordanelli)

dados <- read.csv("D:/Users/fabricio_giordanelli/Downloads/dados_seguradora.csv", stringsAsFactors = TRUE)

dados$preco_seguro <- as.numeric(dados$preco_seguro)
dados$franquia <- as.numeric(dados$franquia)



descritiva <- function(dados,soma) {
  for (i in 1:ncol(dados)) {
    if (is.factor(dados[,i]) == TRUE | is.character(dados[,i]) == TRUE)  {
      teste <- dados %>%
        dplyr::group_by(dados[,i]) %>%
        dplyr::summarise(qtd = n()) %>%
        dplyr::mutate(perc = 100*round(qtd/sum(qtd),4)) %>%
        dplyr::arrange(-perc) %>%
        dplyr::mutate(n_Count = row_number()) %>%
        dplyr::filter(n_Count <= 10) %>%
        dplyr::rename(col1 = 1)

      print(
        ggplot2::ggplot(teste, aes(x = col1, y = qtd, fill = col1)) +
          ggplot2::geom_bar(stat = "identity") +
          ggplot2::geom_text(aes(label = paste0("(",qtd,", ",perc,"%",")")), size = 2.5, vjust = -1) +
          scale_fill_brewer(palette="Paired") +
          ggplot2::labs(x = NULL,
                        y = "count & perc",
                        fill = colnames(dados[i]),
                        title = paste("Quantidade e Percentual de", colnames(dados[i])))
      )
      
      print(
      kable(
      dados %>% 
        dplyr::group_by(!!sym(colnames(dados[i]))) %>%
              dplyr::summarize(n = n()) %>%
        dplyr::mutate(perc = 100*round(n/sum(n),4)) %>%
        dplyr::arrange(-perc)
          ) %>% 
        kable_styling()
          )
    }
  }

  
  for (i in 1:ncol(dados)) {
    if (is.numeric(dados[,i]) == TRUE) {
      
      boxplot(dados[,i], main= colnames(dados)[i]) 
      

      print(
      kable(
      as_tibble(dados) %>%
          na.omit() %>%
          anomalize(colnames(dados)[i], method = "gesd") %>%
          group_by(anomaly) %>%
          summarise(qtd = n()) %>% 
          mutate(perc = 100*round(qtd/sum(qtd),4))
      ) %>%
        kable_styling()
      )
      
    }
}
  
#   dados2 <- dados %>%
#     keep(is.numeric)
#   
#   ggplot(dat) +
#   aes(x = "", y = hwy) +
#   geom_boxplot(fill = "#0c4c8a") +
#   theme_minimal()
#   
#   #aqui mostra os outliers
#   boxplot.stats(dat$hwy)$out
# 
#   out <- boxplot.stats(dat$hwy)$out
# out_ind <- which(dat$hwy %in% c(out))
# out_ind
# dat[out_ind, ]
# 
# #AQUI É O TESTE ESTATÍSTICO PARA OUTLIER
# library(EnvStats)
# test <- rosnerTest(dat$hwy,
#   k = 3
# )
# 
# test$all.stats

#Does the data drift, trend, or cycle ?
#Is the variability fixed or is it itself variable ?
#Are there other series you can use for 'benchmarking' ?
  
  # res.pca <- prcomp(dados2, scale = TRUE)
  # 
  # fviz_pca_var(res.pca,
  #              col.var = "contrib", # Color by contributions to the PC
  #              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
  #              repel = TRUE     # Avoid text overlapping
  #              )
  # 


  for (i in 1:ncol(dados)-1) {
    for (j in (1+i):ncol(dados)) {
      if ((is.factor(dados[,i]) == TRUE | is.character(dados[,i]) == TRUE) &
          (is.factor(dados[,j]) == TRUE | is.character(dados[,j]) == TRUE)) {

        teste <- dados %>%
          dplyr::group_by(dados[,i],dados[,j]) %>%
          dplyr::summarise(qtd = n()) %>%
          dplyr::mutate(perc = 100*round(qtd/sum(qtd),4)) %>%
          dplyr::mutate(n_Count = row_number()) %>%
          dplyr::filter(n_Count <= 10) %>%
          dplyr::rename(col1 = 1, col2 = 2)

        print(
          ggplot2::ggplot(teste, aes(x = col1,y = qtd, fill = col2)) +
            ggplot2::geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
            ggplot2::geom_text(aes(label = paste0("(",qtd,", ",perc,"%",")")),position = position_dodge(width = 0.9), vjust = -1, size = 2) +
            scale_fill_brewer(palette="Paired") +
            ggplot2::labs(x = NULL,
                          y = "count & perc",
                          fill = colnames(dados[j]),
                          title = paste("Quantidade e Percentual de", colnames(dados[i])," por ",colnames(dados[j])))


        )
      

      tryCatch({
        
        if (dados %>% select_at(colnames(dados[i])) %>% distinct() %>% count()  == 2){
        
      f <- as.formula(paste(colnames(dados[i]), "~", paste(colnames(dados)[j], collapse=" + ")))

reg <- glm(f, family=binomial, data = dados)

  print(
    knitr::kable(odds.ratio(reg),
                 format = "html",
                         digits = 4,
                         caption = paste(colnames(dados[i]),  " = ",levels(dados[,i])[2], "e", colnames(dados[j]),  " = ", levels(dados[,j])[1]))  %>% 
      kableExtra::kable_styling()
    )
        }
  stop("precisa ser fator")} ,error = function(e){})
        
      }
    }
  }



  for (i in 1:ncol(dados)) {
    if (is.factor(dados[,i]) == TRUE | is.character(dados[,i]) == TRUE) {
      for (j in 1:ncol(dados)) {
        if (is.numeric(dados[,j]) == TRUE) {

          nome1 <- colnames(dados[i])
          nome2 <- colnames(dados[j])

          if(!missing(soma)) {
            teste <- dados %>%
              dplyr::group_by_at(nome1) %>%
              dplyr::summarize_at(.vars = nome2,
                                  list(soma = ~sum(.,na.rm = TRUE))) %>%
              dplyr::mutate(perc = 100*round(soma/sum(soma),4)) %>%
              dplyr::arrange(-perc) %>%
              dplyr::mutate(n_Count = row_number()) %>%
              dplyr::filter(n_Count <= 10) %>%
              dplyr::rename(col1 = 1)

            print(
              ggplot2::ggplot(teste, aes(x = col1, y = soma, fill = col1)) +
                ggplot2::geom_bar(stat = "identity") +
                ggplot2::geom_text(aes(label = paste0("(",round(soma,0),", ",perc,"%",")")), size = 2.5, vjust = -1) +
                scale_fill_brewer(palette="Paired") +
                ggplot2::labs(x = NULL,
                              y = "count & perc",
                              fill = colnames(dados[i]),
                              title = paste("Soma total de ", colnames(dados[j])))
            )
          }
          else {

            teste <- dados %>%
              dplyr::group_by_at(nome1) %>%
              dplyr::summarize_at(.vars = nome2,
                                  list(media = ~mean(.,na.rm = TRUE))) %>%
              dplyr::mutate(perc = 100*round(media/sum(media),4)) %>%
              dplyr::arrange(-perc) %>%
              dplyr::mutate(n_Count = row_number()) %>%
              dplyr::filter(n_Count <= 10) %>%
              dplyr::rename(col1 = 1)

            print(
              ggplot2::ggplot(teste, aes(x = col1, y = media, fill = col1)) +
                ggplot2::geom_bar(stat = "identity") +
                ggplot2::geom_text(aes(label = paste0("(",round(media,2),", ",perc,"%",")")), size = 2.5, vjust = -1) +
                scale_fill_brewer(palette="Paired") +
                ggplot2::labs(x = NULL,
                              y = "count & perc",
                              fill = colnames(dados[i]),
                              title = paste("Média total de ", colnames(dados[j])))
            )


          }






          print(
            ggplot2::ggplot(dados, aes_string(x = nome1,y = nome2)) +
              ggplot2::geom_boxplot() +
              ggplot2::labs(
                title = paste(colnames(dados[i]),colnames(dados[j]),sep = " x ")
              )
          )


          print(
            knitr::kable(dados %>%
                           dplyr::group_by_at(colnames(dados[i])) %>%
                           dplyr::summarize_at(.vars = colnames(dados[j]),
                                               list(n = ~ n(),
                                                    ~ min(.,na.rm = TRUE),
                                                    q1 = ~ quantile(.,
                                                                    probs = c(0.25),
                                                                    na.rm = TRUE),
                                                    q3 = ~ quantile(.,
                                                                    probs = c(0.75),
                                                                    na.rm = TRUE),
                                                    ~ max(.,na.rm = TRUE),
                                                    ~ mean(., na.rm = TRUE),
                                                    ~ sd(., na.rm = TRUE))),
                         format = "html",
                         digits = 2,
                         caption = paste(colnames(dados[i]),colnames(dados[j]),sep = " x ")) %>%
              kableExtra::kable_styling(bootstrap_options = c("striped", "hover"))
          )

        print(  
          lm(as.formula(paste(colnames(dados[j]),"~",colnames(dados[i]))), data = dados) %>%
   summary() %>%
    xtable() %>%
    kable(format = "html",
          digits = 4,
          caption = paste("Variável numérica ",colnames(dados[j]),  " pela variável categórica ",colnames(dados[i]), "=", levels(dados[,i])[1])) %>% 
     kable_styling()
          )
        
        
        tryCatch({
        
        if (dados %>% select_at(colnames(dados[i])) %>% distinct() %>% count()  == 2){
        
      f <- as.formula(paste(colnames(dados[i]), "~", paste(colnames(dados)[j], collapse=" + ")))

reg <- glm(f, family=binomial, data = dados)

  print(
    knitr::kable(odds.ratio(reg),
                 format = "html",
                         digits = 4,
                         caption = paste(colnames(dados[i]),  " = ",levels(dados[,i])[2], "e", colnames(dados[j]),  " = ", "aqui considero o aumento de uma unidade"))  %>% 
      kableExtra::kable_styling()
    )
        }
  stop("precisa ser fator")} ,error = function(e){})
        

          # f <- as.formula(paste(colnames(oi[j]),"~",colnames(oi[i])))
          # reg <- lm(f, data = dados)
          # print(
          # tab_model(reg, CSS = css_theme("cells"))
          # )
        }
      }
    }
  }


  for (i in 1:ncol(dados)) {
    if (is.factor(dados[,i]) == TRUE | is.character(dados[,i]) == TRUE) {
      for (j in 1:ncol(dados)) {
        if (is.numeric(dados[,j]) == TRUE) {
          tryCatch({
            dados2 <- dados %>%
              dplyr::select(colnames(dados[i]),colnames(dados[j])) %>%
              dplyr::group_by_at(colnames(dados[i]))

            dados3 <- dados2 %>%
              dplyr::group_split() %>%
              setNames(unlist(group_keys(dados2)))



            dados4 <- do.call("cbind", dados3)

            dados5 <- dplyr::select_if(dados4, is.numeric)

            print(
              PerformanceAnalytics::chart.Correlation(dados5)
            )
            stop("teste")} ,error = function(e){})

        }
      }
    }
  }


# nunca colocar uma variável dicotômica na última coluna. Ajeitar esse for para que consiga pegar uma variável dicotomica na última coluna para a regressão logística.
  
  for (i in 1:ncol(dados)-1) {
    for (j in (1+i):ncol(dados)) {
      for (k in 1:ncol(dados)) {
        if ((is.factor(dados[,i]) == TRUE | is.character(dados[,i]) == TRUE) &
            (is.factor(dados[,j]) == TRUE | is.character(dados[,j]) == TRUE) &
            is.numeric(dados[,k]) == TRUE ){
          {


            tryCatch({

              nome1 <- colnames(dados[i])
              nome2 <- colnames(dados[j])
              nome3 <- colnames(dados[k])

              if (!missing(soma)){

                teste <- dados %>%
                  dplyr::group_by_at(vars(all_of(nome1),all_of(nome2))) %>%
                  dplyr::summarize_at(.vars = nome3,
                                      list(soma = ~sum(.,na.rm = TRUE))) %>%
                  dplyr::mutate(perc = 100*round(soma/sum(soma),4)) %>%
                  dplyr::rename(col1 = 1, col2 = 2)

                print(
                  ggplot2::ggplot(teste, aes(x = col1, y = soma, fill = col2)) +
                    ggplot2::geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
                    ggplot2::geom_text(aes(label = paste0("(",round(soma,0),", ",perc,"%",")")),position = position_dodge(width = 0.9), vjust = -1, size = 2) +
                    scale_fill_brewer(palette="Paired") +
                    ggplot2::labs(x = NULL,
                                  y = "count & perc",
                                  fill = colnames(dados[i]),
                                  title = paste("Soma total de tarifa"))
                )

              }
              else {

                teste <- dados %>%
                  dplyr::group_by_at(vars(all_of(nome1),all_of(nome2))) %>%
                  dplyr::summarize_at(.vars = nome3,
                                      list(media = ~mean(.,na.rm = TRUE))) %>%
                  dplyr::mutate(perc = 100*round(media/sum(media),4)) %>%
                  dplyr::rename(col1 = 1, col2 = 2)

                print(
                  ggplot2::ggplot(teste, aes(x = col1, y = media, fill = col2)) +
                    ggplot2::geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
                    ggplot2::geom_text(aes(label = paste0("(",round(media,2),", ",perc,"%",")")),position = position_dodge(width = 0.9), vjust = -1, size = 2) +
                    scale_fill_brewer(palette="Paired") +
                    ggplot2::labs(x = NULL,
                                  y = "count & perc",
                                  fill = colnames(dados[i]),
                                  title = paste("Média total de tarifa"))
                )



              }

              print(
                ggplot2::ggplot(dados, aes_string(x = nome3,y = nome1)) +
                  ggplot2::geom_boxplot() +
                  ggplot2::coord_flip() +
                  ggplot2::facet_wrap(as.formula(paste("~", nome2))) +
                  ggplot2::labs(
                    title = paste(nome1,nome2,nome3,sep = " x "))
              )
              stop("teste")} ,error = function(e){})

            tryCatch({
              print(
                knitr::kable(dados %>%
                               dplyr::group_by_at(vars(all_of(nome1),all_of(nome2))) %>%
                               dplyr::summarize_at(.vars = nome3,
                                                   list(n = ~ n(),
                                                        ~ min(.,na.rm = TRUE),
                                                        q1 = ~ quantile(.,
                                                                        probs = c(0.25),
                                                                        na.rm = TRUE),
                                                        q3 = ~ quantile(.,
                                                                        probs = c(0.75),
                                                                        na.rm = TRUE),
                                                        ~ max(.,na.rm = TRUE),
                                                        ~ mean(., na.rm = TRUE),
                                                        ~ sd(., na.rm = TRUE))),
                             format = "html",
                             digits = 2,
                             caption = paste(nome1,nome2,nome3,sep = " x ")) %>%
                  kableExtra::kable_styling(bootstrap_options = c("striped", "hover")))
              stop("teste")} ,error = function(e){})


            
            
           tryCatch({
             print(  
          lm(as.formula(paste(colnames(dados[k]),"~",colnames(dados[i]), " + ", colnames(dados[j]))), data = dados) %>%
   summary() %>%
    xtable() %>%
    kable(format = "html",
          digits = 4,
          caption = paste("Variável numérica ",colnames(dados[k]),  " pela variável categórica ",colnames(dados[i]), "=", levels(dados[,i])[1]," e ", colnames(dados[j]), "=", levels(dados[,j])[1])) %>% 
     kable_styling()
            )
             stop("teste")} ,error = function(e){})
            
            
            tryCatch({
        
        if (dados %>% select_at(colnames(dados[i])) %>% distinct() %>% count()  == 2){
        
      f <- as.formula(paste(colnames(dados[i]), "~", paste(colnames(dados)[j], "+", paste(colnames(dados)[k]))))

reg <- glm(f, family=binomial, data = dados)

  print(
    knitr::kable(odds.ratio(reg),
                 format = "html",
                         digits = 4,
                         caption = paste(colnames(dados[i]),  " = ",levels(dados[,i])[2], "e", colnames(dados[j]),  " = ", levels(dados[,j])[1], "e", colnames(dados[k]),  " = Numérica"))  %>% 
      kableExtra::kable_styling()
    )
        }
  stop("precisa ser fator")} ,error = function(e){})
            
            
            
            
          }
        }
      }
    }
  }






for (i in 1:ncol(dados)-1) {
  for (j in (1+i):ncol(dados)) {
    if (is.numeric(dados[,i]) == TRUE & is.numeric(dados[,j]) == TRUE )  {
      
      nome1 <- colnames(dados[i])
      nome2 <- colnames(dados[j])
      
      print(
        PerformanceAnalytics::chart.Correlation(dados[,c(i,j)], histogram=TRUE, pch=19
        )
        
      )
      
      for (k in 1:ncol(dados)){
        if (is.factor(dados[,k]) == TRUE | is.character(dados[,k]) == TRUE) {
          
          tryCatch({
            a <- dados %>%
              dplyr::group_by(dados[,k]) %>%
              dplyr::summarize(corr = cor(!!sym(colnames(dados[i])),!!sym(colnames(dados[j]))))  %>%
              rename(col1 = 1)
            
            
            print(
              ggplot2::ggplot(a, aes(x = col1, y = corr,fill = col1)) +
                ggplot2::geom_bar(stat = "identity") +
                ggplot2::geom_text(aes(label = round(corr,2))) +
                scale_fill_brewer(palette="Paired") +
                ggplot2::labs(x = NULL,
                              y = "Correlação",
                              fill = colnames(dados[k]),
                              title = paste("Correlação entre ",colnames(dados[i])," e ",colnames(dados[j]), "pela variável categórica",colnames(dados[k]))) +
                ggplot2::lims(y = c(-1,1)) +
                ggplot2::coord_flip()
            )
            
            stop("teste")} ,error = function(e){})
        }
        
        
      }
      
    }
    
    
    
  }
}






}


descritiva(dados)

```
