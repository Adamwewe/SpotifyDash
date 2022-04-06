library(shiny)
library(tidyverse)
library(dplyr)
library(class)
library(tree)
library(randomForest)

shinyServer(function(input, output) {
    
    set.seed(42)
    
    # loading the data
    df <- read.csv('./Spotify-2000.csv')
    df <- df[,-1] # drop Index/ i..Index
    
    # Creating validation and training sets 
    splits <- c(rep('train', 997), rep('validation', 997))
    df <- mutate(df, splits = sample(splits))
    df_train <- filter(df, splits == 'train')
    df_validation <- filter(df, splits =='validation')
    
    # Observed plot
        ## This plot represents the observations and will be the basis upon
        ## Which the user will see the effects of the classifiers
    output$obsPlot <- renderPlot({ 
        
        # user-adjustable binary variable of Popularity is created
            ## The threshold upon which a song can be considered popular or not
            ## depends entirely on the user 
        df_validation$Popularity_bin <- ifelse(df_validation$Popularity >= input$popInput, 1, 0)
        df_validation$Popularity_bin <- as.factor(df_validation$Popularity_bin)
        
        ggplot(data = df_validation, 
               mapping = aes(x = get(input$var1Input), y = get(input$var2Input), color=Popularity_bin)) + 
            geom_point(size = 1.3) + 
            theme_minimal() +
            labs(x = input$var1Input, y = input$var2Input, color="Observed Popularity") +
            ggtitle(paste("Scatter plot of",input$var1Input, "versus", input$var2Input)) + 
            scale_color_manual(values = c("#191414","#1DB954"), ## Spotify colors are encoded
                               labels=c("Not Popular", "Popular"))
        })
    
    # Classification plot
    output$predPlot <- renderPlot({
        
        # binary variable of Popularity
        df_train$Popularity_bin <- ifelse(df_train$Popularity >= input$popInput, 1, 0)
        df_train$Popularity_bin <- as.factor(df_train$Popularity_bin)
        df_validation$Popularity_bin <- ifelse(df_validation$Popularity >= input$popInput, 1, 0)
        df_validation$Popularity_bin <- as.factor(df_validation$Popularity_bin)

        # logistic regression
        model.log <- glm(Popularity_bin~Danceability + Loudness..dB. + Speechiness + Liveness, family = 'binomial', data =  df_train)
        df_validation$pred <- predict(model.log, newdata = df_validation, type = "response")
        y_pred.log <- ifelse(df_validation$pred > 0.5, 1, 0) 
        
        # knn
            ## Choice of variables explained on the complementary rmd file
        model.knn <- knn(df_train[c('Danceability', 'Loudness..dB.','Speechiness','Liveness')],
                         df_validation[c('Danceability', 'Loudness..dB.','Speechiness','Liveness')],
                         cl = df_train[['Popularity_bin']],
                         k = input$kInput) # user defines number of neighbors
        
        # classification tree
            ## Investigation on optimal tree model has been conducted on the rmd file
        model.tree <- tree(Popularity_bin ~ Danceability + Loudness..dB. + Speechiness + Liveness, data = df_train)
        y_pred.tree <- predict(model.tree, newdata = df_validation, type='class')
        
        # random forest
        model.forest <- randomForest(formula = Popularity_bin ~ Danceability + Loudness..dB. + Speechiness + Liveness,
                                     data = select(df_train, -c(Title, Year, Artist, Top.Genre)), 
                                     ntree= input$ntreeInput, # user defines number of trees to be used 
                                     importance = TRUE) 
        y_pred.forest <- predict(model.forest, newdata = df_validation)
        
        # creating logistic regression plot
        log_class <- reactive({
            ggplot(data = df_validation,
                   mapping = aes(x = get(input$var1Input), y = get(input$var2Input), color=as.factor(y_pred.log))) +
                geom_point(size = 1.3) +
                theme_minimal() +
                labs(x = input$var1Input, y = input$var2Input, title = "Logistic Regression Classification", color="Predicted Popularity") +
                scale_color_manual(values = c("#191414","#1DB954"), # Spotify colors
                                   labels=c("Not Popular (0)", "Popular (1)"))
                
        })
        
        # creating KNN plot
        knn_class <- reactive({
            ggplot(data = df_validation,
                   mapping = aes(x = get(input$var1Input), y = get(input$var2Input), color=model.knn)) +
                geom_point(size = 1.3) +
                theme_minimal() +
                labs(x = input$var1Input, y = input$var2Input, title = "KNN Classification", color="Predicted Popularity") +
                scale_color_manual(values = c("#191414","#1DB954"),
                                   labels=c("Not Popular (0)", "Popular (1)"))
        })
        
        # creating classification tree plot
        tree_class <- reactive({
            ggplot(data = df_validation,
                   mapping = aes(x = get(input$var1Input), y = get(input$var2Input), color=y_pred.tree)) +
                geom_point(size = 1.3) +
                theme_minimal() +
                labs(x = input$var1Input, y = input$var2Input, title = "Classification Tree Classification", color="Predicted Popularity") +
                scale_color_manual(values = c("#191414","#1DB954"), 
                                   labels=c("Not Popular (0)", "Popular (1)"))
        })
        
        # creating Random Forest plot
        randomforest_class <- reactive({
            ggplot(data = df_validation,
                   mapping = aes(x = get(input$var1Input), y = get(input$var2Input), color=y_pred.forest)) +
                geom_point(size = 1.3) +
                theme_minimal() +
                labs(x = input$var1Input, y = input$var2Input, title = "Random Forest Classification", color="Predicted Popularity") +
                scale_color_manual(values = c("#191414","#1DB954"), 
                                   labels=c("Not Popular (0)", "Popular (1)"))
        })
        
        
        # Return the requested graph
        graphInput_class <- reactive({
            switch(input$graph_class,
                   "Logistic Regression" = log_class(),
                   "KNN" = knn_class(),
                   "Classification Tree" = tree_class(),
                   "Random Forest" = randomforest_class()
            )})
        
        graphInput_class()
    })
    
    # in server component 
    output$cm <- renderPrint({
        
        # binary variable of Popularity
        df_train$Popularity_bin <- ifelse(df_train$Popularity >= input$popInput, 1, 0)
        df_train$Popularity_bin <- as.factor(df_train$Popularity_bin)
        df_validation$Popularity_bin <- ifelse(df_validation$Popularity >= input$popInput, 1, 0)
        df_validation$Popularity_bin <- as.factor(df_validation$Popularity_bin)
        
        # logistic regression
        model.log <- glm(Popularity_bin~Danceability + Loudness..dB. + Speechiness + Liveness, family = 'binomial', data =  df_train)
        df_validation$pred <- predict(model.log, newdata = df_validation, type = "response")
        y_pred.log <- ifelse(df_validation$pred > 0.5, 1, 0) 
        
        # knn
        model.knn <- knn(df_train[c('Danceability', 'Loudness..dB.','Speechiness','Liveness')],
                         df_validation[c('Danceability', 'Loudness..dB.','Speechiness','Liveness')],
                         cl = df_train[['Popularity_bin']],
                         k = input$kInput) # user defines number of neighbors
        
        # classification tree
        model.tree <- tree(Popularity_bin ~ Danceability + Loudness..dB. + Speechiness + Liveness, data = df_train)
        y_pred.tree <- predict(model.tree, newdata = df_validation, type='class')
        
        # random forest
        model.forest <- randomForest(formula = Popularity_bin ~ Danceability + Loudness..dB. + Speechiness + Liveness,
                                     data = select(df_train, -c(Title, Year, Artist, Top.Genre)), 
                                     ntree= input$ntreeInput, # user defines number of trees
                                     importance = TRUE)
        y_pred.forest <- predict(model.forest, newdata = df_validation)
        
        # creating Logistic Regression confusion matrix
        log_cm <- reactive({
            conf_mat_log <-table(true = df_validation$Popularity_bin,predicted = y_pred.log)
            conf_mat_log
        })
        
        # creating KNN confusion matrix
        knn_cm <- reactive({
            conf_mat_knn <-table(true = df_validation$Popularity_bin, predicted = model.knn)
            conf_mat_knn
        })

        # creating classification tree confusion matrix
        tree_cm <- reactive({
            conf_mat_tree <- table(true = df_validation$Popularity_bin, pred = y_pred.tree)
            conf_mat_tree
        })
        
        # creating Random Forest confusion matrix
        randomforest_cm <- reactive({
            conf_mat_forest <- table(true = df_validation$Popularity_bin, pred = y_pred.forest)
            conf_mat_forest
        })
        
        # Return the requested graph
        graphInput_cm <- reactive({
            switch(input$graph_class,
                   "Logistic Regression" = log_cm(),
                   "KNN" = knn_cm(),
                   "Classification Tree" = tree_cm(),
                   "Random Forest" = randomforest_cm()
            )})
        
        graphInput_cm()
    })
    
    # Results of the models
    output$modelres <- renderTable({
        
        # binary variable of Popularity
        df_train$Popularity_bin <- ifelse(df_train$Popularity >= input$popInput, 1, 0)
        df_train$Popularity_bin <- as.factor(df_train$Popularity_bin)
        df_validation$Popularity_bin <- ifelse(df_validation$Popularity >= input$popInput, 1, 0)
        df_validation$Popularity_bin <- as.factor(df_validation$Popularity_bin)
        
        # logistic regression
        model.log <- glm(Popularity_bin~Danceability + Loudness..dB. + Speechiness + Liveness, family = 'binomial', data =  df_train)
        df_validation$pred <- predict(model.log, newdata = df_validation, type = "response")
        y_pred.log <- ifelse(df_validation$pred > 0.5, 1, 0) 
        conf_mat_log1 <-table(true = df_validation$Popularity_bin,predicted = y_pred.log)
        
        # knn
        model.knn <- knn(df_train[c('Danceability', 'Loudness..dB.','Speechiness','Liveness')],
                         df_validation[c('Danceability', 'Loudness..dB.','Speechiness','Liveness')],
                         cl = df_train[['Popularity_bin']],
                         k = input$kInput) # user defines number of neighbors
        conf_mat_knn1 <-table(true = df_validation$Popularity_bin, predicted = model.knn)
        
        # classification tree
        model.tree <- tree(Popularity_bin ~ Danceability + Loudness..dB. + Speechiness + Liveness, data = df_train)
        y_pred.tree <- predict(model.tree, newdata = df_validation, type='class')
        conf_mat_tree1 <- table(true = df_validation$Popularity_bin, pred = y_pred.tree)
        
        # random forest
        model.forest <- randomForest(formula = Popularity_bin ~ Danceability + Loudness..dB. + Speechiness + Liveness,
                                     data = select(df_train, -c(Title, Year, Artist, Top.Genre)), 
                                     ntree= input$ntreeInput, # user defines number of trees
                                     importance = TRUE)
        y_pred.forest <- predict(model.forest, newdata = df_validation)
        conf_mat_forest1 <- table(true = df_validation$Popularity_bin, pred = y_pred.forest)
        
        # result table
        tablemodelres <- matrix(c("Logistic Regression", "KNN", "Classification Tree","Random Forest",
                                  # specificity
                                  round(conf_mat_log1[1,1] / (conf_mat_log1[1,1] + conf_mat_log1[1,2]), 3),          # logistic regression
                                  round(conf_mat_knn1[1,1] / (conf_mat_knn1[1,1] + conf_mat_knn1[1,2]),3),           # KNN
                                  round(conf_mat_tree1[1,1] / (conf_mat_tree1[1,1] + conf_mat_tree1[1,2]), 3),       # Classification Tree
                                  round(conf_mat_forest1[1,1] / (conf_mat_forest1[1,1] + conf_mat_forest1[1,2]), 3), # Random Forest
                                  # Sensitivity
                                  round(conf_mat_log1[2,2] / (conf_mat_log1[2,2] + conf_mat_log1[2,1]), 3),          # logistic regression
                                  round(conf_mat_knn1[2,2] / (conf_mat_knn1[2,2] + conf_mat_knn1[2,1]), 3),          # KNN
                                  round(conf_mat_tree1[2,2] / (conf_mat_tree1[2,2] + conf_mat_tree1[2,1]), 3),       # Classification Tree
                                  round(conf_mat_forest1[2,2] / (conf_mat_forest1[2,2] + conf_mat_forest1[2,1]), 3), # Random Forest
                                  # Accuracy
                                  round((conf_mat_log1[2,2] + conf_mat_log1[1,1]) /sum(conf_mat_log1), 3),           # logistic regression
                                  round((conf_mat_knn1[2,2] + conf_mat_knn1[1,1]) /sum(conf_mat_knn1), 3),           # KNN
                                  round((conf_mat_tree1[2,2] + conf_mat_tree1[1,1]) /sum(conf_mat_tree1), 3),        # Classification Tree
                                  round((conf_mat_forest1[2,2] + conf_mat_forest1[1,1]) /sum(conf_mat_forest1), 3)), # Random Forest
                                  ncol = 4)
        colnames(tablemodelres) <- c(" ", "Specificity", "Sensitivity", "Accuracy")

        tablemodelres
        
    })
    
    # Model Statistical Output (Text)
    output$statout <- renderText({
        
        # binary variable of Popularity
        df_train$Popularity_bin <- ifelse(df_train$Popularity >= input$popInput, 1, 0)
        df_train$Popularity_bin <- as.factor(df_train$Popularity_bin)
        df_validation$Popularity_bin <- ifelse(df_validation$Popularity >= input$popInput, 1, 0)
        df_validation$Popularity_bin <- as.factor(df_validation$Popularity_bin)
        
        # logistic regression
        model.log <- glm(Popularity_bin~Danceability + Loudness..dB. + Speechiness + Liveness, family = 'binomial', data =  df_train)
        df_validation$pred <- predict(model.log, newdata = df_validation, type = "response")
        y_pred.log <- ifelse(df_validation$pred > 0.5, 1, 0) 
        conf_mat_log1 <-table(true = df_validation$Popularity_bin,predicted = y_pred.log)
        model.log.sum <- summary(model.log)
        
        # knn
        model.knn <- knn(df_train[c('Danceability', 'Loudness..dB.','Speechiness','Liveness')],
                         df_validation[c('Danceability', 'Loudness..dB.','Speechiness','Liveness')],
                         cl = df_train[['Popularity_bin']],
                         k = input$kInput) # user defines number of neighbors
        conf_mat_knn1 <-table(true = df_validation$Popularity_bin, predicted = model.knn)
        model.knn.sum <- summary(model.knn)
        
        # classification tree
        model.tree <- tree(Popularity_bin ~ Danceability + Loudness..dB. + Speechiness + Liveness, data = df_train)
        y_pred.tree <- predict(model.tree, newdata = df_validation, type='class')
        conf_mat_tree1 <- table(true = df_validation$Popularity_bin, pred = y_pred.tree)
        model.tree.sum <- summary(model.tree)
        
        # random forest
        model.forest <- randomForest(formula = Popularity_bin ~ Danceability + Loudness..dB. + Speechiness + Liveness,
                                     data = select(df_train, -c(Title, Year, Artist, Top.Genre)), 
                                     ntree= input$ntreeInput, # user defines number of trees
                                     importance = TRUE)
        y_pred.forest <- predict(model.forest, newdata = df_validation)
        conf_mat_forest1 <- table(true = df_validation$Popularity_bin, pred = y_pred.forest)
        model.forest.sum <- summary(model.forest)
        
        # accuracy
        log_acc <- round((conf_mat_log1[2,2] + conf_mat_log1[1,1]) /sum(conf_mat_log1), 3) 
        knn_acc <- round((conf_mat_knn1[2,2] + conf_mat_knn1[1,1]) /sum(conf_mat_knn1), 3)
        tree_acc <- round((conf_mat_tree1[2,2] + conf_mat_tree1[1,1]) /sum(conf_mat_tree1), 3)
        forest_acc <- round((conf_mat_forest1[2,2] + conf_mat_forest1[1,1]) /sum(conf_mat_forest1), 3)
        
        model.log.sum[["name"]] <- "Logistic Regression"
        model.knn.sum[["name"]] <- "K Nearest Neighbors"
        model.tree.sum[["name"]] <- "Decision Tree"
        model.forest.sum[["name"]] <- "Random Forest"
        
        # Best fitting model, with accuracy
        if(log_acc > knn_acc && log_acc > tree_acc && log_acc > forest_acc){
            model.text.out <- model.log.sum
            best.acc <- log_acc
        } else if(knn_acc > log_acc && knn_acc > tree_acc && knn_acc > forest_acc){
            model.text.out <- model.knn.sum
            best.acc <- knn_acc
        } else if (tree_acc > log_acc && tree_acc > knn_acc && tree_acc > forest_acc){
            model.text.out <- model.tree.sum
            best.acc <- tree_acc
        } else if (forest_acc > log_acc && forest_acc > knn_acc && forest_acc > tree_acc){
            model.text.out <- model.forest.sum
            best.acc <- forest_acc
        }
        
        # oob of random forest
            ## we manually retrieve the OOB score from the fit object:
        oob <- model.forest$err.rate[input$ntreeInput,1]
        
        ## Interpretation:
        
        paste("The best Classification method is: ", model.text.out$name, 
              ", with an Accuracy of: ", best.acc, "<br>", "The OOB of the Random Forest is: " , round(oob,3)*100, "%","</b><br><br>",
              "<p><b>Interpretation:</b>  <br> As demonstrated in the .rmd file, the data at hand is not well suited for linear models. <br>
              This is therefore a classification problem which we made reactive, making the user fully responsible for fine-tuning the threshold. <br>
              <br>
              From the four examined classifiers, the logistic regression was the most accurate (except when the decision threshold is between 49 and 53). However, due to the absence of linearity, 
              plotting was instead used to reflect the predicted classes in a similar fashion as the forest, tree and KNN classifiers. <br>
              By plotting them the same way, the user can fine tune the models at their will and compare both observed and predicted values 
              using the tabs. This visual comparison extends to the confusion matrix which reveals accuracy, specificity and sensitivity. <br>
              <br>
              Through those three metrics, it can be seen that there is a negative correlation between the decision threshold and the sensitivity. <br>
              In other words, the higher the threshold on which we decide if a song is popular or not, the more accurate and specific the 
              classifications will be, while the classifiers will not be sensitive at all. Sensitivity, is in fact very low on average between 
              all classifiers. <br>
              <br>
              To conclude, all four classifiers are accurate and specific, however due to the high amount of false negatives, 
              the classifiers cannot be considered as sensitive. <br>
              </p>")
        
        
        })
    
    
    output$dataInfo <- renderText({
        
        # info about the dataset
        paste("<p>This dataset contains audio statistics of the top 2000 tracks on Spotify.<br>
               It includes 1990 observations and 4 variables about popular songs released between 1956 and 2019.<br>
               From the 15 variables present in the dataset, we have selected the following 4:</p>
               <ul><li>Danceability: The higher the value, the easier it is to dance to this song</li>
               <li>Loudness: The higher the value, the louder the song</li>
               <li>Speechiness: The higher the value the more spoken words the song contains</li>
               <li>Popularity: The higher the value the more popular the song is</li></ul>")
    })
})

