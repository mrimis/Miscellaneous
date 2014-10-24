##Statistical Test for analysis of Intraheterogeneity of morphological feature on each day
path_in1="/Users/mrinalmishra/Documents/thesis/control/LNCAP/DMSO/Tex/tex_param_Day4_features_mean.txt"
path_in2="/Users/mrinalmishra/Documents/thesis/Co-culture/LNCAP/DMSO/Tex/tex_param_Day4_features_mean.txt.txt"
path_in3="/Users/mrinalmishra/Documents/thesis/control/LNCAP/DMSO/Tex/tex_param_Day5_features_mean.txt"
path_in4="/Users/mrinalmishra/Documents/thesis/Co-culture/LNCAP/DMSO/Tex/tex_param_Day5_features_mean.txt.txt"
path_in5="/Users/mrinalmishra/Documents/thesis/control/LNCAP/DMSO/Tex/tex_param_Day6_features_mean.txt"
path_in6="/Users/mrinalmishra/Documents/thesis/Co-culture/LNCAP/DMSO/Tex/tex_param_Day6_features_mean.txt.txt"
path_in7="/Users/mrinalmishra/Documents/thesis/control/LNCAP/DMSO/Tex/tex_param_Day7_features_mean.txt"
path_in8="/Users/mrinalmishra/Documents/thesis/Co-culture/LNCAP/DMSO/Tex/tex_param_Day7_features_mean.txt.txt"
path_in9="/Users/mrinalmishra/Documents/thesis/control/LNCAP/DMSO/Tex/tex_param_Day8_features_mean.txt"
path_in10="/Users/mrinalmishra/Documents/thesis/Co-culture/LNCAP/DMSO/Tex/tex_param_Day8_features_mean.txt.txt"
path_in11="/Users/mrinalmishra/Documents/thesis/control/LNCAP/DMSO/Tex/tex_param_Day11_features_mean.txt"
path_in12="/Users/mrinalmishra/Documents/thesis/Co-culture/LNCAP/DMSO/Tex/tex_param_Day11_features_mean.txt.txt"

df1=read.table(path_in1, header=TRUE,sep='')
df2=read.table(path_in2, header=TRUE,sep='')
df3=read.table(path_in3, header=TRUE,sep='')
df4=read.table(path_in4, header=TRUE,sep='')
df5=read.table(path_in5, header=TRUE,sep='')
df6=read.table(path_in6, header=TRUE,sep='')
df7=read.table(path_in7, header=TRUE,sep='')
df8=read.table(path_in8, header=TRUE,sep='')
df9=read.table(path_in9, header=TRUE,sep='')
df10=read.table(path_in10, header=TRUE,sep='')
df11=read.table(path_in11, header=TRUE,sep='')
df12=read.table(path_in12, header=TRUE,sep='')
dfC04=rbind(df1,df2)
dfC05=rbind(df3,df4)
dfC06=rbind(df5,df6)
dfC07=rbind(df7,df8)
dfC08=rbind(df9,df10)
dfC11=rbind(df11,df12)
dfC_all=rbind(dfC04,dfC05,dfC06,dfC07,dfC08,dfC11)
results_all = aov(dfC_all$h.ent.s ~ dfC_all$category )
capture.output(summary(results_all),file="/Users/mrinalmishra/Documents/thesis/Intra_het_analysis/anova_test__DMSO_d_all.txt")
test_all=TukeyHSD(results_all, conf.level = 0.95) 
df_all=as.data.frame(test_all$dfC_all)
write.table(df_all, "/Users/mrinalmishra/Documents/thesis/Intra_het_analysis/TukeyHSD_test_DMSO_d_all.txt", sep="\t",row.names = TRUE, col.names=TRUE) 

results4 = aov(dfC04$h.ent.s ~ dfC04$category )
capture.output(summary(results4),file="/Users/mrinalmishra/Documents/thesis/Intra_het_analysis/anova_test__DMSO_d4.txt")
t.test4=pairwise.t.test(dfC04$h.ent.s, dfC04$category, p.adjust="bonferroni") 
test4=TukeyHSD(results4, conf.level = 0.95) 
df4=as.data.frame(test4$dfC04)
write.table(df4, "/Users/mrinalmishra/Documents/thesis/Intra_het_analysis/TukeyHSD_test_DMSO_d4.txt", sep="\t",row.names = TRUE, col.names=TRUE) 

results5 = aov(dfC05$h.ent.s ~ dfC05$category)
capture.output(summary(results5),file="/Users/mrinalmishra/Documents/thesis/Intra_het_analysis/anova_test__DMSO_d5.txt")

t.test5=pairwise.t.test(dfC05$h.ent.s, dfC05$category, p.adjust="bonferroni") 
test5=TukeyHSD(results5, conf.level = 0.95) 
df5=as.data.frame(test5$dfC05)
write.table(df5, "/Users/mrinalmishra/Documents/thesis/Intra_het_analysis/TukeyHSD_test_DMSO_d5.txt", sep="\t",row.names = TRUE, col.names=TRUE) 

results6 = aov(dfC06$h.ent.s ~ dfC06$category)
capture.output(summary(results6),file="/Users/mrinalmishra/Documents/thesis/Intra_het_analysis/anova_test__DMSO_d6.txt")

t.test6=pairwise.t.test(dfC06$h.ent.s, dfC06$category, p.adjust="bonferroni") 
test6=TukeyHSD(results6, conf.level = 0.95) 
df6=as.data.frame(test6$dfC06)
write.table(df6, "/Users/mrinalmishra/Documents/thesis/Intra_het_analysis/TukeyHSD_test_DMSO_d6.txt", sep="\t",row.names = TRUE, col.names=TRUE) 

results7 = aov(dfC07$h.ent.s ~ dfC07$category)
capture.output(summary(results7),file="/Users/mrinalmishra/Documents/thesis/Intra_het_analysis/anova_test__DMSO_d7.txt")

t.test7=pairwise.t.test(dfC07$h.ent.s, dfC07$category, p.adjust="bonferroni") 
test7=TukeyHSD(results7, conf.level = 0.95) 
df7=as.data.frame(test7$dfC07)
write.table(df7, "/Users/mrinalmishra/Documents/thesis/Intra_het_analysis/TukeyHSD_test_DMSO_d7.txt", sep="\t",row.names = TRUE, col.names=TRUE) 

results8 = aov(dfC08$h.ent.s ~ dfC08$category)
capture.output(summary(results8),file="/Users/mrinalmishra/Documents/thesis/Intra_het_analysis/anova_test__DMSO_d8.txt")

t.test8=pairwise.t.test(dfC08$h.ent.s, dfC08$category, p.adjust="bonferroni") 
test8=TukeyHSD(results8, conf.level = 0.95) 
df8=as.data.frame(test8$dfC08)
write.table(df8, "/Users/mrinalmishra/Documents/thesis/Intra_het_analysis/TukeyHSD_test_DMSO_d8.txt", sep="\t",row.names = TRUE, col.names=TRUE) 

results11 = aov(dfC11$h.ent.s ~ dfC11$category)
capture.output(summary(results11),file="/Users/mrinalmishra/Documents/thesis/Intra_het_analysis/anova_test__DMSO_d11.txt")

t.test11=pairwise.t.test(dfC11$h.ent.s, dfC11$category, p.adjust="bonferroni") 
test11=TukeyHSD(results11, conf.level = 0.95) 
df11=as.data.frame(test11$dfC11)
write.table(df11, "/Users/mrinalmishra/Documents/thesis/Intra_het_analysis/TukeyHSD_test_DMSO_d11.txt", sep="\t",row.names = TRUE, col.names=TRUE) 

