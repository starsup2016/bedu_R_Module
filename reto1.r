hist(BD_Altura_Alunos$Altura)

ggplot(BD_Altura_Alunos, aes(Altura)) +
  geom_histogram(bins = 10, binwidth = 5)
