am_shad = read.csv("shad.txt")
head(am_shad)

hist(x = am_shad$Length,
     main = '',
     xlab = 'Total length (cm)',
     col = rgb(0.87, 0.87, 0.87, 0.50),
     xaxt='n',
     yaxt='n',
     ylab = ''
     )
abline(v=mean(am_shad$Length), col='red', lwd=4)
text(x = 50, y=500, "Hi")

axis(side=1, pos=0)

plot(y = am_shad$Length, x = am_shad$Age, type="p",
     pch = 21, bg='gray87')

par(mar=c(5,5,1,1))
boxplot(Length~Sex+Age, data=am_shad,
        col=c('gray87', 'gray40'),
        xlab='Age (years)',
        ylab = 'Total length (mm)'
        )


1 + 1
a = 1 + 1
a
print(a)
show(a)
a = 3
a
A
A = 1 + 1
A
a


shad = am_shad[am_shad$Length >= 10, ]




a = c(1,2,3)
a
nrow(a)
length(a)


1 + 2
1 - 2
4 * 4
4 / 4
4 ^ 2
exp(4)
log(4)
log10(4)
min(A)
max(A)
sqrt(A)
mean(A)
sd(A)
var(A)








head(am_shad)


means = ddply(am_shad, c('Sex', 'Age'),
              summarize,
              meanLength = mean(Length),
              sdLength = sd(Length)
              )

means





am_shad = read.csv(file = "http://employees.oneonta.edu/stichds/classes/BIOL217/ctr_fish.csv", header = TRUE)

shad = am_shad

females = shad[shad$Sex == 'R' &
                 shad$backCalculated == FALSE,]

nrow(females)

females$fecundity = 0.445 * females$Length ^ 3.5
head(females)
mean(females$fecundity)

means = ddply(females, c('Age'),
              summarize,
              meanFec = mean(fecundity),
              sdFec = sd(fecundity)
              )

age7b = rnorm(n = 1e4, mean=means$meanFec[means$Age==7],
              sd = means$sdFec[means$Age==7])

hist(age7b,
     xaxt='n'
     )

axis(side = 1,
     at = seq(from = 2e5, to = 5e5, by= 5e4),
     labels = format(seq(from = 2e5, to = 5e5, by= 5e4),
                     scientific = FALSE)
     )
























