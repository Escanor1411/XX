# XX
## Find Probability to Draw 3 card from a deck and have the values between 17 - 21 Using Monte Carlo 

number <- c("Ace", 2, 3, 4, 5, 6, 7, 8, 9, 10 ,10 ,10 ,10)
suits <- c("Diamonds", "Clubs", "Hearts", "Spades")
deck = expand.grid(number = number,suit=suits)

total <- function(x){
    x <- as.numeric(x)
    if (any(x==1)){
        sum_temp = sum(x) - 1
        if(sum_temp <=10){
            final_sum = sum_temp + 11
        }
        else{final_sum = sum(x)}}
    else {final_sum=sum(x)}
    final_sum
}


check <- function(a){
    prob<-total(a)>=17 & total(a)<=21
    prob
}
check(a)

B = 500000
prob <- replicate(B,{
    check(sample(deck$number,3,replace = FALSE))
})
mean(prob)
