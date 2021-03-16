confusion = function(actual, predicted, names = NULL, printit = TRUE, prior = NULL) 
{
  if (is.null(names))
    names = levels(actual)
  tab = table(actual, predicted)
  acctab = t(apply(tab, 1, function(x) x/sum(x)))
  dimnames(acctab) = list(Actual = names, "Predicted (cv)" = names)
  if (is.null(prior)) 
  {
    relnum = table(actual)
    prior = relnum/sum(relnum)
    acc = sum(tab[row(tab) == col(tab)])/sum(tab)
  }
  else 
  {
    acc = sum(prior * diag(acctab))
    names(prior) = names
  }
  
  print(round(c("Accuracy" = acc, "Prior Frequency" = prior), 4))
  cat("\nConfusion Matrix", "\n")
  print(round(acctab, 4))
}