Key summary
-----------

There have been arguments about building a ‘safer’ portfolio. By
‘safer,’ we mean the portfolio is less volatile. The following are some
of the most common ideas.

**1. Hedge Funds is safer**

Professional fund managers actively manage hedge Funds, and some people
believe that makes it ‘safer’ to invest in a hedge fund because these
fund managers have skills and knowledge to prevent the fund from
suffering colossal loss.

We use ETFs that track hedge fund’s investments to build our portfolio.
We picked top 10 (by total assets) ETFs that are categorized in hedge
funds from eftdb.co. Then select 5 ETFs that have 5-year data to run our
simulation.

The hedge fund portfolio has a 20-day 5% VaR of around USD 3,000.

**2. Real Estate is safer**

A lot of people think real estate investment is safer because the market
value of real estate assets doesn’t change all the time as the stock
market does. Thus, we built a portfolio using five real-estate ETFs.
However, it turned out the portfolio has a 20-day 5% VaR of around USD
6,500. It’s two times as the VaR of the hedge fund portfolio.

**3. Fixed-income securities and stocks**

Some say we should diversify the portfolio by investing in different
types of security to make it less volatile. We created a portfolio that
consists of three stock ETFs and two fixed-income ETFs. The VaR is about
the same level as the real-estate portfolio.

### set library and functions

``` r
library(mosaic)
library(quantmod)
library(foreach)

get_allreturn <- function(usesymbols){
  # get 5-years data
  for( i in c(1:length(usesymbols))){ 
    usesymbols[[i]] = usesymbols[[i]][index(usesymbols[[i]]) >= startdate]
  }
  
  # for( i in c(1:length(usesymbols))){ 
  #   print(head(usesymbols[[i]])) 
  # }
  
  # Combine close to close changes in a single matrix
  all_returns = NULL
  for( i in c(1:length(usesymbols))){ 
    if(is.null(all_returns)){
      all_returns = ClCl(usesymbols[[i]])
    } else {
      all_returns = cbind(all_returns, ClCl(usesymbols[[i]]))
    }
  }
  names(all_returns) = paste0(usesymbolsnames, '.ClCl')
  all_returns = as.matrix(na.omit(all_returns))
  
  return(all_returns)
}

get_sim1 <- function(all_returns, 
                     initial_wealth = 10000, 
                     nsim=5000, 
                     weights = c(0.2, 0.2, 0.2, 0.2, 0.2), 
                     n_days = 20)
{
  sim1 = foreach(i=1:nsim, .combine='rbind') %do% {
    total_wealth = initial_wealth
    holdings = weights * total_wealth
    wealthtracker = rep(0, n_days)
    for(today in 1:n_days) {
        return.today = resample(all_returns, 1, orig.ids=FALSE)
        holdings = holdings + holdings*return.today
        total_wealth = sum(holdings)
        wealthtracker[today] = total_wealth
        holdings = weights * total_wealth
    }
    wealthtracker
  }
  return(sim1)
}
```

### Hedge fund

**input symbols**

``` r
#input#######################
startdate = as.Date('2014-8-17')
mystocks = c("MNA", "FVC", "WTMF", "PUTW", "GMOM", "RLY", "BEMO", "CPI", "JPMF", "RYZZ")
##############################

getSymbols(mystocks)
```

    ## Warning: FVC contains missing values. Some functions will not work if
    ## objects contain missing values in the middle of the series. Consider using
    ## na.omit(), na.approx(), na.fill(), etc to remove or replace them.

    ## Warning: BEMO contains missing values. Some functions will not work if
    ## objects contain missing values in the middle of the series. Consider using
    ## na.omit(), na.approx(), na.fill(), etc to remove or replace them.

    ##  [1] "MNA"  "FVC"  "WTMF" "PUTW" "GMOM" "RLY"  "BEMO" "CPI"  "JPMF" "RYZZ"

``` r
#input######################################
mysymbols = list(MNA, FVC, WTMF, PUTW, GMOM, RLY, BEMO, CPI, JPMF, RYZZ)
############################################

# don't know how to append element if list, just pick symbols mannually
for(i in c(1:length(mysymbols))){
  print( paste(mystocks[[i]], min(index(mysymbols[[i]])) <= startdate))
}
```

    ## [1] "MNA TRUE"
    ## [1] "FVC TRUE"
    ## [1] "WTMF TRUE"
    ## [1] "PUTW FALSE"
    ## [1] "GMOM FALSE"
    ## [1] "RLY TRUE"
    ## [1] "BEMO TRUE"
    ## [1] "CPI TRUE"
    ## [1] "JPMF FALSE"
    ## [1] "RYZZ FALSE"

**decide symbols**

``` r
# pick symbols mannually ################################
usesymbols = list(MNA, FVC, WTMF, RLY, CPI)
usesymbolsnames = list("MNA", "FVC", "WTMF", "RLY", "CPI")
##########################################################
```

**pre-process data, ClCl, take a look**

``` r
all_returns = get_allreturn(usesymbols)

# check if anything weried
head(all_returns)
```

    ##                 MNA.ClCl     FVC.ClCl     WTMF.ClCl      RLY.ClCl
    ## 2014-08-19 -0.0007278384  0.051428571 -0.0023871569  0.0013148915
    ## 2014-08-20  0.0025492353 -0.021739130  0.0045465182  0.0003282994
    ## 2014-08-21 -0.0010897566 -0.005555556 -0.0004764412  0.0000000000
    ## 2014-08-22 -0.0003636364  0.005586592  0.0009533127 -0.0042664590
    ## 2014-08-25 -0.0040014915  0.016666667  0.0021428571  0.0023071852
    ## 2014-08-26  0.0010957268  0.049180328  0.0000000000  0.0023018744
    ##                 CPI.ClCl
    ## 2014-08-19  0.0000000000
    ## 2014-08-20  0.0007489887
    ## 2014-08-21  0.0007485404
    ## 2014-08-22  0.0000000000
    ## 2014-08-25 -0.0014958489
    ## 2014-08-26  0.0003744569

``` r
pairs(all_returns)
```

![](portfolio_ver2_files/figure-markdown_github/unnamed-chunk-4-1.png)

**simulation**

``` r
initial_wealth = 100000
n_days = 20
sim1 = get_sim1(all_returns=all_returns, initial_wealth = initial_wealth, n_days=n_days)
# Profit/loss
mean(sim1[,n_days])
```

    ## [1] 100387.4

``` r
hist(sim1[,n_days]- initial_wealth, breaks=30)
```

![](portfolio_ver2_files/figure-markdown_github/unnamed-chunk-5-1.png)

``` r
quantile(sim1[,n_days]- initial_wealth, .05)
```

    ##        5% 
    ## -3126.905

### Real Estate

**input symbols**

``` r
#input#######################
startdate = as.Date('2014-8-17')
mystocks = c("VNQ", "SCHH", "IYR",  "XLRE", "RWR",  "ICF",  "USRT", "REM",  "FREL", "REZ",  "BBRE")
##############################

getSymbols(mystocks)
```

    ## Warning: FREL contains missing values. Some functions will not work if
    ## objects contain missing values in the middle of the series. Consider using
    ## na.omit(), na.approx(), na.fill(), etc to remove or replace them.

    ##  [1] "VNQ"  "SCHH" "IYR"  "XLRE" "RWR"  "ICF"  "USRT" "REM"  "FREL" "REZ" 
    ## [11] "BBRE"

``` r
#input######################################
mysymbols = list(VNQ,   SCHH,   IYR,    XLRE,   RWR,    ICF,    USRT,   REM,    FREL,   REZ,    BBRE)
############################################

# don't know how to append element if list, just pick symbols mannually
for(i in c(1:length(mysymbols))){
  print( paste(mystocks[[i]], min(index(mysymbols[[i]])) <= startdate))
}
```

    ## [1] "VNQ TRUE"
    ## [1] "SCHH TRUE"
    ## [1] "IYR TRUE"
    ## [1] "XLRE FALSE"
    ## [1] "RWR TRUE"
    ## [1] "ICF TRUE"
    ## [1] "USRT TRUE"
    ## [1] "REM TRUE"
    ## [1] "FREL FALSE"
    ## [1] "REZ TRUE"
    ## [1] "BBRE FALSE"

**decide symbols**

``` r
# pick symbols mannually ################################
usesymbols = list(VNQ, SCHH, IYR, RWR, ICF)
usesymbolsnames = list("VNQ", "SCHH", "IYR", "RWR", "ICF")
##########################################################
```

**pre-process data, ClCl, take a look**

``` r
all_returns = get_allreturn(usesymbols)

# check if anything weried
head(all_returns)
```

    ##                VNQ.ClCl    SCHH.ClCl      IYR.ClCl     RWR.ClCl
    ## 2014-08-19  0.002078410  0.002754035  0.0008112223  0.002462176
    ## 2014-08-20  0.004537192  0.004394397  0.0048635506  0.004093544
    ## 2014-08-21 -0.001548522 -0.001367241 -0.0009410998 -0.001747245
    ## 2014-08-22 -0.008013480 -0.007940854 -0.0067285697 -0.008518040
    ## 2014-08-25 -0.002214958 -0.002208060 -0.0004064355 -0.001765341
    ## 2014-08-26  0.001958749  0.001106418  0.0000000000  0.001650542
    ##                 ICF.ClCl
    ## 2014-08-19  0.0021052853
    ## 2014-08-20  0.0046439517
    ## 2014-08-21 -0.0014307506
    ## 2014-08-22 -0.0085970018
    ## 2014-08-25 -0.0016675265
    ## 2014-08-26  0.0002226837

``` r
pairs(all_returns)
```

![](portfolio_ver2_files/figure-markdown_github/unnamed-chunk-8-1.png)

``` r
initial_wealth = 100000
n_days = 20
sim1 = get_sim1(all_returns=all_returns, initial_wealth = initial_wealth, n_days=n_days)
# Profit/loss
mean(sim1[,n_days])
```

    ## [1] 100396.6

``` r
hist(sim1[,n_days]- initial_wealth, breaks=30)
```

![](portfolio_ver2_files/figure-markdown_github/unnamed-chunk-9-1.png)

``` r
quantile(sim1[,n_days]- initial_wealth, .05)
```

    ##        5% 
    ## -6469.763

### Stocks + fixed income

**options from etfs from All Cap Equities ETFs**

IWR IWS VXF ARKK TILT

**options from Government Bonds ETFs**

SHV IEF SHY TLT GOVT

**make stock to fixed-income ratio as 3:2**

``` r
#input#######################
startdate = as.Date('2014-8-17')
mystocks = c("IWR", "IWS",  "VXF",  "ARKK", "TILT", "SHV",  "IEF",  "SHY",  "TLT",  "GOVT")
##############################

getSymbols(mystocks)
```

    ## Warning: TILT contains missing values. Some functions will not work if
    ## objects contain missing values in the middle of the series. Consider using
    ## na.omit(), na.approx(), na.fill(), etc to remove or replace them.

    ##  [1] "IWR"  "IWS"  "VXF"  "ARKK" "TILT" "SHV"  "IEF"  "SHY"  "TLT"  "GOVT"

``` r
#input######################################
mysymbols = list(IWR,   IWS,    VXF,    ARKK,   TILT, SHV,  IEF,    SHY,    TLT,    GOVT)
############################################

# don't know how to append element if list, just pick symbols mannually
for(i in c(1:length(mysymbols))){
  print( paste(mystocks[[i]], min(index(mysymbols[[i]])) <= startdate))
}
```

    ## [1] "IWR TRUE"
    ## [1] "IWS TRUE"
    ## [1] "VXF TRUE"
    ## [1] "ARKK FALSE"
    ## [1] "TILT TRUE"
    ## [1] "SHV TRUE"
    ## [1] "IEF TRUE"
    ## [1] "SHY TRUE"
    ## [1] "TLT TRUE"
    ## [1] "GOVT TRUE"

**decide symbols**

``` r
# pick symbols mannually ################################
usesymbols = list(IWR, IWS, VXF, SHV, IEF)
usesymbolsnames = list("IWR", "IWS", "VXF", "SHV", "IEF")
##########################################################
```

**pre-process data, ClCl, take a look**

``` r
all_returns = get_allreturn(usesymbols)

# check if anything weried
head(all_returns)
```

    ##                 IWR.ClCl      IWS.ClCl     VXF.ClCl      SHV.ClCl
    ## 2014-08-19  0.0050201175  0.0048767732 0.0038106467  0.000000e+00
    ## 2014-08-20  0.0020966699  0.0029117997 0.0006901875  0.000000e+00
    ## 2014-08-21  0.0024614892  0.0024885939 0.0008046902  9.070464e-05
    ## 2014-08-22 -0.0006137999 -0.0026203557 0.0000000000  0.000000e+00
    ## 2014-08-25  0.0044840048  0.0037333518 0.0044796578 -9.069641e-05
    ## 2014-08-26  0.0009172140  0.0008266428 0.0044596800 -9.064116e-05
    ##                 IEF.ClCl
    ## 2014-08-19 -1.051659e-03
    ## 2014-08-20 -2.967123e-03
    ## 2014-08-21  2.111942e-03
    ## 2014-08-22  9.582336e-05
    ## 2014-08-25  7.662931e-04
    ## 2014-08-26 -1.914625e-04

``` r
pairs(all_returns)
```

![](portfolio_ver2_files/figure-markdown_github/unnamed-chunk-12-1.png)

``` r
initial_wealth = 100000
n_days = 20
sim1 = get_sim1(all_returns=all_returns, initial_wealth = initial_wealth, n_days=n_days)
# Profit/loss
mean(sim1[,n_days])
```

    ## [1] 100318.9

``` r
hist(sim1[,n_days]- initial_wealth, breaks=30)
```

![](portfolio_ver2_files/figure-markdown_github/unnamed-chunk-13-1.png)

``` r
quantile(sim1[,n_days]- initial_wealth, .05)
```

    ##        5% 
    ## -3459.421
