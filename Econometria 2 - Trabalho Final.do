use "E:\Trabalho de Econometria 2\Dados do trabalho\Econometria 2 - Trabalho FInal.dta"
generate t=ym(ano, mês)
format t %tm
tsset t
set more off

gen libov=log(ibov)
gen lbr20=log(br20)

*GERANDO CRITERIO DE Newey-West 

*Tamanho da amosta T=120 para o teste ADF

scalar T=120
scalar defNW=abs(4*((T/100)^(2/9)))
disp defNW 

*Gerando critério  DE SCHWERT

scalar defSW=abs(12*((T/100)^(1/4)))
disp defSW

*Teste de raiz unitária dickey fuller 
*MODELO M3

dfuller libov, lags(4)trend regress
regress D.libov L.libov L.d.libov L2.d.libov L3.d.libov L4.d.libov t
estimates store m3
scalar sqrm3=e(rss)
predict resm3,residuals
corrgram resm3, lags(12)

*MODELO M2

dfuller libov, lags(4) drift regress
regress D.libov L.libov L.d.libov L2.d.libov L3.d.libov L4.d.libov
estimates store m2
scalar sqrm2=e(rss)
predict resm2,residuals
corrgram resm2, lags(12)

*Calculando phi3

corrgram libov

*Tamanho da amosta T=120

scalar T=120
scalar J=2

scalar phi3=((sqrm2-sqrm3)/J)/(sqrm3/(T-7))

display phi3

*ANALISAR O RESULTADO PARA O PHI TABELADO DICKEY FULLER(1981)
*PHI3 TABELADO ENTRE 6.34<PHI3<6.49 PARA ALPHA DE 5% 
*SEM DRIFT E TREND
*MODELO M1

dfuller libov, lags(4) noconstant regress
regress D.libov L.libov L.d.libov L2.d.libov L3.d.libov L4.d.libov, noconstant
estimates store m1
scalar sqrm1=e(rss)
predict resm1,residuals
corrgram resm1, lags(12)

*CALCULANDO PHI1

scalar J=2

scalar phi1=((sqrm1-sqrm2)/J)/(sqrm2/(T-6))

display phi1

*ANALISAR O RESULTADO PARA O PHI TABELADO DICKEY FULLER(1981)
*PHI3 TABELADO ENTRE 4.63<PHI3<4.71 PARA ALPHA DE 5% 

*CALCULANDO PHI2

scalar J2=3

scalar phi2=((sqrm1-sqrm3)/J)/(sqrm3/(T-7))

display phi2

*ANALISAR O RESULTADO PARA O PHI TABELADO DICKEY FULLER(1981)
*PHI2 TABELADO ENTRE 4.75<PHI3<4.88 PARA ALPHA DE 5%

*TESTE KPPS

kpss libov, notrend
kpss lbr20, notrend

**Teste para libov**
*SERIE libov COM DRIFT E TREND

dfuller libov, lags(4)trend regress
regress d.libov l.libov t

*COM DRIFT

dfuller libov, lags(4) drift regress
regress d.libov l.libov

*SEM DRIFT E TREND

dfuller libov, lags(4) noconstant regress
regress d.libov l.libov, noconstant

*Teste para lbr20
*SERIE lbr20 COM DRIFT E TREND

dfuller lbr20, lags(4)trend regress
regress d.lbr20 l.lbr20 t

*COM DRIFT

dfuller lbr20, lags(4) drift regress
regress d.lbr20 l.lbr20

*SEM DRIFT E TREND

dfuller lbr20, lags(4) noconstant regress
regress d.lbr20 l.lbr20, noconstant

**TESTES DE RAIZ UNITÁRIAS - ADF PARA A PRIMEIRA DIFERENÇA DE YT**

dfuller d.libov, lags(4)trend regress
dfuller d.lbr20, lags(4)trend regress

dfuller d.libov, lags(4)drift regress
dfuller d.lbr20, lags(4)drift regress

dfuller d.libov, lags(4)noconstant regress
dfuller d.lbr20, lags(4)noconstant regress

*GERANDO CRITERIO DE Newey-West e de SHWERT
*Tamanho da amosta T=120

scalar def=abs(4*((120/100)^(2/9)))
scalar def1=abs(12*((120/100)^(1/4)))
disp def 
disp def1

varsoc libov lbr20, maxlag(12)

********************TESTES DE COINTEGRAÇÃO DE JOHANSEN****************************
*********************************************************************************

*TESTE DE COINTEGRAÇÃO DO TRAÇO e RAIZ MAXIMA 1 DEFASAGEM

 set more off
 vecrank libov lbr20, trend(trend) lags(1) max
 vecrank libov lbr20, trend(const) lags(1) max
 vecrank libov lbr20, trend(none) lags(1) max
 
*ESTIMANDO MODELOS VAR*

var d.libov d.lbr20,  lags(1)
varstable
varnorm
vargranger
varlmar, mlag(12)

var d.libov d.lbr20,  lags(2)
varstable
varnorm
vargranger
varlmar, mlag(12)

 **********************************************************************************
*******************TESTES DE CAUSALIDADE DE GRANGER - VARGRANGER********************
 **********************************************************************************
 
adoupdate package sts14 from http://www.stata.com/stb/stb51

granger d.libov d.lbr20, lag(1)
granger d.lbr20 d.libov, lag(1)

global ordem1 d.libov d.lbr20
var $ordem1, lag(2)
varlmar, mlag(12)
varstable
varnorm
varstable 

irf create ordem1, step(12) set(E:\Trabalho de Econometria 2\Dados do trabalho\irf1, replace)

*Choque da variável lbr20 na libov

irf table irf cirf, irf(ordem1) impulse(d.lbr20) response(d.libov) individual 
 

*Decomposição da variância do erro de previsão

irf table fevd
