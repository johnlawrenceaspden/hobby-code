open arithmeticTheory listTheory;

`!n . n < n+1`

DECIDE_TAC

val LESS_ADD_1 = prove (`` !n . n < n + 1``, DECIDE_TAC);

`!n . n < n`

DECIDE_TAC


Term(`!n . n < n+1`);

g(`!n . n < n+1`);

M-h g : wraps thing in g

`!n . n < n+1` <-put cursor in quotes and press M-h g


e(DECIDE_TAC); <- select fluffy and press M-h M-r

drop();  <- sfapMhMr