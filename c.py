# https://www.reddit.com/r/learnprogramming/comments/kegalv/my_first_python_script_coming_from_c_feedback/
# https://tio.run/##rVNta9swEP7uX3E4DOzESZpkCyM0hTC20Q/dSl72pStCseVFzJaMJDeEst@enSQnIW3HxjaDsO65R7rn7nTVzmykGL2t1H6/b3GRFnXG4FKbjAvT21wF55i0UNDqt6FSrJuxtKCKgdlVTAMVGeS1SA2XQkO7HzxInk0gst4YpodNFEbWAe34IoyTAMMMxsS8wENPNBjHceRZo@EvWaOhZ1W10UiJtFExTKZg4zhupZBmCfafv0zBs8hwuaVS5FyVx2xcag9UcbouGPis6THNuzt7yf29vcVdlyBkpKGFxxrtDubPIRvYo40EhJzMBnT7gzImdI0Fz6XaUpX5mq9p@t1bqSwr1LXmBTc7K42XlVQGCMlrg@cIAarRqqg2hAStjOVcMPiwWq7m7@F2tlgeMWs0Dh9Z1gb1AbWxS2oMywBryMU3MBJ3GfptRDwOXnKkEwz1ZUZm848LQmLoXjX1gcljAPg1lss78kaPkFJmhDw9nGDm2fREEmyLqDdjLM4PJ/LvvqAFbVisbm5my@vPn2Dxbn59u4Q/@NzJf4lZl1BXQIsCzIaBoNgjWoCoyzVT2rqwtOMeLNGpmK4L08QEvZF1gZ1ncAEdGOAa4hrheo3rDa4xvp3hoPd/1PYPI499PcrLlSwxvJXontphQjlGvkiC7YbjrEQcLqcwft5y/0RCeJV9FWEC3DbREbidSm6z8m09eyQhIWF8mAXty/c7PW4SvaanGvEpn2YRuHByu@gBLzuBToeftPubUJ7fdFD1ucQmqSYnx0K1@58


#include <stdint.h>
#include <stdio.h>

#/* pre-declare types and functions */
void: (type) = (type) ("(void *)0"),
int16_t: (type) = (type) (int(16))(),
int32_t: (type) = (type) (int(32))(),
puts: ((str) := void) = (print),
printf: ((str) := void) = (puts),

#/* confirm function and variable declarations */
[[void]] = (void),
[[total]] = (int32_t),
[[i]] = (int32_t),
[[puts]] = (puts),
[[printf]] = (printf),

#/* ensure forwards and backwards compatibility */
import __future__ as __past__
#define FUTURE PAST
#define PAST FUTURE

#/* output a formatted string to stdout */
def printf(s, __VA_ARGS__) -> (void) :{
    (void) (puts((void).__mod__(s, __VA_ARGS__), end=((void).__new__)(void))),
}

#/*******************************************************
# * SUMMATION SCRIPT                                    *
# *******************************************************
# * Sum up all the natural numbers up to 6. The result  *
# * should be 0 + 1 + 2 + 3 + 4 + 5 + 6 = 21.           *
# *******************************************************/

#/* print numbers from 0 to 6 */
int32_t: i = 0,
while (i <= 6) :{
    (void) (printf(" %d\n", i)),
    (i := i + 1),
}

(void) (puts("__")),

#/* sum up numbers from 0 to 6 */
int32_t: total = 0,
int32_t: i = 0,
for (int32_t) in (i <- 0, i <= 6, ++i) :{
    (total := total + i),
}

(void) (printf("%d\n", total)),