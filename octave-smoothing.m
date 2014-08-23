# Octave doesn't have MATLAB's smooth function, but if you install octave-forge's signal toolbox
# then it has a function to do Savitsky-Golay smoothing

#  $ sudo apt-get install octave-signal

# square wave
x = [zeros(1,15), 10*ones(1,10), zeros(1,15)];

plot(x);

# an order 5 butterworth filter
[b, a] = butter (5, 1/3);
butterworth=filtfilt (b, a, x);

plot (sgolayfilt (x), "r;sgolayfilt;", ...
                  filtfilt (ones (1,5)/5, 1, x), "g;5 sample average;", ...
                  butterworth, "c;order 5 butterworth;", ...
                  x, "+b;original data;");


 
