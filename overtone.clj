to install overtone
pom.xml with clojars and clojure 1.3 but not contrib, also overtone

$ sudo apt-get install jack-tools ant openjdk-6-jdk fftw3 qjackctl
$ jackd -r -d alsa -r 44100


    <dependency>
      <groupId>org.clojure</groupId>
      <artifactId>clojure</artifactId>
      <version>1.3.0</version>
    </dependency>

    <dependency>
      <groupId>overtone</groupId>
      <artifactId>overtone</artifactId>
      <version>0.5.0</version>
    </dependency>
