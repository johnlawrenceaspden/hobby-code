;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A Simple Explanation Of Public Key Encryption With Code

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; I'm no cryptographer! I've known about the RSA method for years, but as a rather
;; abstract piece of mathematics. On the basis that if you understand something, you
;; can program it, I sat down to try to explain it to myself this morning.

;; I hope the code might be useful to other people who get the heebie-jeebies
;; at the phrase "Euler's totient".

;; I'm completely avoiding proofs, but trying to explain how RSA works through
;; explaining how to implement it.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Caesar's Cipher

;; Julius Caesar encrypted his messages thus:

(def alphabet (take 26 (map char (iterate inc 0x61))))
(def shift 3)
(def permutation (concat (drop shift alphabet) (take shift alphabet)))
(def caesar-function (apply sorted-map (interleave alphabet permutation)))
(def caesar-inverse  (apply sorted-map (interleave permutation alphabet)))

;; To encrypt:
(apply str (map caesar-function "helloclassicalworld")) ;; "khoorfodvvlfdozruog"

;; To decrypt:
(apply str (map caesar-inverse  "khoorfodvvlfdozruog")) ;; "helloclassicalworld"

;; This cipher, whilst robust against the cryptanalytic talents of the
;; Transalpine Gauls, leaves something to be desired in a modern setting.

;; Of particular concern is the the need to communicate the cipher key (in this
;; case 3) secretly in some way prior to the sending of the message.

;; The key allows one to calculate both the encryption function and the
;; decryption function, as demonstrated above.

;; Caesar's generals would have met up early in the campaign to discuss the
;; cipher they were going to use.

;; Caesar's cipher allows us to see certain basic principles of encryption.

;; All information can be treated as sequences of numbers.  The numbers can be
;; obscured by applying a function to them.

;; To read an encrypted message, calculate the inverse function, and apply it to
;; the message.

;; The problem with Caesar's cipher, and other more modern ciphers like it, is
;; that by telling someone the encryption function, you've also told them the
;; decryption function.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; In 1970, a method was invented that allowed people to communicate the
;; encryption function without giving away the decryption function.

;; It turns out that powers of numbers modulo other numbers are very easy to
;; calculate even by hand. Here's a method that is fast.

(defn square [n] (* n n))

(defn power-modulo [a n m]
  (mod (cond
        (= n 0) 1
        (= n 1) a
        (odd? n) (* a (power-modulo a (dec n) m))
        :else (square (power-modulo a (/ n 2) m)))
       m))

;; It doesn't take many steps to calculate 7^5 mod 247 
(power-modulo 7 5 247) ;; 11
;; But neither does it take many step to calculate 11^29 mod 247
(power-modulo 11 29 247) ;; 7
;; Or 243^229 mod 247
(power-modulo 243 229 247) ;; 48

;; If we fix n and m, then we can make a function very like Caesar's encryption function:
(defn make-cipher[n m] (fn[a] (power-modulo a n m)))

(def cipher-5-247 (make-cipher 5 247))

;; So we could use x -> x^5 mod 247 in the same way as Caesar:
(def encrypted-message (map cipher-5-247 (map int "hellomodernworld")))
;; (130 43 166 166 232 200 232 237 43 95 2 123 232 95 166 237)

;; But what on earth is the inverse function to get the message back?

;; Well, as it happens, it's another, similar function.
;; Something like (make-cipher b 247) where the problem is to find b

;; We can crack this problem fairly easily:
;; If we guess at b, what will the combination of ciphering and unciphering look like?
(defn cipher-uncipher [b]
  (fn [x] ((make-cipher b 247) (cipher-5-247 x))))

;; This function takes a range of bs, and tries to encrypt and decrypt the single
;; number x, to see if it comes back to itself. It throws away any potential bs
;; where this doesn't happen
(defn sieve [rnge x]
  (filter #(= ((cipher-uncipher %) x) x) rnge))

;; Let's try all b from 0 to 247
(sieve (range 247) 1) ;;Irritatingly 1 always comes back to 1
;; But we get luckier with 2. 2 only comes back to 2 with a few bs in our range.
(sieve (sieve (range 247) 1) 2) ;; (29 65 101 137 173 209 245)

;; Let's try 29

(def cipher-29-247 (make-cipher 29 247))
(apply str (map char (map cipher-29-247 encrypted-message)))
;; And we appear to have found our decryption method!

;; Let's just check that cipher-29-247 and cipher 5-247 really are inverses.
(= (map cipher-29-247 (map cipher-5-247 (range 247)))
   (range 247))

;; Now the thing is, although it can be done, there's quite a lot of computation
;; involved in finding the inverse cipher.

;; It's not at all obvious that power 5 is the inverse of power 29 in arithmetic mod 247
;; Even though we can work it out by trying loads of alternatives.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; But actually, if you can factorize 247, it's easy to calculate inverse
;; functions for powers mod 247

;; 247 is (* 13 19), and using that, we can calculate a magic number

(def magic-number-for-247 (* (dec 13) (dec 19))) ;;216

;; We can find a number which is inverse to 5 modulo the magic-number
(filter #(= 1 (mod (* 5 %) magic-number-for-247)) (range 247)) ;; 173

;; Check that 173 is inverse to 5 mod 216
(mod (* 5 173) 216) ;; 1

;; And that proves that cipher-173-247 is inverse to cipher-5-247. Let's try it.
(def cipher-173-247 (make-cipher 173 247))

(=
 (range 247)
 (map (comp cipher-173-247 cipher-5-247) (range 247))) ;; true

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Now, (make-cipher 5 247) is not a very strong method of encryption.

;; But if we can find larger prime numbers to use, then we can make better ciphers.

;; Getting a couple of large primes is easy:
;; Here's a way to get some four-digit ones
;; (require 'clojure.contrib.lazy-seqs)
;; (take 2 (drop 1000 clojure.contrib.lazy-seqs/primes))

;; Let's take 7927 and 7933. Again, they're not nearly large enough to be practically
;; useful, but they are large enough to make easy cracks more difficult.

;; Multiply them to get the modulus for the cipher
(def modulus (* 7927 7933)) ;; 62884891

;; We just choose an encrypting power at random, say 203
(def encipher (make-cipher 203 modulus))

;; Calculating the magic number is easy:
(def magic-number (* (dec 7927) (dec 7933))) ;; 62869032

;; Now to find the inverse function, we need a number a such that (* a 203) = 1 mod 62869032
;; These are easy to find
(take 3 (filter #(= (int %) %) (map #(/ % 203) (iterate #(+ % magic-number) 1)))) ;;(929099 63798131 126667163 .....)
;; Let's take the first one

;; Check that it does what we want:
(mod (* 203 929099) 62869032) ;; 1

;; Now we can create the deciphering function:
(def decipher (make-cipher 929099 modulus))
;; Now, notice that it has taken almost no computational power to create this cipher.
;; You could do this easily by hand.

;; Encryption and decryption would be hard by hand, but possible due to the extreme efficiency of the
;; algorithm in the power-modulo function.

;; With a computer, it is easy:
(def secret-message (map encipher (map int "darling world, we must meet soon...")))
;; (48146327 60382173 57991209 5919317 9620507 44143496 27738744 52452577
;; 2896855 2177970 57991209 5919317 48146327 54215221 52452577 2896855 8841709
;; 52452577 38496398 16747575 44075818 60012204 52452577 38496398 8841709
;; 8841709 60012204 52452577 44075818 2177970 2177970 44143496 37504468 37504468
;; 37504468)

(map decipher secret-message)
;; (100 97 114 108 105 110 103 32 119 111 114 108 100 44 32 119 101 32 109 117
;; 115 116 32 109 101 101 116 32 115 111 111 110 46 46 46)

(apply str (map char (map decipher secret-message)))
;;"darling world, we must meet soon..."

;; But just verifying that the cipher works over its whole range 62884891 is time consuming
(def test-cipher (comp decipher encipher))

(time (= (range 1000) (map test-cipher (range 1000))))
"Elapsed time: 115.158749 msecs"
;; The time to check the whole thing would be about 10 minutes!
;; It would be utterly impossible by hand.

;; The cracking methods also suffer:
(defn cipher-uncipher [b]
  (fn [x] ((make-cipher b modulus) (encipher x))))

(defn sieve [rnge x]
  (filter #(= ((cipher-uncipher %) x) x) rnge))

(time (doall (sieve (sieve (range 10000) 1) 2))) ;;()
"Elapsed time: 1246.92162 msecs"

;; In one second, we have checked 10000 possibilities and not found an answer

;; If we were to sieve a much larger range, we would find an answer, but it would take ages
;; even by computer

;; (time (doall (sieve (sieve (range 1000000) 1) 2)))
;; "Elapsed time: 170559.809623 msecs"
;; (929099)

;; It does work eventually, but it would have been a lot easier to factorize
;; 62884891 and then work out the inverse in the same way that the generator did.

;; Finding very large prime numbers is very easy. Factorising their products is very hard.

;; If we choose gigantic primes then the only methods known of finding the
;; inverse functions involve factorising very large numbers which are the
;; products of two very large primes.

;; This is thought to be computationally infeasible once the number of digits in the primes
;; goes above about 1000.

;; This is the basis of public key encryption.  It is very easy to create these
;; ciphers, fairly easy to use them to encrypt, and very very hard indeed to
;; invert them.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Footnotes


;; This algorithm was discovered by Clifford Cocks, a mathematician working for
;; British Intelligence in 1973.

;; However, in 1973 it would not have been terribly practical to use on the
;; hardware of the time and his work was considered a curiosity by the agency.

;; It was nevertheless classified top secret, preventing him from publishing the
;; scheme.

;; It was independently discovered by Rivest, Shamir, and Adleman at MIT in
;; 1978, and is today universally known as the RSA algorithm.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; We've left a big hole in our encryption method!

;; So long as we're only enciphering the ASCII codes directly, we don't actually
;; need to invert the cipher. Just a lookup table of known characters will do.

(def sneaky (apply sorted-map (interleave
                               (map encipher (map int alphabet))
                               (map int alphabet))))

(apply str (map char (filter #(not (nil? %))
                             (map sneaky
                                  '(48146327 60382173 57991209 5919317 9620507
                                             44143496 27738744 52452577 2896855
                                             2177970 57991209 5919317 48146327 54215221
                                             52452577 2896855 8841709 52452577 38496398
                                             16747575 44075818 60012204 52452577 38496398
                                             8841709 8841709 60012204 52452577
                                             44075818 2177970 2177970 44143496)))))
;;"darlingworldwemustmeetsoon"

;; And of course this will be a problem with any method where the encryption
;; algorithm is publically known.

;; But if we were using the whole range 0-62884891, then this lookup table would
;; need to be extremely large, and take a long time to generate. Again, it would
;; just be easier to factorize the modulus and calculate the inverse.

;; We must ensure we're using the whole range by compressing the data before
;; encrypting it.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Even with compression, RSA is dubious as method for transmitting
;; messages. It's quite slow for the sorts of very large primes used in
;; practice, and it's vulnerable to various known plaintext attacks,
;; chosen-ciphertext attacks, etc.

;; However, it's perfect for swapping cipher keys for other forms of encryption.

;; This means that two people who have no opportunity to communicate securely
;; with each other can securely swap cipher keys, and from then on, they can use
;; a better suited coding method to communicate securely.

;; Something from nothing!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The security of RSA rests on the problem of factoring large numbers, which is thought to be
;; very difficult for a classical computer.

;; However, it is known to be very easy for a quantum computer, using Shor's algorithm.
;; Quantum computation is in its infancy, but a quantum computer has been built which 
;; used Shor's algorithm to factor the number fifteen.

;; If it turns out that quantum computers capable of handling 1000 digit numbers can be built,
;; then RSA will be blown wide open.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; I seem to have found a way to blow stack

(reduce sieve '(929099) (range 1 1000)) ;;fails

(reduce sieve '(929099) (range 1 250)) ;; all work individually
(reduce sieve '(929099) (range 250 500))
(reduce sieve '(929099) (range 500 750))
(reduce sieve '(929099) (range 750 1000))

;; I don't understand why this should be happening.







