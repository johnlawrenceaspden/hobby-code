;; A short clojure program to do AES encryption/decryption through the java libraries

(import [javax.crypto KeyGenerator SecretKey Cipher]
        [javax.crypto.spec SecretKeySpec] )


(defn print-bytes[byteseq] ;; can you believe that java's bytes are signed... sigh....
  (map #(Integer/toHexString (bit-and % 0xff)) byteseq ))

;; generate 256 bit secret key in the usual spazzy java style
(def kg-aes (. KeyGenerator getInstance "AES"))
(. kg-aes init 256)
(def sks (. kg-aes generateKey))

(print-bytes (. sks getEncoded))

;; Make something called a secret key spec. I really don't understand what this is for
(def skeyspec (SecretKeySpec. (. sks getEncoded) "AES"))


(def plaintext (. "01223 This is an example" getBytes))

(print-bytes plaintext)

;; Get an AES cipher 'object'
(def encrypt-cipher ( . Cipher getInstance "AES"))
(. encrypt-cipher init  Cipher/ENCRYPT_MODE skeyspec)

;; do the encryption
(def encrypted (. encrypt-cipher doFinal plaintext))

(print-bytes encrypted)

;; Make a decrypter object
(def decrypt-cipher (. Cipher getInstance "AES"))
(. decrypt-cipher init  Cipher/DECRYPT_MODE skeyspec)

;; Do the decryption
(def decrypted-text (. decrypt-cipher doFinal encrypted))

(print-bytes decrypted-text)

(String. decrypted-text )


