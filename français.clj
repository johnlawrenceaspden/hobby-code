;; URL Encoding
;; http://stackoverflow.com/questions/3644125/clojure-building-of-url-from-constituent-parts"

(import [java.net URLEncoder])

(URLEncoder/encode "un.url.étrange.et.diffiçile")
(URLEncoder/encode "space separated")


(str (URLEncoder/encode "http://un.url.étrange.et.diffiçile/cherche?q=")
     (URLEncoder/encode "hello world"))

"http%3A%2F%2Fun.url.%C3%A9trange.et.diffi%C3%A7ile%2Fcherche%3Fq%3Dhello+world"